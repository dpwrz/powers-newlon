#!/usr/bin/env python3
import csv, json, math, os, re, time, unicodedata, urllib.parse, urllib.request
from collections import defaultdict
from datetime import datetime, timezone

SEASON = int(os.getenv("BENCHMARK_SEASON", "2026"))
WEEK = int(os.getenv("BENCHMARK_WEEK", "4"))
POSITIONS = ("QB","RB","WR","TE")
STARTER = {"QB":12,"RB":24,"WR":36,"TE":12}
RELEVANT = {"QB":24,"RB":36,"WR":48,"TE":24}
TOPN = {"QB":12,"RB":24,"WR":24,"TE":12}
OUT_PREFIX = f"output/competitor_benchmark_week_{WEEK}_{SEASON}"

def num(x, default=float("nan")):
    try:
        v=float(x)
        return v if math.isfinite(v) else default
    except Exception:
        return default

def norm_name(x):
    s=unicodedata.normalize("NFKD", str(x or "")).encode("ascii","ignore").decode().lower()
    s=re.sub(r"\b(jr|sr|ii|iii|iv|v)\b","",s)
    return re.sub(r"[^a-z0-9]","",s)

def http_json(url, headers=None, timeout=45):
    req=urllib.request.Request(url, headers={"User-Agent":"FantasyModel/3.1 competitor-benchmark", **(headers or {})})
    with urllib.request.urlopen(req, timeout=timeout) as r:
        return json.loads(r.read().decode("utf-8"))

def read_csv(path):
    with open(path, newline="", encoding="utf-8-sig") as f:
        return list(csv.DictReader(f))

def write_csv(path, rows, fields=None):
    os.makedirs(os.path.dirname(path), exist_ok=True)
    if fields is None:
        fields=[]
        for r in rows:
            for k in r:
                if k not in fields: fields.append(k)
    with open(path,"w",newline="",encoding="utf-8") as f:
        w=csv.DictWriter(f,fieldnames=fields,extrasaction="ignore")
        w.writeheader()
        w.writerows(rows)

def half_ppr(st):
    def g(*keys):
        for k in keys:
            if k in st and st[k] not in (None,""):
                return num(st[k],0.0)
        return 0.0
    return (
        .04*g("pass_yd","passing_yards") + 4*g("pass_td","passing_tds") - 2*g("pass_int","interceptions")
        + 2*g("pass_2pt","pass_2pt_conv")
        + .10*g("rush_yd","rushing_yards") + 6*g("rush_td","rushing_tds") + 2*g("rush_2pt","rush_2pt_conv")
        + .50*g("rec","receptions") + .10*g("rec_yd","receiving_yards") + 6*g("rec_td","receiving_tds")
        + 2*g("rec_2pt","rec_2pt_conv") - 2*g("fum_lost","fumbles_lost")
    )

def rankdata(vals, descending=True):
    n=len(vals); out=[float("nan")]*n
    order=sorted(range(n), key=lambda i: vals[i], reverse=descending)
    i=0
    while i<n:
        j=i+1
        while j<n and vals[order[j]]==vals[order[i]]: j+=1
        rank=(i+1+j)/2.0
        for k in range(i,j): out[order[k]]=rank
        i=j
    return out

def pearson(a,b):
    pairs=[(x,y) for x,y in zip(a,b) if math.isfinite(x) and math.isfinite(y)]
    if len(pairs)<4: return float("nan")
    xs=[p[0] for p in pairs]; ys=[p[1] for p in pairs]
    mx=sum(xs)/len(xs); my=sum(ys)/len(ys)
    sx=sum((x-mx)**2 for x in xs); sy=sum((y-my)**2 for y in ys)
    if sx<=0 or sy<=0: return float("nan")
    return sum((x-mx)*(y-my) for x,y in pairs)/math.sqrt(sx*sy)

def spearman(a,b):
    pairs=[(x,y) for x,y in zip(a,b) if math.isfinite(x) and math.isfinite(y)]
    if len(pairs)<4: return float("nan")
    return pearson(rankdata([p[0] for p in pairs],False), rankdata([p[1] for p in pairs],False))

def mae(actual,pred):
    return sum(abs(a-p) for a,p in zip(actual,pred))/len(actual) if actual else float("nan")
def rmse(actual,pred):
    return math.sqrt(sum((a-p)**2 for a,p in zip(actual,pred))/len(actual)) if actual else float("nan")
def bias(actual,pred):
    return sum(a-p for a,p in zip(actual,pred))/len(actual) if actual else float("nan")

def pair_accuracy(pred_rank, actual_rank, eligible):
    ok=tot=0
    ids=[i for i in eligible if math.isfinite(pred_rank[i]) and math.isfinite(actual_rank[i])]
    for x in range(len(ids)):
        for y in range(x+1,len(ids)):
            i,j=ids[x],ids[y]
            if pred_rank[i]==pred_rank[j] or actual_rank[i]==actual_rank[j]: continue
            tot+=1
            if (pred_rank[i]-pred_rank[j])*(actual_rank[i]-actual_rank[j])>0: ok+=1
    return ok,tot,(ok/tot if tot else float("nan"))

def load_model_rows():
    path=f"output/live_accuracy_player_weeks_{SEASON}.csv"
    rows=read_csv(path)
    out=[]
    for r in rows:
        if int(num(r.get("week"),-1))!=WEEK or r.get("position") not in POSITIONS: continue
        p=num(r.get("projection")); a=num(r.get("actual_fppg"))
        if not (math.isfinite(p) and math.isfinite(a)): continue
        out.append({
            "player_id":r.get("player_id",""), "name":r.get("player_display_name",""),
            "position":r.get("position",""), "team":r.get("team",""),
            "model_projection":p, "actual":a
        })
    return out

def archived_provider(provider):
    path=f"output/benchmark_pregame_archive_{SEASON}.csv"
    if not os.path.exists(path):
        return []
    rows=[]
    for r in read_csv(path):
        if int(num(r.get("week"),-1))!=WEEK or r.get("provider")!=provider:
            continue
        proj=num(r.get("projection"))
        if not math.isfinite(proj):
            continue
        rows.append({
            "provider":provider,
            "provider_id":r.get("sleeper_id",""),
            "player_id":r.get("player_id",""),
            "name":r.get("player_display_name",""),
            "position":r.get("position",""),
            "team":r.get("team",""),
            "projection":proj,
            "source":path
        })
    return rows

def sleeper_players(position):
    try:
        url="https://api.sleeper.app/v1/players/nfl?"+urllib.parse.urlencode({"position":position,"active":"true"})
        payload=http_json(url)
        if isinstance(payload,dict): return payload
    except Exception:
        pass
    return {}

def sleeper_fetch():
    rows=[]; messages=[]
    for pos in POSITIONS:
        params=urllib.parse.urlencode({"season_type":"regular","position":pos,"order_by":"pts_half_ppr"})
        url=f"https://api.sleeper.com/projections/nfl/{SEASON}/{WEEK}?{params}"
        try:
            payload=http_json(url)
            catalog={}
            items=list(payload.values()) if isinstance(payload,dict) else payload if isinstance(payload,list) else []
            if any(not ((z.get("player") or {}).get("full_name") or z.get("player_name") or z.get("full_name")) for z in items if isinstance(z,dict)):
                catalog=sleeper_players(pos)
            for z in items:
                if not isinstance(z,dict): continue
                player=z.get("player") if isinstance(z.get("player"),dict) else {}
                pid=str(z.get("player_id") or player.get("player_id") or "")
                meta=catalog.get(pid,{}) if isinstance(catalog,dict) else {}
                stats=z.get("stats") if isinstance(z.get("stats"),dict) else z
                name=(z.get("player_name") or z.get("full_name") or player.get("full_name") or
                      " ".join(filter(None,[player.get("first_name"),player.get("last_name")])) or
                      meta.get("full_name") or " ".join(filter(None,[meta.get("first_name"),meta.get("last_name")])))
                ppos=str(z.get("position") or player.get("position") or meta.get("position") or pos).upper()
                team=str(z.get("team") or player.get("team") or meta.get("team") or "")
                proj=num(stats.get("pts_half_ppr"))
                if not math.isfinite(proj): proj=half_ppr(stats)
                if name and ppos in POSITIONS and math.isfinite(proj):
                    rows.append({"provider":"sleeper","provider_id":pid,"name":name,"position":ppos,"team":team,
                                 "projection":proj,"source":url})
            messages.append(f"{pos}: {sum(1 for r in rows if r['position']==pos)} rows")
        except Exception as e:
            messages.append(f"{pos}: {type(e).__name__}: {e}")
    return rows, " | ".join(messages)

ESPN_POS={1:"QB",2:"RB",3:"WR",4:"TE"}

def espn_half_ppr(stats):
    def g(k): return num(stats.get(str(k), stats.get(k,0.0)),0.0)
    # ESPN football stat IDs verified by ffverse/ffscrapr.
    return .04*g(3)+4*g(4)-2*g(20)+2*g(19)+.10*g(24)+6*g(25)+2*g(26)+.50*g(53)+.10*g(42)+6*g(43)+2*g(44)-2*g(72)

def espn_fetch():
    # ESPN's public league-default player pool preserves per-week projected raw
    # stat lines. Request the broad player card at scoringPeriodId=0, then select
    # the historical WEEK entry locally; filtering on WEEK itself can omit the
    # projection split after the week is complete.
    url=f"https://lm-api-reads.fantasy.espn.com/apis/v3/games/ffl/seasons/{SEASON}/segments/0/leaguedefaults/3?view=kona_player_info&scoringPeriodId=0"
    filt={"players":{"limit":2000,"sortPercOwned":{"sortPriority":1,"sortAsc":False}}}
    try:
        payload=http_json(url,{"X-Fantasy-Filter":json.dumps(filt,separators=(",",":")),"X-Fantasy-Source":"kona"},60)
        if isinstance(payload, dict):
            entries=payload.get("players",[])
        elif isinstance(payload, list):
            entries=payload
        else:
            entries=[]
        rows=[]
        for z in entries:
            if not isinstance(z,dict): continue
            pool=z.get("playerPoolEntry") if isinstance(z.get("playerPoolEntry"),dict) else {}
            player=z.get("player") if isinstance(z.get("player"),dict) else pool.get("player",{})
            if not isinstance(player,dict): continue
            pos=ESPN_POS.get(int(num(player.get("defaultPositionId"),-1)),"")
            if pos not in POSITIONS: continue
            name=player.get("fullName") or player.get("displayName") or ""
            pid=str(player.get("id") or z.get("id") or "")
            team=str(player.get("proTeamId") or "")
            stat_entries=player.get("stats") or pool.get("stats") or z.get("stats") or []
            chosen=None
            for st in stat_entries:
                if not isinstance(st,dict): continue
                if int(num(st.get("scoringPeriodId"),-1))!=WEEK: continue
                source=int(num(st.get("statSourceId"),-1))
                stype=int(num(st.get("statTypeId"),-1))
                if source==1 or stype in (1,2,3):
                    raw=st.get("stats") if isinstance(st.get("stats"),dict) else st.get("appliedStats")
                    if isinstance(raw,dict):
                        chosen=raw; break
            if chosen is None: continue
            proj=espn_half_ppr(chosen)
            if name and math.isfinite(proj):
                rows.append({"provider":"espn","provider_id":pid,"name":name,"position":pos,"team":team,
                             "projection":proj,"source":url})
        return rows, f"{len(rows)} projected rows"
    except Exception as e:
        return [], f"{type(e).__name__}: {e}"

def match_provider(provider_rows, model_rows):
    byid={r["player_id"]:r for r in model_rows if r["player_id"]}
    bykey=defaultdict(list)
    for r in model_rows: bykey[(norm_name(r["name"]),r["position"])].append(r)
    matches=[]
    for p in provider_rows:
        direct=byid.get(p.get("player_id",""))
        if direct is not None:
            m=direct
        else:
            cand=bykey.get((norm_name(p["name"]),p["position"]),[])
            if len(cand)==1:
                m=cand[0]
            elif len(cand)>1:
                team=[x for x in cand if x["team"] and p["team"] and x["team"]==p["team"]]
                if len(team)!=1: continue
                m=team[0]
            else: continue
        matches.append({**m,"provider":p["provider"],"provider_projection":p["projection"],
                        "provider_id":p["provider_id"],"provider_name":p["name"],"source":p["source"]})
    # one row per model player
    seen=set(); out=[]
    for r in matches:
        k=r["player_id"]
        if k in seen: continue
        seen.add(k); out.append(r)
    return out

def position_metrics(provider, pos, rows, model_pos_n):
    a=[r["actual"] for r in rows]; mp=[r["model_projection"] for r in rows]; pp=[r["provider_projection"] for r in rows]
    mr=rankdata(mp,True); pr=rankdata(pp,True); ar=rankdata(a,True)
    starter_cut=min(STARTER[pos],len(rows)); relevant_cut=min(RELEVANT[pos],len(rows)); topn=min(TOPN[pos],len(rows))
    starter=[i for i in range(len(rows)) if mr[i]<=starter_cut or pr[i]<=starter_cut]
    relevant=[i for i in range(len(rows)) if mr[i]<=relevant_cut or pr[i]<=relevant_cut]
    mok,mpairs,macc=pair_accuracy(mr,ar,relevant); pok,ppairs,pacc=pair_accuracy(pr,ar,relevant)
    actual_top={i for i,r in enumerate(ar) if r<=topn}
    model_top={i for i,r in enumerate(mr) if r<=topn}
    provider_top={i for i,r in enumerate(pr) if r<=topn}
    def subset_metric(pred, ids):
        aa=[a[i] for i in ids]; xx=[pred[i] for i in ids]
        return mae(aa,xx) if ids else float("nan")
    return {
      "provider":provider,"position":pos,"common_n":len(rows),"model_position_n":model_pos_n,
      "coverage_pct":100*len(rows)/model_pos_n if model_pos_n else float("nan"),
      "model_MAE":mae(a,mp),"provider_MAE":mae(a,pp),"model_MAE_advantage":mae(a,pp)-mae(a,mp),
      "model_RMSE":rmse(a,mp),"provider_RMSE":rmse(a,pp),"model_RMSE_advantage":rmse(a,pp)-rmse(a,mp),
      "model_bias":bias(a,mp),"provider_bias":bias(a,pp),
      "model_correlation":pearson(mp,a),"provider_correlation":pearson(pp,a),
      "model_rank_correlation":spearman(mp,a),"provider_rank_correlation":spearman(pp,a),
      "starter_union_n":len(starter),"model_starter_MAE":subset_metric(mp,starter),"provider_starter_MAE":subset_metric(pp,starter),
      "relevant_union_n":len(relevant),"model_relevant_MAE":subset_metric(mp,relevant),"provider_relevant_MAE":subset_metric(pp,relevant),
      "top_n":topn,"model_top_n_hit_rate":len(model_top & actual_top)/topn if topn else float("nan"),
      "provider_top_n_hit_rate":len(provider_top & actual_top)/topn if topn else float("nan"),
      "model_start_sit_pairs":mpairs,"provider_start_sit_pairs":ppairs,
      "model_start_sit_accuracy":macc,"provider_start_sit_accuracy":pacc,
      "model_start_sit_advantage":macc-pacc if math.isfinite(macc) and math.isfinite(pacc) else float("nan")
    }

def fmt_rows(rows):
    out=[]
    for r in rows:
        out.append({k:("" if isinstance(v,float) and not math.isfinite(v) else v) for k,v in r.items()})
    return out

def main():
    model=load_model_rows()
    status=[]
    metrics=[]
    matched_long=[]
    counts=defaultdict(int)
    for r in model: counts[r["position"]]+=1

    providers=[]

    # Prefer a true pre-kickoff Sleeper snapshot when the production archive has
    # one. Historical endpoint backfill is only the fallback for older weeks.
    archived_sleeper=archived_provider("sleeper")
    if archived_sleeper:
        sleeper_raw=archived_sleeper
        sleeper_msg=f"{len(sleeper_raw)} frozen pregame rows from benchmark archive"
        sleeper_quality="frozen_pregame_archive"
    else:
        sleeper_raw,sleeper_msg=sleeper_fetch()
        sleeper_quality="historical_endpoint_backfill_not_contemporaneous_capture"

    for name,raw,msg,quality in (
        ("sleeper",sleeper_raw,sleeper_msg,sleeper_quality),
        ("espn",*espn_fetch(),"historical_endpoint_backfill_not_contemporaneous_capture"),
    ):
        matched=match_provider(raw,model)
        status.append({
          "season":SEASON,"week":WEEK,"provider":name,
          "status":"ok" if matched else "unavailable",
          "provider_rows":len(raw),"matched_rows":len(matched),
          "model_rows":len(model),
          "data_quality":quality,
          "message":msg,
          "fetched_at_utc":datetime.now(timezone.utc).isoformat()
        })
        if matched:
            providers.append(name)
            matched_long.extend(matched)
            for pos in POSITIONS:
                d=[r for r in matched if r["position"]==pos]
                if d: metrics.append(position_metrics(name,pos,d,counts[pos]))

    # Explicitly record sources that are not being inferred/scraped.
    status += [
      {"season":SEASON,"week":WEEK,"provider":"fantasypros","status":"not_run_requires_api_key_or_authorized_snapshot",
       "provider_rows":0,"matched_rows":0,"model_rows":len(model),"data_quality":"not_available",
       "message":"No FantasyPros API key/authorized Week snapshot configured; no values inferred.","fetched_at_utc":datetime.now(timezone.utc).isoformat()},
      {"season":SEASON,"week":WEEK,"provider":"walterpicks","status":"not_run_no_supported_feed",
       "provider_rows":0,"matched_rows":0,"model_rows":len(model),"data_quality":"not_available",
       "message":"No supported authorized projection feed configured; no values inferred.","fetched_at_utc":datetime.now(timezone.utc).isoformat()}
    ]

    write_csv(OUT_PREFIX+"_status.csv",fmt_rows(status))
    write_csv(OUT_PREFIX+"_metrics.csv",fmt_rows(metrics))
    write_csv(OUT_PREFIX+"_matched_players.csv",fmt_rows(matched_long))

    print(json.dumps({"season":SEASON,"week":WEEK,"model_rows":len(model),"providers":providers,
                      "status":status,"metric_rows":len(metrics)},indent=2))

if __name__=="__main__":
    main()
