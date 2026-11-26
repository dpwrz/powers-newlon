#!/usr/bin/env python3
"""Independent frozen forecast benchmark; only publish fully covered final games."""
import csv, json, math, datetime as dt, io, urllib.request, pathlib, collections, statistics, hashlib
ROOT=pathlib.Path(__file__).resolve().parents[1]
OUT=ROOT/'output'
SEASON=2026
POS={'QB':12,'RB':24,'WR':24,'TE':12}
def download(url):
    req=urllib.request.Request(url,headers={'User-Agent':'Powers-Newlon benchmark','Cache-Control':'no-cache'})
    with urllib.request.urlopen(req,timeout=120) as r: return r.read()
def parse(b): return list(csv.DictReader(io.StringIO(b.decode('utf-8-sig'))))
def num(v):
    try:
        x=float(v)
        return x if math.isfinite(x) else None
    except (TypeError,ValueError): return None
def stamp(v):
    try:
        d=dt.datetime.fromisoformat(v.replace('Z','+00:00'))
        return d if d.tzinfo else d.replace(tzinfo=dt.timezone.utc)
    except (ValueError,AttributeError): return None
def team(v): return {'JAC':'JAX','LAR':'LA','WSH':'WAS'}.get(v,v)
def ranks(xs):
    out=[0.]*len(xs); order=sorted(range(len(xs)),key=xs.__getitem__); i=0
    while i<len(order):
        j=i+1
        while j<len(order) and xs[order[j]]==xs[order[i]]: j+=1
        for k in order[i:j]: out[k]=(i+j+1)/2
        i=j
    return out
def corr(x,y):
    try: return statistics.correlation(x,y)
    except (ValueError,statistics.StatisticsError): return None
def metrics(rows,p):
    if not rows:return None
    x=[r[p] for r in rows]; y=[r['actual'] for r in rows]; e=[a-b for a,b in zip(x,y)]
    return dict(n=len(e),mae=statistics.mean(map(abs,e)),rmse=math.sqrt(statistics.mean(v*v for v in e)),bias=statistics.mean(e),pearson=corr(x,y),spearman=corr(ranks(x),ranks(y)))
def decisions(rows,p):
    hits=slots=correct=pairs=0; starters=[]; relevant=[]
    for w in sorted({r['week'] for r in rows}):
        for pos,n in POS.items():
            g=[r for r in rows if r['week']==w and r['position']==pos]
            if not g:continue
            actual=sorted(g,key=lambda r:(-r['actual'],r['id']))
            pred=sorted(g,key=lambda r:(-r[p],r['id']))
            hits+=len({r['id'] for r in actual[:n]} & {r['id'] for r in pred[:n]}); slots+=min(n,len(g))
            starters.extend(actual[:n]); relevant.extend(actual[:2*n])
            for a in actual[:n]:
                for b in actual[n:]:
                    if a['actual']==b['actual']:continue
                    pairs+=1; correct+=1 if a[p]>b[p] else .5 if a[p]==b[p] else 0
    return dict(top_n=hits/slots if slots else None,start_sit=correct/pairs if pairs else None,starter=metrics(starters,p),relevant=metrics(relevant,p))
def main():
    base='https://github.com/nflverse/nflverse-data/releases/download/'
    import gzip, urllib.error
    sb=download(base+'schedules/games.csv')
    try: pb=download(base+f'stats_player/stats_player_week_{SEASON}.csv.gz')
    except urllib.error.HTTPError: pb=download(base+f'stats_player/stats_player_week_{SEASON}.csv')
    if pb[:2]==b'\x1f\x8b': pb=gzip.decompress(pb)
    games=[g for g in parse(sb) if g['season']==str(SEASON) and g['game_type']=='REG']
    stats=[r for r in parse(pb) if r['season']==str(SEASON) and r['season_type']=='REG']
    archive=parse((OUT/f'benchmark_pregame_archive_{SEASON}.csv').read_bytes())
    if not games or not stats or not archive: raise RuntimeError('Empty source')
    final=set(); issues={}
    for w in sorted({int(g['week']) for g in games}):
        gs=[g for g in games if int(g['week'])==w]
        if not all(num(g['result']) is not None and num(g['home_score']) is not None and num(g['away_score']) is not None for g in gs):continue
        ids={g['game_id'] for g in gs}; st=[r for r in stats if int(r['week'])==w]
        missing=ids-{r['game_id'] for r in st}
        if missing: issues[w]={'missing_games':sorted(missing)};continue
        final.add(w)
    # Refuse to silently downgrade the latest final schedule week.
    from zoneinfo import ZoneInfo
    now=dt.datetime.now(dt.timezone.utc)
    elapsed=[w for w in {int(g['week']) for g in games} if all(dt.datetime.fromisoformat(g['gameday']+'T'+g['gametime']).replace(tzinfo=ZoneInfo('America/New_York'))+dt.timedelta(hours=5)<now for g in games if int(g['week'])==w)]
    latest=max(elapsed)
    if latest not in final: raise RuntimeError(f'Latest week {latest} is incomplete in schedule or player stats: {issues}')
    actual={}; schedule={}
    for g in games:
        if int(g['week']) in final:
            for t,o in [(g['home_team'],g['away_team']),(g['away_team'],g['home_team'])]:
                from zoneinfo import ZoneInfo
                kickoff=dt.datetime.fromisoformat(g['gameday']+'T'+g['gametime']).replace(tzinfo=ZoneInfo('America/New_York'))
                schedule[(int(g['week']),team(t))]=(g['game_id'],team(o),kickoff)
    for r in stats:
        w=int(r['week']); pos=r['position']; pid=r['player_id']
        if w not in final or pos not in POS or not pid or pid=='NA':continue
        s=schedule.get((w,team(r['team'])))
        if not s or s[0]!=r['game_id'] or s[1]!=team(r['opponent_team']):raise RuntimeError('Actual identity mismatch')
        ppr=num(r['fantasy_points_ppr']); rec=num(r['receptions'])
        if ppr is None or rec is None:raise RuntimeError('Missing scoring stats')
        k=(w,pid)
        if k in actual:raise RuntimeError('Duplicate actual ID')
        actual[k]=(r,ppr-.5*rec)
    chosen={}; rejected=collections.Counter(); available=collections.Counter()
    for r in archive:
        w=int(r['week']); p=r['provider']; k=(w,r['player_id'])
        if w not in final or p not in ('fantasy_model','sleeper') or r['position'] not in POS:continue
        available[(w,p)]+=1
        a=actual.get(k); s=schedule.get((w,team(r['team'])))
        cap=stamp(r['captured_at_utc']); kick=stamp(r['kickoff_utc']); v=num(r['projection'])
        if not a or not s or not cap or not kick or cap>=min(kick,s[2]) or team(a[0]['team'])!=team(r['team']) or a[0]['position']!=r['position'] or s[1]!=team(r['opponent']) or v is None or r['scoring_id']!='half_ppr':
            rejected[(w,p)]+=1;continue
        key=(k,p)
        if key not in chosen or cap>chosen[key][0]:chosen[key]=(cap,v)
    rows=[]
    for k,(r,y) in actual.items():
        if all((k,p) in chosen for p in ('fantasy_model','sleeper')):
            rows.append(dict(week=k[0],id=k[1],name=r['player_display_name'],position=r['position'],actual=y,**{p:chosen[k,p][1] for p in ('fantasy_model','sleeper')}))
    if not any(r['week']==latest for r in rows):raise RuntimeError('No matched latest-week rows')
    report={'season':SEASON,'latest_week':latest,'scoring':'half_ppr','generated_at':dt.datetime.now(dt.timezone.utc).isoformat(),'source_sha256':{'schedule':hashlib.sha256(sb).hexdigest(),'stats':hashlib.sha256(pb).hexdigest()},'weeks':{},'cumulative':{},'coverage':{},'excluded_weeks':issues}
    for w in sorted(final):
        g=[r for r in rows if r['week']==w]
        report['coverage'][w]={'common':len(g),'actual_offense':sum(k[0]==w for k in actual),'archive_rows':{p:available[w,p] for p in ('fantasy_model','sleeper')},'rejected':{p:rejected[w,p] for p in ('fantasy_model','sleeper')}}
        if g:report['weeks'][w]=summarize(g)
    report['cumulative']=summarize(rows)
    report['largest_errors']=sorted([r for r in rows if r['week']==latest],key=lambda r:abs(r['fantasy_model']-r['actual']),reverse=True)[:15]
    dest=OUT/f'benchmark_verified_{SEASON}.json'; tmp=dest.with_suffix('.tmp');tmp.write_text(json.dumps(report,indent=2,allow_nan=False));tmp.replace(dest)
    print(json.dumps(report['coverage'],indent=2))
def summarize(rows):
    return {pos:{p:{**metrics(g,p),**decisions(g,p)} for p in ('fantasy_model','sleeper')} for pos in ('ALL',*POS) if (g:=[r for r in rows if pos=='ALL' or r['position']==pos])}
if __name__=='__main__':main()
