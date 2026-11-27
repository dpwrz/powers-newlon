#!/usr/bin/env python3
"""Historical, strictly lagged player/team/opponent research; independent of production."""
import csv, gzip, json, pathlib, collections, math, statistics
import numpy as np
from sklearn.ensemble import HistGradientBoostingRegressor, RandomForestRegressor
from sklearn.inspection import permutation_importance
from benchmark_verified import download,parse,num,POS,metrics,decisions
OUT=pathlib.Path(__file__).resolve().parents[1]/'output'
BASE='https://github.com/nflverse/nflverse-data/releases/download/stats_player/'
GROUPS={'form':['points','yards','td'],'opportunity':['attempts','carries','targets','receptions'],'efficiency':['yards_per_touch','catch_rate'],'context':['team_points','opp_points'],'trend':['points_delta','touch_delta']}
def get(r,k):return num(r.get(k)) or 0.
def mean(h,k,n):return statistics.mean(x[k] for x in h[-n:]) if h else 0.
def build():
    rows=[]; coverage={}
    for season in range(2016,2027):
        try:
            b=download(BASE+f'stats_player_week_{season}.csv.gz')
        except Exception:
            b=download(BASE+f'stats_player_week_{season}.csv')
        if b[:2]==b'\x1f\x8b':b=gzip.decompress(b)
        data=[r for r in parse(b) if r.get('season_type')=='REG' and r.get('position') in POS and r.get('player_id') not in ('','NA',None)]
        if season==2026:data=[r for r in data if int(r['week'])<=11]
        coverage[season]=len(data)
        hist=collections.defaultdict(list); teams=collections.defaultdict(list); defense=collections.defaultdict(list)
        for week in sorted({int(r['week']) for r in data}):
            pending=[]; team_tot=collections.defaultdict(float); opp_tot=collections.defaultdict(float)
            for r in [r for r in data if int(r['week'])==week]:
                pid=r['player_id'];pos=r['position'];team=r['team'];opp=r['opponent_team'];h=hist[pid]
                ppr=num(r.get('fantasy_points_ppr'));rec=num(r.get('receptions'))
                if ppr is None or rec is None:continue
                y=ppr-.5*rec
                attempts=get(r,'attempts');carries=get(r,'carries');targets=get(r,'targets');yards=get(r,'passing_yards')+get(r,'rushing_yards')+get(r,'receiving_yards')
                td=get(r,'passing_tds')+get(r,'rushing_tds')+get(r,'receiving_tds')
                obs=dict(points=y,yards=yards,td=td,attempts=attempts,carries=carries,targets=targets,receptions=rec,yards_per_touch=yards/max(1,attempts+carries+rec),catch_rate=rec/max(1,targets))
                if h:
                    f={k:mean(h,k,4) for k in obs}
                    f['points_delta']=mean(h,'points',2)-mean(h,'points',8)
                    f['touch_delta']=sum(mean(h,k,2)-mean(h,k,8) for k in ('attempts','carries','targets'))
                    f['team_points']=statistics.mean(teams[(team,pos)][-4:]) if teams[(team,pos)] else 0.
                    f['opp_points']=statistics.mean(defense[(opp,pos)][-4:]) if defense[(opp,pos)] else 0.
                    rows.append(dict(season=season,week=season*100+week,id=pid,position=pos,actual=y,**f))
                pending.append((pid,obs));team_tot[team,pos]+=y;opp_tot[opp,pos]+=y
            for pid,obs in pending:hist[pid].append(obs)
            for k,v in team_tot.items():teams[k].append(v)
            for k,v in opp_tot.items():defense[k].append(v)
    return rows,coverage
def main():
    rows,coverage=build(); features=sum(GROUPS.values(),[])
    report={'coverage':coverage,'features':features,'status':'historical_challenger_not_production','positions':{}}
    for pos in POS:
        result=[]; importance=collections.defaultdict(list); ablations=collections.defaultdict(list)
        for year in range(2022,2027):
            train=[r for r in rows if r['position']==pos and r['season']<year]
            test=[r for r in rows if r['position']==pos and r['season']==year]
            if not train or not test:continue
            def matrix(g,cols):return np.array([[r[k] for k in cols] for r in g])
            y=np.array([r['actual'] for r in train]); actual=np.array([r['actual'] for r in test])
            def fit(cols,loss='squared_error'):
                m=HistGradientBoostingRegressor(max_iter=100,max_leaf_nodes=15,l2_regularization=10,loss=loss,random_state=42)
                m.fit(matrix(train,cols),y);return m
            m=fit(features); pred=m.predict(matrix(test,features))
            lo=HistGradientBoostingRegressor(loss='quantile',quantile=.1,max_iter=80,max_leaf_nodes=15,l2_regularization=10).fit(matrix(train,features),y)
            hi=HistGradientBoostingRegressor(loss='quantile',quantile=.9,max_iter=80,max_leaf_nodes=15,l2_regularization=10).fit(matrix(train,features),y)
            low=lo.predict(matrix(test,features)); high=hi.predict(matrix(test,features))
            rf=RandomForestRegressor(n_estimators=60,max_depth=8,min_samples_leaf=25,n_jobs=2,random_state=42).fit(matrix(train,features),y)
            rp=rf.predict(matrix(test,features))
            # Ensemble weights selected on prior season only, never test season.
            inner=[r for r in train if r['season']==year-1]; earlier=[r for r in train if r['season']<year-1]
            im=HistGradientBoostingRegressor(max_iter=100,max_leaf_nodes=15,l2_regularization=10).fit(matrix(earlier,features),[r['actual'] for r in earlier])
            ip=im.predict(matrix(inner,features)); iy=np.array([r['actual'] for r in inner]); ib=np.array([r['points'] for r in inner])
            weight=min([0,.25,.5,.75,1],key=lambda w:np.mean(abs(w*ip+(1-w)*ib-iy)))
            for i,r in enumerate(test):result.append({**r,'baseline':r['points'],'boosted':float(pred[i]),'forest':float(rp[i]),'ensemble':float(weight*pred[i]+(1-weight)*r['points']),'lo':float(min(low[i],high[i])),'hi':float(max(low[i],high[i]))})
            imp=permutation_importance(m,matrix(test,features),actual,scoring='neg_mean_absolute_error',n_repeats=3,random_state=42,n_jobs=2)
            for k,v in zip(features,imp.importances_mean):importance[k].append(float(v))
            for group,drop in GROUPS.items():
                cols=[k for k in features if k not in drop]; ap=fit(cols).predict(matrix(test,cols))
                ablations[group].append(float(np.mean(abs(ap-actual))-np.mean(abs(pred-actual))))
        if result:
            report['positions'][pos]={'metrics':{p:{**metrics(result,p),**decisions(result,p)} for p in ('baseline','boosted','forest','ensemble')},'permutation_mae_increase':{k:statistics.mean(v) for k,v in importance.items()},'ablation_mae_increase':{k:statistics.mean(v) for k,v in ablations.items()},'interval_coverage':statistics.mean(r['lo']<=r['actual']<=r['hi'] for r in result),'interval_width':statistics.mean(r['hi']-r['lo'] for r in result)}
    (OUT/'research_signals_2026.json').write_text(json.dumps(report,indent=2,allow_nan=False))
    print(json.dumps(report,indent=2,allow_nan=False))
if __name__=='__main__':main()
