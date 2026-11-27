#!/usr/bin/env python3
"""Chronological position-specific calibration challenger. No Sleeper inputs."""
import csv,json,statistics,math,pathlib
from benchmark_verified import metrics,decisions,POS
OUT=pathlib.Path(__file__).resolve().parents[1]/'output'
def fit(train,kind):
    if kind=='identity':return (0.,1.)
    x=[r['fantasy_model'] for r in train];y=[r['actual'] for r in train]
    if not x:return (0.,1.)
    if kind=='bias':return (sum(b-a for a,b in zip(x,y))/(len(x)+40),1.)
    mx=statistics.mean(x);my=statistics.mean(y)
    slope=(sum((a-mx)*(b-my) for a,b in zip(x,y))+400)/(sum((a-mx)**2 for a in x)+400)
    slope=max(.65,min(1.35,slope))
    return (max(-3.,min(3.,my-slope*mx)),slope)
def apply(r,coef):return max(0.,coef[0]+coef[1]*r['fantasy_model'])
def main():
    with (OUT/'benchmark_matched_2026.csv').open() as f:
        rows=[{**r,'week':int(r['week']),'actual':float(r['actual']),'fantasy_model':float(r['fantasy_model']),'sleeper':float(r['sleeper'])} for r in csv.DictReader(f)]
    weeks=sorted({r['week'] for r in rows if r['week']>=6})
    kinds=['identity','bias','affine']; tested=[]; choices=[]
    for w in weeks[3:]:
        for pos in POS:
            history=[r for r in rows if 6<=r['week']<w and r['position']==pos]
            validation=[]
            for v in weeks:
                if v>=w:break
                train=[r for r in history if r['week']<v]
                if len(train)<40:continue
                val=[r for r in history if r['week']==v]
                validation.extend([{**r,**{k:apply(r,fit(train,k)) for k in kinds}} for r in val])
            winner='identity'
            if validation:
                scores={k:metrics(validation,k)['mae'] for k in kinds}
                candidate=min(scores,key=scores.get)
                if scores[candidate]<scores['identity']*.99:winner=candidate
            coef=fit(history,winner)
            choices.append(dict(week=w,position=pos,method=winner,intercept=coef[0],slope=coef[1],inner_validation_n=len(validation)))
            tested.extend({**r,'challenger':apply(r,coef)} for r in rows if r['week']==w and r['position']==pos)
    report={'status':'experimental_not_promoted','test_weeks':sorted({r['week'] for r in tested}),'choices':choices,'metrics':{}}
    for pos in ('ALL',*POS):
        g=[r for r in tested if pos=='ALL' or r['position']==pos]
        if g:report['metrics'][pos]={p:{**metrics(g,p),**decisions(g,p)} for p in ('fantasy_model','challenger','sleeper')}
    (OUT/'research_calibration_2026.json').write_text(json.dumps(report,indent=2,allow_nan=False))
    print(json.dumps(report,indent=2,allow_nan=False))
if __name__=='__main__':main()
