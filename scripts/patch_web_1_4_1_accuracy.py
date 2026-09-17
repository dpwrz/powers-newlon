from pathlib import Path
import re, json

MAIN=Path('web/src/main.js')
PKG=Path('web/package.json')
EXPORT=Path('scripts/export_model_snapshot.R')
SCORE=Path('pipeline/24_score_live_accuracy_2_5.R')

s=MAIN.read_text(encoding='utf-8')
old_pat=r"function adminAccuracyData\(\)\{.*?\}\nfunction adminTools\(\)"
m=re.search(old_pat,s,flags=re.S)
if not m:
    raise SystemExit('admin accuracy block not found')

new=r'''function adminRows(x){if(Array.isArray(x))return x.filter(v=>v&&typeof v==='object');if(!x||typeof x!=='object')return[];return Object.values(x).filter(v=>v&&typeof v==='object')}
function adminPct(v){const x=n(v);return x==null?'—':`${(100*x).toFixed(1)}%`}
function adminSigned(v,d=2){const x=n(v);return x==null?'—':`${x>0?'+':''}${x.toFixed(d)}`}
function adminImprove(v){const x=n(v);return x==null?'—':`${x>0?'+':''}${x.toFixed(1)}%`}
function adminWeightedOverall(pos){if(!pos.length)return null;const usable=pos.filter(r=>n(r.n,0)>0),N=usable.reduce((a,r)=>a+n(r.n,0),0);if(!N)return null;const wavg=k=>usable.reduce((a,r)=>a+n(r[k],0)*n(r.n,0),0)/N,starterN=usable.reduce((a,r)=>a+n(r.starter_n,0),0),pairN=usable.reduce((a,r)=>a+n(r.start_sit_pairs,0),0),inc=wavg('incumbent_mae'),mae=wavg('mae');return{n:N,mae,rmse:Math.sqrt(usable.reduce((a,r)=>a+(n(r.rmse,0)**2)*n(r.n,0),0)/N),bias:wavg('bias'),incumbent_mae:inc,mae_improvement_pct:inc>0?100*(inc-mae)/inc:null,starter_n:starterN,starter_mae:starterN?usable.reduce((a,r)=>a+n(r.starter_mae,0)*n(r.starter_n,0),0)/starterN:null,start_sit_pairs:pairN,start_sit_accuracy:pairN?usable.reduce((a,r)=>a+n(r.start_sit_accuracy,0)*n(r.start_sit_pairs,0),0)/pairN:null,interval_coverage:wavg('interval_coverage'),expected_error_coverage:wavg('expected_error_coverage'),_derived:true}}
function adminAccuracyData(){const q=st.snapshot?.quality||{},overallRows=adminRows(q.live_overall||q.live_cumulative_summary),weekly=adminRows(q.live_weekly),position=adminRows(q.live_cumulative),weeklyPosition=adminRows(q.live_position),misses=adminRows(q.live_misses);return{overall:overallRows[0]||adminWeightedOverall(position),weekly,position,weeklyPosition,misses,historical:q.accuracy25||q.validation||null}}
function adminAccuracyTable(rows,mode){if(!rows.length)return'<div class="empty">No completed accuracy rows have been exported yet.</div>';if(mode==='position')return`<div class="table-wrap admin-accuracy-table"><table><thead><tr><th>Pos</th><th>N</th><th>MAE</th><th>RMSE</th><th>Bias</th><th>Corr</th><th>Starter MAE</th><th>vs incumbent</th><th>Start/Sit</th></tr></thead><tbody>${rows.map(r=>`<tr><td><strong>${esc(r.position||'—')}</strong></td><td>${fi(r.n)}</td><td>${f(r.mae,2)}</td><td>${f(r.rmse,2)}</td><td>${adminSigned(r.bias)}</td><td>${f(r.correlation,3)}</td><td>${f(r.starter_mae,2)}</td><td>${adminImprove(r.mae_improvement_pct)}</td><td>${adminPct(r.start_sit_accuracy)} <span class="muted small">(${fi(r.start_sit_pairs)})</span></td></tr>`).join('')}</tbody></table></div>`;return`<div class="table-wrap admin-accuracy-table"><table><thead><tr><th>Week</th><th>N</th><th>MAE</th><th>RMSE</th><th>Bias</th><th>Corr</th><th>Starter MAE</th><th>vs incumbent</th><th>Start/Sit</th></tr></thead><tbody>${rows.map(r=>`<tr><td><strong>W${esc(r.week)}</strong></td><td>${fi(r.n)}</td><td>${f(r.mae,2)}</td><td>${f(r.rmse,2)}</td><td>${adminSigned(r.bias)}</td><td>${f(r.correlation,3)}</td><td>${f(r.starter_mae,2)}</td><td>${adminImprove(r.mae_improvement_pct)}</td><td>${adminPct(r.start_sit_accuracy)} <span class="muted small">(${fi(r.start_sit_pairs)})</span></td></tr>`).join('')}</tbody></table></div>`}
function adminMisses(rows){if(!rows.length)return'<div class="empty">No scored misses have been exported yet.</div>';return`<div class="table-wrap admin-accuracy-table"><table><thead><tr><th>Week</th><th>Player</th><th>Pos</th><th>Projection</th><th>Actual</th><th>Error</th><th>Abs error</th></tr></thead><tbody>${rows.slice(0,25).map(r=>`<tr><td>W${esc(r.week)}</td><td><strong>${esc(r.player_display_name||r.player_name||r.player_id||'—')}</strong></td><td>${esc(r.position||'—')}</td><td>${f(r.projection,2)}</td><td>${f(r.actual_fppg,2)}</td><td>${adminSigned(r.error)}</td><td>${f(r.abs_error,2)}</td></tr>`).join('')}</tbody></table></div>`}
function adminAccuracy(){const d=adminAccuracyData(),o=d.overall,weeks=d.weekly.map(r=>n(r.week)).filter(Number.isFinite),through=weeks.length?Math.max(...weeks):null,improve=n(o?.mae_improvement_pct),improveSub=improve==null?'Incumbent comparison unavailable':improve>=0?'Production MAE is lower':'Production MAE is higher',derived=o?._derived?'Overall core metrics derived from cumulative position rows until the next live refresh exports an explicit season aggregate.':'Exact season aggregate from frozen pregame player-weeks.':'';return`<div class="admin-toolbar"><div><h2>${st.season} Live Accuracy</h2><p class="muted">Honest season-to-date scoring of frozen final pregame forecasts against completed player-week outcomes${through?` through Week ${through}`:''}.</p></div><button class="btn btn-ghost" data-a="adminRefresh">Refresh data</button></div>${o?`<div class="admin-grid admin-grid-4">${adminCard('Scored player-weeks',fi(o.n),through?`Through Week ${through}`:'Completed games only')}${adminCard('MAE',f(o.mae,2),'Average absolute miss')}${adminCard('RMSE',f(o.rmse,2),'Penalizes large misses')}${adminCard('Bias',adminSigned(o.bias),'Positive = model under-projected')}${adminCard('Starter MAE',f(o.starter_mae,2),`${fi(o.starter_n)} starter player-weeks`)}${adminCard('vs incumbent',adminImprove(o.mae_improvement_pct),improveSub)}${adminCard('Start/Sit accuracy',adminPct(o.start_sit_accuracy),`${fi(o.start_sit_pairs)} meaningful pairs`)}${adminCard('Floor–ceiling coverage',adminPct(o.interval_coverage),'Actual inside modeled range')}</div><p class="muted small admin-accuracy-note">${esc(derived)} Accuracy is measured only where a pregame forecast was frozen before kickoff; postgame/current-week rewrites are not allowed into the score.</p>`:'<div class="empty">No completed season accuracy has been exported yet. The tracker begins once a frozen pregame forecast has a completed player-week actual.</div>'}<div class="section-head"><h2>Season by position</h2><span class="muted small">Cumulative ${st.season}</span></div>${adminAccuracyTable(d.position,'position')}<div class="section-head"><h2>Weekly trend</h2><span class="muted small">Production forecast vs actual</span></div>${adminAccuracyTable(d.weekly,'week')}<div class="section-head"><h2>Biggest misses</h2><span class="muted small">Largest absolute errors this season</span></div>${adminMisses(d.misses)}${d.historical?`<details class="admin-panel" style="margin-top:14px"><summary class="admin-panel-title" style="cursor:pointer">Historical validation / guardrail payload</summary><p class="muted small">This is separate from the live ${st.season} scoreboard above.</p><pre class="admin-json tall">${esc(JSON.stringify(d.historical,null,2))}</pre></details>`:''}`}
function adminTools()'''
s=s[:m.start()]+new+s[m.end():]
MAIN.write_text(s,encoding='utf-8')

pkg=json.loads(PKG.read_text(encoding='utf-8'))
pkg['version']='1.4.1'
PKG.write_text(json.dumps(pkg,indent=2)+'\n',encoding='utf-8')

x=EXPORT.read_text(encoding='utf-8')
needle='live_cumulative_file <- first_existing(c(file.path(out,paste0("live_accuracy_cumulative_position_", 2026, ".csv"))))\n'
if needle not in x: raise SystemExit('export live cumulative anchor missing')
x=x.replace(needle,needle+'live_overall_file <- first_existing(c(file.path(out,paste0("live_accuracy_cumulative_summary_", 2026, ".csv"))))\n',1)
needle='live_cumulative <- read_optional(live_cumulative_file)\n'
if needle not in x: raise SystemExit('export read anchor missing')
x=x.replace(needle,needle+'live_overall <- read_optional(live_overall_file)\n',1)
needle='  live_cumulative = if (!is.null(live_cumulative)) live_cumulative else list(),\n'
if needle not in x: raise SystemExit('export quality anchor missing')
x=x.replace(needle,needle+'  live_overall = if (!is.null(live_overall)) live_overall else list(),\n',1)
EXPORT.write_text(x,encoding='utf-8')

r=SCORE.read_text(encoding='utf-8')
needle='  cumulative_position <- dplyr::bind_rows(lapply(split(scored, scored$position), function(d) cbind(data.frame(position = as.character(d$position[1])), summarise_accuracy(d)))) |> dplyr::arrange(factor(position, levels = POSITIONS))\n'
if needle not in r: raise SystemExit('score cumulative anchor missing')
r=r.replace(needle,needle+'  cumulative_summary <- cbind(data.frame(season = CURRENT_SEASON), summarise_accuracy(scored))\n',1)
needle='  readr::write_csv(cumulative_position, paste0("output/live_accuracy_cumulative_position_", CURRENT_SEASON, ".csv"))\n'
if needle not in r: raise SystemExit('score write anchor missing')
r=r.replace(needle,needle+'  readr::write_csv(cumulative_summary, paste0("output/live_accuracy_cumulative_summary_", CURRENT_SEASON, ".csv"))\n',1)
SCORE.write_text(r,encoding='utf-8')
print('patched Web 1.4.1 live accuracy dashboard')
