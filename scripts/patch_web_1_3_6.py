from pathlib import Path

MAIN=Path('web/src/main.js')
PKG=Path('web/package.json')
CSS=Path('web/src/styles.css')
s=MAIN.read_text(encoding='utf-8')

old="""function leader(title,keys){const a=st.rows.map(r=>({v:n(field(r,keys)),name:field(r,['player_name','full_name','name','player'])})).filter(x=>x.v!=null&&x.name).sort((a,b)=>b.v-a.v).slice(0,5);return`<div class=\"card card-pad\"><div class=\"eyebrow\">NFL leader</div><h3>${title}</h3>${a.length?a.map((x,i)=>`<div class=\"leader-row\"><strong>${i+1}</strong><span>${esc(x.name)}</span><strong>${fi(x.v)}</strong></div>`).join(''):'<div class=\"empty\">Not in current snapshot.</div>'}</div>`}
function leaders(){const c=[['Passing Yards',['passing_yards','pass_yards']],['Passing TDs',['passing_tds','pass_tds']],['Rushing Yards',['rushing_yards','rush_yards']],['Rushing TDs',['rushing_tds','rush_tds']],['Receiving Yards',['receiving_yards','rec_yards']],['Receptions',['receptions','rec']],['Receiving TDs',['receiving_tds','rec_tds']],['Targets',['targets']]];return layout('NFL Leaders',`<h1 class=\"page-title\">NFL Leaders</h1><p class=\"page-subtitle\">Top five real-NFL statistical leaders when those season totals are present in the production snapshot.</p><div class=\"leaders-grid\">${c.map(x=>leader(x[0],x[1])).join('')}</div>`)}
"""
new=r"""function leader(title,keys){const a=[...st.seasonStats.entries()].map(([id,row])=>{const p=st.players?.[id]||{},x=dstStats(row),v=maybeNum(field(x,keys));return{id,p,v,name:p.full_name||[p.first_name,p.last_name].filter(Boolean).join(' ')||`Player ${id}`}}).filter(x=>x.v!=null&&x.v>0&&x.name&&['QB','RB','WR','TE'].includes(String(x.p?.position||'').toUpperCase())).sort((a,b)=>b.v-a.v).slice(0,10);return`<div class="card card-pad"><div class="eyebrow">${st.season} NFL season</div><h3>${esc(title)}</h3>${a.length?a.map((x,i)=>`<div class="leader-row leader-open" data-player="${esc(x.id)}" role="button" tabindex="0"><strong>${i+1}</strong><div class="leader-player"><img src="${head(x.id)}" alt="" onerror="this.style.display='none'"><span><strong>${esc(x.name)}</strong><small>${esc(x.p.position||'')} · ${esc(x.p.team||'FA')}</small></span></div><strong>${fi(x.v)}</strong></div>`).join(''):'<div class="empty">Season stats are loading or unavailable.</div>'}</div>`}
function leaders(){const c=[['Passing Yards',['pass_yd','pass_yds','passing_yards','pass_yards']],['Passing TDs',['pass_td','passing_tds','pass_tds']],['Rushing Yards',['rush_yd','rush_yds','rushing_yards','rush_yards']],['Rushing TDs',['rush_td','rushing_tds','rush_tds']],['Receiving Yards',['rec_yd','rec_yds','receiving_yards','rec_yards']],['Receptions',['rec','receptions']],['Receiving TDs',['rec_td','receiving_tds','rec_tds']],['Targets',['rec_tgt','targets']]];return layout('NFL Leaders',`<div class="toolbar"><div><h1 class="page-title">NFL Leaders</h1><p class="page-subtitle">Current ${st.season} regular-season league leaders from live Sleeper season totals.</p></div><button class="btn" data-a="refreshLeaders">Refresh leaders</button></div>${st.seasonStats.size?`<div class="leaders-grid">${c.map(x=>leader(x[0],x[1])).join('')}</div>`:'<div class="empty">Loading current-season NFL statistics…</div>'}`)}
"""
if old not in s:
    raise SystemExit('old leaders block missing')
s=s.replace(old,new,1)

old_route="if(st.route==='live'&&!st.games.length)liveLoad();if(st.route==='startsit')players()"
new_route="if(st.route==='live'&&!st.games.length)liveLoad();if(st.route==='leaders'){await Promise.all([players(),loadSeasonStats()]);render()}if(st.route==='startsit')players()"
if old_route not in s:
    raise SystemExit('route handler anchor missing')
s=s.replace(old_route,new_route,1)

old_refresh="$('[data-a=\"refreshLive\"]')?.addEventListener('click',liveLoad);"
new_refresh="$('[data-a=\"refreshLive\"]')?.addEventListener('click',liveLoad);$('[data-a=\"refreshLeaders\"]')?.addEventListener('click',async()=>{await Promise.all([players(true),loadSeasonStats(true)]);render();toast('NFL season leaders refreshed.')});"
if old_refresh not in s:
    raise SystemExit('refresh live anchor missing')
s=s.replace(old_refresh,new_refresh,1)

old_init="if(st.route==='live')liveLoad()}"
new_init="if(st.route==='live')liveLoad();if(st.route==='leaders'){await Promise.all([players(),loadSeasonStats()]);render()}}"
if old_init not in s:
    raise SystemExit('init anchor missing')
s=s.replace(old_init,new_init,1)
MAIN.write_text(s,encoding='utf-8')

css=CSS.read_text(encoding='utf-8')
css += r'''

/* Web 1.3.6 — live current-season NFL leaders */
.leader-open{cursor:pointer;border-radius:9px;transition:.15s}.leader-open:hover{background:rgba(182,255,69,.045)}.leader-player{display:flex;align-items:center;gap:9px;min-width:0}.leader-player img{width:34px;height:34px;border-radius:9px;object-fit:cover;object-position:center 15%;background:#17211c}.leader-player span{display:grid;min-width:0}.leader-player span>strong{white-space:nowrap;overflow:hidden;text-overflow:ellipsis}.leader-player small{color:var(--muted);font-size:10px;margin-top:2px}
'''
CSS.write_text(css,encoding='utf-8')

p=PKG.read_text(encoding='utf-8')
if '"version": "1.3.5"' not in p:
    raise SystemExit('expected Web 1.3.5')
p=p.replace('"version": "1.3.5"','"version": "1.3.6"',1)
PKG.write_text(p,encoding='utf-8')
print('Patched Web 1.3.6: live current-season NFL leaders')
