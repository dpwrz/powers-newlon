import React,{useCallback,useEffect,useMemo,useRef,useState,type FormEvent,type ReactNode} from 'react';
import {useQuery,useQueryClient} from '@tanstack/react-query';
import {ArrowDownRight,ArrowLeft,ArrowRight,ArrowUpRight,BarChart3,BookOpen,Check,ChevronDown,
 ChevronRight,Clock3,Compass,GitCompareArrows,Info,Layers,Menu,RefreshCw,Search,ShieldCheck,
 SlidersHorizontal,Sparkles,Target,TrendingUp,X} from 'lucide-react';
import {getLeagueBundle,getLeagues,getNflState,getPlayers,getSleeperUser,getSnapshot,remember,safeStored,
 type LeagueBundle} from './client';
import {ModelIndex,canonicalTeam,eligible,fmt,lineup,matchupView,num,optimal,pf,recordOf,rosterName,
 type Decision,type Forecast,type League,type MatchupView,type Member,type PlayerMap,type Roster,type SleeperUser} from './domain';
import {compare,lineupDecisions,rosterAssets,standings,tradeSummary,waiverIdeas} from './decisions';

type Screen='briefing'|'decisions'|'scout'|'league';
type LeagueScreen='standings'|'roster'|'trade'|'draft'|'health';
type DecisionScreen='opportunities'|'compare'|'waivers';
type AppData={league:League;users:Member[];rosters:Roster[];players:PlayerMap;bundle:LeagueBundle;
 index:ModelIndex;week:number;nflWeek:number;myRoster:Roster|null;matchup:MatchupView};
const iconSize=18;
function Caption({children}: {children:ReactNode}){return <span className="caption">{children}</span>}
function Empty({title,detail,action}:{title:string;detail:string;action?:ReactNode}){return <div className="empty-state"><Info size={22}/><h3>{title}</h3><p>{detail}</p>{action}</div>}
function ErrorLine({error,onRetry}:{error:unknown;onRetry?:()=>void}){const message=error instanceof Error?error.message:'This information could not be loaded.';return <div className="error-line" role="alert"><Info size={17}/><span>{message}</span>{onRetry&&<button className="text-action" onClick={onRetry}>Retry</button>}</div>}
function Symbol({kind}:{kind:Screen}){return kind==='briefing'?<Layers size={iconSize}/>:kind==='decisions'?<SlidersHorizontal size={iconSize}/>:kind==='scout'?<Search size={iconSize}/>:<BarChart3 size={iconSize}/>}
function AppShell({screen,setScreen,league,week,setWeek,onSwitch,children,updatedAt}:
 {screen:Screen;setScreen:(s:Screen)=>void;league:League;week:number;setWeek:(w:number)=>void;onSwitch:()=>void;
 children:ReactNode;updatedAt:number|null}){
 const sections:{key:Screen;label:string;sub:string}[]=[
  {key:'briefing',label:'Briefing',sub:'Your week'},
  {key:'decisions',label:'Decisions',sub:'Your next move'},
  {key:'scout',label:'Scout',sub:'Player intelligence'},
  {key:'league',label:'League',sub:'Competitive context'}];
 const [menu,setMenu]=useState(false);
 const change=(next:Screen)=>{if(next===screen)return;setScreen(next);window.scrollTo({top:0,behavior:'instant'});setMenu(false)};
 return <div className="app-frame">
  <a className="skip-link" href="#main-content">Skip to content</a>
  <aside className="rail" aria-label="Primary navigation">
   <div className="brand"><div className="brand-symbol">PN</div><div><strong>POWERS–NEWLON</strong><span>FANTASY MANAGER</span></div></div>
   <div className="rail-description">A sharper way to see the game.</div>
   <div className="rail-section">WORKSPACE <span>/ 04</span></div>
   <nav className="rail-nav" aria-label="Main sections">{sections.map((s,i)=><button key={s.key} type="button" className={'rail-item '+(s.key===screen?'selected':'')} aria-current={s.key===screen?'page':undefined} onClick={()=>change(s.key)}><span className="rail-numeral">{String(i+1).padStart(2,'0')}</span><Symbol kind={s.key}/><span>{s.label}</span><ChevronRight size={14} className="rail-arrow"/></button>)}</nav>
   <div className="rail-bottom"><div className="rail-bottom-label">MODEL / SOURCE</div><strong>Powers–Newlon Model</strong><span>Sleeper provides league data and actual scoring only.</span><span className="rail-rule"/></div>
  </aside>
  <div className="app-main">
   <header className="top-strip">
    <div className="top-context"><span className="top-mark">PN</span><span>WORKSPACE <b>/ {screen.toUpperCase()}</b></span></div>
    <div className="top-actions">
     <button type="button" className="league-switcher" onClick={onSwitch} title="Choose another league"><span className="league-title">{league.name}</span><ChevronDown size={15}/></button>
     <label className="week-switch"><span className="sr-only">NFL week</span><select aria-label="Select NFL week" value={week} onChange={e=>setWeek(Number(e.target.value))}>{Array.from({length:18},(_,i)=><option key={i+1} value={i+1}>W{i+1}</option>)}</select></label>
     <button className="mobile-menu-button" type="button" aria-label="More navigation options" aria-expanded={menu} onClick={()=>setMenu(x=>!x)}><Menu size={19}/></button>
    </div>
   </header>
   {menu&&<div className="mobile-overflow"><button onClick={onSwitch}>Switch league</button><span>Last league sync {updatedAt?new Date(updatedAt).toLocaleTimeString():'unavailable'}</span></div>}
   <main id="main-content" className="main-content" tabIndex={-1} key={screen}><div className="content-inner">{children}</div></main>
   <footer className="app-footer"><span>Powers–Newlon / Independent fantasy intelligence</span><span>MODEL OUTPUTS ARE ADVISORY</span></footer>
  </div>
  <nav className="mobile-dock" aria-label="Main sections">{sections.map(s=><button type="button" key={s.key} className={screen===s.key?'active':''} aria-current={screen===s.key?'page':undefined} onClick={()=>change(s.key)}><Symbol kind={s.key}/><span>{s.label}</span></button>)}</nav>
 </div>;
}
function Connect({onConnect,onSelect,leagues,username,year,loading,error,connected}:
 {onConnect:(name:string,year:number)=>void;onSelect:(id:string)=>void;leagues:League[];username:string;year:number;
 loading:boolean;error:string|null;connected:boolean}){
 const [name,setName]=useState(username),[season,setSeason]=useState(String(year));
 useEffect(()=>{if(username)setName(username)},[username]);
 const submit=(e:FormEvent<HTMLFormElement>)=>{e.preventDefault();const yr=Number(season);if(!name.trim()||!Number.isInteger(yr)||yr<2020||yr>2100)return;onConnect(name.trim(),yr)};
 return <div className="connect-frame"><div className="connect-brand"><span className="brand-symbol">PN</span><span>POWERS–NEWLON<br/>FANTASY MANAGER</span></div>
  <div className="connect-grid"><div className="connect-intro">
    <Caption>01 / ESTABLISH YOUR WORKSPACE</Caption><h1>Better decisions start with better information<span>.</span></h1>
    <p>The Powers–Newlon Model turns independent NFL forecasts into decisions for your actual fantasy league.</p>
    <div className="connect-signals"><span><ShieldCheck size={17}/> Sleeper is used for league access, not projections.</span><span><Target size={17}/> Your model stays independent.</span><span><Clock3 size={17}/> Current rosters and scoring are labeled separately.</span></div>
   </div><section className="connect-panel"><div className="connect-panel-head"><span>LEAGUE CONNECTION</span><strong>01—02</strong></div>
    <h2>Find your league.</h2><p>Enter your Sleeper username. No password or account authorization is required for public league information.</p>
    <form onSubmit={submit} className="connect-form"><label htmlFor="sleeper-name">SLEEPER USERNAME</label><input id="sleeper-name" type="text" autoComplete="username" autoCapitalize="off" spellCheck={false} value={name} onChange={e=>setName(e.target.value)} placeholder="Enter your username" required minLength={1}/>
     <label htmlFor="league-season">SEASON</label><select id="league-season" value={season} onChange={e=>setSeason(e.target.value)}>{Array.from({length:7},(_,i)=>new Date().getFullYear()+1-i).map(y=><option value={y} key={y}>{y}</option>)}</select>
     <button className="primary-action" disabled={loading||!name.trim()} type="submit">{loading?'Finding leagues…':'Find my leagues'} <ArrowRight size={17}/></button>
    </form>{error&&<div role="alert" className="connect-error">{error}</div>}
    {connected&&<div className="connected-leagues"><div className="mini-label">{leagues.length?'AVAILABLE LEAGUES':'NO LEAGUES FOUND'}</div>
     {leagues.length?leagues.map(l=><button className="connect-league" key={l.league_id} onClick={()=>onSelect(l.league_id)} type="button"><span><strong>{l.name}</strong><small>{l.season} · {l.total_rosters||'—'} teams · {Number(l.settings?.type)===2?'Dynasty':'League'}</small></span><ArrowUpRight size={18}/></button>)
     :<p>No NFL leagues were returned for this user and season. Try a different season or username.</p>}</div>}
    <small className="connect-note">Connected league data remains read-only. No lineup or roster changes will be submitted.</small>
   </section></div>
  <div className="connect-bottom">ENGINEERED FOR THE DECISION. <span>ATLAS × MONOGRAPH</span></div>
 </div>;
}
function SectionTitle({index,title,action}:{index:string;title:string;action?:ReactNode}){return <div className="section-title"><span className="section-index">{index}</span><span>{title}</span><span className="section-line"/>{action}</div>}
function Score({value,decimals=1}:{value:number|null|undefined;decimals?:number}){return <span className="tabular">{fmt(value,decimals)}</span>}
function MatchupInstrument({matchup,week,league,onNavigate,updated}:{matchup:MatchupView;week:number;league:League;onNavigate:()=>void;updated:number}){
 const actual=matchup.scoringReported,share=matchup.my.total!==null&&matchup.their.total!==null&&matchup.my.total+matchup.their.total>0?100*matchup.my.total/(matchup.my.total+matchup.their.total):50;
 const score1=actual?matchup.myActual:matchup.my.total,score2=actual?matchup.theirActual:matchup.their.total;
 return <section className="instrument" aria-label={'Week '+week+' matchup'}>
  <div className="instrument-top"><span>01 / MATCHUP INTELLIGENCE</span><span className="instrument-status"><i/>{matchup.state==='complete'?'COMPLETED WEEK':actual?'SCORES REPORTED':'PREGAME FORECAST'}</span></div>
  <div className="instrument-main"><div className="instrument-stat"><span className="stat-kicker">{actual?'CURRENT POINT DIFFERENCE':'EXPECTED POINT DIFFERENCE'}</span><div className="instrument-number"><Score value={matchup.margin}/><small>PPR</small></div><p>{actual?'Based on actual points reported by Sleeper.':'Based on model forecasts for current starters.'} Not a calibrated probability of winning.</p></div>
   <div className="instrument-teams"><div className="instrument-team"><div><span>01 / YOUR TEAM</span><strong>{matchup.myTeam}</strong></div><b><Score value={score1}/></b></div>
    <div className="meter"><i style={{width:share+'%'}}/></div>
    <div className="instrument-team"><div><span>02 / OPPONENT</span><strong>{matchup.opponentTeam}</strong></div><b><Score value={score2}/></b></div>
    <div className="meter opponent"><i style={{width:(100-share)+'%'}}/></div>
    <div className="instrument-caption">{actual?'SLEEPER ACTUAL SCORES':'CURRENT STARTERS / MODEL PPR'} · WEEK {String(week).padStart(2,'0')}</div></div></div>
  <div className="instrument-bottom"><span>{league.name} · {new Date(updated).toLocaleTimeString([],{hour:'numeric',minute:'2-digit'})} sync</span><button onClick={onNavigate} type="button">Inspect matchup <ArrowUpRight size={15}/></button></div>
 </section>;
}
function Briefing({data,onScreen,onPlayer,ideas}:{data:AppData;onScreen:(s:Screen)=>void;onPlayer:(id:string)=>void;ideas:Decision[]}){
 const {week,matchup,myRoster,index,players,league,bundle}=data;
 const ids=(myRoster?.starters||[]).filter(id=>id&&id!=='0').slice(0,6);
 return <div className="screen briefing-screen">
  <div className="hero-heading"><div><Caption>PN / STRATEGY SYSTEM / WEEK {String(week).padStart(2,'0')}</Caption>
   <h1>Matchup<br/><span>signal.</span></h1><p>{league.name} · Model forecast and upcoming decisions</p></div>
   <div className="hero-aside"><span>W{String(week).padStart(2,'0')}</span><small>{bundle.rosters.length} LEAGUE TEAMS</small></div></div>
  <MatchupInstrument matchup={matchup} week={week} league={league} updated={bundle.fetchedAt} onNavigate={()=>onScreen('league')}/>
  <SectionTitle index="02" title="DECISION QUEUE" action={<button className="plain-link" onClick={()=>onScreen('decisions')}>View lab <ArrowRight size={15}/></button>}/>
  <div className="decision-queue">{ideas.slice(0,3).map((d,i)=><button className="queue-row" key={d.id} onClick={()=>onScreen('decisions')}><span className="queue-index">{String(i+1).padStart(2,'0')}</span><span className="queue-main"><strong>{d.title}</strong><small>{d.detail}</small></span><span className="queue-value">{d.gain!==null?'+'+fmt(d.gain)+' PPR':'REVIEW'}</span><ArrowUpRight size={17}/></button>)}</div>
  <SectionTitle index="03" title="STARTING EXPOSURE" action={<button className="plain-link" onClick={()=>onScreen('league')}>Full roster <ArrowRight size={15}/></button>}/>
  <div className="exposure-table"><div className="exposure-head"><span>PLAYER</span><span>MODEL PPR</span></div>{ids.map(id=>{
    const player=players[id],f=index.get(id,player,week);
    return <button type="button" key={id} className="exposure-row" onClick={()=>onPlayer(id)}><span><b>{f.name}</b><small>{f.position} / {f.team}{f.opponent?' / '+f.opponent:''}</small></span><strong><Score value={f.projection}/></strong><ChevronRight size={15}/></button>;
   })}{!ids.length&&<Empty title="No starters found" detail="The selected league does not currently expose an active lineup."/>}</div>
  <div className="editorial-note"><div><Caption>THE MODEL / DATA DISCIPLINE</Caption><h3>Precision starts with context.</h3></div><p>Model projections are independent of Sleeper. Missing players remain unprojected, and actual scores are kept separate from forecasts. Your starting lineup and an optimized scenario are never presented as the same thing.</p></div>
 </div>;
}
function QuickForecast({forecast}:{forecast:Forecast}){return <div className="forecast-stats">
  <div><Caption>MODEL PPR</Caption><strong><Score value={forecast.projection}/></strong></div>
  <div><Caption>LOW / HIGH</Caption><strong>{fmt(forecast.floor)} <span>—</span> {fmt(forecast.ceiling)}</strong></div>
  <div><Caption>DYNASTY</Caption><strong><Score value={forecast.dynasty} decimals={0}/></strong></div>
 </div>}
function DecisionLab({data,ideas,onPlayer}:{data:AppData;ideas:Decision[];onPlayer:(id:string)=>void}){
 const {myRoster,index,players,week,league,rosters}=data;
 const [section,setSection]=useState<DecisionScreen>('opportunities');
 const owned=(myRoster?.players||[]).filter(id=>players[id]&&['QB','RB','WR','TE'].includes(String(players[id].position||'')));
 const [left,setLeft]=useState(''),[right,setRight]=useState(''),[reviewed,setReviewed]=useState<string[]>([]);
 const leftId=owned.includes(left)?left:owned[0],rightId=owned.includes(right)?right:(owned[1]||owned[0]);
 const a=leftId?index.get(leftId,players[leftId],week):null,b=rightId?index.get(rightId,players[rightId],week):null;
 const waived=useMemo(()=>myRoster?waiverIdeas(myRoster,rosters,league,index,players,week):[],[myRoster,rosters,league,index,players,week]);
 const toggle=(id:string)=>setReviewed(current=>current.includes(id)?current.filter(x=>x!==id):[...current,id]);
 return <div className="screen">
  <header className="page-heading"><Caption>02 / DECISION INTELLIGENCE</Caption><h1>Decision lab<span>.</span></h1><p>Explore realistic alternatives and the evidence behind them. Nothing is submitted to Sleeper.</p></header>
  <div className="segmented" role="group" aria-label="Decision lab mode">{([['opportunities','Opportunities'],['compare','Start / Sit'],['waivers','Waiver radar']] as const).map(([key,label])=><button type="button" key={key} className={section===key?'selected':''} aria-pressed={section===key} onClick={()=>setSection(key)}>{label}</button>)}</div>
  {section==='opportunities'&&<><SectionTitle index="01" title="PRIORITIZED ACTIONS"/><div className="opportunity-list">{ideas.map((idea,i)=><article className="opportunity" key={idea.id}>
    <div className="opportunity-number">{String(i+1).padStart(2,'0')}</div><div className="opportunity-text"><Caption>{idea.kind.toUpperCase()} / ADVISORY</Caption><h2>{idea.title}</h2><p>{idea.detail}</p><span className="small-note">{idea.note}</span>
    <div className="opportunity-cta">{idea.playerId&&<button className="outline-action" onClick={()=>onPlayer(idea.playerId!)}>Inspect player <ArrowUpRight size={15}/></button>}
      <button className={reviewed.includes(idea.id)?'review-button reviewed':'review-button'} onClick={()=>toggle(idea.id)}>{reviewed.includes(idea.id)?<><Check size={15}/> Reviewed</>:<>Mark reviewed <ArrowRight size={15}/></>}</button></div></div>
    <div className="opportunity-impact"><span>EST. WEEKLY GAIN</span><strong>{idea.gain!==null?'+'+fmt(idea.gain):'—'}</strong><small>PPR / MODEL</small></div></article>)}</div>
    <div className="context-strip"><Info size={18}/>Lineup recommendations use projected player production, not actual in-game fantasy points. Waiver eligibility and transaction rules require final verification in Sleeper.</div></>}
  {section==='compare'&&<><SectionTitle index="01" title="PLAYER COMPARISON"/><div className="compare-controls">
    <label>PLAYER A<select aria-label="First player" value={leftId||''} onChange={e=>setLeft(e.target.value)}>{owned.map(id=><option value={id} key={id}>{index.get(id,players[id],week).name}</option>)}</select></label>
    <label>PLAYER B<select aria-label="Second player" value={rightId||''} onChange={e=>setRight(e.target.value)}>{owned.map(id=><option value={id} key={id}>{index.get(id,players[id],week).name}</option>)}</select></label></div>
    {a&&b?<><div className="comparison"><div className="comparison-player"><Caption>{a.position} / {a.team}</Caption><h2>{a.name}</h2><QuickForecast forecast={a}/><button className="plain-link" onClick={()=>onPlayer(a.id)}>Player evidence <ArrowUpRight size={15}/></button></div>
      <div className="comparison-player"><Caption>{b.position} / {b.team}</Caption><h2>{b.name}</h2><QuickForecast forecast={b}/><button className="plain-link" onClick={()=>onPlayer(b.id)}>Player evidence <ArrowUpRight size={15}/></button></div></div>
      <div className="comparison-summary"><span>MODEL DIFFERENCE / PLAYER A MINUS B</span><strong>{compare(a,b).delta===null?'—':(compare(a,b).delta!>0?'+':'')+fmt(compare(a,b).delta)}</strong><p>{compare(a,b).delta===null?'The model is missing at least one weekly projection.':'This is a forecast difference, not an assurance of future points.'} Check eligibility and lineup locks before starting either player.</p></div></>
     :<Empty title="No comparable players" detail="Connect a league with offensive skill-position players."/>}</>}
  {section==='waivers'&&<><SectionTitle index="01" title="AVAILABLE PLAYER SCENARIOS"/>{waived.length?<div className="opportunity-list">{waived.map((w,i)=><article className="opportunity" key={w.id}><div className="opportunity-number">{String(i+1).padStart(2,'0')}</div><div className="opportunity-text"><Caption>UNROSTERED IN LEAGUE / REVIEW REQUIRED</Caption><h2>{w.title}</h2><p>{w.detail}</p><span className="small-note">{w.note}</span>
     <div className="opportunity-cta"><button className="outline-action" onClick={()=>w.playerId&&onPlayer(w.playerId)}>Inspect player <ArrowUpRight size={15}/></button></div></div>
     <div className="opportunity-impact"><span>PROJECTED IMPACT</span><strong>+{fmt(w.gain)}</strong><small>PPR / MODEL</small></div></article>)}</div>:<Empty title="No actionable waiver upgrades" detail="The model did not identify an unrostered player that improved the optimal lineup above the threshold. Missing projections or a full roster may limit recommendations."/>}
    <div className="context-strip"><ShieldCheck size={17}/>Availability means the player is not on a currently loaded roster. It does not guarantee the player is claimable, eligible, or unlocked.</div></>}
 </div>;
}
function Scout({data,onPlayer}:{data:AppData;onPlayer:(id:string)=>void}){
 const {players,index,week,myRoster}=data;
 const [search,setSearch]=useState(''),[position,setPosition]=useState('ALL'),[showAll,setShowAll]=useState(false);
 const results=useMemo(()=>{
  const q=search.trim().toLowerCase();
  const rosterIds=new Set(myRoster?.players||[]);
  const all=Object.entries(players).filter(([id,p])=>['QB','RB','WR','TE'].includes(String(p.position||''))&&(position==='ALL'||p.position===position)&&
   (!q||String(p.full_name||[p.first_name,p.last_name].join(' ')).toLowerCase().includes(q)));
  const sorted=all.sort(([a],[b])=>{
   if(!q&&!showAll){const ownA=rosterIds.has(a)?1:0,ownB=rosterIds.has(b)?1:0;if(ownA!==ownB)return ownB-ownA}
   return (index.get(b,players[b],week).projection??-1)-(index.get(a,players[a],week).projection??-1);
  });
  return sorted.slice(0,showAll||q?80:30);
 },[players,index,week,search,position,showAll,myRoster]);
 return <div className="screen"><header className="page-heading"><Caption>03 / MODEL INTELLIGENCE</Caption><h1>Scout<span>.</span></h1><p>Search the NFL player universe and inspect the actual Powers–Newlon forecast.</p></header>
  <div className="scout-search"><Search size={20}/><input type="search" placeholder="Search a player by name…" aria-label="Search players" autoCapitalize="off" autoCorrect="off" value={search} onChange={e=>setSearch(e.target.value)}/>{search&&<button type="button" aria-label="Clear search" onClick={()=>setSearch('')}><X size={17}/></button>}</div>
  <div className="scout-filter"><span>POSITION</span><div>{['ALL','QB','RB','WR','TE'].map(p=><button key={p} className={position===p?'active':''} aria-pressed={position===p} onClick={()=>setPosition(p)}>{p==='ALL'?'ALL':p}</button>)}</div></div>
  <div className="scout-table"><div className="scout-head"><span>PLAYER / TEAM</span><span>FORECAST</span><span>RANGE</span><span></span></div>
    {results.map(([id,p])=>{const f=index.get(id,p,week);return <button className="scout-row" key={id} onClick={()=>onPlayer(id)}>
      <span className="scout-name"><strong>{f.name}</strong><small>{f.position} / {f.team||'FA'} {f.opponent?' / '+f.opponent:''}</small></span>
      <strong className="scout-proj"><Score value={f.projection}/></strong><span className="scout-range">{fmt(f.floor)} – {fmt(f.ceiling)}</span><ArrowUpRight size={16}/></button>})}
    {!results.length&&<Empty title="No matching players" detail="Try a shorter name or another position filter."/>}</div>
    {!search&&!showAll&&<button className="plain-link more-players" onClick={()=>setShowAll(true)}>Explore more players <ArrowRight size={16}/></button>}
  <p className="fine-print">All projections on this page are sourced from the Powers–Newlon Model. Unmatched player identities are shown without fabricated model values.</p>
 </div>;
}
function Trend({f}:{f:Forecast}){
 const rows=f.weekly.filter(r=>r.projection!==null).slice(-12);
 if(rows.length<2)return <Empty title="Trend not available" detail="The current model snapshot has fewer than two historical weekly values for this player."/>;
 const width=500,height=170,pad=20;
 const max=Math.max(5,...rows.flatMap(r=>[r.projection||0,r.actual||0]))*1.1;
 const x=(i:number)=>pad+i*(width-2*pad)/Math.max(1,rows.length-1),y=(val:number)=>height-pad-val*(height-2*pad)/max;
 const projected=rows.map((r,i)=>x(i)+','+y(r.projection||0)).join(' ');
 const actuals=rows.filter(r=>r.actual!==null&&r.played);
 return <div className="trend-wrap"><svg viewBox={'0 0 '+width+' '+height} role="img" aria-label="Weekly model projection trend; historical actual values shown as copper points where available">
  {[0,.5,1].map(n=><line key={n} x1={pad} x2={width-pad} y1={y(n*max)} y2={y(n*max)} className="chart-grid"/>)}
  <polyline points={projected} className="chart-line"/>{actuals.map(r=>{const i=rows.indexOf(r);return <circle key={r.week} cx={x(i)} cy={y(r.actual||0)} r={4} className="chart-actual"/ >})}
 </svg><div className="trend-legend"><span><i/> MODEL PROJECTION</span><span><i/> REPORTED ACTUAL</span></div></div>;
}
function Dossier({f,onClose,onCompare}:{f:Forecast;onClose:()=>void;onCompare:()=>void}){
 const ref=useRef<HTMLDivElement>(null);
 useEffect(()=>{
  const focused=document.activeElement instanceof HTMLElement?document.activeElement:null;
  ref.current?.querySelector<HTMLButtonElement>('.dossier-close')?.focus();
  const handler=(e:KeyboardEvent)=>{
   if(e.key==='Escape'){e.preventDefault();onClose();return}
   if(e.key!=='Tab'||!ref.current)return;
   const nodes=[...ref.current.querySelectorAll<HTMLElement>('button:not([disabled]),a[href],input,select,[tabindex]:not([tabindex="-1"])')].filter(x=>x.getClientRects().length);
   if(!nodes.length)return;
   if(e.shiftKey&&document.activeElement===nodes[0]){e.preventDefault();nodes[nodes.length-1].focus()}
   else if(!e.shiftKey&&document.activeElement===nodes[nodes.length-1]){e.preventDefault();nodes[0].focus()}
  };
  document.addEventListener('keydown',handler);
  const original=document.body.style.overflow;document.body.style.overflow='hidden';
  return()=>{document.removeEventListener('keydown',handler);document.body.style.overflow=original;focused?.focus({preventScroll:true})};
 },[onClose]);
 return <div className="dossier-backdrop" onMouseDown={e=>{if(e.target===e.currentTarget)onClose()}}>
   <div className="dossier" ref={ref} role="dialog" aria-modal="true" aria-label={'Player dossier: '+f.name}>
    <div className="dossier-bar"><Caption>PN / SCOUT REPORT</Caption><button className="dossier-close" onClick={onClose} aria-label="Close player dossier"><X size={21}/></button></div>
    <div className="dossier-title"><span>{f.position} / {f.team} / WEEK {f.week}</span><h2>{f.name}</h2><p>{f.opponent?'Opponent: '+f.opponent+' · ':''}Status: {f.status||'Not provided'}</p></div>
    <div className="dossier-main-stat"><Caption>MODEL PPR FORECAST</Caption><strong><Score value={f.projection}/></strong><p>{f.matched?'Powers–Newlon Model snapshot':'No validated model match found'}</p></div>
    <QuickForecast forecast={f}/>
    <div className="dossier-section"><SectionTitle index="01" title="WEEKLY FORECAST HISTORY"/><Trend f={f}/></div>
    <div className="dossier-section"><SectionTitle index="02" title="MODEL EVIDENCE"/><div className="dossier-facts">
      <div><span>PROJECTION ORIGIN</span><strong>{f.source==='powers-newlon-model'?'Powers–Newlon':'Unavailable'}</strong></div>
      <div><span>PREGAME PROJECTION</span><strong>{fmt(f.pregame)}</strong></div>
      <div><span>CONFIDENCE FIELD</span><strong>{f.confidence||'Not reported'}</strong></div>
      <div><span>SEASON FORECAST PPG</span><strong>{fmt(f.seasonPpg)}</strong></div>
    </div></div>
    <div className="dossier-bottom"><button className="primary-action" onClick={onCompare}>Compare in Decision Lab <ArrowRight size={17}/></button><span>Advisory only · No transaction will occur</span></div>
   </div>
  </div>;
}

type TradeAsset={key:string;name:string;position:string;value:number|null;type:'player'|'pick'};
function availableTradeAssets(r:Roster,data:AppData):TradeAsset[]{
 const assets:TradeAsset[]=rosterAssets(r,data.index,data.players,data.week)
  .filter(p=>p.position!=='DEF').map(p=>({key:p.id,name:p.name,position:p.position,value:p.dynasty,type:'player'}));
 const roundCount=Math.min(8,num(data.league.settings?.draft_rounds)??4);
 for(let season=Number(data.league.season)+1;season<=Number(data.league.season)+2;season++){
  for(let round=1;round<=roundCount;round++){
   const transfer=data.bundle.picks.find(p=>Number(p.season)===season&&Number(p.round)===round&&Number(p.roster_id)===r.roster_id);
   const currentOwner=transfer?Number(transfer.owner_id):r.roster_id;
   if(currentOwner!==r.roster_id)continue;
   assets.push({key:'pick:'+season+':'+round+':'+r.roster_id,name:season+' Round '+round,
    position:'FUTURE PICK',value:data.index.pickValue(season,round),type:'pick'});
  }
 }
 return assets;
}
function TradeWorkbench({data}:{data:AppData}){
 const {myRoster,rosters,index,players,week,users}=data;
 const [otherId,setOtherId]=useState<string>('');
 const [send,setSend]=useState<string[]>([]),[receive,setReceive]=useState<string[]>([]);
 const r=myRoster||rosters[0],other=rosters.find(x=>String(x.roster_id)===(otherId||String(rosters.find(x=>x.roster_id!==r?.roster_id)?.roster_id)))||null;
 useEffect(()=>{setSend([]);setReceive([])},[otherId]);
 if(!r||!other)return <Empty title="A second team is required" detail="The trade workbench needs a connected league with at least two rosters."/>;
 const a=availableTradeAssets(r,data),b=availableTradeAssets(other,data);
 const selectedA=a.filter(x=>send.includes(x.key)),selectedB=b.filter(x=>receive.includes(x.key));
 const total=(xs:TradeAsset[])=>xs.reduce((n,x)=>n+(x.value??0),0);
 const missing=[...selectedA,...selectedB].some(x=>x.value===null);
 const diff=selectedA.length&&selectedB.length&&!missing?total(selectedB)-total(selectedA):null;
 const toggle=(v:string,selected:string[],setter:(next:string[])=>void)=>setter(selected.includes(v)?selected.filter(x=>x!==v):[...selected,v]);
 return <div className="trade-workbench">
  <SectionTitle index="01" title="BUILD A PROPOSED TRADE"/>
  <div className="trade-control"><label htmlFor="trade-opponent">TRADE PARTNER</label><select id="trade-opponent" value={other.roster_id} onChange={e=>setOtherId(e.target.value)}>{rosters.filter(x=>x.roster_id!==r.roster_id).map(x=><option key={x.roster_id} value={x.roster_id}>{rosterName(x,users)}</option>)}</select></div>
  <div className="trade-overview">
   <div><Caption>MODEL DYNASTY VALUE / NET RECEIVED</Caption><strong>{diff===null?'—':(diff>0?'+':'')+fmt(diff,0)}</strong></div>
   <div><Caption>SELECTED ASSETS</Caption><strong>{send.length} <span>⇄</span> {receive.length}</strong></div>
   <p>{missing?'A selected asset has no model valuation. Totals are not comparable.':!send.length||!receive.length?'Choose at least one asset from each side.':'Positive means the package you receive has higher summed model dynasty value.'}</p>
  </div>
  <div className="trade-columns">
   <AssetList label="01 / ASSETS YOU SEND" team={rosterName(r,users)} assets={a} selected={send} onSelect={id=>toggle(id,send,setSend)}/>
   <AssetList label="02 / ASSETS YOU RECEIVE" team={rosterName(other,users)} assets={b} selected={receive} onSelect={id=>toggle(id,receive,setReceive)}/>
  </div>
  <div className="context-strip"><Info size={17}/>This is a valuation comparison, not an acceptance or win-probability forecast. Pick values are round-level model baselines, not predictions of exact draft position. Nothing is sent to Sleeper.</div>
 </div>;
}
function AssetList({label,team,assets,selected,onSelect}:{label:string;team:string;assets:TradeAsset[];selected:string[];onSelect:(key:string)=>void}){
 const [filter,setFilter]=useState('ALL');
 return <section className="asset-column"><div className="asset-column-top"><Caption>{label}</Caption><h3>{team}</h3></div>
  <div className="asset-filters">{['ALL','QB','RB','WR','TE','PICKS'].map(x=><button key={x} className={filter===x?'active':''} onClick={()=>setFilter(x)}>{x}</button>)}</div>
  <div className="asset-scroller">{assets.filter(a=>filter==='ALL'||(filter==='PICKS'?a.type==='pick':a.position===filter)).map(asset=><button className={'asset-line '+(selected.includes(asset.key)?'picked':'')} type="button" key={asset.key} onClick={()=>onSelect(asset.key)} aria-pressed={selected.includes(asset.key)}>
   <span className="asset-check">{selected.includes(asset.key)&&<Check size={14}/>}</span><span className="asset-name"><strong>{asset.name}</strong><small>{asset.position}</small></span><b>{fmt(asset.value,0)}</b></button>)}
   {assets.length===0&&<p className="small-note">No eligible assets loaded.</p>}</div>
   <div className="asset-total"><span>SELECTED / {selected.length}</span><strong>{fmt(assets.filter(a=>selected.includes(a.key)).reduce((v,a)=>v+(a.value??0),0),0)}</strong></div>
 </section>;
}
function LeagueView({data,onPlayer,onRefresh,refreshing}:{data:AppData;onPlayer:(id:string)=>void;onRefresh:()=>void;refreshing:boolean}){
 const [section,setSection]=useState<LeagueScreen>('standings');
 const [teamId,setTeamId]=useState<string>('');
 const {rosters,users,myRoster,players,index,league,week,matchup,bundle}=data;
 const chosen=rosters.find(x=>String(x.roster_id)===(teamId||String(myRoster?.roster_id)))||rosters[0];
 const ranked=useMemo(()=>standings(rosters,users),[rosters,users]);
 const ownIds=new Set(myRoster?.players||[]);
 return <div className="screen league-screen"><header className="page-heading"><Caption>04 / LEAGUE INTELLIGENCE</Caption><h1>League<span>.</span></h1><p>{league.name} · A competitive view of the season, roster composition, and long-term assets.</p></header>
  <div className="league-subnav" role="group" aria-label="League sections">{([['standings','Standings'],['roster','Roster'],['trade','Trade'],['draft','Draft'],['health','Data health']] as const).map(([key,label])=><button className={section===key?'active':''} aria-pressed={section===key} key={key} onClick={()=>setSection(key)}>{label}</button>)}</div>
  {section==='standings'&&<><SectionTitle index="01" title="LEAGUE STANDINGS"/><div className="standings-table">
   <div className="standings-head"><span>RANK / TEAM</span><span>RECORD</span><span>POINTS FOR</span></div>
   {ranked.map((r,i)=><button key={r.id} className={'standings-row '+(r.id===myRoster?.roster_id?'yours':'')} onClick={()=>{setTeamId(String(r.id));setSection('roster')}}><span className="standings-index">{String(i+1).padStart(2,'0')}</span><span className="standings-name"><strong>{r.name}</strong><small>{r.id===myRoster?.roster_id?'YOUR TEAM':'VIEW ROSTER'}</small></span><span className="standings-record">{r.wins}–{r.losses}{r.ties?'-'+r.ties:''}</span><span className="standings-points">{fmt(r.pts)}</span><ChevronRight size={15}/></button>)}</div>
    <div className="context-strip"><Info size={17}/>Records and points for are taken from Sleeper. These are actual league statistics, not model predictions.</div></>}
  {section==='roster'&&<><div className="roster-heading"><SectionTitle index="01" title="ROSTER INTELLIGENCE"/><label>TEAM<select value={chosen?.roster_id||''} onChange={e=>setTeamId(e.target.value)}>{rosters.map(r=><option key={r.roster_id} value={r.roster_id}>{rosterName(r,users)}</option>)}</select></label></div>
   <div className="roster-overview"><div><Caption>TEAM RECORD</Caption><strong>{chosen?recordOf(chosen).slice(0,2).join('–'):'—'}</strong></div>
    <div><Caption>ACTUAL POINTS FOR</Caption><strong>{chosen?fmt(pf(chosen)):'—'}</strong></div>
    <div><Caption>MODELED OPTIMAL PPR</Caption><strong>{chosen?fmt(optimal(chosen,league,index,players,week).total):'—'}</strong></div></div>
   {chosen&&<><SectionTitle index="02" title="STARTERS"/><div className="roster-list">{lineup(chosen,league,index,players,week).members.map((m,i)=>{
     const f=m.id==='0'?null:index.get(m.id,players[m.id],week);
     return <button key={i} className="roster-row" disabled={!f} onClick={()=>f&&onPlayer(f.id)}><span className="roster-position">{m.slot}</span><span><strong>{f?.name||'Unfilled'}</strong><small>{f?f.team+' · '+f.position:'No starter assigned'}</small></span><b>{fmt(m.projection)}</b><ChevronRight size={15}/></button>})}</div>
     <SectionTitle index="03" title="BENCH & RESERVES"/><div className="roster-list">{(chosen.players||[]).filter(id=>!chosen.starters.includes(id)).map(id=>{
      const f=index.get(id,players[id],week);return <button key={id} className="roster-row" onClick={()=>onPlayer(id)}><span className="roster-position">{f.position}</span><span><strong>{f.name}</strong><small>{f.team} · {f.status||'Status unavailable'}</small></span><b>{fmt(f.projection)}</b><ChevronRight size={15}/></button>})}</div></>}
   </>}
  {section==='trade'&&<TradeWorkbench data={data}/>}
  {section==='draft'&&<><SectionTitle index="01" title="FUTURE PICK OUTLOOK"/><p className="section-description">Model dynasty values by draft year and round. Exact pick slots are not forecast by this screen.</p>
    <div className="draft-table">{Array.from({length:3},(_,offset)=>Number(league.season)+offset).flatMap(y=>Array.from({length:Math.min(6,num(league.settings?.draft_rounds)??4)},(_,i)=>({year:y,round:i+1}))).map(x=><div className="draft-row" key={x.year+'-'+x.round}><strong>{x.year} · Round {x.round}</strong><span>Model baseline</span><b>{fmt(index.pickValue(x.year,x.round),0)}</b></div>)}</div>
    <p className="fine-print">Pick ownership and exact selection position vary with trades, season results, and league rules. This table is a valuation reference, not your confirmed pick inventory.</p></>}
  {section==='health'&&<><SectionTitle index="01" title="DATA & PROVENANCE"/><div className="health-list">
    <div><span>MODEL SOURCE</span><strong>Powers–Newlon Model</strong><small>{index.rows.length.toLocaleString()} player rows in snapshot</small></div>
    <div><span>LEAGUE SOURCE</span><strong>Sleeper · Read-only</strong><small>{rosters.length} connected rosters, {bundle.matchups.length} matchup entries</small></div>
    <div><span>PROJECTION WEEK</span><strong>{String(index.snapshot.projection_week??'Unspecified')}</strong><small>Model production week, not necessarily the chosen league week</small></div>
    <div><span>LEAGUE LAST REFRESH</span><strong>{new Date(bundle.fetchedAt).toLocaleString()}</strong><small>Actual points and roster changes may update independently.</small></div>
    <div><span>STARTER COVERAGE</span><strong>{matchup.my.known}/{matchup.my.slots}</strong><small>{matchup.my.missing.length?matchup.my.missing.length+' starter slots have no model forecast.':'All current starter slots have model forecasts.'}</small></div>
   </div><button className="outline-action" onClick={onRefresh} disabled={refreshing}><RefreshCw size={16}/>{refreshing?' Refreshing…':' Refresh league data'}</button>
   <div className="context-strip"><ShieldCheck size={17}/>No Sleeper projections are used to fill missing model values. A missing forecast remains missing. Historical forecasts should be retained as locked benchmarks by the model pipeline.</div></>}
 </div>;
}
function LoadingWorkspace({error,onRetry}:{error:unknown;onRetry:()=>void}){
 return <div className="loading-workspace"><div className="brand-symbol">PN</div><Caption>PREPARING YOUR INTELLIGENCE WORKSPACE</Caption><h1>Connecting the pieces<span>.</span></h1><p>Loading your Sleeper league, roster identities and the Powers–Newlon Model snapshot.</p><div className="loading-track"><span/></div>{error&&<ErrorLine error={error} onRetry={onRetry}/>}
 </div>;
}
export function App(){
 const queryClient=useQueryClient();
 const savedUser=safeStored('pn_next_username')||'';
 const savedSeason=Number(safeStored('pn_next_season'))||new Date().getFullYear();
 const [username,setUsername]=useState(savedUser),[season,setSeason]=useState(savedSeason);
 const [user,setUser]=useState<SleeperUser|null>(null);
 const [leagues,setLeagues]=useState<League[]>([]);
 const [connected,setConnected]=useState(false),[connecting,setConnecting]=useState(false),[connectError,setConnectError]=useState<string|null>(null);
 const [leagueId,setLeagueId]=useState<string|null>(null);
 const [weekOverride,setWeekOverride]=useState<number|null>(null);
 const [screen,setScreen]=useState<Screen>('briefing');
 const [dossierId,setDossierId]=useState<string|null>(null);
 const pendingRequest=useRef(0),started=useRef(false);
 const connect=useCallback(async(name:string,yr:number,restore=false)=>{
  const id=++pendingRequest.current;
  setConnecting(true);setConnectError(null);
  try{
   const found=await getSleeperUser(name);
   const results=await getLeagues(found.user_id,yr);
   if(id!==pendingRequest.current)return;
   setUser(found);setUsername(name);setSeason(yr);setLeagues(results);setConnected(true);
   remember('pn_next_username',name);remember('pn_next_season',String(yr));
   const chosen=restore?safeStored('pn_next_league'):null;
   if(chosen&&results.some(l=>l.league_id===chosen))setLeagueId(chosen);
  }catch(error){
   if(id!==pendingRequest.current)return;
   setConnectError(error instanceof Error?error.message:'Could not retrieve Sleeper leagues.');
   setConnected(false);
  }finally{if(id===pendingRequest.current)setConnecting(false)}
 },[]);
 useEffect(()=>{if(started.current)return;started.current=true;if(savedUser)void connect(savedUser,savedSeason,true)},[connect,savedUser,savedSeason]);
 const nfl=useQuery({queryKey:['nfl-state'],queryFn:({signal})=>getNflState(signal),staleTime:10*60*1000});
 const nflWeek=nfl.data?.week??1;
 const selectedWeek=weekOverride??nflWeek;
 const bundleQuery=useQuery({queryKey:['next-bundle',leagueId,selectedWeek],queryFn:({signal})=>getLeagueBundle(leagueId!,selectedWeek,signal),enabled:!!leagueId,staleTime:70_000});
 const modelQuery=useQuery({queryKey:['next-model'],queryFn:({signal})=>getSnapshot(signal),enabled:!!leagueId,staleTime:Infinity});
 const playersQuery=useQuery({queryKey:['next-players'],queryFn:({signal})=>getPlayers(signal),enabled:!!leagueId,staleTime:12*60*60*1000});
 const index=useMemo(()=>modelQuery.data?new ModelIndex(modelQuery.data):null,[modelQuery.data]);
 const data=useMemo<AppData|null>(()=>{
  if(!bundleQuery.data||!playersQuery.data||!index||!leagueId)return null;
  const bundle=bundleQuery.data,players=playersQuery.data,roster=bundle.rosters.find(r=>String(r.owner_id)===String(user?.user_id))||null;
  return {league:bundle.league,users:bundle.users,rosters:bundle.rosters,players,bundle,index,week:selectedWeek,
   nflWeek,myRoster:roster,matchup:matchupView(roster,bundle.rosters,bundle.users,bundle.matchups,bundle.league,index,players,selectedWeek,nflWeek)};
 },[bundleQuery.data,playersQuery.data,index,leagueId,user?.user_id,selectedWeek,nflWeek]);
 const ideas=useMemo(()=>data?.myRoster?lineupDecisions(data.myRoster,data.league,data.index,data.players,data.week):[],[data]);
 const selectLeague=(id:string)=>{setLeagueId(id);remember('pn_next_league',id);setWeekOverride(null);setScreen('briefing')};
 const switchLeague=()=>{setLeagueId(null);remember('pn_next_league',null);setDossierId(null);setScreen('briefing')};
 const closeDossier=useCallback(()=>setDossierId(null),[]);
 const openPlayer=(id:string)=>setDossierId(id);
 const goCompare=()=>{setDossierId(null);setScreen('decisions')};
 const refresh=()=>{void queryClient.invalidateQueries({queryKey:['next-bundle',leagueId]})};
 useEffect(()=>{
  if(!leagueId)return;
  const check=()=>{if(document.visibilityState==='visible')refresh()};
  const timer=window.setInterval(check,90_000);
  document.addEventListener('visibilitychange',check);
  return()=>{window.clearInterval(timer);document.removeEventListener('visibilitychange',check)};
 },[leagueId,queryClient]);
 if(!leagueId)return <Connect onConnect={connect} onSelect={selectLeague} leagues={leagues} username={username} year={season} loading={connecting} error={connectError} connected={connected}/>;
 const error=bundleQuery.error||modelQuery.error||playersQuery.error;
 if(!data)return <LoadingWorkspace error={error} onRetry={()=>{void bundleQuery.refetch();void modelQuery.refetch();void playersQuery.refetch()}}/>;
 const dossier=dossierId?data.index.get(dossierId,data.players[dossierId],data.week):null;
 return <AppShell screen={screen} setScreen={setScreen} league={data.league} week={selectedWeek} setWeek={setWeekOverride} onSwitch={switchLeague} updatedAt={data.bundle.fetchedAt}>
   {nfl.isError&&<div className="nfl-warning">NFL week could not be refreshed. Select the correct week manually.</div>}
   {bundleQuery.isFetching&&<div className="refresh-indicator"><RefreshCw size={13}/> Updating league data without leaving this screen</div>}
   {screen==='briefing'&&<Briefing data={data} onScreen={setScreen} onPlayer={openPlayer} ideas={ideas}/>}
   {screen==='decisions'&&<DecisionLab data={data} ideas={ideas} onPlayer={openPlayer}/>}
   {screen==='scout'&&<Scout data={data} onPlayer={openPlayer}/>}
   {screen==='league'&&<LeagueView data={data} onPlayer={openPlayer} onRefresh={refresh} refreshing={bundleQuery.isFetching}/>}
   {dossier&&<Dossier f={dossier} onClose={closeDossier} onCompare={goCompare}/>}
  </AppShell>;
}
