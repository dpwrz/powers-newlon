import React,{useCallback,useEffect,useMemo,useRef,useState,type FormEvent,type ReactNode} from 'react';
import {useQuery,useQueryClient} from '@tanstack/react-query';
import {createPortal} from 'react-dom';
import {ArrowDownRight,ArrowLeft,ArrowRight,ArrowUpRight,BarChart3,BookOpen,Check,ChevronDown,
 ChevronRight,Clock3,Compass,GitCompareArrows,Info,Layers,Menu,RefreshCw,Search,ShieldCheck,
 SlidersHorizontal,Sparkles,Target,TrendingUp,X} from 'lucide-react';
import {getLeagueBundle,getLeagues,getNflState,getPlayers,getSleeperUser,getSnapshot,getLeagueActualHistory,getNflScores,remember,safeStored,
 type LeagueBundle} from './client';
import {ModelIndex,canonicalTeam,eligible,fmt,lineup,matchupView,num,optimal,pf,recordOf,rosterName,
 type Decision,type Forecast,type League,type MatchupView,type Member,type PlayerMap,type Roster,type SleeperUser} from './domain';
import {compare,lineupDecisions,rosterAssets,standings,tradeSummary,waiverIdeas} from './decisions';

type Screen='briefing'|'decisions'|'scout'|'league'|'scores';
type LeagueScreen='standings'|'roster'|'leaders'|'trade'|'draft';
type DecisionScreen='opportunities'|'compare'|'waivers';
type AppData={league:League;users:Member[];rosters:Roster[];players:PlayerMap;bundle:LeagueBundle;
 index:ModelIndex;week:number;nflWeek:number;myRoster:Roster|null;matchup:MatchupView};
const iconSize=18;
function Caption({children}: {children:ReactNode}){return <span className="caption">{children}</span>}
function Empty({title,detail,action}:{title:string;detail:string;action?:ReactNode}){return <div className="empty-state"><Info size={22}/><h3>{title}</h3><p>{detail}</p>{action}</div>}
function ErrorLine({error,onRetry}:{error:unknown;onRetry?:()=>void}){const message=error instanceof Error?error.message:'This information could not be loaded.';return <div className="error-line" role="alert"><Info size={17}/><span>{message}</span>{onRetry&&<button className="text-action" onClick={onRetry}>Retry</button>}</div>}
function Symbol({kind}:{kind:Screen}){return kind==='briefing'?<Layers size={iconSize}/>:kind==='decisions'?<Target size={iconSize}/>:kind==='scout'?<Search size={iconSize}/>:kind==='scores'?<Clock3 size={iconSize}/>:<BarChart3 size={iconSize}/>}
function AppShell({screen,setScreen,league,week,setWeek,onSwitch,children,updatedAt}:
 {screen:Screen;setScreen:(s:Screen)=>void;league:League;week:number;setWeek:(w:number)=>void;onSwitch:()=>void;
 children:ReactNode;updatedAt:number|null}){
 const sections:{key:Screen;label:string;sub:string}[]=[
  {key:'briefing',label:'My Team',sub:'Team dashboard'},
  {key:'league',label:'League',sub:'Standings, trades and draft'},
  {key:'decisions',label:'Start / Sit',sub:'Recommendations'},
  {key:'scout',label:'Available',sub:'Unrostered players'},
  {key:'scores',label:'Scores',sub:'NFL matchups'}];
 const [menu,setMenu]=useState(false);
 const change=(next:Screen)=>{if(next===screen)return;setScreen(next);window.scrollTo({top:0,behavior:'instant'});setMenu(false)};
 return <div className="app-frame">
  <a className="skip-link" href="#main-content">Skip to content</a>
  <aside className="rail" aria-label="Primary navigation">
   <div className="brand"><div className="brand-symbol">PN</div><div><strong>POWERS–NEWLON</strong><span>FANTASY MANAGER</span></div></div>
   <div className="rail-description">A sharper way to see the game.</div>
   <div className="rail-section">WORKSPACE <span>/ 05</span></div>
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
  {createPortal(<nav className="mobile-dock" aria-label="Mobile main sections">{sections.map(s=><button type="button" key={s.key} className={screen===s.key?'active':''} aria-current={screen===s.key?'page':undefined} onClick={()=>change(s.key)}><Symbol kind={s.key}/><span>{s.label}</span></button>)}</nav>,document.body)}
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
function PlayerPortrait({id,name,position,team,size='regular'}:{id:string;name:string;position?:string;team?:string;size?:'regular'|'large'}){
 const [failed,setFailed]=useState(false);
 const defense=position==='DEF'||position==='D/ST';
 const code=(team||id).toLowerCase();
 const url=defense?'https://sleepercdn.com/images/team_logos/nfl/'+encodeURIComponent(code)+'.png':
  'https://sleepercdn.com/content/nfl/players/thumb/'+encodeURIComponent(id)+'.jpg';
 return <span className={'player-portrait '+(size==='large'?'portrait-large':'')} aria-hidden="true">
  {!failed&&id!=='0'?<img src={url} alt="" loading="lazy" decoding="async" onError={()=>setFailed(true)}/>:
   <span className="portrait-fallback">{name.split(/\s+/).filter(Boolean).slice(0,2).map(v=>v[0]).join('').toUpperCase()||'PN'}</span>}
 </span>;
}
function PlayerLine({id,slot,forecast,actual,onPlayer}:{id:string;slot:string;forecast:Forecast|null;actual:number|null;onPlayer:(id:string)=>void}){
 return <button className="team-player-line" disabled={!forecast} onClick={()=>forecast&&onPlayer(id)} type="button">
  <span className="team-slot">{slot}</span>
  <PlayerPortrait id={id} name={forecast?.name||'Empty'} position={forecast?.position} team={forecast?.team}/>
  <span className="team-player-identity"><strong>{forecast?.name||'Empty roster slot'}</strong>
   <small>{forecast?forecast.position+' · '+forecast.team+' · '+(forecast.status||'Status not provided'):'No assigned player'}</small></span>
  <span className="team-player-proj"><strong>{fmt(forecast?.projection)}</strong><small>MODEL</small></span>
  {actual!==null&&<span className="team-player-actual" title="Sleeper league-scored actual points">{fmt(actual)}<small>ACTUAL</small></span>}
  <ChevronRight size={15} className="team-row-arrow"/>
 </button>;
}
function StartSitSpotlight({data,ideas,onScreen,onPlayer}:{data:AppData;ideas:Decision[];onScreen:(s:Screen)=>void;onPlayer:(id:string)=>void}){
 const recommendation=ideas.find(d=>d.kind==='lineup'&&d.playerId&&d.sitPlayerId);
 if(!recommendation){
  return <section className="startsit-spotlight startsit-none">
   <div className="startsit-header"><Caption>YOUR START / SIT OUTLOOK</Caption><span>MODEL-REVIEWED</span></div>
   <h2>No clear lineup upgrade</h2><p>{ideas[0]?.detail||'A confirmed model advantage is not available for your current starters.'}</p>
   <button className="outline-action" onClick={()=>onScreen('decisions')}>Compare your players <ArrowRight size={16}/></button>
  </section>;
 }
 const starter=data.index.get(recommendation.playerId!,data.players[recommendation.playerId!],data.week);
 const sitter=data.index.get(recommendation.sitPlayerId!,data.players[recommendation.sitPlayerId!],data.week);
 return <section className="startsit-spotlight" aria-label="Start Sit recommendation">
  <div className="startsit-header"><Caption>START / SIT RECOMMENDATION</Caption><span>MODEL OPPORTUNITY</span></div>
  <div className="startsit-duel">
   <button className="startsit-athlete" type="button" onClick={()=>onPlayer(starter.id)}>
    <span className="startsit-label start">START</span>
    <PlayerPortrait id={starter.id} name={starter.name} position={starter.position} team={starter.team} size="large"/>
    <span className="startsit-athlete-copy"><strong>{starter.name}</strong><small>{starter.position} · {starter.team}</small></span>
    <span className="startsit-athlete-score">{fmt(starter.projection)}<small>MODEL PPR</small></span>
   </button>
   <span className="startsit-versus">OVER</span>
   <button className="startsit-athlete" type="button" onClick={()=>onPlayer(sitter.id)}>
    <span className="startsit-label sit">SIT</span>
    <PlayerPortrait id={sitter.id} name={sitter.name} position={sitter.position} team={sitter.team} size="large"/>
    <span className="startsit-athlete-copy"><strong>{sitter.name}</strong><small>{sitter.position} · {sitter.team}</small></span>
    <span className="startsit-athlete-score">{fmt(sitter.projection)}<small>MODEL PPR</small></span>
   </button>
  </div>
  <div className="startsit-foot"><div><strong>+{fmt(recommendation.gain)} <span>PPR</span></strong><small>PROJECTED PLAYER DIFFERENCE</small></div>
    <button className="primary-action" onClick={()=>onScreen('decisions')}>Open Start / Sit <ArrowRight size={16}/></button></div>
  <p className="startsit-note">Model recommendation only. Sleeper lineup locks, availability and injury status must be checked.</p>
 </section>;
}
function Briefing({data,onScreen,onPlayer,ideas}:{data:AppData;onScreen:(s:Screen)=>void;onPlayer:(id:string)=>void;ideas:Decision[]}){
 const {week,matchup,myRoster,index,players,league,bundle}=data;
 const starters=lineup(myRoster,league,index,players,week);
 const bench=(myRoster?.players||[]).filter(id=>!(myRoster?.starters||[]).includes(id));
 const rank=standings(data.rosters,data.users).findIndex(x=>x.id===myRoster?.roster_id);
 const record=myRoster?recordOf(myRoster):null;
 const optimized=myRoster?optimal(myRoster,league,index,players,week):null;
 const scoreReported=matchup.scoringReported;
 const playerActuals=(bundle.matchups.find(m=>m.roster_id===myRoster?.roster_id) as Record<string,unknown>|undefined)?.players_points;
 const reported:Record<string,unknown>=scoreReported&&playerActuals&&typeof playerActuals==='object'&&!Array.isArray(playerActuals)?playerActuals as Record<string,unknown>:{};
 const actualFor=(id:string)=>num(reported[id]);
 return <div className="screen briefing-screen">
  <div className="team-page-heading">
   <div><Caption>01 / YOUR TEAM / WEEK {String(week).padStart(2,'0')}</Caption>
    <h1>{matchup.myTeam}<span>.</span></h1><p>{league.name} · Your lineup, matchup and next decisions</p></div>
   <div className="team-rank"><small>LEAGUE POSITION</small><strong>{rank<0?'—':String(rank+1).padStart(2,'0')}</strong><span>{record?record[0]+'–'+record[1]:'Record unavailable'}</span></div>
  </div>
  <div className="team-metrics">
    <div><Caption>CURRENT STARTERS</Caption><strong>{fmt(starters.total)}</strong><small>Model PPR · {starters.known}/{starters.slots} slots projected</small></div>
    <div><Caption>OPTIMIZED LINEUP</Caption><strong>{fmt(optimized?.total)}</strong><small>Hypothetical model lineup</small></div>
    <div><Caption>ACTUAL POINTS FOR</Caption><strong>{myRoster?fmt(pf(myRoster)):'—'}</strong><small>Sleeper season scoring</small></div>
  </div>
  <SectionTitle index="02" title="THIS WEEK'S MATCHUP" action={<button className="plain-link" onClick={()=>onScreen('league')}>League context <ArrowRight size={15}/></button>}/>
  <MatchupInstrument matchup={matchup} week={week} league={league} updated={bundle.fetchedAt} onNavigate={()=>onScreen('league')}/>
  <SectionTitle index="03" title="YOUR BEST LINEUP DECISION" action={<button className="plain-link" onClick={()=>onScreen('decisions')}>All Start / Sit <ArrowRight size={15}/></button>}/>
  <StartSitSpotlight data={data} ideas={ideas} onScreen={onScreen} onPlayer={onPlayer}/>
  <SectionTitle index="04" title="YOUR STARTING LINEUP" action={<span className="section-meta">{starters.known}/{starters.slots} MODEL PROJECTIONS</span>}/>
  <div className="team-roster-list">
   {starters.members.map((m,i)=><PlayerLine key={m.slot+'-'+i} id={m.id} slot={m.slot}
     forecast={m.id==='0'?null:index.get(m.id,players[m.id],week)} actual={actualFor(m.id)} onPlayer={onPlayer}/>)}
   {!starters.members.length&&<Empty title="No starters available" detail="Your selected league has not supplied a current starting lineup."/>}
  </div>
  <SectionTitle index="05" title="BENCH & RESERVES" action={<span className="section-meta">{bench.length} PLAYERS</span>}/>
  <div className="team-roster-list">
   {bench.map(id=><PlayerLine key={id} id={id} slot="BN" forecast={index.get(id,players[id],week)} actual={actualFor(id)} onPlayer={onPlayer}/>)}
   {!bench.length&&<Empty title="No bench players" detail="Your roster currently has no reserve players listed in Sleeper."/>}
  </div>
  <p className="team-footer-note">Your lineup and seasonal actuals come from Sleeper. Every projected value comes from the Powers–Newlon Model. Player actuals are shown separately when reported.</p>
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
  <header className="page-heading"><Caption>02 / LINEUP ADVISOR</Caption><h1>Start / Sit<span>.</span></h1><p>Specific, model-derived starting lineup recommendations and player comparisons. Nothing is submitted to Sleeper.</p></header>
  <StartSitSpotlight data={data} ideas={ideas} onScreen={()=>setSection('compare')} onPlayer={onPlayer}/>
  <div className="segmented" role="group" aria-label="Decision lab mode">{([['opportunities','Opportunities'],['compare','Start / Sit'],['waivers','Waiver radar']] as const).map(([key,label])=><button type="button" key={key} className={section===key?'selected':''} aria-pressed={section===key} onClick={()=>setSection(key)}>{label}</button>)}</div>
  {section==='opportunities'&&<><SectionTitle index="01" title="PRIORITIZED ACTIONS"/><div className="opportunity-list">{ideas.map((idea,i)=><article className="opportunity" key={idea.id}>
    <div className="opportunity-number">{String(i+1).padStart(2,'0')}</div><div className="opportunity-text"><Caption>{idea.kind.toUpperCase()} / ADVISORY</Caption><div className="opportunity-title">{idea.playerId&&<PlayerPortrait id={idea.playerId} name={data.index.get(idea.playerId,data.players[idea.playerId],data.week).name}
 position={data.players[idea.playerId]?.position} team={data.players[idea.playerId]?.team}/>}<h2>{idea.title}</h2></div><p>{idea.detail}</p><span className="small-note">{idea.note}</span>
    <div className="opportunity-cta">{idea.playerId&&<button className="outline-action" onClick={()=>onPlayer(idea.playerId!)}>Inspect player <ArrowUpRight size={15}/></button>}
      <button className={reviewed.includes(idea.id)?'review-button reviewed':'review-button'} onClick={()=>toggle(idea.id)}>{reviewed.includes(idea.id)?<><Check size={15}/> Reviewed</>:<>Mark reviewed <ArrowRight size={15}/></>}</button></div></div>
    <div className="opportunity-impact"><span>EST. WEEKLY GAIN</span><strong>{idea.gain!==null?'+'+fmt(idea.gain):'—'}</strong><small>PPR / MODEL</small></div></article>)}</div>
    <div className="context-strip"><Info size={18}/>Lineup recommendations use projected player production, not actual in-game fantasy points. Waiver eligibility and transaction rules require final verification in Sleeper.</div></>}
  {section==='compare'&&<><SectionTitle index="01" title="PLAYER COMPARISON"/><div className="compare-controls">
    <label>PLAYER A<select aria-label="First player" value={leftId||''} onChange={e=>setLeft(e.target.value)}>{owned.map(id=><option value={id} key={id}>{index.get(id,players[id],week).name}</option>)}</select></label>
    <label>PLAYER B<select aria-label="Second player" value={rightId||''} onChange={e=>setRight(e.target.value)}>{owned.map(id=><option value={id} key={id}>{index.get(id,players[id],week).name}</option>)}</select></label></div>
    {a&&b?<><div className="comparison"><div className="comparison-player"><div className="comparison-player-heading"><PlayerPortrait id={a.id} name={a.name} position={a.position} team={a.team} size="large"/><div><Caption>{a.position} / {a.team}</Caption><h2>{a.name}</h2></div></div><QuickForecast forecast={a}/><button className="plain-link" onClick={()=>onPlayer(a.id)}>Player evidence <ArrowUpRight size={15}/></button></div>
      <div className="comparison-player"><div className="comparison-player-heading"><PlayerPortrait id={b.id} name={b.name} position={b.position} team={b.team} size="large"/><div><Caption>{b.position} / {b.team}</Caption><h2>{b.name}</h2></div></div><QuickForecast forecast={b}/><button className="plain-link" onClick={()=>onPlayer(b.id)}>Player evidence <ArrowUpRight size={15}/></button></div></div>
      <div className="comparison-summary"><span>MODEL DIFFERENCE / PLAYER A MINUS B</span><strong>{compare(a,b).delta===null?'—':(compare(a,b).delta!>0?'+':'')+fmt(compare(a,b).delta)}</strong><p>{compare(a,b).delta===null?'The model is missing at least one weekly projection.':'This is a forecast difference, not an assurance of future points.'} Check eligibility and lineup locks before starting either player.</p></div></>
     :<Empty title="No comparable players" detail="Connect a league with offensive skill-position players."/>}</>}
  {section==='waivers'&&<><SectionTitle index="01" title="AVAILABLE PLAYER SCENARIOS"/>{waived.length?<div className="opportunity-list">{waived.map((w,i)=><article className="opportunity" key={w.id}><div className="opportunity-number">{String(i+1).padStart(2,'0')}</div><div className="opportunity-text"><Caption>UNROSTERED IN LEAGUE / REVIEW REQUIRED</Caption><div className="opportunity-title">{w.playerId&&<PlayerPortrait id={w.playerId} name={data.index.get(w.playerId,data.players[w.playerId],data.week).name}
 position={data.players[w.playerId]?.position} team={data.players[w.playerId]?.team}/>}<h2>{w.title}</h2></div><p>{w.detail}</p><span className="small-note">{w.note}</span>
     <div className="opportunity-cta"><button className="outline-action" onClick={()=>w.playerId&&onPlayer(w.playerId)}>Inspect player <ArrowUpRight size={15}/></button></div></div>
     <div className="opportunity-impact"><span>PROJECTED IMPACT</span><strong>+{fmt(w.gain)}</strong><small>PPR / MODEL</small></div></article>)}</div>:<Empty title="No actionable waiver upgrades" detail="The model did not identify an unrostered player that improved the optimal lineup above the threshold. Missing projections or a full roster may limit recommendations."/>}
    <div className="context-strip"><ShieldCheck size={17}/>Availability means the player is not on a currently loaded roster. It does not guarantee the player is claimable, eligible, or unlocked.</div></>}
 </div>;
}
function Scout({data,onPlayer}:{data:AppData;onPlayer:(id:string)=>void}){
 const {players,index,week,rosters}=data;
 const [search,setSearch]=useState(''),[position,setPosition]=useState('ALL'),[showAll,setShowAll]=useState(false);
 const owned=useMemo(()=>new Set(rosters.flatMap(r=>r.players||[]).map(String)),[rosters]);
 const available=useMemo(()=>Object.entries(players).filter(([id,p])=>{
  const pos=String(p.position||'').toUpperCase();
  return !owned.has(id)&&['QB','RB','WR','TE'].includes(pos)&&Boolean(p.team)&&
   !/retired|inactive/i.test(String(p.status||''));
 }),[players,owned]);
 const results=useMemo(()=>{
  const q=search.trim().toLowerCase();
  const filtered=available.filter(([,p])=>(position==='ALL'||p.position===position)&&
   (!q||String(p.full_name||[p.first_name,p.last_name].filter(Boolean).join(' ')).toLowerCase().includes(q)));
  filtered.sort(([a],[b])=>(index.get(b,players[b],week).projection??-1)-(index.get(a,players[a],week).projection??-1));
  return filtered.slice(0,showAll||q?100:35);
 },[players,index,week,search,position,showAll,available]);
 return <div className="screen"><header className="page-heading"><Caption>03 / WAIVER WIRE / {available.length} FREE AGENTS</Caption>
  <h1>Available players<span>.</span></h1><p>Players not currently held by any team in {data.league.name}. Ranked by independent Powers–Newlon Model projections.</p></header>
  <div className="scout-search"><Search size={20}/><input type="search" placeholder="Search unrostered players…" aria-label="Search available players" autoCapitalize="off" autoCorrect="off" value={search} onChange={e=>setSearch(e.target.value)}/>{search&&<button type="button" aria-label="Clear search" onClick={()=>setSearch('')}><X size={17}/></button>}</div>
  <div className="scout-filter"><span>POSITION</span><div>{['ALL','QB','RB','WR','TE'].map(p=><button key={p} className={position===p?'active':''} aria-pressed={position===p} onClick={()=>setPosition(p)}>{p}</button>)}</div></div>
  <div className="scout-table"><div className="scout-head"><span>AVAILABLE PLAYER / TEAM</span><span>MODEL PPR</span><span>RANGE</span><span></span></div>
    {results.map(([id,p])=>{const f=index.get(id,p,week);return <button className="scout-row" key={id} onClick={()=>onPlayer(id)}>
      <span className="scout-name"><PlayerPortrait id={id} name={f.name} position={f.position} team={f.team}/><span><strong>{f.name}</strong><small>{f.position} / {f.team||'FA'}{f.opponent?' / '+f.opponent:''}</small></span></span>
      <strong className="scout-proj"><Score value={f.projection}/></strong><span className="scout-range">{fmt(f.floor)} – {fmt(f.ceiling)}</span><ArrowUpRight size={16}/></button>})}
    {!results.length&&<Empty title="No available players match" detail="No unrostered player matches that search and position. Change the filter or search another name."/>}</div>
    {!search&&!showAll&&results.length===35&&<button className="plain-link more-players" onClick={()=>setShowAll(true)}>Show more available players <ArrowRight size={16}/></button>}
  <p className="fine-print">Availability means unrostered in the currently loaded Sleeper league. Waiver claims, locked players, FAAB and roster eligibility must still be checked in Sleeper.</p>
 </div>;
}
function Trend({f,leagueActuals}:{f:Forecast;leagueActuals?:Record<number,number>}){
 const weeks=new Set<number>(f.weekly.map(w=>w.week));
 for(const raw of Object.keys(leagueActuals||{}))weeks.add(Number(raw));
 const rows=[...weeks].filter(n=>Number.isInteger(n)&&n>=1&&n<=Math.min(18,f.week)).sort((a,b)=>a-b).map(week=>{
  const model=f.weekly.find(w=>w.week===week);
  // A numeric recorded PPR actual is authoritative even if the source's
  // game_played flag is unreliable (Week 1 in the production export is 0).
  const modelActual=model?.actual??null;
  const actual=modelActual??leagueActuals?.[week]??null;
  return {week,projection:model?.projection??null,actual,origin:modelActual!==null?'model':leagueActuals?.[week]!=null?'sleeper':null};
 });
 if(!rows.length)return <Empty title="Historical values unavailable" detail="No saved weekly projections or reported actual scoring could be located for this player."/>;
 const width=510,height=182,padX=27,padY=22;
 const values=rows.flatMap(r=>[r.projection,r.actual]).filter((v):v is number=>v!==null);
 const max=Math.max(5,...values)*1.1;
 const x=(i:number)=>padX+i*(width-2*padX)/Math.max(1,rows.length-1);
 const y=(v:number)=>height-padY-v*(height-2*padY)/max;
 const path=(key:'projection'|'actual')=>{
  let d='';
  rows.forEach((r,i)=>{const v=r[key];if(v===null)return;const previous=i>0?rows[i-1][key]:null;
   d+=(i===0||previous===null?'M':'L')+x(i).toFixed(1)+' '+y(v).toFixed(1)+' ';});
  return d.trim();
 };
 const actualCount=rows.filter(r=>r.actual!==null).length;
 return <div className="trend-wrap">
  <svg viewBox={'0 0 '+width+' '+height} role="img" aria-label="Model projected fantasy points in forest green and reported actual fantasy points in copper, by week">
   {[0,.5,1].map(n=><line key={n} x1={padX} x2={width-padX} y1={y(n*max)} y2={y(n*max)} className="chart-grid"/>)}
   <path d={path('projection')} className="chart-line"/><path d={path('actual')} className="chart-actual-line"/>
   {rows.map((r,i)=>r.actual!==null?<circle key={r.week} cx={x(i)} cy={y(r.actual)} r={4} className="chart-actual"/>:null)}
   {rows.map((r,i)=><text key={'w'+r.week} x={x(i)} y={height-2} textAnchor="middle" className="chart-week-label">{r.week}</text>)}
  </svg>
  <div className="trend-legend"><span><i/> MODEL PROJECTION</span><span><i/> REPORTED ACTUAL</span></div>
  <div className="history-table" role="table" aria-label="Historical model and reported actual weekly points">
   <div className="history-row history-header" role="row"><span>WEEK</span><span>MODEL PPR</span><span>ACTUAL POINTS</span></div>
   {rows.map(r=><div className="history-row" role="row" key={r.week}><strong>W{r.week}</strong><span>{fmt(r.projection)}</span>
    <span className={r.actual!==null?'reported-actual':''}>{fmt(r.actual)}{r.actual!==null&&<small>{r.origin==='model'?' PPR':' LEAGUE'}</small>}</span></div>)}
  </div>
  <p className="history-note">{actualCount?actualCount+' week'+(actualCount===1?'':'s')+' of reported scoring shown.':'No actual points have been reported for this player in the available sources.'} Model actuals use PPR; Sleeper fallback actuals use your league’s scoring settings and may differ from PPR.</p>
 </div>;
}
function Dossier({f,onClose,onCompare,leagueId,season,lastCompletedWeek}:{f:Forecast;onClose:()=>void;onCompare:()=>void;leagueId:string;season:number;lastCompletedWeek:number}){
 const actualHistory=useQuery({queryKey:['league-actual-history',leagueId,season,lastCompletedWeek],
  queryFn:({signal})=>getLeagueActualHistory(leagueId,lastCompletedWeek,signal),staleTime:10*60*1000,enabled:lastCompletedWeek>0});
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
 // Portal to the body: the animated screen container has a transform, which
 // otherwise makes a position:fixed drawer relative to the scrolled page on iOS.
 return createPortal(<div className="dossier-backdrop" onClick={e=>{if(e.target===e.currentTarget)onClose()}}> 
   <div className="dossier" ref={ref} role="dialog" aria-modal="true" aria-label={'Player dossier: '+f.name}>
    <div className="dossier-bar"><Caption>PN / SCOUT REPORT</Caption><button className="dossier-close" onClick={onClose} aria-label="Close player dossier"><X size={21}/></button></div>
    <div className="dossier-title"><PlayerPortrait id={f.id} name={f.name} position={f.position} team={f.team} size="large"/><div><span>{f.position} / {f.team} / WEEK {f.week}</span><h2>{f.name}</h2><p>{f.opponent?'Opponent: '+f.opponent+' · ':''}Status: {f.status||'Not provided'}</p></div></div>
    <div className="dossier-main-stat"><Caption>MODEL PPR FORECAST</Caption><strong><Score value={f.projection}/></strong><p>{f.matched?'Powers–Newlon Model snapshot':'No validated model match found'}</p></div>
    <QuickForecast forecast={f}/>
    <div className="dossier-section"><SectionTitle index="01" title="PROJECTION VS REPORTED ACTUAL"/><Trend f={f} leagueActuals={actualHistory.data?.[f.id]}/>{actualHistory.isLoading&&<p className="history-note">Checking historical Sleeper matchup scoring…</p>}{actualHistory.isError&&<p className="history-note">Historical Sleeper actuals could not be loaded. Model records are still shown.</p>}</div>
    <div className="dossier-section"><SectionTitle index="02" title="MODEL EVIDENCE"/><div className="dossier-facts">
      <div><span>PROJECTION ORIGIN</span><strong>{f.source==='powers-newlon-model'?'Powers–Newlon':'Unavailable'}</strong></div>
      <div><span>PREGAME PROJECTION</span><strong>{fmt(f.pregame)}</strong></div>
      <div><span>CONFIDENCE FIELD</span><strong>{f.confidence||'Not reported'}</strong></div>
      <div><span>SEASON FORECAST PPG</span><strong>{fmt(f.seasonPpg)}</strong></div>
    </div></div>
    <div className="dossier-bottom"><button className="primary-action" onClick={onCompare}>Compare in Decision Lab <ArrowRight size={17}/></button><span>Advisory only · No transaction will occur</span></div>
   </div>
  </div>,document.body);
}

type TradeAsset={key:string;name:string;position:string;value:number|null;type:'player'|'pick';source?:'model'|'estimated-baseline'|'unavailable'};
function availableTradeAssets(r:Roster,data:AppData):TradeAsset[]{
 const assets:TradeAsset[]=rosterAssets(r,data.index,data.players,data.week)
  .filter(p=>p.position!=='DEF').map(p=>({key:p.id,name:p.name,position:p.position,value:p.dynasty,type:'player'}));
 const roundCount=Math.min(8,num(data.league.settings?.draft_rounds)??4);
 const today=Number(data.league.season)||new Date().getFullYear();
 for(let season=today+1;season<=today+3;season++)for(let round=1;round<=roundCount;round++){
  for(const original of data.rosters){
   const transfer=data.bundle.picks.find(p=>Number(p.season)===season&&Number(p.round)===round&&Number(p.roster_id)===original.roster_id);
   const currentOwner=transfer?Number(transfer.owner_id):original.roster_id;
   if(currentOwner!==r.roster_id)continue;
   const valuation=data.index.pickValuation(season,round);
   assets.push({key:'pick:'+season+':'+round+':'+original.roster_id,
    name:season+' R'+round+(original.roster_id!==r.roster_id?' · From '+rosterName(original,data.users):' · Own pick'),
    position:'FUTURE PICK',value:valuation.value,type:'pick',source:valuation.source});
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
   <div><Caption>DYNASTY VALUE / NET RECEIVED</Caption><strong>{diff===null?'—':(diff>0?'+':'')+fmt(diff,0)}</strong></div>
   <div><Caption>SELECTED ASSETS</Caption><strong>{send.length} <span>⇄</span> {receive.length}</strong></div>
   <p>{missing?'A selected asset has no model valuation. Totals are not comparable.':!send.length||!receive.length?'Choose at least one asset from each side.':'Positive means the package you receive has higher summed model dynasty value.'}</p>
  </div>
  <div className="trade-columns">
   <AssetList label="01 / ASSETS YOU SEND" team={rosterName(r,users)} assets={a} selected={send} onSelect={id=>toggle(id,send,setSend)}/>
   <AssetList label="02 / ASSETS YOU RECEIVE" team={rosterName(other,users)} assets={b} selected={receive} onSelect={id=>toggle(id,receive,setReceive)}/>
  </div>
  <div className="context-strip"><Info size={17}/>This is a valuation comparison, not an acceptance or win-probability forecast. Pick values are model valuations when present; otherwise clearly labeled round-level estimates, never exact draft-slot predictions. Nothing is sent to Sleeper.</div>
 </div>;
}
function AssetList({label,team,assets,selected,onSelect}:{label:string;team:string;assets:TradeAsset[];selected:string[];onSelect:(key:string)=>void}){
 const [filter,setFilter]=useState('ALL');
 return <section className="asset-column"><div className="asset-column-top"><Caption>{label}</Caption><h3>{team}</h3></div>
  <div className="asset-filters">{['ALL','QB','RB','WR','TE','PICKS'].map(x=><button key={x} className={filter===x?'active':''} onClick={()=>setFilter(x)}>{x}</button>)}</div>
  <div className="asset-scroller">{assets.filter(a=>filter==='ALL'||(filter==='PICKS'?a.type==='pick':a.position===filter)).map(asset=><button className={'asset-line '+(selected.includes(asset.key)?'picked':'')} type="button" key={asset.key} onClick={()=>onSelect(asset.key)} aria-pressed={selected.includes(asset.key)}>
   <span className="asset-check">{selected.includes(asset.key)&&<Check size={14}/>}</span>
    <span className="asset-thumb">{asset.type==='player'?<PlayerPortrait id={asset.key} name={asset.name} position={asset.position}/>:<Layers size={17}/>}</span><span className="asset-name"><strong>{asset.name}</strong><small>{asset.type==='pick'?(asset.source==='model'?'MODEL PICK VALUE':asset.source==='estimated-baseline'?'ESTIMATED PICK VALUE':'PICK VALUE UNAVAILABLE'):asset.position}</small></span><b>{fmt(asset.value,0)}</b></button>)}
   {assets.length===0&&<p className="small-note">No eligible assets loaded.</p>}</div>
   <div className="asset-total"><span>SELECTED / {selected.length}</span><strong>{fmt(assets.filter(a=>selected.includes(a.key)).reduce((v,a)=>v+(a.value??0),0),0)}</strong></div>
 </section>;
}
function ModelLeaders({data,onPlayer}:{data:AppData;onPlayer:(id:string)=>void}){
 const [position,setPosition]=useState('ALL');
 const {players,index,week,rosters}=data;
 const owned=new Set(rosters.flatMap(r=>r.players||[]));
 const rows=useMemo(()=>Object.entries(players).filter(([,p])=>['QB','RB','WR','TE'].includes(String(p.position||''))&&
  (position==='ALL'||p.position===position)).map(([id,p])=>index.get(id,p,week)).filter(f=>f.projection!==null)
  .sort((a,b)=>(b.projection??0)-(a.projection??0)).slice(0,40),[players,index,week,position]);
 return <div className="leaders-view"><SectionTitle index="01" title="MODEL PROJECTION LEADERS"/>
  <p className="section-description">Weekly Powers–Newlon Model projected PPR leaders across the NFL. Not reported league or season statistics.</p>
  <div className="scout-filter"><span>POSITION</span><div>{['ALL','QB','RB','WR','TE'].map(p=><button key={p} className={position===p?'active':''} onClick={()=>setPosition(p)}>{p}</button>)}</div></div>
  <div className="scout-table">{rows.map((f,i)=><button className="scout-row" key={f.id} onClick={()=>onPlayer(f.id)}>
   <span className="scout-name"><PlayerPortrait id={f.id} name={f.name} position={f.position} team={f.team}/><span><strong>{String(i+1).padStart(2,'0')} · {f.name}</strong><small>{f.position} / {f.team} / {owned.has(f.id)?'ROSTERED':'AVAILABLE'}</small></span></span>
   <strong className="scout-proj">{fmt(f.projection)}</strong><span className="scout-range">MODEL PPR</span><ArrowUpRight size={16}/></button>)}</div>
 </div>;
}
function ScoresView({data}:{data:AppData}){
 const season=Number(data.league.season)||new Date().getFullYear();
 const scores=useQuery({queryKey:['nfl-scores',season,data.week],
  queryFn:({signal})=>getNflScores(season,data.week,signal),staleTime:60_000,refetchInterval:90_000});
 type ScoreCompetitor={homeAway?:string;score?:string;team?:{abbreviation?:string;displayName?:string;logo?:string}};
 type ScoreEvent={id?:string;name?:string;date?:string;status?:{type?:{description?:string;state?:string}};
  competitions?:{competitors?:ScoreCompetitor[]}[]};
 const raw=scores.data as {events?:ScoreEvent[]}|undefined;
 const events=raw?.events||[];
 return <div className="screen scores-screen"><header className="page-heading"><Caption>05 / NFL LIVE SCORES / WEEK {data.week}</Caption>
  <h1>Scores<span>.</span></h1><p>Reported NFL game scores and schedules. This screen is separate from the Powers–Newlon projections.</p></header>
  {scores.isLoading&&<div className="context-strip"><Clock3 size={17}/>Fetching the NFL scoreboard…</div>}
  {scores.isError&&<ErrorLine error={scores.error} onRetry={()=>void scores.refetch()}/>}
  <div className="scores-grid">{events.map(event=>{
   const teams=event.competitions?.[0]?.competitors||[];
   const home=teams.find(t=>t.homeAway==='home'),away=teams.find(t=>t.homeAway==='away');
   const isLive=event.status?.type?.state==='in';
   return <article key={event.id||event.name} className="nfl-score-card">
    <div className="score-card-top"><span>{event.status?.type?.description||'Game status not reported'}</span>{isLive&&<span className="score-live-dot">LIVE</span>}</div>
    {[away,home].map((t,i)=><div key={i} className="nfl-score-team">
      {t?.team?.logo?<img src={t.team.logo} alt="" loading="lazy"/>:<span className="score-team-fallback">NFL</span>}
      <span><strong>{t?.team?.displayName||'Team'}</strong><small>{t?.homeAway?.toUpperCase()||''}</small></span>
      <b>{t?.score??'—'}</b>
    </div>)}
    <div className="score-card-bottom">{event.date?new Date(event.date).toLocaleString([],{weekday:'short',month:'short',day:'numeric',hour:'numeric',minute:'2-digit'}):'Date unavailable'}</div>
   </article>;
  })}</div>
  {!scores.isLoading&&!scores.isError&&!events.length&&<Empty title="No NFL games returned" detail="There are no listed scoreboard events for the chosen week. Try another week."/>}
  <p className="fine-print">NFL scoreboard data provided by ESPN. These are reported game results, not league fantasy points or model forecasts.</p>
 </div>;
}
function LeagueView({data,onPlayer,onRefresh,refreshing}:{data:AppData;onPlayer:(id:string)=>void;onRefresh:()=>void;refreshing:boolean}){
 const [section,setSection]=useState<LeagueScreen>('roster');
 const [teamId,setTeamId]=useState<string>('');
 const {rosters,users,myRoster,players,index,league,week,matchup,bundle}=data;
 const chosen=rosters.find(x=>String(x.roster_id)===(teamId||String(myRoster?.roster_id)))||rosters[0];
 const ranked=useMemo(()=>standings(rosters,users),[rosters,users]);
 const ownIds=new Set(myRoster?.players||[]);
 return <div className="screen league-screen"><header className="page-heading"><Caption>04 / LEAGUE INTELLIGENCE</Caption><h1>League<span>.</span></h1><p>{league.name} · A competitive view of the season, roster composition, and long-term assets.</p></header>
  <div className="league-subnav" role="group" aria-label="League sections">{([['roster','Roster'],['standings','Standings'],['leaders','Leaders'],['trade','Trade'],['draft','Draft']] as const).map(([key,label])=><button className={section===key?'active':''} aria-pressed={section===key} key={key} onClick={()=>setSection(key)}>{label}</button>)}</div>
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
     return <button key={i} className="roster-row" disabled={!f} onClick={()=>f&&onPlayer(f.id)}><span className="roster-position">{m.slot}</span><PlayerPortrait id={m.id} name={f?.name||'Empty'} position={f?.position} team={f?.team}/><span><strong>{f?.name||'Unfilled'}</strong><small>{f?f.team+' · '+f.position:'No starter assigned'}</small></span><b>{fmt(m.projection)}</b><ChevronRight size={15}/></button>})}</div>
     <SectionTitle index="03" title="BENCH & RESERVES"/><div className="roster-list">{(chosen.players||[]).filter(id=>!chosen.starters.includes(id)).map(id=>{
      const f=index.get(id,players[id],week);return <button key={id} className="roster-row" onClick={()=>onPlayer(id)}><span className="roster-position">{f.position}</span><PlayerPortrait id={id} name={f.name} position={f.position} team={f.team}/><span><strong>{f.name}</strong><small>{f.team} · {f.status||'Status unavailable'}</small></span><b>{fmt(f.projection)}</b><ChevronRight size={15}/></button>})}</div></>}
   </>}
  {section==='trade'&&<TradeWorkbench data={data}/>}
  {section==='leaders'&&<ModelLeaders data={data} onPlayer={onPlayer}/>}
  {section==='draft'&&<><SectionTitle index="01" title="YOUR FUTURE DRAFT PICKS"/>
    <p className="section-description">Projected dynasty values of the picks currently attributed to your roster, with source labels. Exact draft positions are not assumed.</p>
    <div className="pick-ownership">
     {(myRoster?availableTradeAssets(myRoster,data).filter(a=>a.type==='pick'):[]).map(p=><div className="pick-owned-row" key={p.key}>
       <div><strong>{p.name}</strong><small>{p.source==='model'?'Powers–Newlon modeled value':p.source==='estimated-baseline'?'Estimated round-level baseline':'Value unavailable'}</small></div>
       <b>{fmt(p.value,0)}</b></div>)}
     {!myRoster&&<Empty title="Your roster was not identified" detail="Connect the correct Sleeper account to see your draft picks."/>}
    </div>
    <SectionTitle index="02" title="PICK VALUATION REFERENCE"/>
    <div className="draft-table">{Array.from({length:3},(_,offset)=>Number(league.season)+1+offset)
      .flatMap(y=>Array.from({length:Math.min(8,num(league.settings?.draft_rounds)??4)},(_,i)=>({year:y,round:i+1})))
      .map(x=>{const v=index.pickValuation(x.year,x.round);return <div className="draft-row" key={x.year+'-'+x.round}>
       <strong>{x.year} · Round {x.round}</strong><span>{v.source==='model'?'Model value':v.source==='estimated-baseline'?'Estimated baseline':'Unavailable'}</span><b>{fmt(v.value,0)}</b></div>})}</div>
    <p className="fine-print">Baseline valuations are labeled estimates when the current model snapshot omits a pick value. Pick inventories are derived from Sleeper’s traded-picks records; confirm all transferred pick ownership with your league commissioner.</p></>}

 </div>;
}
function LoadingWorkspace({error,onRetry}:{error:unknown;onRetry:()=>void}){
 return <div className="loading-workspace"><div className="brand-symbol">PN</div><Caption>PREPARING YOUR INTELLIGENCE WORKSPACE</Caption><h1>Connecting the pieces<span>.</span></h1><p>Loading your Sleeper league, roster identities and the Powers–Newlon Model snapshot.</p><div className="loading-track"><span/></div>{error!=null&&<ErrorLine error={error} onRetry={onRetry}/>}
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
   {screen==='scores'&&<ScoresView data={data}/>}
   {dossier&&<Dossier f={dossier} onClose={closeDossier} onCompare={goCompare}
     leagueId={data.league.league_id} season={Number(data.league.season)}
     lastCompletedWeek={Number(data.league.season)<(nfl.data?.season??new Date().getFullYear())?18:Math.max(0,Math.min(data.week,data.nflWeek-1))}/>}
  </AppShell>;
}
