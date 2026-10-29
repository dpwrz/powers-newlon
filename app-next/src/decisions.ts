import {eligible,fmt,lineup,num,optimal,rosterName,type Decision,type Forecast,type League,type Member,
 type ModelIndex,type PlayerMap,type Roster} from './domain';

export function lineupDecisions(roster:Roster,league:League,index:ModelIndex,players:PlayerMap,week:number):Decision[]{
 const current=lineup(roster,league,index,players,week);
 const started=new Set(roster.starters||[]);
 const bench=(roster.players||[]).filter(id=>!started.has(id)&&players[id]);
 const proposals:{start:string;sit:string;slot:string;gain:number;startPoints:number;sitPoints:number}[]=[];
 // Evaluate real, eligible one-for-one swaps. Missing DEF/K forecasts cannot
 // suppress an otherwise valid QB/RB/WR/TE decision.
 for(const member of current.members){
  if(member.id==='0'||member.projection===null)continue;
  for(const candidateId of bench){
   const player=players[candidateId];
   if(!eligible(member.slot,String(player?.position||'')))continue;
   const start=index.get(candidateId,player,week),sit=index.get(member.id,players[member.id],week);
   if(start.projection===null||sit.projection===null||/\b(out|inactive|suspended)\b/i.test(start.status))continue;
   const gain=start.projection-sit.projection;
   if(gain>=0.6)proposals.push({start:candidateId,sit:member.id,slot:member.slot,
    gain,startPoints:start.projection,sitPoints:sit.projection});
  }
 }
 proposals.sort((a,b)=>b.gain-a.gain);
 const usedStart=new Set<string>(),usedSit=new Set<string>();
 const output:Decision[]=[];
 for(const p of proposals){
  if(usedStart.has(p.start)||usedSit.has(p.sit))continue;
  usedStart.add(p.start);usedSit.add(p.sit);
  const nameStart=index.get(p.start,players[p.start],week).name;
  const nameSit=index.get(p.sit,players[p.sit],week).name;
  output.push({id:'lineup:'+p.start+':'+p.sit,kind:'lineup',
   title:'START '+nameStart+' · SIT '+nameSit,
   detail:nameStart+' '+p.startPoints.toFixed(1)+' vs '+nameSit+' '+p.sitPoints.toFixed(1)+
    ' projected PPR for an eligible '+p.slot+' lineup spot.',
   gain:p.gain,playerId:p.start,sitPlayerId:p.sit,actionable:true,
   note:'Projected gain is advisory. Verify injuries, player statuses and lineup lock times in Sleeper.'
  });
  if(output.length>=3)break;
 }
 if(!output.length)output.push({id:'lineup:review',kind:'check',title:'No clear Start / Sit upgrade',
  detail:'No eligible bench player has a modeled advantage above 0.6 PPR against a projected starter in this lineup.',
  gain:null,actionable:true,note:'Unprojected or locked players cannot be evaluated reliably. Check news and kickoff locks.'});
 return output;
}

export type WaiverIdea=Decision & {dropValue:number|null;addValue:number|null};
export function waiverIdeas(roster:Roster,rosters:Roster[],league:League,index:ModelIndex,players:PlayerMap,week:number):WaiverIdea[]{
 const owned=new Set(rosters.flatMap(r=>r.players||[]).map(String));
 const rosterSize=num(league.settings?.max_roster_size)??0;
 const needsDrop=rosterSize>0&&(roster.players||[]).length>=rosterSize;
 const original=optimal(roster,league,index,players,week);
 const candidates:Forecast[]=[];
 const allIds=Object.keys(players);
 // Avoid scanning and simulating thousands of free agents per render:
 // scan the model's existing player universe and rank once for the requested week.
 const candidateIds=new Set(index.rows.map(row=>String(row.sleeper_id||row.player_id||row.gsis_id||'')));
 for(const id of allIds){
  if(!candidateIds.has(id)||owned.has(id))continue;
  const p=players[id];
  if(!['QB','RB','WR','TE'].includes(String(p?.position||''))||!p.team)continue;
  const m=index.get(id,p,week);
  if(m.projection===null||m.projection<4||/\b(out|inactive)\b/i.test(m.status))continue;
  candidates.push(m);
 }
 candidates.sort((a,b)=>(b.projection??0)-(a.projection??0));
 const out:WaiverIdea[]=[];
 const dropCandidates=needsDrop?(roster.players||[]).filter(id=>!roster.starters.includes(id)).filter(id=>
  players[id]&&['QB','RB','WR','TE'].includes(String(players[id].position||''))):[null];
 for(const add of candidates.slice(0,60)){
  let best:{gain:number;dropId:string|null}|null=null;
  for(const dropId of dropCandidates){
   const simulated={...roster,players:[...(roster.players||[]).filter(id=>id!==dropId),add.id]};
   const projected=optimal(simulated,league,index,players,week);
   const gain=projected.total!==null&&original.total!==null?projected.total-original.total:null;
   if(gain!==null&&(!best||gain>best.gain))best={gain,dropId};
  }
  if(!best||best.gain<0.7)continue;
  const drop=best.dropId?index.get(best.dropId,players[best.dropId],week):null;
  out.push({id:'waiver:'+add.id,kind:'waiver',title:'Consider adding '+add.name,
   detail:'Estimated optimal-lineup improvement: +'+fmt(best.gain)+' projected PPR points.',
   gain:best.gain,playerId:add.id,dropId:best.dropId||undefined,actionable:false,
   note:best.dropId?'Potential drop: '+(drop?.name||best.dropId)+'. Validate dynasty cost before acting.':'A roster opening may be available. Confirm limits and waiver rules.',
   dropValue:drop?.dynasty??null,addValue:add.dynasty});
 }
 return out.sort((a,b)=>(b.gain||0)-(a.gain||0)).slice(0,4);
}
export function rosterAssets(r:Roster,index:ModelIndex,players:PlayerMap,week:number){
 return (r.players||[]).filter(id=>players[id]).map(id=>{
  const fc=index.get(id,players[id],week);
  return {id,name:fc.name,position:fc.position,dynasty:fc.dynasty,projection:fc.projection};
 }).sort((a,b)=>(b.dynasty??-1)-(a.dynasty??-1));
}
export function tradeSummary(a:Roster,b:Roster,from:string[],receive:string[],index:ModelIndex,players:PlayerMap,week:number){
 const value=(id:string)=>index.get(id,players[id],week).dynasty;
 const aVal=from.map(value),bVal=receive.map(value);
 const knownA=aVal.filter((v):v is number=>v!==null),knownB=bVal.filter((v):v is number=>v!==null);
 return {send:knownA.length?knownA.reduce((a,b)=>a+b,0):null,
  receive:knownB.length?knownB.reduce((a,b)=>a+b,0):null,
  missing:[...from.filter((_,i)=>aVal[i]===null),...receive.filter((_,i)=>bVal[i]===null)],
  teamA:a.roster_id,teamB:b.roster_id};
}
export function standings(rosters:Roster[],users:Member[]){
 return rosters.map(r=>{const s=r.settings||{},wins=num(s.wins)??0,losses=num(s.losses)??0,
  ties=num(s.ties)??0,pts=(num(s.fpts)??0)+(num(s.fpts_decimal)??0)/100;
  return {id:r.roster_id,name:rosterName(r,users),wins,losses,ties,pts};})
  .sort((a,b)=>b.wins-a.wins||a.losses-b.losses||b.pts-a.pts);
}
export const compare=(a:Forecast,b:Forecast)=>{
 const delta=a.projection!==null&&b.projection!==null?a.projection-b.projection:null;
 return {delta,leader:delta===null?'Unavailable':delta>0?a.name:delta<0?b.name:'Even projection'};
};
export function isPlayerEligibleForSlot(slot:string,fc:Forecast){return eligible(slot,fc.position)}
