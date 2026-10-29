import {eligible,fmt,lineup,num,optimal,rosterName,type Decision,type Forecast,type League,type Member,
 type ModelIndex,type PlayerMap,type Roster} from './domain';

export function lineupDecisions(roster:Roster,league:League,index:ModelIndex,players:PlayerMap,week:number):Decision[]{
 const actual=lineup(roster,league,index,players,week),proposed=optimal(roster,league,index,players,week);
 const output:Decision[]=[];
 if(actual.total!==null&&proposed.total!==null&&proposed.total-actual.total>=0.6){
  const starting=new Set(roster.starters||[]);
  const add=proposed.members.find(p=>!starting.has(p.id));
  if(add)output.push({
   id:'lineup:'+add.id,kind:'lineup',title:'Review '+index.get(add.id,players[add.id],week).name+' for '+add.slot,
   detail:'The Powers–Newlon lineup model prefers a different eligible starter combination.',
   gain:proposed.total-actual.total,playerId:add.id,actionable:true,
   note:'Projected gain is model-derived, not guaranteed. Verify kickoff lock and status in Sleeper.'
  });
 }
 if(!output.length)output.push({id:'lineup:review',kind:'check',title:'Check your starting lineup',
  detail:'No confirmed model upgrade clears the current threshold, or some projections are unavailable.',
  gain:null,actionable:true,note:'Confirm injuries, kickoff locks, and roster eligibility before games.'});
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
