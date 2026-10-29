import {z} from 'zod';

export type Json=Record<string,unknown>;
export type Position='QB'|'RB'|'WR'|'TE'|'DEF'|'K'|string;
export type Player=Json & {
  full_name?:string;first_name?:string;last_name?:string;position?:Position;team?:string;
  status?:string;injury_status?:string;age?:number;sleeper_id?:string;player_id?:string;gsis_id?:string;
};
export type PlayerMap=Record<string,Player>;
export type League=Json & {league_id:string;name:string;season:string;total_rosters?:number;
  roster_positions?:string[];settings?:Json;scoring_settings?:Json;avatar?:string;};
export type SleeperUser=Json & {user_id:string;display_name?:string;avatar?:string};
export type Member=Json & {user_id:string;display_name?:string;metadata?:{team_name?:string}};
export type Roster=Json & {roster_id:number;owner_id:string;players:string[];starters:string[];settings?:Json};
export type Matchup=Json & {roster_id:number;matchup_id?:number;points?:number;starters_points?:number[]};
export type TradedPick=Json & {season:string|number;round:number;roster_id:number;owner_id:number;previous_owner_id?:number};
export type ModelRow=Json & {sleeper_id?:string;player_id?:string;gsis_id?:string;
  player_name?:string;full_name?:string;position?:string;team?:string;weekly_projections?:Json[]};
export type Snapshot=Json & {players:ModelRow[];projection_week?:number;pick_values?:Json[]};
export type Forecast={id:string;name:string;position:string;team:string;status:string;week:number;
  projection:number|null;pregame:number|null;floor:number|null;ceiling:number|null;confidence:string|null;
  opponent:string|null;dynasty:number|null;seasonPpg:number|null;matched:boolean;source:'powers-newlon-model'|'unavailable';
  weekly:{week:number;projection:number|null;actual:number|null;played:boolean}[];};
export type LineupSummary={total:number|null;missing:string[];known:number;slots:number;members:{id:string;slot:string;projection:number|null}[]};
export type MatchupView={
  myRoster:Roster|null;opponent:Roster|null;myTeam:string;opponentTeam:string;my:LineupSummary;their:LineupSummary;
  myActual:number|null;theirActual:number|null;scoringReported:boolean;
  margin:number|null;state:'pregame'|'reported'|'complete'|'no-opponent';
};
export type Decision={id:string;kind:'lineup'|'waiver'|'check';title:string;detail:string;
  gain:number|null;playerId?:string;sitPlayerId?:string;dropId?:string;actionable:boolean;note:string;};
const objectSchema=z.record(z.string(),z.unknown());
export const userSchema=z.object({user_id:z.string(),display_name:z.string().optional()}).passthrough();
export const leagueSchema=z.object({league_id:z.string(),name:z.string(),season:z.string().or(z.number()),roster_positions:z.array(z.string()).optional()}).passthrough();
export const rosterSchema=z.object({roster_id:z.number(),owner_id:z.string().or(z.number()),players:z.array(z.string()).nullable().optional(),starters:z.array(z.string()).nullable().optional()}).passthrough();
export const matchupSchema=z.object({roster_id:z.number(),matchup_id:z.number().optional(),points:z.number().nullable().optional()}).passthrough();
export const snapshotSchema=z.object({players:z.union([z.array(objectSchema),objectSchema])}).passthrough();
export const playerMapSchema=z.record(z.string(),objectSchema);
export const memberSchema=z.object({user_id:z.string().or(z.number()),display_name:z.string().optional(),metadata:z.object({team_name:z.string().optional()}).passthrough().optional()}).passthrough();

export const num=(x:unknown):number|null=>{
 if(x===null||x===undefined||x==='')return null;
 const n=Number(x);return Number.isFinite(n)?n:null;
};
export const field=(row:Json|undefined|null,names:string[]):unknown=>{
 if(!row)return null;
 for(const name of names){const key=Object.keys(row).find(k=>k.toLowerCase()===name.toLowerCase());
  if(key&&row[key]!==null&&row[key]!==undefined&&row[key]!=='')return row[key];}
 return null;
};
export const norm=(name:unknown)=>String(name??'').normalize('NFKD').replace(/[\u0300-\u036f]/g,'')
 .toLowerCase().replace(/[.’']/g,'').replace(/[-_]/g,' ').replace(/\b(jr|sr|ii|iii|iv|v)\b/g,'')
 .replace(/[^a-z0-9]+/g,' ').trim().replace(/\s+/g,' ');
const currentKeys=['live_projection','current_projection','projected_points','projection','proj_fpts','fpts_proj','pred','prediction','mean','weekly_projection'];
const pregameKeys=['pregame_projection','original_projection','base_projection','neutral_projection','projected_points','projection'];
const floorKeys=['floor','projection_floor','p10','lower','lower_bound'];
const ceilingKeys=['ceiling','projection_ceiling','p90','upper','upper_bound'];
const valueKeys=['dynasty_value','dynasty','asset_value','value','dynasty_score'];
const teamAliases:Record<string,string>={LA:'LAR',STL:'LAR',JAC:'JAX',OAK:'LV',SD:'LAC',WAS:'WSH'};
export const canonicalTeam=(x:unknown)=>teamAliases[String(x??'').toUpperCase()]||String(x??'').toUpperCase();
export const nameOf=(id:string,players:PlayerMap)=>players[id]?.full_name||
  [players[id]?.first_name,players[id]?.last_name].filter(Boolean).join(' ')||String(id);
export const rosterName=(roster:Roster|undefined|null,users:Member[]):string=>{
 if(!roster)return 'Opponent unavailable';
 const user=users.find(u=>String(u.user_id)===String(roster.owner_id));
 return user?.metadata?.team_name||user?.display_name||'Team '+roster.roster_id;
};
export class ModelIndex{
 private ids=new Map<string,ModelRow>();
 private names=new Map<string,ModelRow[]>();
 private weeks=new WeakMap<ModelRow,Map<number,Json>>();
 private forecastCache=new Map<string,Forecast>();
 public readonly snapshot:Snapshot;
 public readonly rows:ModelRow[];
 constructor(raw:unknown){
  const parsed=snapshotSchema.parse(raw);
  this.snapshot={...parsed,players:Array.isArray(parsed.players)?parsed.players as ModelRow[]:Object.values(parsed.players) as ModelRow[]};
  this.rows=this.snapshot.players;
  for(const row of this.rows){
   const wk=new Map<number,Json>();
   for(const w of row.weekly_projections||[]){const n=num(field(w,['week','game_week','target_week']));if(n!==null)wk.set(n,w)}
   this.weeks.set(row,wk);
   for(const id of [row.sleeper_id,row.sleeper_player_id,row.player_id,row.gsis_id]){
    if(id!==null&&id!==undefined&&id!=='')this.ids.set(String(id),row);
   }
   for(const rawName of [row.player_name,row.full_name,row.name]){
    const key=norm(rawName);if(!key)continue;
    const arr=this.names.get(key)||[];if(!arr.includes(row))arr.push(row);this.names.set(key,arr);
   }
  }
 }
 resolve(id:string,p?:Player):ModelRow|undefined{
  const byId=[id,p?.sleeper_id,p?.player_id,p?.gsis_id].map(x=>x==null?'':String(x)).find(x=>this.ids.has(x));
  if(byId)return this.ids.get(byId);
  const key=norm(p?.full_name||[p?.first_name,p?.last_name].filter(Boolean).join(' '));
  const matches=this.names.get(key)||[];
  if(matches.length<=1)return matches[0];
  const pos=String(p?.position||'').toUpperCase(),team=canonicalTeam(p?.team);
  return matches.find(x=>String(x.position||'').toUpperCase()===pos&&canonicalTeam(x.team)===team)
    ||matches.find(x=>String(x.position||'').toUpperCase()===pos)||matches[0];
 }
 get(id:string,p:Player|undefined,week:number):Forecast{
  const key=id+'|'+week;const cached=this.forecastCache.get(key);if(cached)return cached;
  const row=this.resolve(id,p),target=row?this.weeks.get(row)?.get(week):undefined;
  const current=num(this.snapshot.projection_week)===week;
  const get=(names:string[])=>field(target,names)??(current?field(row,names):null);
  // The new UI never uses Sleeper-generated DEF/K projections. These positions
  // need an explicit matching Powers-Newlon row to display a projection.
  const projection=num(get(currentKeys));
  const floor=num(get(floorKeys)),ceiling=num(get(ceilingKeys));
  const projectionStatus=p?.injury_status||p?.status||'';
  const weekly=(row?.weekly_projections||[]).map(w=>({
    week:num(field(w,['week','game_week','target_week']))??0,
    projection:num(field(w,currentKeys)),
    actual:num(field(w,['actual','actual_points','fantasy_points','actual_fpts','actual_ppr_points','fantasy_points_ppr','fpts_actual'])),
    // Some exported historical weeks (notably Week 1) contain real PPR
    // actuals even when game_played=0. The numeric score takes precedence.
    played:num(field(w,['actual','actual_points','fantasy_points','actual_fpts','actual_ppr_points','fantasy_points_ppr','fpts_actual']))!==null
      ||(num(field(w,['game_played','played']))??0)>0,
  })).filter(w=>w.week>0).sort((a,b)=>a.week-b.week);
  const forecast:Forecast={
   id,name:nameOf(id,{[id]:p||{full_name:String(row?.player_name||row?.full_name||id)}}),
   position:String(p?.position||row?.position||'—'),team:canonicalTeam(p?.team||row?.team),
   status:String(projectionStatus),week,
   projection,pregame:num(get(pregameKeys)),floor,ceiling,
   confidence:get(['confidence','projection_confidence','conf','confidence_score'])==null?null:String(get(['confidence','projection_confidence','conf','confidence_score'])),
   opponent:get(['opponent','opp','opponent_team','opp_team'])==null?null:String(get(['opponent','opp','opponent_team','opp_team'])),
   dynasty:num(field(row,valueKeys)??field(target,valueKeys)),
   seasonPpg:num(field(row,['season_fppg','projected_fppg','year1_fppg','fppg'])),
   matched:!!row,source:row&&projection!==null?'powers-newlon-model':'unavailable',weekly
  };
  this.forecastCache.set(key,forecast);return forecast;
 }
 pickValuation(season:number,round:number):{value:number|null;source:'model'|'estimated-baseline'|'unavailable'}{
  const row=(this.snapshot.pick_values||[]).find(p=>num(field(p,['season','year','pick_year']))===season&&num(field(p,['round','pick_round']))===round);
  const modelValue=num(field(row,valueKeys.concat(['pick_value'])));
  if(modelValue!==null)return {value:modelValue,source:'model'};
  // Mirror the established legacy future-pick curve, explicitly labeled an
  // estimate; never pass it off as an individual modeled draft selection.
  const baseSeason=num(field(this.snapshot,['season','projection_season']))??new Date().getFullYear();
  if(round<1||round>8||season<baseSeason||season>baseSeason+6)return {value:null,source:'unavailable'};
  const baseline:Record<number,number>={1:4200,2:1800,3:800,4:350};
  const raw=baseline[round]??100;
  return {value:Math.round(raw*Math.pow(.92,Math.max(0,season-baseSeason-1))),source:'estimated-baseline'};
 }
 pickValue(season:number,round:number):number|null{return this.pickValuation(season,round).value}
}
export const isSlot=(slot:string)=>!['BN','IR','TAXI'].includes(slot.toUpperCase());
export const eligible=(slot:string,position:string)=>{
 const s=slot.toUpperCase(),p=position.toUpperCase();if(s===p)return true;
 if(['FLEX','RB_WR_TE','WRRB_FLEX','REC_FLEX'].includes(s))return ['RB','WR','TE'].includes(p);
 if(['SUPER_FLEX','SUPERFLEX','OP'].includes(s))return ['QB','RB','WR','TE'].includes(p);
 if(s==='WR_TE')return ['WR','TE'].includes(p);
 return false;
};
export const lineup=(r:Roster|null|undefined,league:League,index:ModelIndex,players:PlayerMap,week:number):LineupSummary=>{
 const slots=(league.roster_positions||[]).filter(isSlot);
 const members=slots.map((slot,i)=>{
  const id=r?.starters?.[i]??'0';
  return {id,slot,projection:id==='0'?null:index.get(id,players[id],week).projection};
 });
 const known=members.filter(m=>m.projection!==null).length;
 return {total:known?members.reduce((t,m)=>t+(m.projection??0),0):null,
  missing:members.filter(m=>m.projection===null).map(m=>m.id),
  known,slots:slots.length,members};
};
export const optimal=(r:Roster,league:League,index:ModelIndex,players:PlayerMap,week:number):LineupSummary=>{
 const slots=(league.roster_positions||[]).filter(isSlot);
 const ids=(r.players||[]).filter(id=>players[id]);
 // Slot-mask dynamic programming finds the best assignment without reusing a player.
 // It prioritizes filling eligible slots, then maximizes modeled PPR points.
 type State={score:number;count:number;picks:{id:string;slot:string;projection:number|null}[]};
 let states=new Map<number,State>([[0,{score:0,count:0,picks:[]}]]);
 for(const id of ids){
  const position=players[id]?.position||'';
  const forecast=index.get(id,players[id],week);
  if(forecast.projection===null)continue;
  const next=new Map(states);
  for(const [mask,prior] of states)for(let i=0;i<slots.length;i++){
   if((mask&(1<<i))||!eligible(slots[i],position))continue;
   const nm=mask|(1<<i),score=prior.score+forecast.projection;
   const old=next.get(nm);
   if(!old||score>old.score)next.set(nm,{score,count:prior.count+1,picks:[...prior.picks,{id,slot:slots[i],projection:forecast.projection}]});
  }
  states=next;
 }
 const selected=[...states.values()].sort((a,b)=>b.count-a.count||b.score-a.score)[0]||{score:0,count:0,picks:[]};
 return {total:selected.count?selected.score:null,known:selected.count,slots:slots.length,
  missing:slots.length>selected.count?['unfilled or missing projection']:[],
  members:selected.picks};
};
export const matchupView=(r:Roster|null,rosters:Roster[],users:Member[],matchups:Matchup[],league:League,
 index:ModelIndex,players:PlayerMap,week:number,currentWeek:number):MatchupView=>{
 const mine=matchups.find(m=>m.roster_id===r?.roster_id);
 const opponentId=matchups.find(m=>mine&&m.matchup_id===mine.matchup_id&&m.roster_id!==r?.roster_id)?.roster_id;
 const opponent=rosters.find(x=>x.roster_id===opponentId)||null;
 const my=lineup(r,league,index,players,week),their=lineup(opponent,league,index,players,week);
 const theirActual=num(matchups.find(m=>m.roster_id===opponentId)?.points);
 const myActual=num(mine?.points);
 const scoringReported=(myActual!==null&&myActual!==0)||(theirActual!==null&&theirActual!==0);
 return {myRoster:r,opponent,myTeam:rosterName(r,users),opponentTeam:rosterName(opponent,users),my,their,
  myActual:scoringReported?myActual:null,theirActual:scoringReported?theirActual:null,scoringReported,
  margin:scoringReported&&myActual!==null&&theirActual!==null?myActual-theirActual:
   my.total!==null&&their.total!==null?my.total-their.total:null,
  state:!opponent?'no-opponent':scoringReported?(week<currentWeek?'complete':'reported'):'pregame'};
};
export const fmt=(v:number|null|undefined,d=1)=>v==null?'—':v.toFixed(d);
export const pf=(r:Roster)=>{const s=r.settings||{};return (num(s.fpts)||0)+(num(s.fpts_decimal)||0)/100};
export const recordOf=(r:Roster)=>[num(r.settings?.wins)||0,num(r.settings?.losses)||0,num(r.settings?.ties)||0];
