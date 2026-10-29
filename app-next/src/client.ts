import {leagueSchema,matchupSchema,memberSchema,playerMapSchema,rosterSchema,snapshotSchema,userSchema,
 type League,type Matchup,type Member,type ModelRow,type PlayerMap,type Roster,type SleeperUser,type Snapshot,type TradedPick} from './domain';

const API='https://api.sleeper.app/v1';
const CACHE='pnfm-model-next-v1';
export type LeagueBundle={league:League;users:Member[];rosters:Roster[];matchups:Matchup[];
 picks:TradedPick[];drafts:Record<string,unknown>[];fetchedAt:number;};
export type NflState={week:number;season:number;season_type:string};
export class SourceError extends Error{
 constructor(public readonly source:string,public readonly code:number|string,message:string){super(message);this.name='SourceError'}
}
const fetchJson=async(url:string,signal?:AbortSignal):Promise<unknown>=>{
 let response:Response;
 try{response=await fetch(url,{signal,headers:{accept:'application/json'}})}
 catch(e){if(signal?.aborted)throw e;throw new SourceError(url,'network','Cannot reach '+(url.includes('sleeper')?'Sleeper':'the data source')+'. Check your connection.')}
 if(!response.ok){
  if(response.status===404)throw new SourceError(url,404,'Not found. Check the username, season, or league.');
  throw new SourceError(url,response.status,'Data source returned HTTP '+response.status+'. Please try again.');
 }
 try{return await response.json()}catch{throw new SourceError(url,'invalid','The data source returned invalid JSON.')}
};
export async function getSleeperUser(username:string,signal?:AbortSignal):Promise<SleeperUser>{
 const raw=await fetchJson(API+'/user/'+encodeURIComponent(username.trim()),signal);
 const parsed=userSchema.safeParse(raw);
 if(!parsed.success)throw new SourceError('Sleeper','user','That Sleeper username was not found.');
 return {...parsed.data,user_id:String(parsed.data.user_id)};
}
export async function getLeagues(userId:string,season:number,signal?:AbortSignal):Promise<League[]>{
 const raw=await fetchJson(API+'/user/'+encodeURIComponent(userId)+'/leagues/nfl/'+season,signal);
 if(!Array.isArray(raw))throw new SourceError('Sleeper','leagues','Cannot read this season’s leagues.');
 return raw.map(row=>{
  const p=leagueSchema.parse(row);
  return {...p,league_id:String(p.league_id),season:String(p.season)} as League;
 });
}
const list=<T>(raw:unknown,parser:(v:unknown)=>T):T[]=>Array.isArray(raw)?raw.map(parser):[];
export async function getLeagueBundle(id:string,week:number,signal?:AbortSignal):Promise<LeagueBundle>{
 const origin=API+'/league/'+encodeURIComponent(id);
 const [rawLeague,rawUsers,rawRosters,rawMatchups,rawPicks,rawDrafts]=await Promise.all([
  fetchJson(origin,signal),fetchJson(origin+'/users',signal),fetchJson(origin+'/rosters',signal),
  fetchJson(origin+'/matchups/'+week,signal),fetchJson(origin+'/traded_picks',signal),
  fetchJson(origin+'/drafts',signal)
 ]);
 const parsedLeague=leagueSchema.parse(rawLeague);
 return {league:{...parsedLeague,league_id:String(parsedLeague.league_id),season:String(parsedLeague.season)} as League,
  users:list(rawUsers,v=>{const p=memberSchema.parse(v);return {...p,user_id:String(p.user_id)} as Member;}),
  rosters:list(rawRosters,v=>{const p=rosterSchema.parse(v);
   return {...p,owner_id:String(p.owner_id),players:p.players||[],starters:p.starters||[]} as Roster;}),
  matchups:list(rawMatchups,v=>matchupSchema.parse(v) as Matchup),
  picks:list(rawPicks,v=>v as TradedPick),
  drafts:list(rawDrafts,v=>v as Record<string,unknown>),
  fetchedAt:Date.now()};
}
let playersPromise:Promise<PlayerMap>|null=null;
export async function getPlayers(signal?:AbortSignal):Promise<PlayerMap>{
 if(!playersPromise)playersPromise=fetchJson(API+'/players/nfl',signal).then(v=>playerMapSchema.parse(v) as PlayerMap).catch(error=>{playersPromise=null;throw error});
 return playersPromise;
}
let snapshotPromise:Promise<Snapshot>|null=null;
export async function getSnapshot(signal?:AbortSignal):Promise<Snapshot>{
 if(snapshotPromise)return snapshotPromise;
 snapshotPromise=(async()=>{
  const url='/data/model_snapshot.json';
  let version='';
  try{
   const res=await fetch('/data/snapshot_version.json',{signal,cache:'no-store'});
   if(res.ok){const data=await res.json();version=String(data.version||data.hash||data.etag||'')}
  }catch{/* Snapshot version manifest is optional; never block offline cache. */}
  let cached:Response|undefined;
  let cache:Cache|undefined;
  if(typeof caches!=='undefined')try{
   cache=await caches.open(CACHE);
   const key=version?url+'?version='+encodeURIComponent(version):url;
   const match=await cache.match(key);
   if(match)cached=match;
   if(!cached){
    const fresh=await fetch(url,{signal,cache:'no-cache'});
    if(!fresh.ok)throw new SourceError('Powers-Newlon Model',fresh.status,'Model snapshot is not currently available.');
    await cache.put(key,fresh.clone());
    cached=fresh;
   }
  }catch(error){if(signal?.aborted)throw error;cached=undefined;}
  const raw=cached?await cached.json():await fetchJson(url,signal);
  const parsed=snapshotSchema.parse(raw);
  return {...parsed,players:Array.isArray(parsed.players)?parsed.players as ModelRow[]:Object.values(parsed.players) as ModelRow[]} as Snapshot;
 })().catch(e=>{snapshotPromise=null;throw e});
 return snapshotPromise;
}
export async function getNflState(signal?:AbortSignal):Promise<NflState>{
 const x=await fetchJson(API+'/state/nfl',signal) as Record<string,unknown>;
 return {week:typeof x.week==='number'?x.week:1,season:typeof x.season==='number'?x.season:new Date().getFullYear(),season_type:String(x.season_type||'regular')};
}
export type ReportedActualHistory=Record<string,Record<number,number>>;
export async function getLeagueActualHistory(leagueId:string,lastCompletedWeek:number,signal?:AbortSignal):Promise<ReportedActualHistory>{
 const end=Math.min(18,Math.max(0,lastCompletedWeek));
 const weeks=Array.from({length:end},(_,i)=>i+1);
 const results=await Promise.all(weeks.map(async week=>{
  try{
   const raw=await fetchJson(API+'/league/'+encodeURIComponent(leagueId)+'/matchups/'+week,signal);
   return {week,matchups:Array.isArray(raw)?raw:[]};
  }catch(e){if(signal?.aborted)throw e;return {week,matchups:[]};}
 }));
 const history:ReportedActualHistory={};
 for(const {week,matchups} of results)for(const matchup of matchups){
  if(!matchup||typeof matchup!=='object')continue;
  const actuals=(matchup as Record<string,unknown>).players_points;
  if(!actuals||typeof actuals!=='object'||Array.isArray(actuals))continue;
  for(const [id,value] of Object.entries(actuals as Record<string,unknown>)){
   const points=typeof value==='number'&&Number.isFinite(value)?value:null;
   if(points===null)continue;
   (history[id]??={})[week]=points;
  }
 }
 return history;
}
export async function getNflScores(season:number,week:number,signal?:AbortSignal):Promise<unknown>{
 const url='https://site.api.espn.com/apis/site/v2/sports/football/nfl/scoreboard?dates='+
  encodeURIComponent(String(season))+'&seasontype=2&week='+encodeURIComponent(String(week));
 return fetchJson(url,signal);
}
export const safeStored=(name:string)=>{try{return localStorage.getItem(name)}catch{return null}};
export const remember=(name:string,value:string|null)=>{try{if(value===null)localStorage.removeItem(name);else localStorage.setItem(name,value)}catch{/* Private browsing must not break input or navigation. */}};
