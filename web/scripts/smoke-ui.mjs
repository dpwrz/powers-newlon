import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { setTimeout as delay } from 'node:timers/promises';
import { chromium } from 'playwright';
import fs from 'node:fs';

const server=spawn('npm',['run','preview','--','--host','127.0.0.1','--port','4173'],{
  cwd:new URL('../',import.meta.url),stdio:['ignore','pipe','pipe']
});
let serverLog='';
server.stdout.on('data',chunk=>serverLog+=chunk.toString());
server.stderr.on('data',chunk=>serverLog+=chunk.toString());

async function ready(){
  for(let i=0;i<80;i++){
    try{
      const r=await fetch('http://127.0.0.1:4173/');
      if(r.ok)return;
    }catch{}
    await delay(250);
  }
  throw Error('Preview server did not start: '+serverLog);
}

const players={
  qb1:{full_name:'Test Quarterback',position:'QB',team:'DET',status:'Active'},
  rb1:{full_name:'Test Running Back',position:'RB',team:'KC',status:'Active'},
  wr1:{full_name:'Test Receiver',position:'WR',team:'PHI',status:'Active'},
  te1:{full_name:'Test Tight End',position:'TE',team:'SF',status:'Active'},
  qb2:{full_name:'Opponent Quarterback',position:'QB',team:'BUF',status:'Active'},
  rb2:{full_name:'Opponent Running Back',position:'RB',team:'BAL',status:'Active'},
  wr2:{full_name:'Opponent Receiver',position:'WR',team:'LAR',status:'Active'},
  te2:{full_name:'Opponent Tight End',position:'TE',team:'MIN',status:'Active'}
};
const league={
  league_id:'ui-league',name:'Design QA League',season:'2026',
  total_rosters:2,roster_positions:['QB','RB','WR','TE'],
  scoring_settings:{rec:1},settings:{type:2,draft_rounds:4,playoff_teams:2}
};
const data=(path)=>{
  if(path==='/v1/state/nfl')return {week:8,season:2026,season_type:'regular'};
  if(path==='/v1/user/ui-tester')return {user_id:'u1',display_name:'ui-tester'};
  if(path==='/v1/user/u1/leagues/nfl/2026')return [league];
  if(path==='/v1/players/nfl')return players;
  if(path==='/v1/league/ui-league')return league;
  if(path==='/v1/league/ui-league/users')return [
    {user_id:'u1',display_name:'My Team',metadata:{team_name:'Powers Test Team'}},
    {user_id:'u2',display_name:'Opponent',metadata:{team_name:'Opponent Team'}}
  ];
  if(path==='/v1/league/ui-league/rosters')return [
    {roster_id:1,owner_id:'u1',players:['qb1','rb1','wr1','te1'],starters:['qb1','rb1','wr1','te1'],settings:{wins:4,losses:3}},
    {roster_id:2,owner_id:'u2',players:['qb2','rb2','wr2','te2'],starters:['qb2','rb2','wr2','te2'],settings:{wins:5,losses:2}}
  ];
  if(path==='/v1/league/ui-league/matchups/8')return [
    {roster_id:1,matchup_id:1,points:0},{roster_id:2,matchup_id:1,points:0}
  ];
  if(path.includes('/traded_picks')||path.includes('/drafts'))return [];
  if(path.includes('/projections/')||path.includes('/stats/'))return [];
  return {};
};

async function test(){
 await ready();
 fs.mkdirSync(new URL('../ui-qa/',import.meta.url),{recursive:true});
 const browser=await chromium.launch({headless:true,args:['--no-sandbox']});
 const errors=[];
 try{
  for(const width of [390,1440]){
   const context=await browser.newContext({
    viewport:{width,height:width===390?844:960},
    deviceScaleFactor:width===390?2:1,isMobile:width===390,hasTouch:width===390,
    reducedMotion:'reduce'
   });
   await context.addInitScript(()=>{
    localStorage.setItem('fm_username','ui-tester');
    localStorage.setItem('fm_league_id','ui-league');
    localStorage.setItem('fm_route','dashboard');
    localStorage.setItem('fm_season','2026');
   });
   await context.route(/api\.sleeper\.app/,route=>{
    const path=new URL(route.request().url()).pathname;
    return route.fulfill({status:200,contentType:'application/json',body:JSON.stringify(data(path))});
   });
   await context.route(/espn\.com/,route=>route.fulfill({status:200,contentType:'application/json',body:'{}'}));
   const page=await context.newPage();
   page.on('pageerror',e=>errors.push(width+': '+e.message));
   await page.goto('http://127.0.0.1:4173/',{waitUntil:'domcontentloaded'});
   await page.locator('.pn-home .pn-match-card').waitFor({timeout:30000});
   assert.equal(await page.locator('.nav-btn').count(),4,'Main nav has four destinations');
   if(width===390){const height=await page.locator('.nav-btn').first().evaluate(el=>el.getBoundingClientRect().height);assert(height>=44,'Touch targets meet minimum height')}
   assert.equal(await page.locator('.pn-home .pn-player').count(),4,'Home uses real league starters');
   const positions=await page.locator('.pn-home-grid').evaluate(el=>{
    const match=el.querySelector('.pn-match-card').getBoundingClientRect();
    const edge=el.querySelector('.pn-actions').getBoundingClientRect();
    const players=el.querySelector('.pn-players').getBoundingClientRect();
    return {matchBottom:match.bottom,edgeTop:edge.top,playerTop:players.top};
   });
   if(width===390)assert(positions.edgeTop<positions.playerTop,'Optimizer must precede player list on mobile');
   await page.screenshot({path:new URL('../ui-qa/home-'+width+'.png',import.meta.url).pathname,fullPage:true});
   await page.locator('.pn-player').first().click();
   await page.locator('.player-profile').waitFor();
   const bg=await page.locator('.profile-metric').first().evaluate(el=>getComputedStyle(el).backgroundImage);
   assert(bg.includes('gradient'),'Player metrics must have styled surfaces');
   await page.screenshot({path:new URL('../ui-qa/player-'+width+'.png',import.meta.url).pathname});
   await page.keyboard.press('Escape');
   await page.locator('.player-profile').waitFor({state:'detached'});
   await page.locator('[data-route="about"].nav-btn').click();
   await page.locator('.pn-hub-tile').first().waitFor();
   await page.screenshot({path:new URL('../ui-qa/more-'+width+'.png',import.meta.url).pathname,fullPage:true});
   await page.locator('[data-route="leaders"].nav-btn').click();
   await page.locator('.pn-explore').waitFor();
   await page.screenshot({path:new URL('../ui-qa/explore-'+width+'.png',import.meta.url).pathname,fullPage:true});
   await page.locator('[data-route="league"].nav-btn').click();
   await page.locator('.league-tabs').waitFor();
   assert.equal(await page.locator('.league-tab').count(),5,'All league tools remain accessible');
   await context.close();
  }
  assert.deepEqual(errors,[],'No uncaught browser errors');
  console.log('UI smoke checks passed for iPhone and desktop layouts.');
 }finally{await browser.close()}
}
try{await test()}finally{server.kill('SIGTERM')}
