import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { setTimeout as sleep } from 'node:timers/promises';
import { chromium, webkit } from 'playwright';

const port=4183,base=`http://127.0.0.1:${port}`;
const server=spawn(process.execPath,['node_modules/vite/bin/vite.js','preview','--host','127.0.0.1','--port',String(port)],{cwd:new URL('../',import.meta.url),stdio:['ignore','pipe','pipe']});
let output='';
for(const stream of [server.stdout,server.stderr])stream.on('data',x=>output+=String(x));
async function waitReady(){for(let i=0;i<80;i++){try{if((await fetch(base)).ok)return}catch{}await sleep(250)}throw Error('Preview did not start: '+output)}

const league={league_id:'connect-test',name:'Connection QA',season:'2026',total_rosters:2,roster_positions:['QB','RB'],scoring_settings:{rec:1},settings:{type:2,draft_rounds:4,playoff_teams:2}};
function data(path){
 if(path==='/v1/state/nfl')return {week:8,season:2026,season_type:'regular'};
 if(path==='/v1/user/qa_writer')return {user_id:'qa-1',display_name:'qa_writer'};
 if(path==='/v1/user/qa_another')return {user_id:'qa-2',display_name:'qa_another'};
 if(path==='/v1/user/unknown')return null;
 if(path==='/v1/user/qa-1/leagues/nfl/2026'||path==='/v1/user/qa-2/leagues/nfl/2026')return [league];
 if(path==='/v1/league/connect-test')return league;
 if(path==='/v1/league/connect-test/users')return [{user_id:'qa-1',display_name:'QA',metadata:{team_name:'QA Team'}},{user_id:'qa-2',display_name:'Opponent',metadata:{team_name:'Opponents'}}];
 if(path==='/v1/league/connect-test/rosters')return [{roster_id:1,owner_id:'qa-1',players:['qb1','rb1'],starters:['qb1','rb1'],settings:{wins:2,losses:1}},{roster_id:2,owner_id:'qa-2',players:['qb2','rb2'],starters:['qb2','rb2'],settings:{wins:1,losses:2}}];
 if(path==='/v1/players/nfl')return {qb1:{full_name:'Test QB',position:'QB',team:'DET'},rb1:{full_name:'Test RB',position:'RB',team:'NYG'},qb2:{full_name:'Opp QB',position:'QB',team:'BUF'},rb2:{full_name:'Opp RB',position:'RB',team:'SF'}};
 if(path.startsWith('/v1/league/connect-test/matchups/'))return [{roster_id:1,matchup_id:1,points:0},{roster_id:2,matchup_id:1,points:0}];
 if(path.includes('/traded_picks')||path.includes('/drafts'))return [];
 return {};
}
async function runOne(engine,name){
 const browser=await engine.launch({headless:true,args:name==='chromium'?['--no-sandbox']:[]});
 const context=await browser.newContext({viewport:{width:390,height:844},isMobile:true,hasTouch:true,reducedMotion:'reduce'});
 const page=await context.newPage();page.setDefaultTimeout(20000);
 const errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.addInitScript(()=>{localStorage.clear();sessionStorage.clear()});
 await page.route(/api\.sleeper\.app/,route=>route.fulfill({status:200,contentType:'application/json',body:JSON.stringify(data(new URL(route.request().url()).pathname))}));
 await page.route(/espn\.com/,route=>route.fulfill({status:200,contentType:'application/json',body:'{}'}));
 try{
  await page.goto(base+'/legacy/',{waitUntil:'domcontentloaded'});
  const input=page.locator('#username'),year=page.locator('#season');
  await input.waitFor({state:'visible',timeout:30000});
  assert.equal(await input.isEnabled(),true,name+' username enabled');
  await input.click();
  await input.pressSequentially('qa_writer',{delay:35});
  assert.equal(await input.inputValue(),'qa_writer',name+' accepts actual typed username');
  await year.fill('2026');
  assert.equal(await year.inputValue(),'2026',name+' accepts season input');
  await page.locator('#nxConnectForm button[type="submit"]').click();
  await page.locator('[data-league="connect-test"]').waitFor();
  assert.equal(await input.inputValue(),'qa_writer',name+' keeps username after lookup');
  await page.locator('[data-league="connect-test"]').click();
  await page.locator('.nx-home').waitFor();
  await page.locator('[data-a="back"]').click();
  await input.waitFor({state:'visible'});
  await input.fill('qa_another');
  assert.equal(await input.inputValue(),'qa_another',name+' editable after returning to league selector');
  await page.locator('#nxConnectForm button[type="submit"]').click();
  await page.locator('[data-league="connect-test"]').waitFor();
  assert.equal(await input.inputValue(),'qa_another',name+' remains editable on repeated lookup');
  assert.deepEqual(errors,[],name+' browser runtime errors');
  console.log(name+': onboarding form and league switch passed');
 }finally{await context.close();await browser.close()}
}
try{await waitReady();await runOne(chromium,'chromium');await runOne(webkit,'webkit');console.log('All onboarding checks passed')}
finally{server.kill('SIGTERM')}
