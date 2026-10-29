import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {setTimeout as pause} from 'node:timers/promises';
import {chromium,webkit} from 'playwright';
import {mkdir} from 'node:fs/promises';
const port=4197,url='http://127.0.0.1:'+port+'/';
const server=spawn(process.execPath,['node_modules/vite/bin/vite.js','preview','--host','127.0.0.1','--port',String(port)],{cwd:new URL('../',import.meta.url),stdio:['ignore','pipe','pipe']});
let logs='';for(const s of [server.stdout,server.stderr])s.on('data',x=>logs+=String(x));
async function ready(){for(let i=0;i<85;i++){try{const r=await fetch(url);if(r.ok)return}catch{}await pause(250)}throw Error('Preview did not start: '+logs)}
const modelRows=[
 ['qb1','Test Quarterback','QB','DET',17.1,10.8,23.4,5300],
 ['rb1','Test Running Back','RB','KC',15.4,9.2,22.1,4300],
 ['wr1','Test Receiver','WR','PHI',13.1,8.1,20.8,3400],
 ['te1','Test Tight End','TE','SF',8.7,4.1,14.1,1800],
 ['qb2','Opponent Quarterback','QB','BUF',22.7,16.1,29.3,6000],
 ['rb2','Opponent Running Back','RB','BAL',18.9,10.0,28.2,5100],
 ['wr2','Opponent Receiver','WR','LAR',15.5,8.4,23.4,4200],
 ['te2','Opponent Tight End','TE','MIN',10.1,5.2,17.2,1900],
 ['fa1','Waiver Candidate','WR','DAL',18.1,12,25.1,3300],
 ['bench1','Bench Upgrade','WR','NYG',19.8,12,27,800]
].map(([sleeper_id,player_name,position,team,projection,floor,ceiling,dynasty_value])=>({
 sleeper_id,player_name,position,team,dynasty_value,
 weekly_projections:[
  ...Array.from({length:6},(_,i)=>({
   week:i+1,projection:Math.max(0,projection-(7-i)*.6),
   actual:i===2?{}:Math.max(0,projection-(6-i)),
   // The live snapshot has numeric Week 1 actuals with game_played=0.
   game_played:i===0?0:1
  })),
  {week:7,projection:Math.max(0,projection-2),actual:projection-3,game_played:1},
  {week:8,projection,floor,ceiling,pregame_projection:projection,confidence:.77,opponent:'MIN'}
 ]
}));
const model={projection_week:8,version:'fixture-next-1',players:modelRows,
 pick_values:[{season:2027,round:1,dynasty_value:4250},{season:2027,round:2,dynasty_value:1720}]};
const players=Object.fromEntries(modelRows.map(r=>[r.sleeper_id,{full_name:r.player_name,position:r.position,team:r.team,status:'Active'}]));
const league={league_id:'next-qa',name:'Next Application QA',season:'2026',total_rosters:2,
 roster_positions:['QB','RB','WR','TE'],scoring_settings:{rec:1},settings:{type:2,draft_rounds:2,playoff_teams:2,max_roster_size:6}};
function mock(path){
 if(path==='/v1/state/nfl')return {week:8,season:2026,season_type:'regular'};
 if(path==='/v1/user/qa_writer')return {user_id:'qa-user',display_name:'qa_writer'};
 if(path==='/v1/user/qa-user/leagues/nfl/2026')return [league];
 if(path==='/v1/league/next-qa')return league;
 if(path==='/v1/league/next-qa/users')return [{user_id:'qa-user',display_name:'My Test Team',metadata:{team_name:'Powers Test'}},{user_id:'other',display_name:'Opponent',metadata:{team_name:'Opponent Test'}}];
 if(path==='/v1/league/next-qa/rosters')return [
  {roster_id:1,owner_id:'qa-user',players:['qb1','rb1','wr1','te1','bench1'],starters:['qb1','rb1','wr1','te1'],settings:{wins:4,losses:3,fpts:960,fpts_decimal:23}},
  {roster_id:2,owner_id:'other',players:['qb2','rb2','wr2','te2'],starters:['qb2','rb2','wr2','te2'],settings:{wins:5,losses:2,fpts:1000,fpts_decimal:11}}];
 if(path==='/v1/league/next-qa/matchups/8')return [{roster_id:1,matchup_id:1,points:0},{roster_id:2,matchup_id:1,points:0}];
 if(path==='/v1/league/next-qa/matchups/7')return [{roster_id:1,matchup_id:1,points:73.5,players_points:{qb1:22.3,rb1:11.2,wr1:10.3}},{roster_id:2,matchup_id:1,points:81,players_points:{qb2:23.5}}];
 if(path.startsWith('/v1/league/next-qa/matchups/'))return [];
 if(path==='/v1/league/next-qa/traded_picks'||path==='/v1/league/next-qa/drafts')return [];
 if(path==='/v1/players/nfl')return players;
 return {};
}
const screens=new URL('../ui-qa-next/',import.meta.url);await mkdir(screens,{recursive:true});
async function test(engine,label,width){
 const browser=await engine.launch({headless:true,args:label==='chromium'?['--no-sandbox']:[]});
 const context=await browser.newContext({viewport:{width,height:width>900?900:844},isMobile:width<=390,hasTouch:width<=390,reducedMotion:'reduce'});
 const page=await context.newPage();page.setDefaultTimeout(20000);
 const errors=[];page.on('pageerror',e=>errors.push(e.message));
 await page.route('**/data/model_snapshot.json',r=>r.fulfill({status:200,contentType:'application/json',body:JSON.stringify(model)}));
 await page.route('**/data/snapshot_version.json',r=>r.fulfill({status:200,contentType:'application/json',body:'{"version":"fixture-next-1"}'}));
 await page.route('https://api.sleeper.app/**',r=>r.fulfill({status:200,contentType:'application/json',body:JSON.stringify(mock(new URL(r.request().url()).pathname))}));
 await page.route('https://site.api.espn.com/**',r=>r.fulfill({status:200,contentType:'application/json',body:JSON.stringify({events:[{id:'nfl-fixture-1',date:'2026-10-29T20:00:00Z',status:{type:{description:'Scheduled',state:'pre'}},competitions:[{competitors:[{homeAway:'away',score:'0',team:{displayName:'Detroit Lions'}},{homeAway:'home',score:'0',team:{displayName:'Minnesota Vikings'}}]}]}]})}));
 try{
  const [rootResponse,legacyResponse]=await Promise.all([page.request.get(url),page.request.get(url+'legacy/')]);
  assert.equal(rootResponse.status(),200,'Root homepage must be accessible');
  assert.equal(legacyResponse.status(),200,'Legacy rollback must be accessible');
  assert((await rootResponse.text()).includes('id="root"'),'Main homepage serves Atlas React');
  assert((await legacyResponse.text()).includes('id="app"'),'Legacy rollback serves old UI');
  await page.goto(url,{waitUntil:'domcontentloaded'});
  const input=page.locator('#sleeper-name');await input.waitFor({state:'visible'});
  await input.pressSequentially('qa_writer',{delay:7});
  assert.equal(await input.inputValue(),'qa_writer');
  assert.equal(await input.evaluate(el=>document.activeElement===el),true,'Connecting form retains focus');
  const size=await input.evaluate(el=>parseFloat(getComputedStyle(el).fontSize));assert(size>=16,'No iOS input zoom');
  await page.locator('#league-season').selectOption('2026');
  await page.locator('.connect-form button[type="submit"]').click();
  await page.getByRole('button',{name:/Next Application QA/}).waitFor();
  await page.screenshot({path:new URL(label+'-'+width+'-connect.png',screens).pathname});
  await page.getByRole('button',{name:/Next Application QA/}).click();
  await page.locator('.instrument').waitFor({timeout:30000});
  assert((await page.locator('.instrument').innerText()).includes('Powers Test'));
  const scoreText=await page.locator('.instrument-team b').allTextContents();assert.equal(scoreText.length,2);
  const overflow=async where=>{const v=await page.evaluate(()=>({screen:innerWidth,page:document.documentElement.scrollWidth,visual:visualViewport?.width}));assert(v.screen<=width+2,where+' expanded mobile layout viewport '+JSON.stringify(v));assert(v.page<=v.screen+3,where+' has horizontal overflow '+JSON.stringify(v))};
  if(width<=900){const navState=await page.locator('.mobile-dock').evaluate(el=>{const r=el.getBoundingClientRect();const button=el.querySelectorAll('button')[1],b=button.getBoundingClientRect();const hit=document.elementFromPoint(b.x+b.width/2,b.y+b.height/2);return{display:getComputedStyle(el).display,position:getComputedStyle(el).position,z:getComputedStyle(el).zIndex,rect:{x:r.x,y:r.y,width:r.width,height:r.height},button:{x:b.x,y:b.y,width:b.width,height:b.height},innerHeight,visualHeight:visualViewport?.height,scrollY,hit:hit?.closest('.mobile-dock')===el,hitClass:hit?.className,hitTag:hit?.tagName}});console.log('MOBILE NAV GEOMETRY',JSON.stringify(navState));assert(navState.display!=='none'&&navState.hit,'Mobile navigation must be tappable: '+JSON.stringify(navState));}
  assert((await page.locator('.team-player-line').count())>=5,'Full team starters and bench visible');
  assert((await page.locator('.team-player-line .player-portrait').count())>=5,'Player portrait surfaces restored');
  assert((await page.locator('.startsit-spotlight').innerText()).includes('START'),'Direct Start / Sit decision visible on My Team');
  // Open a card after scrolling a long team roster. This catches transformed
  // ancestors that shift fixed mobile sheets below the visible viewport.
  const bottomCard=page.locator('.team-player-line').last();
  await bottomCard.scrollIntoViewIfNeeded();
  if(width<=390)await bottomCard.tap();else await bottomCard.click();
  await page.locator('[role="dialog"]').waitFor({state:'visible'});
  const modal=await page.evaluate(()=>{
   const overlay=document.querySelector('.dossier-backdrop');
   const drawer=document.querySelector('.dossier');
   const close=document.querySelector('.dossier-close');
   const rect=drawer.getBoundingClientRect();
   const button=close.getBoundingClientRect();
   const hit=document.elementFromPoint(button.x+button.width/2,button.y+button.height/2);
   return {portalled:overlay?.parentElement===document.body,top:rect.top,bottom:rect.bottom,
    height:innerHeight,closeTop:button.top,closeHit:!!hit?.closest('.dossier-close')};
  });
  assert(modal.portalled,'Player detail must mount at document.body, not within the animated content');
  assert(modal.top>=-3&&modal.bottom<=modal.height+3,
   'Player sheet must fit within mobile viewport: '+JSON.stringify(modal));
  assert(modal.closeTop>=0&&modal.closeHit,'Player detail close control must remain tappable');
  const rows=await page.locator('.history-row').count();
  assert.equal(rows,9,'Show all historical Weeks 1–8, not only Week 7 onward');
  const history=await page.locator('.history-table').innerText();
  assert(history.includes('ACTUAL POINTS')&&history.includes('W1')&&history.includes('W7'),
   'Full projection and actual history is visible');
  assert(history.includes('13.8'),'Numeric Week 1 actual must show even when the played flag is zero');
  assert(!history.includes('W9'),'Future weeks should not crowd the season-to-date chart');
  await page.locator('.dossier-close').click();
  assert.equal(await page.locator('[role="dialog"]').count(),0,'Player detail should close reliably');
  // The Start/Sit athlete cards use the same dossier presentation on touch.
  const recommendationCard=page.locator('.startsit-athlete').first();
  await recommendationCard.scrollIntoViewIfNeeded();
  if(width<=390)await recommendationCard.tap();else await recommendationCard.click();
  await page.locator('[role="dialog"]').waitFor({state:'visible'});
  assert.equal(await page.locator('.dossier-backdrop').evaluate(el=>el.parentElement===document.body),true);
  await page.locator('.dossier-close').click();
  await overflow('Briefing');await page.screenshot({path:new URL(label+'-'+width+'-briefing.png',screens).pathname});
  await page.locator(width<=900?'.mobile-dock':'.rail-nav').getByRole('button',{name:'Start / Sit'}).first().click();
  await page.locator('.opportunity').first().waitFor();
  await page.locator('.segmented').getByRole('button',{name:'Start / Sit'}).click();
  await page.getByLabel('First player').selectOption('qb1');
  await page.getByLabel('Second player').selectOption('rb1');
  assert((await page.locator('.comparison-summary').innerText()).includes('1.7'));
  await overflow('Decisions');
  await page.screenshot({path:new URL(label+'-'+width+'-decisions.png',screens).pathname});
  await page.locator(width<=900?'.mobile-dock':'.rail-nav').getByRole('button',{name:'Available'}).first().click();
  assert.equal(await page.locator('.scout-row').count(),1,'Only unrostered active players are listed by default');
  assert((await page.locator('.scout-row').innerText()).includes('Waiver Candidate'),'Free agent displayed');
  assert(!(await page.locator('.scout-table').innerText()).includes('Test Quarterback'),'Rostered player excluded from Scout');
  await page.getByLabel('Search available players').fill('Waiver');
  const scoutCard=page.locator('.scout-row').first();
  if(width<=390)await scoutCard.tap();else await scoutCard.click();
  await page.locator('[role="dialog"]').waitFor({state:'visible'});
  const viewportPosition=await page.locator('.dossier').evaluate(el=>{
   const r=el.getBoundingClientRect();return{top:r.top,bottom:r.bottom,visibleHeight:innerHeight};
  });
  assert(viewportPosition.top>=-3&&viewportPosition.bottom<=viewportPosition.visibleHeight+3,
   'Available-player detail should be inside the viewport');
  await page.screenshot({path:new URL(label+'-'+width+'-dossier.png',screens).pathname});
  await page.keyboard.press('Escape');
  assert.equal(await page.locator('[role="dialog"]').count(),0);
  await overflow('Scout');
  await page.locator(width<=900?'.mobile-dock':'.rail-nav').getByRole('button',{name:'League'}).first().click();
  await page.locator('.roster-row').first().waitFor();
  // Confirm roster cards also open a usable viewport-level player sheet.
  const rosterCard=page.locator('.roster-row').first();
  if(width<=390)await rosterCard.tap();else await rosterCard.click();
  await page.locator('[role="dialog"]').waitFor({state:'visible'});
  assert.equal(await page.locator('.dossier-backdrop').evaluate(el=>el.parentElement===document.body),true);
  await page.locator('.dossier-close').click();
  await page.locator('.league-subnav').getByRole('button',{name:'Standings'}).click();
  await page.locator('.standings-row').first().waitFor();
  assert.equal(await page.locator('.standings-row').count(),2);
  await page.getByRole('button',{name:'Trade',exact:true}).click();
  await page.locator('.trade-columns').waitFor();
  await page.locator('.asset-line').first().click();
  assert.equal(await page.locator('.asset-line.picked').count(),1);
  await overflow('Trade');await page.screenshot({path:new URL(label+'-'+width+'-trade.png',screens).pathname});
  await page.locator('.league-subnav').getByRole('button',{name:'Draft'}).click();
  await page.locator('.pick-owned-row').first().waitFor();
  assert((await page.locator('.pick-ownership').innerText()).includes('4250'),'First round pick modeled value is displayed');
  assert(!(await page.locator('.league-subnav').innerText()).includes('Data health'),'Admin details removed from consumer navigation');
  await page.locator(width<=900?'.mobile-dock':'.rail-nav').getByRole('button',{name:'Scores'}).click();
  await page.locator('.nfl-score-card').first().waitFor();
  assert((await page.locator('.nfl-score-card').innerText()).includes('Detroit Lions'),'Scores restored as a main destination');
  await overflow('Scores');
  assert.deepEqual(errors,[],'No uncaught browser errors');
  console.log('PASS '+label+' '+width);
 }finally{await context.close();await browser.close()}
}
try{await ready();for(const width of [320,390,768,1440])await test(chromium,'chromium',width);await test(webkit,'webkit',390);console.log('Next application end-to-end checks passed.')}
finally{server.kill('SIGTERM')}
