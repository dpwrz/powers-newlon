// Screenshot and interaction checks for the isolated Fieldnote concept.
// Fixture-only; does not access Sleeper or modify the production application.
import assert from 'node:assert/strict';
import { spawn } from 'node:child_process';
import { setTimeout as delay } from 'node:timers/promises';
import { chromium } from 'playwright';
import { mkdir } from 'node:fs/promises';
const url='http://127.0.0.1:4174/fieldnote/index.html';
const server=spawn(process.execPath,['node_modules/vite/bin/vite.js','preview','--host','127.0.0.1','--port','4174'],{cwd:new URL('../',import.meta.url),stdio:['ignore','pipe','pipe']});
let logs='';
server.stdout.on('data',x=>logs+=String(x));server.stderr.on('data',x=>logs+=String(x));
async function ready(){for(let i=0;i<70;i++){try{const r=await fetch(url);if(r.ok)return}catch{}await delay(250)}throw Error('Concept preview failed: '+logs)}
const dir=new URL('../ui-qa-fieldnote/',import.meta.url);
await mkdir(dir,{recursive:true});
let browser;
try{
 await ready();browser=await chromium.launch({headless:true,args:['--no-sandbox']});
 const pageErrors=[];
 for(const [name,width,height] of [['phone',390,844],['small',320,740],['tablet',768,1024],['desktop',1440,900]]){
  const context=await browser.newContext({viewport:{width,height},reducedMotion:'reduce'});
  const page=await context.newPage();
  page.on('pageerror',e=>pageErrors.push(name+': '+e.message));
  page.setDefaultTimeout(10000);
  await page.goto(url,{waitUntil:'domcontentloaded'});
  await page.locator('.briefing-hero').waitFor();
  assert.equal(await page.locator('[data-nav="briefing"]').count(),width<=900?2:2);
  const noOverflow=async at=>{const x=await page.evaluate(()=>({screen:innerWidth,page:document.documentElement.scrollWidth}));assert(x.page<=x.screen+2,name+' '+at+' overflow '+JSON.stringify(x))};
  await noOverflow('briefing');
  await page.screenshot({path:new URL(name+'-briefing.png',dir).pathname});
  await page.locator('[data-nav="lab"]:visible').first().click();
  await page.locator('#compare-b').selectOption('goff');
  assert.equal(await page.locator('.decision-result h2').textContent(),'Jared Goff');
  await page.locator('[data-review="one"]').click();
  assert((await page.locator('[data-review="one"]').getAttribute('class')).includes('done'));
  await noOverflow('lab');
  await page.screenshot({path:new URL(name+'-lab.png',dir).pathname});
  await page.locator('[data-nav="scout"]:visible').first().click();
  await page.locator('#scout-query').fill('Goff');
  assert.equal(await page.locator('.player-index-row').count(),1);
  await page.locator('.player-index-row').click();
  await page.locator('.inspector').waitFor();
  assert.equal(await page.locator('#inspector-title').textContent(),'Jared Goff');
  await page.screenshot({path:new URL(name+'-inspector.png',dir).pathname});
  await page.keyboard.press('Escape');
  assert.equal(await page.locator('.inspector').count(),0);
  await page.locator('#scout-query').fill('');
  await page.locator('[data-filter="QB"]').click();
  assert.equal(await page.locator('.player-index-row').count(),3);
  await noOverflow('scout');
  await page.screenshot({path:new URL(name+'-scout.png',dir).pathname});
  await page.locator('[data-nav="league"]:visible').first().click();
  await page.locator('[data-standings="points"]').click();
  assert.equal(await page.locator('.standing-line').count(),5);
  await noOverflow('league');
  await page.screenshot({path:new URL(name+'-league.png',dir).pathname});
  await context.close();
 }
 assert.deepEqual(pageErrors,[]);
 console.log('Fieldnote interaction and responsive checks passed');
}finally{await browser?.close();server.kill('SIGTERM')}
