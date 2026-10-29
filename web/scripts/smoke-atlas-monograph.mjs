
import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {setTimeout as delay} from 'node:timers/promises';
import {mkdir} from 'node:fs/promises';
import {chromium} from 'playwright';

const port=4194;
const url=`http://127.0.0.1:${port}/design-selected/atlas-monograph.html`;
const server=spawn(process.execPath,['node_modules/vite/bin/vite.js','preview','--host','127.0.0.1','--port',String(port)],
 {cwd:new URL('../',import.meta.url),stdio:['ignore','pipe','pipe']});
let logs='';
server.stdout.on('data',x=>logs+=String(x));
server.stderr.on('data',x=>logs+=String(x));
async function ready(){for(let i=0;i<80;i++){try{if((await fetch(url)).ok)return}catch{}await delay(250)}throw Error('Preview not ready: '+logs)}
const out=new URL('../ui-qa-atlas-monograph/',import.meta.url);
await mkdir(out,{recursive:true});
let browser;
try{
 await ready();browser=await chromium.launch({headless:true,args:['--no-sandbox']});
 const errors=[];
 for(const width of [320,390,768,1440]){
  const page=await browser.newPage({viewport:{width,height:width===1440?900:844},reducedMotion:'reduce'});
  page.on('pageerror',error=>errors.push(width+': '+error.message));
  await page.goto(url,{waitUntil:'domcontentloaded'});
  await page.locator('.b-home .b-instrument').waitFor();
  assert.equal(await page.locator('#app').getAttribute('data-concept'),'d','Selected Atlas layout uses heritage palette');
  const palette=await page.locator('#app').evaluate(el=>({
    canvas:getComputedStyle(el).getPropertyValue('--bg').trim().toLowerCase(),
    hero:getComputedStyle(el).getPropertyValue('--dark').trim().toLowerCase()
  }));
  assert.equal(palette.canvas,'#f5f3ee');
  assert.equal(palette.hero,'#23463e');
  const noOverflow=async screen=>{
    const x=await page.evaluate(()=>({width:innerWidth,scroll:document.documentElement.scrollWidth}));
    assert(x.scroll<=x.width+3,screen+' '+width+' overflow '+JSON.stringify(x));
  };
  await noOverflow('Briefing');
  await page.screenshot({path:new URL(width+'-briefing.png',out).pathname,fullPage:true});
  await page.locator('.nav-link[data-nav="decisions"]:visible').first().click();
  await page.locator('#left-select').selectOption('goff');
  await page.locator('#right-select').selectOption('love');
  await page.locator('#review-btn').click();
  assert((await page.locator('#review-btn').innerText()).includes('Marked'));
  await noOverflow('Decisions');
  await page.screenshot({path:new URL(width+'-decisions.png',out).pathname,fullPage:true});
  await page.locator('.nav-link[data-nav="scout"]:visible').first().click();
  await page.locator('#scout-search').fill('Goff');
  assert.equal(await page.locator('.scout-row').count(),1);
  await page.locator('.scout-row').first().click();
  await page.locator('.inspector').waitFor();
  await page.screenshot({path:new URL(width+'-dossier.png',out).pathname,fullPage:true});
  await page.keyboard.press('Escape');
  assert.equal(await page.locator('.inspector').count(),0);
  await noOverflow('Scout');
  await page.locator('.nav-link[data-nav="connect"]:visible').first().click();
  const field=page.locator('#username');
  await field.pressSequentially('atlas_user',{delay:5});
  assert.equal(await field.inputValue(),'atlas_user');
  assert.equal(await field.evaluate(el=>document.activeElement===el),true,'Username focus persists while typing');
  const size=await field.evaluate(el=>parseFloat(getComputedStyle(el).fontSize));
  assert(size>=16,'Mobile form font size avoids iOS zoom');
  await page.locator('#season').fill('2026');
  await page.locator('#connect-form button[type="submit"]').click();
  assert.equal(await page.locator('.connect-success').count(),1);
  assert.equal(await field.inputValue(),'atlas_user');
  await noOverflow('Connect');
  await page.screenshot({path:new URL(width+'-connect.png',out).pathname,fullPage:true});
  await page.close();
 }
 assert.deepEqual(errors,[]);
 console.log('Atlas x Monograph: all screens and interactions passed on four viewport widths.');
}finally{await browser?.close();server.kill('SIGTERM')}
