import assert from 'node:assert/strict';
import {spawn} from 'node:child_process';
import {setTimeout as sleep} from 'node:timers/promises';
import {mkdir} from 'node:fs/promises';
import {chromium} from 'playwright';

const port=4184,url='http://127.0.0.1:'+port+'/design-review/index.html';
const server=spawn(process.execPath,['node_modules/vite/bin/vite.js','preview','--host','127.0.0.1','--port',String(port)],{cwd:new URL('../',import.meta.url),stdio:['ignore','pipe','pipe']});
let logs='';
server.stdout.on('data',x=>logs+=String(x));
server.stderr.on('data',x=>logs+=String(x));
async function ready(){for(let i=0;i<60;i++){try{if((await fetch(url)).ok)return}catch{}await sleep(250)}throw Error('Preview not ready '+logs)}
const output=new URL('../ui-qa-design/',import.meta.url);
await mkdir(output,{recursive:true});
let browser;
try{
 await ready();
 browser=await chromium.launch({headless:true,args:['--no-sandbox']});
 const errors=[];
 for(const width of [320,390,768,1440]){
   const page=await browser.newPage({viewport:{width,height:width===1440?900:844},reducedMotion:'reduce'});
   page.on('pageerror',error=>errors.push(width+': '+error.message));
   await page.goto(url,{waitUntil:'domcontentloaded'});
   await page.locator('.a-home').waitFor();
   assert.equal(await page.locator('[data-concept-select]').count(),3);
   const checkWidth=async where=>{
     const size=await page.evaluate(()=>({width:innerWidth,scroll:document.documentElement.scrollWidth}));
     assert(size.scroll<=size.width+2,where+': overflow '+JSON.stringify(size));
   };
   for(const concept of ['a','b','c']){
     await page.locator('[data-concept-select="'+concept+'"]').click();
     await checkWidth('Home '+width+' '+concept);
     await page.screenshot({path:new URL(width+'-'+concept+'-home.png',output).pathname});
     await page.locator('.nav-link[data-nav="decisions"]').click();
     await page.locator('#left-select').selectOption('goff');
     await page.locator('#right-select').selectOption('love');
     assert((await page.locator('.comp-delta').innerText()).includes('−1.3'),'Common fixture model difference');
     await page.locator('#review-btn').click();
     await checkWidth('Decisions '+width+' '+concept);
     await page.locator('.nav-link[data-nav="scout"]').click();
     await page.locator('#scout-search').fill('Goff');
     assert.equal(await page.locator('.scout-row').count(),1);
     await page.locator('.scout-row').click();
     await page.locator('.inspector').waitFor();
     await page.keyboard.press('Escape');
     assert.equal(await page.locator('.inspector').count(),0);
     await checkWidth('Scout '+width+' '+concept);
     await page.locator('.nav-link[data-nav="connect"]').click();
     const input=page.locator('#username');
     await input.fill('');
     await input.pressSequentially('test_writer',{delay:8});
     assert.equal(await input.inputValue(),'test_writer');
     assert.equal(await input.evaluate(el=>document.activeElement===el),true,'Typing retains focus');
     await page.locator('#season').fill('2026');
     await page.locator('button[type="submit"]').click();
     assert.equal(await page.locator('.connect-success').count(),1);
     assert.equal(await input.inputValue(),'test_writer');
     await checkWidth('Connect '+width+' '+concept);
     await page.locator('.nav-link[data-nav="home"]').click();
   }
   await page.close();
 }
 assert.deepEqual(errors,[],'No browser errors');
 console.log('All three Powers–Newlon design directions passed interactive Chromium checks.');
}finally{await browser?.close();server.kill('SIGTERM')}
