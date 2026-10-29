// Promote the independently built Atlas × Monograph application to '/'.
// Preserve the former Vite homepage under '/legacy/' for a reversible cutover.
// This runs AFTER 'vite build' has copied public/next/ and compiled the legacy app.
import {readFile,writeFile,mkdir,stat} from 'node:fs/promises';
import path from 'node:path';
import {fileURLToPath} from 'node:url';

const web=path.resolve(path.dirname(fileURLToPath(import.meta.url)),'..');
const dist=path.join(web,'dist');
const rootIndex=path.join(dist,'index.html');
const nextIndex=path.join(dist,'next','index.html');
const legacyIndex=path.join(dist,'legacy','index.html');

const [legacy,atlas]=await Promise.all([readFile(rootIndex,'utf8'),readFile(nextIndex,'utf8')]);

if(!legacy.includes('id="app"')||!legacy.includes('/assets/')){
 throw new Error('Unexpected legacy build. Refusing to replace the root homepage without a rollback copy.');
}
if(!atlas.includes('id="root"')||!atlas.includes('/next/assets/')){
 throw new Error('The Atlas app was not built with the expected /next/ asset prefix. Refusing root promotion.');
}
const assetPaths=[...atlas.matchAll(/(?:src|href)="(\/next\/assets\/[^"]+)"/g)].map(m=>m[1]);
if(assetPaths.length===0)throw new Error('No compiled Atlas asset references found.');
for(const asset of assetPaths){
 const target=path.join(dist,asset.slice(1));
 const info=await stat(target);
 if(!info.isFile()||info.size===0)throw new Error('Missing/empty Atlas asset: '+asset);
}
await mkdir(path.dirname(legacyIndex),{recursive:true});
await writeFile(legacyIndex,legacy,'utf8');
await writeFile(rootIndex,atlas,'utf8');

const deployed=await readFile(rootIndex,'utf8');
const rollback=await readFile(legacyIndex,'utf8');
if(!deployed.includes('id="root"')||!rollback.includes('id="app"')){
 throw new Error('Root promotion verification failed.');
}
console.log('Atlas × Monograph is the root homepage. Original UI saved at /legacy/.');
