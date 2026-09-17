from pathlib import Path

main=Path('web/src/main.js')
pkg=Path('web/package.json')
s=main.read_text(encoding='utf-8')

old="async function loadPlayerNews(id,force=false){const key=String(id),cached=st.playerNews.get(key);if(!force&&cached&&Date.now()-n(cached.at,0)<10*60*1000)return cached;const p=st.players?.[id]||{},entry=resolveModel(id,p),base=entry?.base||{},espnId=p?.espn_id||field(base,['espn_id','espn_player_id','espnId']);st.playerNews.set(key,{at:Date.now(),loading:true,articles:[],source:'ESPN Fantasy'});if(String(st.profile)===key)render();let articles=[],ok=false;if(espnId){const data=await liveJson(`https://site.api.espn.com/apis/fantasy/v2/games/ffl/news/players?limit=20&playerId=${encodeURIComponent(espnId)}`,null);ok=!!data;const feed=Array.isArray(data?.feed)?data.feed:[];articles=feed.slice(0,6).map(a=>({headline:a?.headline||a?.title||'Player update',description:a?.description||a?.story||a?.summary||'',published:a?.published||a?.lastModified||a?.date||'',url:a?.links?.web?.href||a?.link?.href||a?.url||''})).filter(a=>a.headline)}if(!articles.length){const data=await liveJson('https://site.api.espn.com/apis/site/v2/sports/football/nfl/news?limit=100',null);ok=ok||!!data;const needle=normName(pname(id));articles=(Array.isArray(data?.articles)?data.articles:[]).filter(a=>normName(`${a?.headline||a?.title||''} ${a?.description||a?.story||''}`).includes(needle)).slice(0,6).map(a=>({headline:a?.headline||a?.title||'Player update',description:a?.description||a?.story||'',published:a?.published||a?.lastModified||'',url:a?.links?.web?.href||a?.link?.href||a?.url||''}))}const z={at:Date.now(),articles,source:'ESPN Fantasy',unavailable:!ok};st.playerNews.set(key,z);if(String(st.profile)===key)render();return z}"
new="async function loadPlayerNews(id,force=false){const key=String(id),cached=st.playerNews.get(key);if(!force&&cached&&Date.now()-n(cached.at,0)<10*60*1000)return cached;const name=pname(id),needle=normName(name);st.playerNews.set(key,{at:Date.now(),loading:true,articles:[],source:'ESPN'});if(String(st.profile)===key)render();const data=await liveJson(`https://site.web.api.espn.com/apis/search/v2?limit=30&query=${encodeURIComponent(name)}`,null),groups=Array.isArray(data?.results)?data.results:[],articleGroup=groups.find(g=>g?.type==='article'),candidates=(Array.isArray(articleGroup?.contents)?articleGroup.contents:[]).filter(a=>{const u=String(a?.link?.web||'');return !u||u.includes('espn.com/nfl/')||u.includes('/nfl/')});const direct=candidates.filter(a=>normName(`${a?.displayName||a?.headline||a?.title||''} ${a?.description||a?.subtitle||''}`).includes(needle)),picked=(direct.length?direct:candidates).slice(0,6),articles=picked.map(a=>({headline:a?.displayName||a?.headline||a?.title||'Player update',description:a?.description||a?.subtitle||'',published:a?.published||a?.date||a?.lastModified||'',url:a?.link?.web||a?.links?.web?.href||a?.url||''}));const z={at:Date.now(),articles,source:'ESPN',unavailable:!data};st.playerNews.set(key,z);if(String(st.profile)===key)render();return z}"
if old not in s:
    raise SystemExit('Web 1.3.3 loadPlayerNews block not found')
s=s.replace(old,new,1)
main.write_text(s,encoding='utf-8')

p=pkg.read_text(encoding='utf-8')
if '"version": "1.3.3"' not in p:
    raise SystemExit('expected Web 1.3.3 package version')
p=p.replace('"version": "1.3.3"','"version": "1.3.4"',1)
pkg.write_text(p,encoding='utf-8')
print('Patched Web 1.3.4: ESPN browser-safe player-news search')
