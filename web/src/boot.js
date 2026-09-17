// Fantasy Model Web 1.1 bootstrap.
// New snapshots are normalized by the model pipeline before they reach the
// browser. A tiny version manifest controls a browser Cache API entry so the
// 10+ MB model snapshot is downloaded only when the model actually changes.

const nativeFetch = window.fetch.bind(window);
const WEB_SCHEMA_VERSION = 2;
const SNAPSHOT_URL = '/data/model_snapshot.json';
const SNAPSHOT_VERSION_URL = '/data/snapshot_version.json';
const SNAPSHOT_CACHE = 'fantasy-model-snapshot-v1';
const SNAPSHOT_VERSION_KEY = 'fm_snapshot_version';

const asNumber = (value) => {
  if (value === null || value === undefined || value === '') return null;
  const x = Number(value);
  return Number.isFinite(x) ? x : null;
};

const cleanText = (value) => String(value ?? '').trim();

const TEAM_ALIASES = {
  LA: 'LAR',
  STL: 'LAR',
  JAC: 'JAX',
  OAK: 'LV',
  SD: 'LAC',
  WAS: 'WSH',
};

const teamCode = (value) => {
  const raw = cleanText(value).toUpperCase();
  if (!raw) return '';
  return TEAM_ALIASES[raw] || raw;
};

const averageCompletedActuals = (weeks) => {
  const values = (Array.isArray(weeks) ? weeks : [])
    .filter((w) => (asNumber(w?.game_played) ?? 0) > 0)
    .map((w) => asNumber(w?.actual))
    .filter((x) => x !== null);
  if (!values.length) return null;
  return values.reduce((a, b) => a + b, 0) / values.length;
};

function adaptPlayer(player, projectionWeek) {
  if (!player || typeof player !== 'object') return player;

  const weeks = Array.isArray(player.weekly_projections)
    ? player.weekly_projections
    : [];
  const currentWeek = asNumber(player.week) ?? asNumber(projectionWeek);
  const current = weeks.find((w) => asNumber(w?.week) === currentWeek) || null;
  const avgPpg = averageCompletedActuals(weeks) ?? asNumber(player.season_fppg);

  const currentProjection = asNumber(current?.projection) ?? asNumber(player.week_projection);
  if (currentProjection !== null) {
    player.projection = currentProjection;
    player.weekly_projection = currentProjection;
    player.pregame_projection = currentProjection;
  }
  if (current && asNumber(current.floor) !== null) player.floor = asNumber(current.floor);
  if (current && asNumber(current.ceiling) !== null) player.ceiling = asNumber(current.ceiling);
  if (current?.confidence != null) player.confidence = current.confidence;
  if (cleanText(current?.opponent)) player.opponent = current.opponent;
  if (avgPpg !== null) {
    player.avg_ppg = avgPpg;
    player.current_avg_ppg = avgPpg;
    player.season_ppg = avgPpg;
  }

  player.player_id = player.sleeper_id || player.gsis_id || player.player_id;

  for (const week of weeks) {
    if (!week || typeof week !== 'object') continue;
    week.player_id = player.sleeper_id || player.gsis_id || week.player_id;
    week.sleeper_id = player.sleeper_id || week.sleeper_id;
    week.gsis_id = player.gsis_id || week.gsis_id;
    week.player_name = player.player_name || week.player_name;
    week.position = player.position || week.position;
    week.team = player.team || week.team;
    week.dynasty_value = player.dynasty_value ?? week.dynasty_value;
    if (avgPpg !== null) {
      week.avg_ppg = avgPpg;
      week.current_avg_ppg = avgPpg;
      week.season_ppg = avgPpg;
    }
    const projection = asNumber(week.projection);
    if (projection !== null) {
      week.weekly_projection = projection;
      week.pregame_projection = projection;
    }
  }

  return player;
}

function repairOpponentCoverage(snapshot) {
  const players = Array.isArray(snapshot?.players) ? snapshot.players : [];
  const startWeek = asNumber(snapshot?.projection_week) ?? 1;
  const byTeamWeek = new Map();

  for (const player of players) {
    const team = teamCode(player?.team);
    if (!team) continue;

    const weeks = Array.isArray(player?.weekly_projections)
      ? player.weekly_projections
      : [];

    for (const week of weeks) {
      const weekNo = asNumber(week?.week);
      const opponent = teamCode(week?.opponent);
      if (weekNo === null || weekNo < startWeek || !opponent) continue;

      const key = `${team}|${weekNo}`;
      if (!byTeamWeek.has(key) || byTeamWeek.get(key) === 'BYE') {
        byTeamWeek.set(key, opponent);
      }

      if (opponent !== 'BYE') {
        const reverseKey = `${opponent}|${weekNo}`;
        if (!byTeamWeek.has(reverseKey)) byTeamWeek.set(reverseKey, team);
      }
    }
  }

  for (const player of players) {
    const team = teamCode(player?.team);
    const weeks = Array.isArray(player?.weekly_projections)
      ? player.weekly_projections
      : [];

    for (const week of weeks) {
      const weekNo = asNumber(week?.week);
      if (!team || weekNo === null || weekNo < startWeek || cleanText(week?.opponent)) continue;
      const inferred = byTeamWeek.get(`${team}|${weekNo}`);
      if (inferred) week.opponent = inferred;
    }

    const currentWeek = asNumber(player?.week) ?? asNumber(snapshot?.projection_week);
    const current = weeks.find((w) => asNumber(w?.week) === currentWeek) || null;
    if (cleanText(current?.opponent)) player.opponent = current.opponent;
  }
}

function ensureBaselinePickValues(snapshot) {
  const storedSeason = asNumber(localStorage.getItem('fm_season'));
  const baseSeason = storedSeason ?? new Date().getFullYear();
  const existing = Array.isArray(snapshot.pick_values) ? snapshot.pick_values : [];
  const seen = new Set(
    existing
      .map((x) => `${asNumber(x?.season)}|${asNumber(x?.round)}`)
      .filter((x) => !x.includes('null')),
  );

  for (let season = baseSeason + 1; season <= baseSeason + 6; season += 1) {
    for (let round = 1; round <= 8; round += 1) {
      const key = `${season}|${round}`;
      if (seen.has(key)) continue;

      const roundBase = ({ 1: 4200, 2: 1800, 3: 800, 4: 350 }[round] || 100);
      const value = Math.round(
        roundBase * Math.pow(0.92, Math.max(0, season - baseSeason - 1)),
      );

      existing.push({
        sleeper_id: null,
        asset_type: 'future_pick',
        season,
        round,
        dynasty_value: value,
        pick_value: value,
        valuation_source: 'baseline_future_pick_curve',
      });
      seen.add(key);
    }
  }

  snapshot.pick_values = existing;
}

function adaptSnapshot(snapshot) {
  if (!snapshot || typeof snapshot !== 'object') return snapshot;

  // Producer-normalized Web 1.1 snapshots are already ready for the UI. This
  // is the normal path after the next model refresh and avoids a full traversal.
  if ((asNumber(snapshot.web_schema_version) ?? 0) >= WEB_SCHEMA_VERSION) {
    return snapshot;
  }

  if (Array.isArray(snapshot.players)) {
    snapshot.players.forEach((p) => adaptPlayer(p, snapshot.projection_week));
    repairOpponentCoverage(snapshot);
  }

  ensureBaselinePickValues(snapshot);
  return snapshot;
}

window.FantasySnapshot = Object.freeze({
  schemaVersion: WEB_SCHEMA_VERSION,
  adapt: adaptSnapshot,
});

async function fetchSnapshotManifest() {
  try {
    const response = await nativeFetch(SNAPSHOT_VERSION_URL, {
      cache: 'no-store',
      headers: { accept: 'application/json' },
    });
    if (!response.ok) return null;
    return await response.json();
  } catch (error) {
    console.warn('Fantasy Model snapshot manifest unavailable.', error);
    return null;
  }
}

async function fetchVersionedSnapshot(input, init) {
  const manifest = await fetchSnapshotManifest();
  const version = cleanText(manifest?.version || manifest?.hash || manifest?.etag);
  const cachedVersion = cleanText(localStorage.getItem(SNAPSHOT_VERSION_KEY));

  let cache = null;
  if ('caches' in window) {
    try {
      cache = await caches.open(SNAPSHOT_CACHE);
    } catch (error) {
      console.warn('Fantasy Model snapshot cache unavailable.', error);
    }
  }

  if (cache && version && cachedVersion === version) {
    try {
      const cached = await cache.match(SNAPSHOT_URL);
      if (cached) return cached;
    } catch (error) {
      console.warn('Fantasy Model cached snapshot read failed.', error);
    }
  }

  const response = await nativeFetch(input, init);
  if (!response.ok) return response;

  if (cache) {
    try {
      await cache.put(SNAPSHOT_URL, response.clone());
      if (version) localStorage.setItem(SNAPSHOT_VERSION_KEY, version);
    } catch (error) {
      console.warn('Fantasy Model snapshot cache write failed.', error);
    }
  }

  return response;
}

// Backward compatibility for the snapshot already deployed today. Instead of
// response.clone().json() -> JSON.stringify() -> new Response() -> .json(),
// override only the snapshot response's json() method. The payload is parsed
// once, adapted in memory once, and handed directly to main.js. Producer-
// normalized snapshots skip the traversal entirely.
window.fetch = async (input, init) => {
  let url = '';
  try {
    url = new URL(typeof input === 'string' ? input : input?.url || '', window.location.href).pathname;
  } catch {
    return nativeFetch(input, init);
  }

  const response = url.endsWith(SNAPSHOT_URL)
    ? await fetchVersionedSnapshot(input, init)
    : await nativeFetch(input, init);

  if (!url.endsWith(SNAPSHOT_URL) || !response.ok) return response;

  let adaptedPromise = null;
  return new Proxy(response, {
    get(target, prop) {
      if (prop === 'json') {
        return () => {
          if (!adaptedPromise) {
            adaptedPromise = target.json()
              .then(adaptSnapshot)
              .catch((error) => {
                console.warn('Fantasy Model snapshot adapter failed; using raw snapshot.', error);
                throw error;
              });
          }
          return adaptedPromise;
        };
      }

      const value = Reflect.get(target, prop, target);
      return typeof value === 'function' ? value.bind(target) : value;
    },
  });
};

await import('./main.js');
