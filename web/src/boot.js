// Fantasy Model Web 1.1 bootstrap.
// Adapts the production model snapshot into the stable field names consumed by
// the UI. The model exporter intentionally preserves richer production names;
// this layer keeps the website compatible without changing model outputs.

const nativeFetch = window.fetch.bind(window);

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

  // Make the current-week production fields explicit for the existing UI.
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

  // Sleeper roster IDs should resolve directly whenever the exporter provides
  // its identity map. GSIS/name remain fallbacks.
  player.player_id = player.sleeper_id || player.gsis_id || player.player_id;

  // The UI flattens snapshot objects when a week is selected. Give every
  // weekly row enough identity + persistent value context to stand alone.
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

// A small number of player-week rows can arrive without an opponent even when
// teammates (or the reciprocal opponent) have the matchup populated. Build one
// canonical team/week schedule from the snapshot and fill only future/current
// gaps. Historical rows are left untouched so player movement cannot rewrite
// past matchups.
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

// The current production snapshot contains player dynasty values but no generic
// future-pick rows. main.js already defines this exact fallback curve; exposing
// it as snapshot rows prevents missing/null values from being rendered as zero
// and keeps the same valuation available everywhere, including the trade tool.
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
        // main.js flatten() retains objects with an identity-shaped key. The
        // value stays null so this row cannot be mistaken for an NFL player.
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

  if (Array.isArray(snapshot.players)) {
    snapshot.players.forEach((p) => adaptPlayer(p, snapshot.projection_week));
    repairOpponentCoverage(snapshot);
  }

  ensureBaselinePickValues(snapshot);
  return snapshot;
}

window.fetch = async (input, init) => {
  const response = await nativeFetch(input, init);
  let url = '';
  try {
    url = new URL(typeof input === 'string' ? input : input?.url || '', window.location.href).pathname;
  } catch {
    return response;
  }

  if (!url.endsWith('/data/model_snapshot.json') || !response.ok) return response;

  try {
    const snapshot = adaptSnapshot(await response.clone().json());
    const headers = new Headers(response.headers);
    headers.set('content-type', 'application/json; charset=utf-8');
    return new Response(JSON.stringify(snapshot), {
      status: response.status,
      statusText: response.statusText,
      headers,
    });
  } catch (error) {
    console.warn('Fantasy Model snapshot adapter failed; using raw snapshot.', error);
    return response;
  }
};

await import('./main.js');
