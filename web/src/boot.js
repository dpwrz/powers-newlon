// Fantasy Model Web 1.1 bootstrap.
// Adapts the production model snapshot into the stable field names consumed by
// the UI. The model exporter intentionally preserves richer production names;
// this layer keeps the website compatible without changing model outputs.

const nativeFetch = window.fetch.bind(window);

const asNumber = (value) => {
  const x = Number(value);
  return Number.isFinite(x) ? x : null;
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
  if (current?.opponent != null) player.opponent = current.opponent;
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

function adaptSnapshot(snapshot) {
  if (!snapshot || typeof snapshot !== 'object') return snapshot;
  if (Array.isArray(snapshot.players)) {
    snapshot.players.forEach((p) => adaptPlayer(p, snapshot.projection_week));
  }
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
