// ---------------------------------------------------------------------------
// State
// ---------------------------------------------------------------------------
let currentData = null;
const staticDataCache = {};  // central-parameter results per measure, fetched once per session

// Static central-parameter results for each housing-supply measure
const STATIC_FILES = {
  permits: '/default_data.json',
  households: '/default_data_households.json',
};

const RATE_HELP = {
  permits: {
    question: 'What is the counterfactual permitting rate?',
    text: 'A measure of how relaxed permitting is in the city under the counterfactual. Higher means more relaxed, so the city facilitates more housing construction.',
  },
  households: {
    question: 'What is counterfactual household growth?',
    text: "The growth in a city's number of households between 2001 and 2021, per 2001 household, under the counterfactual. Each treated city is raised to this value and its population growth is scaled up in proportion. Higher means more homes were allowed.",
  },
};

const CENTRAL = {gamma: 0.07, theta: 0.06, sigma: 0.015, beta: 0.035, lambda: 0.18};

const PALETTE = [
  '#e6194B','#3cb44b','#ffe119','#4363d8','#f58231',
  '#911eb4','#42d4f4','#f032e6','#bfef45','#fabed4',
  '#469990','#dcbeff','#9A6324','#fffac8','#800000',
  '#aaffc3','#808000','#ffd8b1','#000075','#a9a9a9'
];

const CITY_SET_ORDER = ['london_only','top_4','top_6','university_cities','top_10'];

// ---------------------------------------------------------------------------
// Slider wiring
// ---------------------------------------------------------------------------
const PARAMS = ['gamma','theta','sigma','beta','lambda'];

PARAMS.forEach(p => {
  const slider = document.getElementById(p);
  const numInput = document.getElementById(p + '-val');

  // Slider -> number input
  slider.addEventListener('input', () => {
    numInput.value = parseFloat(slider.value).toFixed(3);
  });

  // Number input -> slider (clamped to bounds)
  numInput.addEventListener('change', () => {
    const min = parseFloat(slider.min);
    const max = parseFloat(slider.max);
    let val = parseFloat(numInput.value);
    if (isNaN(val)) val = CENTRAL[p];
    val = Math.min(max, Math.max(min, val));
    numInput.value = val.toFixed(3);
    slider.value = val;
  });
});

document.getElementById('reset-btn').addEventListener('click', () => {
  PARAMS.forEach(p => {
    const slider = document.getElementById(p);
    slider.value = CENTRAL[p];
    document.getElementById(p + '-val').value = CENTRAL[p].toFixed(3);
  });
});

// ---------------------------------------------------------------------------
// Tabs: three top-level tabs; Consumption and City-Specific Metrics each
// have a dropdown choosing which chart (.tab-content) is shown
// ---------------------------------------------------------------------------
const TAB_GROUP_SELECT = {
  'national-income': null,          // single chart, no dropdown
  'consumption': 'consumption-metric',
  'city': 'city-metric',
};
let activeGroup = 'national-income';

function showActiveChart() {
  const selectId = TAB_GROUP_SELECT[activeGroup];
  const contentId = selectId ? document.getElementById(selectId).value : activeGroup;
  document.querySelectorAll('.tab-content').forEach(c => c.classList.toggle('active', c.id === contentId));
  document.querySelectorAll('.metric-picker').forEach(p => p.classList.toggle('active', p.dataset.group === activeGroup));

  // Re-render visible chart so Plotly sizes correctly
  if (currentData) renderAll(currentData);
}

document.querySelectorAll('.tab').forEach(btn => {
  btn.addEventListener('click', () => {
    document.querySelectorAll('.tab').forEach(b => b.classList.toggle('active', b === btn));
    activeGroup = btn.dataset.group;
    showActiveChart();
  });
});

document.querySelectorAll('.metric-select').forEach(sel => {
  sel.addEventListener('change', showActiveChart);
});

// ---------------------------------------------------------------------------
// Sidebar collapse toggle
// ---------------------------------------------------------------------------
document.getElementById('sidebar-toggle').addEventListener('click', () => {
  const container = document.querySelector('.app-container');
  const btn = document.getElementById('sidebar-toggle');
  const collapsed = container.classList.toggle('sidebar-collapsed');
  btn.innerHTML = collapsed ? '&raquo;' : '&laquo;';
  btn.title = collapsed ? 'Expand sidebar' : 'Collapse sidebar';

  // Re-render so Plotly fits the new width
  if (currentData) renderAll(currentData);
});

// ---------------------------------------------------------------------------
// Run button
// ---------------------------------------------------------------------------
document.getElementById('run-btn').addEventListener('click', runComputation);

function isCentral(params) {
  return PARAMS.every(p => Math.abs(params[p] - CENTRAL[p]) < 1e-9);
}

function filterByCitySet(data, citySet) {
  return {
    ...data,
    agg: data.agg.filter(r => r.city_set === citySet),
    city_income: data.city_income.filter(r => r.city_set === citySet),
    city_cons: data.city_cons.filter(r => r.city_set === citySet),
    city_pop: data.city_pop.filter(r => r.city_set === citySet),
    city_set: citySet,
  };
}

async function runComputation() {
  const params = {};
  PARAMS.forEach(p => { params[p] = parseFloat(document.getElementById(p).value); });
  const citySet = document.getElementById('city-set').value;
  params.city_set = citySet;
  const measure = document.getElementById('measure').value;
  params.measure = measure;

  // Second-order condition of the incumbents' problem: urban costs must rise faster than agglomeration benefits
  if (params.gamma + params.theta <= params.sigma + params.beta + 1e-9) {
    alert('Invalid parameters: the model requires \u03B3 + \u03B8 > \u03C3 + \u03B2 ' +
          '(urban costs must rise faster with city size than agglomeration benefits).');
    return;
  }

  document.getElementById('loading').style.display = 'flex';

  try {
    // Fast path: central params -> static JSON (no Python boot)
    if (isCentral(params)) {
      if (!staticDataCache[measure]) {
        const resp = await fetch(STATIC_FILES[measure]);
        if (!resp.ok) throw new Error('Failed to load ' + STATIC_FILES[measure]);
        staticDataCache[measure] = await resp.json();
      }
      const cached = staticDataCache[measure];
      currentData = (citySet === 'all')
        ? {...cached, city_set: 'all'}
        : filterByCitySet(cached, citySet);
      renderAll(currentData);
      return;
    }

    // Custom params: hit the API
    const resp = await fetch('/api/compute', {
      method: 'POST',
      headers: {'Content-Type': 'application/json'},
      body: JSON.stringify(params),
    });
    if (!resp.ok) throw new Error(await resp.text());
    currentData = await resp.json();
    renderAll(currentData);
  } catch (err) {
    alert('Computation failed: ' + err.message);
  } finally {
    document.getElementById('loading').style.display = 'none';
  }
}

// ---------------------------------------------------------------------------
// Rendering
// ---------------------------------------------------------------------------
function paramCaption(params, measure) {
  const caption = `\u03B3=${params.gamma.toFixed(2)}, \u03B8=${params.theta.toFixed(2)}, ` +
                  `\u03C3=${params.sigma.toFixed(3)}, \u03B2=${params.beta.toFixed(3)}, ` +
                  `\u03BB=${params.lambda.toFixed(3)}`;
  return measure === 'households'
    ? caption + ' \u00B7 housing supply measured by census household growth'
    : caption;
}

function updateRateHelp(measure) {
  const help = RATE_HELP[measure] || RATE_HELP.permits;
  document.querySelectorAll('.rate-help').forEach(el => {
    el.innerHTML = help.question + '<span class="tooltip-text">' + help.text + '</span>';
  });
}

function refAnnotations(refRates) {
  return refRates.map(r => ({
    x: r.rate,
    y: 1,
    yref: 'paper',
    text: r.label,
    showarrow: false,
    textangle: -90,
    xanchor: 'left',
    yanchor: 'top',
    font: {size: 10, color: 'gray'},
  }));
}

function refShapes(refRates) {
  return refRates.map(r => ({
    type: 'line',
    x0: r.rate, x1: r.rate,
    y0: 0, y1: 1,
    yref: 'paper',
    line: {dash: 'dash', color: 'rgba(0,0,0,0.25)', width: 1},
  }));
}

function baseLayout(title, ylabel, refRates, params, data) {
  const measure = (data && data.measure) || 'permits';
  return {
    title: {text: title + '<br><span style="font-size:11px;color:gray">' + paramCaption(params, measure) + '</span>'},
    xaxis: {
      title: (data && data.x_label) || 'Counterfactual permitting rate',
      dtick: (data && data.dtick) || 0.02,
      tickformat: '.2f',
      hoverformat: '.3f',
    },
    yaxis: {title: ylabel},
    hovermode: 'x unified',
    shapes: [
      {type:'line', x0:0, x1:1, xref:'paper', y0:0, y1:0, line:{color:'gray',width:1}},
      ...refShapes(refRates),
    ],
    annotations: refAnnotations(refRates),
    legend: {orientation: 'h', x: 0.5, xanchor: 'center', y: -0.18, yanchor: 'top'},
    margin: {t: 80, r: 30, b: 110},
  };
}

// --- Aggregate chart (single or multi-set) ---

function renderAggChart(containerId, data, yVar, ylabel, titleMetric) {
  const agg = data.agg;
  const refs = data.reference_rates;
  const labels = data.city_set_labels;
  const citySet = data.city_set;

  const traces = [];

  if (citySet === 'all') {
    CITY_SET_ORDER.forEach((cs, i) => {
      const rows = agg.filter(r => r.city_set === cs);
      traces.push({
        x: rows.map(r => r.target_rate),
        y: rows.map(r => r[yVar]),
        name: labels[cs],
        mode: 'lines',
        line: {color: PALETTE[i], width: 2},
        hovertemplate: '%{y:.2f}%',
      });
    });
  } else {
    const rows = agg.filter(r => r.city_set === citySet);
    traces.push({
      x: rows.map(r => r.target_rate),
      y: rows.map(r => r[yVar]),
      name: labels[citySet] || citySet,
      mode: 'lines',
      line: {color: PALETTE[0], width: 2},
      hovertemplate: '%{y:.2f}%',
    });
  }

  const title = citySet === 'all'
    ? titleMetric
    : titleMetric + ': ' + (labels[citySet] || citySet);

  Plotly.newPlot(containerId, traces, baseLayout(title, ylabel, refs, data.params, data), {responsive: true});
}

// --- City-level chart ---

function renderCityChart(containerId, rows, citySetName, yVar, ylabel, titleMetric, data) {
  const refs = data.reference_rates;
  const labels = data.city_set_labels;

  const cityRows = rows.filter(r => r.city_set === citySetName);
  // Determine unique cities ordered by descending population
  const cityPops = {};
  cityRows.forEach(r => { cityPops[r.BUA22NM] = r.bua_21_pop; });
  const cityOrder = Object.keys(cityPops).sort((a, b) => cityPops[b] - cityPops[a]);

  const traces = cityOrder.map((city, i) => {
    const cr = cityRows.filter(r => r.BUA22NM === city);
    const displayName = city.replace('Cambridge (Cambridge)', 'Cambridge');
    return {
      x: cr.map(r => r.target_rate),
      y: cr.map(r => r[yVar]),
      name: displayName,
      mode: 'lines',
      line: {color: PALETTE[i % PALETTE.length], width: 2},
      hovertemplate: displayName + ': %{y:.2f}%',
    };
  });

  const title = titleMetric + ': ' + (labels[citySetName] || citySetName);
  Plotly.newPlot(containerId, traces, baseLayout(title, ylabel, refs, data.params, data), {responsive: true});
}

// --- City-level population chart (per-city + aggregate "other" lines) ---

function fmtDelta(n) {
  return (n >= 0 ? '+' : '-') + Math.round(Math.abs(n)).toLocaleString('en-GB');
}

function renderCityPopChart(containerId, rows, citySetName, yVar, ylabel, titleMetric, data) {
  const refs = data.reference_rates;
  const labels = data.city_set_labels;

  const cityRows = rows.filter(r => r.city_set === citySetName);
  const cityPops = {};
  cityRows.forEach(r => { cityPops[r.BUA22NM] = r.bua_21_pop; });
  const cityOrder = Object.keys(cityPops).sort((a, b) => cityPops[b] - cityPops[a]);

  const traces = cityOrder.map((city, i) => {
    const cr = cityRows.filter(r => r.BUA22NM === city);
    const displayName = city.replace('Cambridge (Cambridge)', 'Cambridge');
    return {
      x: cr.map(r => r.target_rate),
      y: cr.map(r => r.pct_chg_pop),
      customdata: cr.map(r => fmtDelta(r.pop_delta)),
      name: displayName,
      mode: 'lines',
      line: {color: PALETTE[i % PALETTE.length], width: 2},
      hovertemplate: displayName + ': %{y:.2f}% (%{customdata})',
    };
  });

  // Aggregate comparison lines from agg, filtered to this city set
  const aggRows = data.agg.filter(r => r.city_set === citySetName);
  traces.push({
    x: aggRows.map(r => r.target_rate),
    y: aggRows.map(r => r.pct_chg_pop_other_cities),
    customdata: aggRows.map(r => fmtDelta(r.pop_delta_other_cities)),
    name: 'Other (non-affected) cities',
    mode: 'lines',
    line: {color: '#000000', width: 2.5},
    hovertemplate: 'Other cities: %{y:.2f}% (%{customdata})',
  });
  traces.push({
    x: aggRows.map(r => r.target_rate),
    y: aggRows.map(r => r.pct_chg_pop_rural),
    customdata: aggRows.map(r => fmtDelta(r.pop_delta_rural)),
    name: 'Rural areas',
    mode: 'lines',
    line: {color: '#6c757d', width: 2.5, dash: 'dash'},
    hovertemplate: 'Rural areas: %{y:.2f}% (%{customdata})',
  });

  const title = titleMetric + ': ' + (labels[citySetName] || citySetName);
  Plotly.newPlot(containerId, traces, baseLayout(title, ylabel, refs, data.params, data), {responsive: true});
}

// --- Sub-tabs for city-level when "all" ---

function buildSubtabs(containerEl, chartElId, dataKey, yVar, ylabel, titleMetric, data, renderFn) {
  const labels = data.city_set_labels;
  const citySet = data.city_set;
  const render = renderFn || renderCityChart;

  containerEl.innerHTML = '';
  const chartEl = document.getElementById(chartElId);

  if (citySet === 'all') {
    CITY_SET_ORDER.forEach((cs, i) => {
      const btn = document.createElement('button');
      btn.className = 'subtab' + (i === 0 ? ' active' : '');
      btn.textContent = labels[cs];
      btn.dataset.cs = cs;
      btn.addEventListener('click', () => {
        containerEl.querySelectorAll('.subtab').forEach(b => b.classList.remove('active'));
        btn.classList.add('active');
        render(chartElId, data[dataKey], cs, yVar, ylabel, titleMetric, data);
      });
      containerEl.appendChild(btn);
    });
    // Render first city set by default
    render(chartElId, data[dataKey], CITY_SET_ORDER[0], yVar, ylabel, titleMetric, data);
  } else {
    render(chartElId, data[dataKey], citySet, yVar, ylabel, titleMetric, data);
  }
}

// --- Render all charts ---

function renderAll(data) {
  updateRateHelp(data.measure || 'permits');
  renderAggChart('chart-national-income', data, 'pct_chg_national_income_pc',
    '% change in income per capita', 'Change in national income per capita');

  renderAggChart('chart-social-cons', data, 'pct_chg_cons_social',
    '% change in consumption', 'Change in consumption per capita (permitting as transfers)');

  renderAggChart('chart-incumbent-cons', data, 'pct_chg_cons_incumb',
    '% change in consumption', 'Change in consumption per capita (permitting as welfare loss)');

  renderAggChart('chart-newcomer-cons', data, 'pct_chg_newcomer_cons',
    '% change in consumption', 'Change in newcomer and rural consumption');

  buildSubtabs(
    document.getElementById('city-income-subtabs'), 'chart-city-income',
    'city_income', 'pct_chg_y', '% change in income per capita',
    'Change in city income per capita', data
  );

  buildSubtabs(
    document.getElementById('city-cons-subtabs'), 'chart-city-cons',
    'city_cons', 'pct_chg_c', '% change in consumption',
    'Change in incumbent consumption by city', data
  );

  buildSubtabs(
    document.getElementById('city-pop-subtabs'), 'chart-city-pop',
    'city_pop', 'pct_chg_pop', '% change in population',
    'Population change by city', data, renderCityPopChart
  );
}

// ---------------------------------------------------------------------------
// Auto-run on load
// ---------------------------------------------------------------------------
window.addEventListener('DOMContentLoaded', runComputation);
