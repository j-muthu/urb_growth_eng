// ---------------------------------------------------------------------------
// State
// ---------------------------------------------------------------------------
let currentData = null;

const CENTRAL = {gamma: 0.05, theta: 0.07, sigma: 0.025, beta: 0.025, lambda: 0.072};

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
  const display = document.getElementById(p + '-val');
  slider.addEventListener('input', () => {
    display.textContent = parseFloat(slider.value).toFixed(3);
  });
});

document.getElementById('reset-btn').addEventListener('click', () => {
  PARAMS.forEach(p => {
    const slider = document.getElementById(p);
    slider.value = CENTRAL[p];
    document.getElementById(p + '-val').textContent = CENTRAL[p].toFixed(3);
  });
});

// ---------------------------------------------------------------------------
// Tabs
// ---------------------------------------------------------------------------
document.querySelectorAll('.tab').forEach(btn => {
  btn.addEventListener('click', () => {
    document.querySelectorAll('.tab').forEach(b => b.classList.remove('active'));
    document.querySelectorAll('.tab-content').forEach(c => c.classList.remove('active'));
    btn.classList.add('active');
    document.getElementById(btn.dataset.tab).classList.add('active');

    // Re-render visible chart so Plotly sizes correctly
    if (currentData) renderAll(currentData);
  });
});

// ---------------------------------------------------------------------------
// Run button
// ---------------------------------------------------------------------------
document.getElementById('run-btn').addEventListener('click', runComputation);

async function runComputation() {
  const params = {};
  PARAMS.forEach(p => { params[p] = parseFloat(document.getElementById(p).value); });
  params.city_set = document.getElementById('city-set').value;

  document.getElementById('loading').style.display = 'flex';

  try {
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
function paramCaption(params) {
  return `\u03B3=${params.gamma.toFixed(2)}, \u03B8=${params.theta.toFixed(2)}, ` +
         `\u03C3=${params.sigma.toFixed(3)}, \u03B2=${params.beta.toFixed(3)}, ` +
         `\u03BB=${params.lambda.toFixed(3)}`;
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

function baseLayout(title, ylabel, refRates, params) {
  return {
    title: {text: title + '<br><span style="font-size:11px;color:gray">' + paramCaption(params) + '</span>', font: {color: '#0a4938'}},
    xaxis: {
      title: {text: 'Counterfactual permitting rate', font: {color: '#888'}},
      dtick: 0.02,
      tickformat: '.2f',
      tickfont: {color: '#888'},
      gridcolor: '#e8e4d4',
    },
    yaxis: {
      title: {text: ylabel, font: {color: '#888'}},
      tickfont: {color: '#888'},
      gridcolor: '#e8e4d4',
    },
    hovermode: 'x unified',
    plot_bgcolor: '#fdf9e8',
    paper_bgcolor: '#fdf9e8',
    shapes: [
      {type:'line', x0:0, x1:1, xref:'paper', y0:0, y1:0, line:{color:'#aaa',width:1}},
      ...refShapes(refRates),
    ],
    annotations: refAnnotations(refRates),
    legend: {orientation: 'v', x: 1.02, y: 1, font: {color: '#888'}},
    margin: {t: 80, r: 160},
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

  Plotly.newPlot(containerId, traces, baseLayout(title, ylabel, refs, data.params), {responsive: true});
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
  Plotly.newPlot(containerId, traces, baseLayout(title, ylabel, refs, data.params), {responsive: true});
}

// --- Sub-tabs for city-level when "all" ---

function buildSubtabs(containerEl, chartElId, dataKey, yVar, ylabel, titleMetric, data) {
  const labels = data.city_set_labels;
  const citySet = data.city_set;

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
        renderCityChart(chartElId, data[dataKey], cs, yVar, ylabel, titleMetric, data);
      });
      containerEl.appendChild(btn);
    });
    // Render first city set by default
    renderCityChart(chartElId, data[dataKey], CITY_SET_ORDER[0], yVar, ylabel, titleMetric, data);
  } else {
    renderCityChart(chartElId, data[dataKey], citySet, yVar, ylabel, titleMetric, data);
  }
}

// --- Render all charts ---

function renderAll(data) {
  renderAggChart('chart-national-income', data, 'pct_chg_national_income_pc',
    '% change in income per capita', 'Change in national income per capita');

  renderAggChart('chart-social-cons', data, 'pct_chg_cons_social',
    '% change in consumption', 'Change in social planner consumption per capita');

  renderAggChart('chart-incumbent-cons', data, 'pct_chg_cons_incumb',
    '% change in consumption', 'Change in incumbent-weighted consumption per capita');

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
}

// ---------------------------------------------------------------------------
// Auto-run on load
// ---------------------------------------------------------------------------
window.addEventListener('DOMContentLoaded', runComputation);
