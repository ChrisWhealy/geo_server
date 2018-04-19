/**
 * =====================================================================================================================
 * Fetch the server status information
 * 
 * Author : Chris Whealy    chris.whealy@sap.com
 * =====================================================================================================================
 **/

var serverTable = []

/* =====================================================================================================================
 * XHR requests
 */

// ---------------------------------------------------------------------------------------------------------------------
// Server status command
function fetch_server_info() {
  var xhr = new XMLHttpRequest()

  xhr.open("GET", "/server_status", true)

  xhr.onload = evt => {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        show_status(xhr.responseText)
      }
      else {
        show_error(xhr.statusText)
      }
    }
  }

  xhr.onerror = evt => show_error(xhr.statusText)

  xhr.send();
}

// ---------------------------------------------------------------------------------------------------------------------
// Toggle country manage debug status
// ---------------------------------------------------------------------------------------------------------------------
function toggleCountryManagerDebug(state) { sendCountryManagerCmd("set_debug", state) }

function sendCountryManagerCmd(cmd, param) {
  var xhr = new XMLHttpRequest()

  if (param === undefined)
    xhr.open("GET", "/country_manager_cmd?cmd=" + cmd, true)
  else
    xhr.open("GET", "/country_manager_cmd?cmd=" + cmd + "&param=" + param, true)

  xhr.onload = evt => {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        if (cmd === "set_debug") {
        // alert(xhr.responseText)
      }
      }
      else {
        show_error(xhr.statusText)
      }
    }
  }

  xhr.onerror = evt => show_error(xhr.statusText)

  xhr.send();
}

// ---------------------------------------------------------------------------------------------------------------------
// Start/stop country servers
// ---------------------------------------------------------------------------------------------------------------------
function startServer(cc) { sendServerCmd(cc,"start") }
function stopServer(cc)  { sendServerCmd(cc,"shutdown") }

function sendServerCmd(cc,cmd) {
  var xhr = new XMLHttpRequest()

  xhr.open("GET", "/server_cmd?country_code=" + cc + "&cmd=" + cmd, true)

  xhr.onload = evt => {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        var newRec = JSON.parse(xhr.responseText)
        var oldRec = document.getElementById(newRec.name)

        oldRec.innerHTML = build_table_columns(newRec).map(htmlGenElement).join('\n')
      }
      else {
        show_error(xhr.statusText)
      }
    }
  }

  xhr.onerror = evt => show_error(xhr.statusText)

  xhr.send();
}

/* =====================================================================================================================
 * Display results from server
 */
function show_status(responseText) {
  var serverObj    = JSON.parse(responseText)
  var serverStatus = document.getElementById("server_status")

  var cbDiv  = build_trace_checkbox(serverObj.country_manager_trace)
  var memDiv = build_memory_usage(serverObj.erlang_memory_usage)

  document.getElementById("server_status").innerHTML = htmlGenElement(cbDiv) + htmlGenElement(memDiv)

  // Is the server table sorted by ZIP file size or by country name within continent?
  serverTable = (serverObj.servers[0].country_code === undefined)
    ? build_table_by_continent(serverObj.servers)
    : build_table_by_size(serverObj.servers)

  document.getElementById("country_servers").innerHTML = htmlGenElement(serverTable)
}

function show_error(statusText) {
  alert(statusText)
}

/* =====================================================================================================================
 * Create HTML elements for the server report
 */
function build_trace_checkbox(state) {
  var id = "country_manager_trace"

  var toggleState = (state === "true") ? "false" : "true"

  var cbParams = [
      htmlParam("name", id)
    , htmlParam("id",   id) 
    , htmlParam("type", "checkbox") 
    , htmlParam("onclick", "toggleCountryManagerDebug('" + toggleState + "')") 
  ]

  if (state === "true") cbParams.push(htmlParam("checked"))

  var traceCB    = htmlElement('input', cbParams)
  var traceLabel = htmlElement('label', htmlParam("htmlFor", id), "Country manager debug trace")

  return htmlElement("div",null,[traceLabel, traceCB])
}

function build_memory_usage(mem) {
  return htmlElement('p', null, "Erlang server memory usage = " + mem)
}

function build_table_by_continent(servers) {
  
}

function build_table_by_size(servers) {
  var table_hdr  = build_column_headers()
  var table_body = servers.map(build_table_row)

  return htmlElement("table", null, [table_hdr].concat(table_body))
}

function build_table_row(country) {
  return htmlElement("tr",
                     [htmlParam("id","country_server_" + country.country_code.toLowerCase())],
                     build_table_columns(country))
}

function build_table_columns(country) {
  return [
      htmlElement("td", [TD_ALIGN("center")], build_action_button(country.substatus, country.country_code))
    , htmlElement("td", [TD_ALIGN("center"), status_colour(country.status, country.substatus)], country.country_code)
    , htmlElement("td", [status_colour(country.status, country.substatus)], country.country_name)
    , htmlElement("td", [TD_ALIGN("center"), status_colour(country.status, country.substatus)], country.status)
    , htmlElement("td", [TD_ALIGN("center"), status_colour(country.status, country.substatus)], country.substatus)
    , htmlElement("td", [TD_ALIGN("right")], country.progress)
    , htmlElement("td", [TD_ALIGN("right")], country.city_count)
    , htmlElement("td", [TD_ALIGN("right")], country.children)
    , htmlElement("td", [TD_ALIGN("right")], country.started_at)
    , htmlElement("td", [TD_ALIGN("right")], country.start_complete)
    , htmlElement("td", [TD_ALIGN("right")], country.mem_usage)
    , htmlElement("td", [TD_ALIGN("right")], country.zip_size)
  ]
}

// ---------------------------------------------------------------------------------------------------------------------
function build_action_button(substatus, cc) {
  var btn = htmlElement('button', [htmlParam('type','button')])

  if (substatus === "running") {
    btn.content = "Stop"
    btn.params.push(htmlParam('onclick','stopServer(\'' + cc + '\')'))
  }
  else {
    btn.content = "Start"
    btn.params.push(htmlParam('onclick','startServer(\'' + cc + '\')'))
  }

  return btn
}

// ---------------------------------------------------------------------------------------------------------------------
function build_continent_header(ContName) {
  return htmlElement('tr', null, htmlElement('th', htmlParam('colspan', '12'), ContName))
}

// ---------------------------------------------------------------------------------------------------------------------
function build_column_headers() {
  var reportColumns = [
    htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Action')
  , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'ISO')
  , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Country')
  , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Status')
  , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Substatus')
  , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Progress')
  , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'City Count')
  , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'City Servers')
  , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Started At')
  , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Startup Time')
  , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Memory Usage')
  , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'ZIP File Size')
  ]

  return htmlElement('tr', null, reportColumns)
}

// ---------------------------------------------------------------------------------------------------------------------
function status_colour(status, substatus) {
  return (status === "started")
         ? (substatus === "running")
           ? BG_GREEN()
           : BG_YELLOW()
         : (status === "starting")
           ? BG_YELLOW()
           : (status === "crashed")
             ? BG_RED()
             : BG_LIGHT_GREY()
}

/* =====================================================================================================================
 * HTML element functions
 */
const emptyElements = ['area',  'base',    'basefont', 'br',   'col',   'frame',   'hr',       'img',
                       'input', 'isindex', 'link',     'meta', 'param', 'command', 'keygen',   'source']
const isEmpty = tagName => emptyElements.indexOf(tagName) >= 0

const htmlElement = (tagName, params, content) => {
  return {
    tagName : tagName,
    params  : params,
    content : content,
    isEmpty : isEmpty(tagName)
  }
}

const htmlParam = (n, v) => { return {name: n, value: v} }

/* =====================================================================================================================
 * HTML generation functions
 */
const htmlGenTagParam  = param  => param.name + (isNullOrUndef(param.value) ? '' : '="' + param.value + '"')
const htmlGenTagParams = params => (isArray(params)) ? (' ' + aggregateParams(params).map(htmlGenTagParam).join(' ')) : ''

const aggregateParams = params => {
  var retVal = []
  
  if (params.length > 0) {
    var paramMap = new Map()
    paramMap.set(params[0].name, params[0].value)

    for (var i=1; i<params.length; i++) {
      var thisVal = paramMap.get(params[i].name)
      paramMap.set(params[i].name, (thisVal === undefined ? "" : thisVal + " ") + params[i].value)
    }

    paramMap.forEach((v, k) => retVal.push(htmlParam(k,v)))
  }

  return retVal
}

/**
 * Generate an HTML element from a tagName, an array of parameter objects and some optional content
 */
const htmlGenElement = el => {
  var html = ""

  // Does the element have anything in it?
  if (!isNullOrUndef(el)) {
    // Is the element a simple string?
    if (typeof el === "string" || typeof el === "number") {
      html = el
    }
    // We assume then that the content is either a single HTML element instance, or an array of HTML element objects
    else {
      // Add the element's start tag
      html += '<' + el.tagName + htmlGenTagParams(el.params) + '>'

      // If we've got just a single object, then wrap it in an array
      if (!isArray(el.content)) {
        el.content = [el.content]
      }

      // Generate the element's content then add an optional closing tag
      html += el.content.map(htmlGenElement).join('\n') + ((el.isEmpty) ? '\n' : '</' + el.tagName + '>\n')
    }
  }

  return html
}

/* =====================================================================================================================
 * Utility functions
 */
const isArray       = obj => !!obj && obj.constructor === Array
const isNullOrUndef = obj => obj === null || obj === undefined || obj === "null" || obj === "undefined"

const BG_RED        = () => htmlParam("style", "background-color: #EE4466;")
const BG_GREEN      = () => htmlParam("style", "background-color: #90EE90;")
const BG_BLUE       = () => htmlParam("style", "background-color: #1478DB;")
const BG_YELLOW     = () => htmlParam("style", "background-color: #FFFF00;")
const BG_LIGHT_GREY = () => htmlParam("style", "background-color: #EEEEEE;")

const TD_ALIGN = dir => htmlParam("style", "text-align: " + dir + ";")

const TH_TEXT_COLOUR = col => htmlParam("style", "color: " + col + ";")
