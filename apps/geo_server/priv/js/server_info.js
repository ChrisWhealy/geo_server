/**
 * =====================================================================================================================
 * Fetch the server status information
 * 
 * Author : Chris Whealy    chris.whealy@sap.com
 * =====================================================================================================================
 **/

var serverTable = []

const url_country_manager_cmd = "/country_manager_cmd"
const url_country_server_cmd  = "/country_server_cmd"
const url_server_status       = "/server_status"

/* =====================================================================================================================
 * XHR requests
 */

// ---------------------------------------------------------------------------------------------------------------------
// Server status command
const fetch_server_info = () => {
  var xhr = new XMLHttpRequest()

  xhr.open("GET", url_server_status, true)

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
// Send commands to the country manager
// ---------------------------------------------------------------------------------------------------------------------
const startAllServers = () => sendCountryManagerCmd("start_all")
const stopAllServers  = () => sendCountryManagerCmd("shutdown_all")

// ---------------------------------------------------------------------------------------------------------------------
// Toggle country manager debug status
// ---------------------------------------------------------------------------------------------------------------------
const toggleCountryManagerDebug = state => sendCountryManagerCmd("set_debug", state)
const toggleNetworkTrace        = state => sendCountryManagerCmd("network_trace", state)

const toggleCountryServerDebug = (cc, state) => sendCountryServerCmd(cc, (state ? "trace_on" : "trace_off"))

const sendCountryManagerCmd = (cmd, param) => {
  var xhr = new XMLHttpRequest()

  if (param === undefined)
    xhr.open("GET", url_country_manager_cmd + "?cmd=" + cmd, true)
  else
    xhr.open("GET", url_country_manager_cmd + "?cmd=" + cmd + "&param=" + param, true)

  xhr.onload = evt => {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        console.log(xhr.responseText)
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
// Send commands to individual country servers
// ---------------------------------------------------------------------------------------------------------------------
const startServer = cc => sendCountryServerCmd(cc,"start")
const stopServer  = cc => sendCountryServerCmd(cc,"shutdown")

const sendCountryServerCmd = (cc,cmd) => {
  var xhr = new XMLHttpRequest()

  xhr.open("GET", url_country_server_cmd + "?country_code=" + cc + "&cmd=" + cmd, true)

  xhr.onload = evt => {
    if (xhr.readyState === 4) {
      if (xhr.status === 200) {
        var cmd_response = JSON.parse(xhr.responseText)

        if (cmd_response.status && cmd_response.status === "error") {
          alert(cmd_response.from_server + " " +
                cmd_response.cmd + " " +
                cmd_response.status + " " +
                cmd_response.reason)
        }
        else {
          if (cmd_response.cmd === "start" ||
              cmd_response.cmd === "shutdown") {
            var newRec = cmd_response.reason
            var oldRec = document.getElementById(newRec.name)

            oldRec.innerHTML = build_table_columns(newRec).map(htmlGenElement).join('\n')
          }
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

/* =====================================================================================================================
 * Display results from server
 */
const show_status = responseText => {
  var serverObj    = JSON.parse(responseText)
  var serverStatus = document.getElementById("server_status")

  var cmDiv   = build_cm_trace_checkbox(serverObj.country_manager_trace)
  var netDiv  = build_net_trace_checkbox(serverObj.network_trace)
  var memDiv  = build_memory_usage(serverObj.erlang_memory_usage)

  var cmdBtnsDiv = htmlElement("div", null, build_cmd_buttons())

  document.getElementById("server_status").innerHTML =
    htmlGenElement(cmDiv) +
    htmlGenElement(netDiv) +
    htmlGenElement(memDiv) +
    htmlGenElement(cmdBtnsDiv)

  // Is the server table sorted by ZIP file size or by country name within continent?
  serverTable = (serverObj.servers[0].country_code === undefined)
    ? build_table_by_continent(serverObj.servers)
    : build_table_by_size(serverObj.servers)

  document.getElementById("country_servers").innerHTML = htmlGenElement(serverTable)
}

const show_error = statusText => alert(statusText)

// ---------------------------------------------------------------------------------------------------------------------
// Create HTML elements for the server report
// ---------------------------------------------------------------------------------------------------------------------
const build_cm_trace_checkbox = state => {
  var id = "country_manager_trace"

  var cbParams = [
      htmlParam("name", id)
    , htmlParam("id",   id) 
    , htmlParam("type", "checkbox") 
    , htmlParam("onclick", "toggleCountryManagerDebug(this.checked)") 
  ]

  if (state === "true") cbParams.push(htmlParam("checked"))

  var traceCB    = htmlElement('input', cbParams)
  var traceLabel = htmlElement('label', htmlParam("htmlFor", id), "Country manager debug trace")

  return htmlElement("div",null,[traceLabel, traceCB])
}

const build_net_trace_checkbox = state => {
  var id = "network_trace"

  var cbParams = [
      htmlParam("name", id)
    , htmlParam("id",   id) 
    , htmlParam("type", "checkbox") 
    , htmlParam("onclick", "toggleNetworkTrace(this.checked)") 
  ]

  if (state === "true") cbParams.push(htmlParam("checked"))

  var traceCB    = htmlElement('input', cbParams)
  var traceLabel = htmlElement('label', htmlParam("htmlFor", id), "Network level trace")

  return htmlElement("div",null,[traceLabel, traceCB])
}

const build_trace_checkbox = (cc, state) => {
  var id = "country_server_" + cc + "_trace"

  var cbParams = [
      htmlParam("name", id)
    , htmlParam("id",   id) 
    , htmlParam("type", "checkbox") 
    , htmlParam("onclick", "toggleCountryServerDebug('" + cc + "',this.checked)") 
  ]

  if (state === "true") cbParams.push(htmlParam("checked"))

  return htmlElement('input', cbParams)
}

const build_memory_usage = mem => htmlElement('p', null, "Erlang runtime memory usage = " + mem)

const build_cmd_buttons = () => {
  var startBtn = htmlElement('button',
                             [htmlParam("onclick","alert('Not implemented yet')")],
                             "Start all servers")
  var stopBtn  = htmlElement('button',
                             [htmlParam('onclick','stopAllServers()')],
                             "Stop all servers")

  return [startBtn, stopBtn]
}

// ---------------------------------------------------------------------------------------------------------------------
// Create HTML elements for server status table
// ---------------------------------------------------------------------------------------------------------------------
const build_table_by_continent = servers => { }

const build_table_by_size = servers => {
  var table_hdr  = build_column_headers()
  var table_body = servers.map(build_table_row)

  return htmlElement("table", null, [table_hdr].concat(table_body))
}

const build_table_row = country => {
  return htmlElement("tr",
                     [htmlParam("id","country_server_" + country.country_code.toLowerCase())],
                     build_table_columns(country))
}

const build_table_columns = country => 
  [ htmlElement("td", [TD_ALIGN("center")], build_action_button(country.substatus, country.country_code))
  , htmlElement("td", [TD_ALIGN("center")], build_trace_checkbox(country.country_code, country.trace))
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

// ---------------------------------------------------------------------------------------------------------------------
const build_action_button = (substatus, cc) => {
  var btn = htmlElement('button', [])

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
const build_continent_header = ContName => htmlElement('tr', null, htmlElement('th', htmlParam('colspan', '12'), ContName))

// ---------------------------------------------------------------------------------------------------------------------
const build_column_headers = () =>
  htmlElement('tr', null, [ htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Action')
                          , htmlElement('th', [TH_TEXT_COLOUR("white"), BG_BLUE()], 'Trace')
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
                          ])

// ---------------------------------------------------------------------------------------------------------------------
var status_colour = (status, substatus) =>
  (status === "started")
   ? (substatus === "running")
      ? BG_GREEN()
      : BG_YELLOW()
   : (status === "starting")
      ? BG_YELLOW()
      : (status === "crashed")
         ? BG_RED()
         : BG_LIGHT_GREY()

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
