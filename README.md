#  geo_server

A web server application that accepts a city name as a search criteria, and returns all cities with a matching name.

This server is written in [Erlang](http://www.erlang.org/) using the [OTP framework](http://erlang.org/doc/) and the [Cowboy web server](https://ninenines.eu/)

All geographic information used by this server is obtained from [http://www.geonames.org](http://www.geonames.org).

## Clone Git Repository

Clone this Git repository onto your local machine

    $ git clone https://github.wdf.sap.corp/I003638/geo_server.git

Then edit the `manifest.yml` file and change the `route` parameter to point to your own Cloud Foundry server.


##  Deploy to Cloud Foundry

***IMPORTANT***  
For performance reasons, all the geographic information supplied by this server is held in memory; therefore, this app requires 2048Mb of memory.  The `geo-server` will probably not fully start if this memory allowance is reduced.

Deploy to Cloud Foundry using the following community build pack for Erlang:

    $ cf push -b https://github.com/ChrisWhealy/cf-buildpack-erlang

##  Server Startup

### Country Manager

When this app is deployed to CF and started, the `country_manager` process starts.  This is the supervisor responsible for managing the lifecycle of all country servers.  One country server is available for each country listed in the GeoNames file [countryInfo.txt](http://download.geonames.org/export/dump/countryInfo.txt).

By default, none of the country servers are started automatically because doing so would create a memory usage spike that exceeds the current Cloud Foundry instance memory limit of 2Gb.  Therefore, country servers must be started manually, one at a time, starting with the largest.

### Admin Interface

In order to start one or more country servers, you must use the `geo-server` admin interface.  This is accessed by adding `/server_info` to the deployed URL.  Currently, no authentication is required to access this page.

If you do not start any country servers, all search queries to the `geo-server` will return an empty JSON array!

### Country Server Startup Sequence

The country servers should be started one at a time, in descending order of ZIP file size.  This is due to the fact that currently, unzipping a file causes a memory usage spike - the larger the ZIP file, the larger the spike.

#### Startup Order

All country servers can be started - except for the United States! This ZIP file is currently too large to unzip within the current 2Gb instance limit.  When this limit is raised to 4Gb, this issue should no longer be a problem.

Apart from the United States, start each country server starting from the largest.

Once, you've pressed the start button next to a country, wait for the substatus to change from `country_file_download` to `file_import` (you must refresh the browser screen to see the substatus change).  Once this happens, you can then start the next country server.

For countries whose ZIP files are larger than about 3Mb, it is safer to start the country servers one at a time.  For country servers with smaller ZIP files, you can start three or four at a time.

For the smallest country servers, you can start twenty or so at one time.

### Country Server Startup Processing

When an individual country server is started, the following sequence of events takes place:

* The country's [ZIP file](http://download.geonames.org/export/dump/) is downloaded from the GeoNames website and unzipped as a plain text file.
* This text file contains a large amount of geographic information - not all of which is relevant for this particular server.  Therefore, the text file is scanned only for those records having a feature class of "P" (population centres) and "A" (administrative areas).
    * A further restriction is imposed here that feature class "P" records must refer to towns or cities having a population greater than some arbitrary limit (currently set to 500).
* An internal file is created (`FCP.txt`) that contains the records from each feature class "p" record supplemented with the region information from the relevant feature class "A" records.
* The eTag for each downloaded country ZIP file is also stored.
    * If the country server is restarted more than 24 hours after the eTag data was downloaded, then the local country data is considered potentially stale.  The eTag value is now used to check if a new version of the country file exists.  If it does, the new ZIP file is downloaded and a new `FCP.txt` file is created.
    * Each time a country server is started, the existence of the `FCP.txt` file is checked.  If it exists and is not stale, then the country server will start using the information in `FCP.txt` rather than downloading the country's ZIP file again.  This greatly reduces the country server's start up time.
* If you start a country server and find that it immediately stops with a substatus of `no_cities`, this is simply because no cities exist in that country with a population greater than the population limit (currently set to 500)



##  API

A client must send an HTTP `GET` request to the `geo-server` hostname with the path `search`:

`geo-server.cfapps.<server>.hana.ondemand.com/search`

Followed by a query string containing the following three parameters

    search_term :: URL encoded string
    starts_with :: [true | false]
    whole_word  :: [true | false]

For example, to search for all cities containing the string "york" somewhere in the name, the URL would be:

`<hostname>/search?search_term=york&starts_with=false&whole_word=false`

Similarly, to search for all cities starting with the whole word "london", the URL would be:

`<hostname>/search?search_term=london&starts_with=true&whole_word=true`

##  Response

The client will receive a JSON array containing zero or more instances of a city object.  An example city object is shown below:

    {
      "name": "London",
      "lat": 51.50853,
      "lng": -0.12574,
      "featureClass": "P",
      "featureCode": "PPLC",
      "countryCode": "GB",
      "admin1Txt": "England",
      "admin2Txt": "Greater London",
      "admin3Txt": "null",
      "admin4Txt": "null",
      "timezone": "Europe/London"
    }

###  City Object

Each city object returned by the server contains the following properties:

| Property | Description |
|---|---|
|  `name` | The name of the city/town |
| `lat` | Latitude |
| `lng` | Longitude |
| `featureClass` | The GeoName feature class (See [http://www.geonames.org/export/codes.html](http://www.geonames.org/export/codes.html) for details) |
| `featureCode` | The GeoName feature code (See [http://www.geonames.org/export/codes.html](http://www.geonames.org/export/codes.html) for details) |
| `admin1Txt` | The name of the top level administrative region to which this town/city belongs |
| `admin2Txt` | The name of the 2nd level administrative region to which this town/city belongs |
| `admin3Txt` | The name of the 3rd level administrative region to which this town/city belongs |
| `admin4Txt` | The name of the 4th level administrative region to which this town/city belongs |
| `timeZone` | The name of the timezone in which this town/city is located |

## Server Performance

For each country listed in the [GeoNames countryInfo.txt](http://download.geonames.org/export/dump/countryInfo.txt) file a corresponding country server can be started.

Each country server then reads its `FCP.txt` file.  This file contains a list of all the cities in that country.  This list is then divided up amongst one or more city servers according to the first character of the town/city's name.  So there will be a city server for all cities in that country starting with `a`, one for `b`, and one for `c` etc.

Therefore, setting the `starts_with` query string parameter to `true` will return a result set much faster because each country server knows it need only send the query to the city server dedicated to handling towns/cities starting with the first letter of the `search_term`.

