#  geo_server

A web server to provide worldwide town/city name lookup functionality.  The server accepts a text string of 3 or more characters as a search criteria and returns all towns and cities with a matching name from anywhere in the world.

This server is written in [Erlang](http://www.erlang.org/) using the [OTP framework](http://erlang.org/doc/) and the [Cowboy web server](https://ninenines.eu/)

All geographic information supplied by this server is obtained from [http://www.geonames.org](http://www.geonames.org).

## Clone Git Repository

Clone this Git repository onto your local machine

    $ git clone https://github.wdf.sap.corp/I003638/geo_server.git

Then edit the `manifest.yml` file and change the `route` parameter to point to your own Cloud Foundry server.


##  Deploy to Cloud Foundry

Deploy to Cloud Foundry using the following community build pack for Erlang:

    $ cf push -b https://github.com/ChrisWhealy/cf-buildpack-erlang

***IMPORTANT***  
For performance reasons, all the geographic information supplied by this server is held in memory; therefore, this app requires 2048Mb of memory.  The `geo-server` will probably not fully start if this memory allowance is reduced.

Once all the country servers have fully started, the memory consumption drops to around 700Mb; however, if all the country servers are started simultaneously, nearly the full 2Gb of memory could be required.

##  Server Startup

### Country Manager

When this app is deployed to CF and started, the `country_manager` process starts.  This is the supervisor responsible for managing the lifecycle of all country servers.  One country server is created for each country listed in the GeoNames file [countryInfo.txt](http://download.geonames.org/export/dump/countryInfo.txt).

After deployment to Cloud Foundry, by default, none of the country servers are started.

### Admin Interface

In order to start one or more country servers, you must use the `geo-server` admin interface.  This is accessed by adding `/server_info` to `geo-server`'s deployed URL.  For instance <https://geo-server.cfapps.us10.hana.ondemand.com/server_info>

Currently, no authentication is required to access this page.

If you do not start any country servers, all search queries will return an empty JSON array!

### Country Server Startup

Once connected to the admin screen, you can either start all the servers at once by clicking on the "Start all servers" button, or individual country servers can be started as required.

#### Startup Processing

The following startup sequence is performed for each country server:

1. When a country server is started for the first time, a [ZIP file](http://download.geonames.org/export/dump/) containing all the geographic information for that country is downloaded from the GeoNames website.

1. Once unzipped, the resulting text file contains a large amount of geographic information - much of which is not relevant for `geo-server`'s requirements.  Therefore, the text file is scanned only for those records having feature class values of "P" (population centres) or "A" (administrative areas).  All other records are ignored.

    * A further restriction is imposed here that feature class "P" records must refer to towns or cities having a population greater than some arbitrary limit (currently set to 500).

    * An internal file is created (`FCP.txt`) that contains the records from each feature class "P" record supplemented with the region information from the relevant feature class "A" records.  This data then becomes the searchable list of towns/cities for that particular country.

    * Each time a country server is started, the existence of the `FCP.txt` file is checked.  If it exists and is not stale, then the country server will start using the information in `FCP.txt` rather than downloading the country's ZIP file again.  Starting a country server from its `FCP.txt` file greatly reduces the start up time from a few minutes down to a few seconds.

1. The eTag for each downloaded country ZIP file is also stored.

    * If the country server is restarted more than 24 hours after the eTag data was downloaded, then the local country data is considered potentially stale.  The eTag value is now used to check if a new version of the country file exists.  If it does, the new ZIP file is downloaded and a new `FCP.txt` file created.


1. If you start a country server and find that it immediately stops with a substatus of `no_cities`, then this simply means the country contains no towns or cities with a population greater than the population limit (currently set to 500)



##  API

In order to perform a search, a client must send an HTTP `GET` request to the `geo-server` hostname with the path `/search`:

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

The client will then receive a JSON array containing zero or more instances of a city object.  An example city object is shown below:

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

Each city object contains the following properties:

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

