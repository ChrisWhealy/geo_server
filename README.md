#  geo_server

An Erlang OTP application that accepts a city name as a search criteria, and returns all cities with a matching name.

All geographic information used by this server is obtained from [http://www.geonames.org](http://www.geonames.org).


##  Deploy to Cloud Foundry

Before deploying this app to Cloud Foundry, edit the `manifest.yml` file and change the `route` parameter to point to your own Cloud Foundry server.

***IMPORTANT***  
Due to the fact that all the country data is held in memory, this app requires 1600Mb of memory.  This app will probably not start if the memory allowance is reduced.

Deploy to Cloud Foundry using the following community build pack for Erlang:

    $ cf push -b https://github.com/ChrisWhealy/cf-buildpack-erlang

##  API

A client must send an HTTP `GET` request to the app's hostname with the path `search`:

`geo_server.cfapps.<server>.hana.ondemand.com/search`

Followed by a query string containing the following three parameters

    search_term :: URL encoded string
    starts_with :: [true | false]
    whole_word  :: [true | false]

For example, to search for all cities containing the string "york" somewhere in the name, the URL would be:

`<hostname>/search?search_term=york&starts_with=false&whole_word=false`

Similarly, to search for all cities starting with the whole word "london", the URL would be:

`<hostname>/search?search_term=york&starts_with=true&whole_word=true`

##  Response

The client receives a JSON array containing zero or more instances of a city object.  The example shown here is for Greater London:

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

##   Server Behaviour

This server only returns GeoName records having feature classes set to `A` (Administrative centres) or `P` (Population Centres).  Also, this server only returns feature class `P` records (I.E. towns or cities) having a population greater than 500.

## Server Performance

Within the server, information about towns and cities is organised alphabetically; therefore, setting the `starts_with` query string parameter set to `true` will return a result much faster because the server knows that it only need search for towns/cities starting with the first letter of the `search_term`.  In this case, towns and cities starting with other letters are simply ignored.
