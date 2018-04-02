
-define(GEONAMES_HOST, "download.geonames.org").
-define(GEONAMES_URL,  "http://" ++ ?GEONAMES_HOST ++ "/export/dump/").
-define(TARGET_DIR,    "../../priv/").

-define(COUNTRY_FILE_FULL(CC), ?TARGET_DIR ++ CC ++ "/" ++ CC ++ ".txt").
-define(COUNTRY_FILE_ETAG(CC), ?TARGET_DIR ++ CC ++ "/" ++ CC ++ ".etag").

-define(COUNTRY_FILE_FCA(CC),  ?TARGET_DIR ++ CC ++ "/" ++ CC ++ "_fca.txt").
-define(COUNTRY_FILE_FCP(CC),  ?TARGET_DIR ++ CC ++ "/" ++ CC ++ "_fcp.txt").
