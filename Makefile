PROJECT = geo_server
PROJECT_DESCRIPTION = Geographic information server
PROJECT_VERSION = 0.1.0

ERLC_OPTS = -Ddebug

debug = true

DEPS = cowboy ibrowse
dep_cowboy_commit = 2.2.2
DEP_PLUGINS = cowboy

include erlang.mk

