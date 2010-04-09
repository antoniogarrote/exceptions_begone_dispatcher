-define(CONFIG_DATABASE,"es_exceptions_agents_config_database") .
-define(EXCEPTIONS_DATABASE,"es_exceptions_buffers_database") .
-define(CONFIG_POOL, config_pool) .
-define(EXCEPTIONS_POOL, exceptions_pool) .
-define(BUFFERS_COLLECTION, "es_buffer_dbs").

-record(exceptions_buffer, {name, database, mails, exceptions, inmediate}).
