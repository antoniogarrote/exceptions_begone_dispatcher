-define(CONFIG_DATABASE,"es_exceptions_agents_config_database") .
-define(CONFIG_POOL, config_pool) .

-record(exceptions_buffer, {name, database, mails, exceptions, inmediate}).
