%%%----------------------------------------------------------------
%%% @author Antonio Garrote Hernandez <antoniogarrote@gmail.com>
%%% @doc
%%%
%%% @end
%%% @copyright 2010 Antonio Garrote Hernandez
%%%----------------------------------------------------------------,
-module(es_json) .

-author("Antonio Garrote Hernandez") .

-include_lib("eunit/include/eunit.hrl").

-export([decode_json/1, is_exception/1, get_identifier/1, is_valid/1, is_notification/1, sample_json/0,
         parse_identifier/1, process_json_message/1, sanitize_json/1]) .


%% Public API

%% @doc
%% Tries to decode an string encoded JSON data and transform it into a set of structs
%% using mochiweb JSON support
decode_json(Data) when is_binary(Data) ->
    decode_json(binary_to_list(Data)) ;
decode_json(Data) ->
    mochijson2:decode(sanitize_json(Data)) .

%% @doc
%% Cleans problematic chars in some Json string
sanitize_json(Json) ->
    sanitize_json_acum(Json, []) .

sanitize_json_acum([0|T],Acum) ->
    sanitize_json_acum(T,Acum) ;
sanitize_json_acum([255|T], Acum) ->
    sanitize_json_acum(T, Acum) ;
sanitize_json_acum([],Acum) ->
    lists:reverse(Acum) ;
sanitize_json_acum([H|T], Acum) ->
    sanitize_json_acum(T, [H|Acum]) .

%% @doc
%% It checks if some decoded JSON matches the Exceptions Begone exception format.
is_exception({struct, [{<<"notification">>, {struct, Json}}]}) ->
    case proplists:get_value(<<"identifier">>, Json) of
        undefined   -> false ;
        _Identifier -> case proplists:get_value(<<"payload">>, Json) of
                           undefined -> false ;
                           _Payload  -> true
                       end
    end .

%% @doc
%% It checks if some decoded JSON matches the Exceptions Begone notification format
is_notification({struct, [{<<"notification">>, {struct, Json}}]}) ->
    case proplists:get_value(<<"category">>, Json) of
        undefined -> false ;
        _Data     -> true
    end .

%% @doc
%% Checks if the provided JSON data is in some format valid for Exceptions Begone
is_valid({struct, [{<<"notification">>, {struct, _Json}}]} = Data) ->
    IsException = is_exception(Data),
    IsNotification = is_notification(Data),
    IsException or IsNotification ;
is_valid(_Something) ->
    false .


%% @doc
%% Returns the :identifier for a Exceptions Begone exception JSON object
get_identifier({struct, [{<<"notification">>, {struct, Json}}]}) ->
    proplists:get_value(<<"identifier">>, Json) .


%% @doc
%% Returns the category of a notification
get_category({struct, [{<<"notification">>, {struct, Json}}]}) ->
    proplists:get_value(<<"category">>, Json) .


%% @doc
%% Parses and identifier for an exception with format:
%% controller#method (Category ...
%% Returning:
%% {binary, binary}
%% with {controller, action}
parse_identifier(Id)  ->
    parse_identifier2(binary_to_list(Id),[]) .

parse_identifier2([$# | T], C) ->
    parse_identifier3(T,C,[]) ;
parse_identifier2([H | T], C) ->
    parse_identifier2(T,[H |C]) ;
parse_identifier2([], C) ->
    {list_to_binary(lists:reverse(C)),list_to_binary([])} .

parse_identifier3([$  | _T], C, M) ->
    {list_to_binary(lists:reverse(C)),list_to_binary(lists:reverse(M))} ;
parse_identifier3([], C, M) ->
    {list_to_binary(lists:reverse(C)), list_to_binary(lists:reverse(M))} ;
parse_identifier3([H | T], C, M) ->
    parse_identifier3(T,C,[H|M]) .


%% @doc
%% Parses a json string, returning one of:
%% {exception, controllery/binary, action/binary, data/binary}
%% {notification, category/binary, data/binary}
%% unknown
process_json_message(JsonStr) ->
    Json = decode_json(JsonStr),
    case is_exception(Json) of
        true ->
            {C,M} = parse_identifier(get_identifier(Json)),
            {exception,C,M,JsonStr} ;
        false ->
            case is_notification(Json) of
                true  -> {notification, get_category(Json), JsonStr} ;
                false -> unknown
            end
    end .



%%
%% Tests
%%


sample_json() ->
    "{\"notification\":{\"payload\":{\"url\":\"http://www.example.com\",\"parameters\":\"\",\"ip\":\"127.0.0.1\",\"request_environment\":{\"MANPATH\":\"/usr/share/man:/usr/local/share/man:/Library/TeX/Distributions/.DefaultTeX/Contents/Man:/usr/X11/man\",\"EMACS\":\"t\",\"GIT_INDEX_FILE\":\"/Users/antonio.garrote/Development/exceptions_begone_notifier/.git/index\",\"GIT_WORK_TREE\":\"/Users/antonio.garrote/Development/exceptions_begone_notifier\",\"R_SHARE_DIR\":\"/Library/Frameworks/R.framework/Resources/share\",\"CLASSPATH\":\":/Users/antonio.garrote/Development/clj-gearman-actors/lib\",\"GIT_DIR\":\"/Users/antonio.garrote/Development/exceptions_begone_notifier/.git\",\"TMPDIR\":\"/var/folders/DD/DDaRMMq+FeSlV2oYXCkDyHntbK2/-Tmp-/\",\"Apple_PubSub_Socket_Render\":\"/tmp/launch-ISsygw/Render\",\"rvm_gem_path\":\"/Users/antonio.garrote/.rvm/gems\",\"USER\":\"antonio.garrote\",\"EMACSLOADPATH\":\"/Applications/Aquamacs Emacs 1.9.app/Contents/Resources/site-lisp:/Applications/Aquamacs Emacs 1.9.app/Contents/Resources/lisp:/Applications/Aquamacs Emacs 1.9.app/Contents/Resources/leim\",\"IMAGEMAGICK_PATH\":\"/usr/local/bin\",\"rvm_log_path\":\"/Users/antonio.garrote/.rvm/log\",\"TERM\":\"dumb\",\"rvm_config_path\":\"/Users/antonio.garrote/.rvm/config\",\"R_HOME\":\"/Library/Frameworks/R.framework/Resources\",\"R_DOC_DIR\":\"/Library/Frameworks/R.framework/Resources/doc\",\"GEM_HOME\":\"/Users/antonio.garrote/Development/invites-project/gems\",\"__CF_USER_TEXT_ENCODING\":\"0x3CF99D61:0:0\",\"EDITOR\":\"emacs\",\"rvm_src_path\":\"/Users/antonio.garrote/.rvm/src\",\"rvm_version\":\"0.0.84\",\"SHLVL\":\"3\",\"JAVACMD\":\"/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/bin/java\",\"SECURITYSESSIONID\":\"b9b120\",\"SHELL\":\"bash\",\"LC_ALL\":\"en_US.UTF-8\",\"COMMAND_MODE\":\"unix2003\",\"TERMCAP\":\"\",\"rvm_temp_path\":\"/Users/antonio.garrote/.rvm/tmp\",\"LOGNAME\":\"antonio.garrote\",\"DISPLAY\":\"/tmp/launch-32KP6t/:0\",\"rvm_scripts_path\":\"/Users/antonio.garrote/.rvm/scripts\",\"EMACSPATH\":\"/Applications/Aquamacs Emacs 1.9.app/Contents/MacOS/libexec:/Applications/Aquamacs Emacs 1.9.app/Contents/MacOS/bin\",\"LD_LIBRARY_PATH\":\"/Library/Frameworks/R.framework/Resources/lib:/Library/Frameworks/R.framework/Resources/bin:\",\"rvm_path\":\"/Users/antonio.garrote/.rvm\",\"PATH\":\"/usr/local/ruby187pl202patched/bin:/usr/local/bin:/opt/local/bin:/opt/local/sbin:/bin:/sbin:/texbin:/Users/antonio.garrote/Development/invites/vendor/plugins/gc_hacks/bin:/Users/antonio.garrote/Development/invites/vendor/xing_production_log_analyzer/bin:/Users/antonio.garrote/Development/invites/gems/bin:/Users/antonio.garrote/Development/invites/bin:/usr/local/ruby187pl202patched/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/texbin:/usr/X11/bin:/opt/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/texbin:/usr/X11/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/texbin:/usr/local/texlive/2008/bin:/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Commands:/opt/local/share/java/groovy/bin:/usr/share/maven/bin\",\"rvm_loaded_flag\":\"1\",\"PWD\":\"/Users/antonio.garrote/Development/exceptions_begone_notifier\",\"LANG\":\"en_US.UTF-8\",\"SSH_AUTH_SOCK\":\"/tmp/launch-kgRsI0/Listeners\",\"JAVA_HOME\":\"/System/Library/Frameworks/JavaVM.framework/Versions/1.6\",\"GEM_PATH\":\"/Users/antonio.garrote/Development/invites-project/gems\",\"INSIDE_EMACS\":\"22.3.1,comint\",\"EMACSDOC\":\"/Applications/Aquamacs Emacs 1.9.app/Contents/Resources/etc\",\"R_INCLUDE_DIR\":\"/Library/Frameworks/R.framework/Resources/include\",\"COLUMNS\":\"238\",\"_\":\"/usr/local/ruby187pl202patched/bin/ruby\",\"HOME\":\"/Users/antonio.garrote\",\"GROOVY_HOME\":\"/opt/local/share/java/groovy\",\"rvm_archives_path\":\"/Users/antonio.garrote/.rvm/archives\",\"rvm_bin_path\":\"/Users/antonio.garrote/.rvm/bin\",\"EMACSDATA\":\"/Applications/Aquamacs Emacs 1.9.app/Contents/Resources/etc\",\"MAVEN_HOME\":\"/usr/share/maven\"},\"environment\":{\"MANPATH\":\"/usr/share/man:/usr/local/share/man:/Library/TeX/Distributions/.DefaultTeX/Contents/Man:/usr/X11/man\",\"EMACS\":\"t\",\"GIT_INDEX_FILE\":\"/Users/antonio.garrote/Development/exceptions_begone_notifier/.git/index\",\"GIT_WORK_TREE\":\"/Users/antonio.garrote/Development/exceptions_begone_notifier\",\"R_SHARE_DIR\":\"/Library/Frameworks/R.framework/Resources/share\",\"CLASSPATH\":\":/Users/antonio.garrote/Development/clj-gearman-actors/lib\",\"GIT_DIR\":\"/Users/antonio.garrote/Development/exceptions_begone_notifier/.git\",\"TMPDIR\":\"/var/folders/DD/DDaRMMq+FeSlV2oYXCkDyHntbK2/-Tmp-/\",\"Apple_PubSub_Socket_Render\":\"/tmp/launch-ISsygw/Render\",\"rvm_gem_path\":\"/Users/antonio.garrote/.rvm/gems\",\"USER\":\"antonio.garrote\",\"EMACSLOADPATH\":\"/Applications/Aquamacs Emacs 1.9.app/Contents/Resources/site-lisp:/Applications/Aquamacs Emacs 1.9.app/Contents/Resources/lisp:/Applications/Aquamacs Emacs 1.9.app/Contents/Resources/leim\",\"IMAGEMAGICK_PATH\":\"/usr/local/bin\",\"rvm_log_path\":\"/Users/antonio.garrote/.rvm/log\",\"TERM\":\"dumb\",\"rvm_config_path\":\"/Users/antonio.garrote/.rvm/config\",\"R_HOME\":\"/Library/Frameworks/R.framework/Resources\",\"R_DOC_DIR\":\"/Library/Frameworks/R.framework/Resources/doc\",\"GEM_HOME\":\"/Users/antonio.garrote/Development/invites-project/gems\",\"__CF_USER_TEXT_ENCODING\":\"0x3CF99D61:0:0\",\"EDITOR\":\"emacs\",\"rvm_src_path\":\"/Users/antonio.garrote/.rvm/src\",\"rvm_version\":\"0.0.84\",\"SHLVL\":\"3\",\"JAVACMD\":\"/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/bin/java\",\"SECURITYSESSIONID\":\"b9b120\",\"SHELL\":\"bash\",\"LC_ALL\":\"en_
US.UTF-8\",\"COMMAND_MODE\":\"unix2003\",\"TERMCAP\":\"\",\"rvm_temp_path\":\"/Users/antonio.garrote/.rvm/tmp\",\"LOGNAME\":\"antonio.garrote\",\"DISPLAY\":\"/tmp/launch-32KP6t/:0\",\"rvm_scripts_path\":\"/Users/antonio.garrote/.rvm/scripts\",\"EMACSPATH\":\"/Applications/Aquamacs Emacs 1.9.app/Contents/MacOS/libexec:/Applications/Aquamacs Emacs 1.9.app/Contents/MacOS/bin\",\"LD_LIBRARY_PATH\":\"/Library/Frameworks/R.framework/Resources/lib:/Library/Frameworks/R.framework/Resources/bin:\",\"rvm_path\":\"/Users/antonio.garrote/.rvm\",\"PATH\":\"/usr/local/ruby187pl202patched/bin:/usr/local/bin:/opt/local/bin:/opt/local/sbin:/bin:/sbin:/texbin:/Users/antonio.garrote/Development/invites/vendor/plugins/gc_hacks/bin:/Users/antonio.garrote/Development/invites/vendor/xing_production_log_analyzer/bin:/Users/antonio.garrote/Development/invites/gems/bin:/Users/antonio.garrote/Development/invites/bin:/usr/local/ruby187pl202patched/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/texbin:/usr/X11/bin:/opt/local/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/texbin:/usr/X11/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/texbin:/usr/local/texlive/2008/bin:/System/Library/Frameworks/JavaVM.framework/Versions/1.6/Commands:/opt/local/share/java/groovy/bin:/usr/share/maven/bin\",\"rvm_loaded_flag\":\"1\",\"PWD\":\"/Users/antonio.garrote/Development/exceptions_begone_notifier\",\"LANG\":\"en_US.UTF-8\",\"SSH_AUTH_SOCK\":\"/tmp/launch-kgRsI0/Listeners\",\"JAVA_HOME\":\"/System/Library/Frameworks/JavaVM.framework/Versions/1.6\",\"GEM_PATH\":\"/Users/antonio.garrote/Development/invites-project/gems\",\"INSIDE_EMACS\":\"22.3.1,comint\",\"EMACSDOC\":\"/Applications/Aquamacs Emacs 1.9.app/Contents/Resources/etc\",\"R_INCLUDE_DIR\":\"/Library/Frameworks/R.framework/Resources/include\",\"COLUMNS\":\"238\",\"_\":\"/usr/local/ruby187pl202patched/bin/ruby\",\"HOME\":\"/Users/antonio.garrote\",\"GROOVY_HOME\":\"/opt/local/share/java/groovy\",\"rvm_archives_path\":\"/Users/antonio.garrote/.rvm/archives\",\"rvm_bin_path\":\"/Users/antonio.garrote/.rvm/bin\",\"EMACSDATA\":\"/Applications/Aquamacs Emacs 1.9.app/Contents/Resources/etc\",\"MAVEN_HOME\":\"/usr/share/maven\"},\"backtrace\":\"\",\"session\":\"\"},\"identifier\":\"base# (Exception) \\\"some exception\\\"\",\"category\":\"exception\"}}" .

valid_parsing_test() ->
    Json = decode_json(sample_json()),
    ?assertEqual(true, is_exception(Json)),
    ?assertEqual(true, is_valid(Json)),
    ?assertEqual(true, is_notification(Json)) .

valid_get_identifier_test() ->
    Json = decode_json(sample_json()),
    ?assertEqual(<<"base# (Exception) \"some exception\"">>, get_identifier(Json)).

valid_parsing_id_test() ->
    Id = <<"base#hola (Exception) \"some exception\"">>,
    {C,M} = parse_identifier(Id),
    ?assertEqual(C,<<"base">>),
    ?assertEqual(M,<<"hola">>) .

sanitize_json_test() ->
    BadJsonStr = binary_to_list(<<0,91,91,34,98,97,115,101,34,44,34,99,111,114,101,34,93,93,255>>),
    JsonStr = sanitize_json(BadJsonStr),
    Json = decode_json(JsonStr),
    ?assertEqual([[<<"base">>,<<"core">>]], Json) .
