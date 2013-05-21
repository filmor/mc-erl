-module(mc_erl_entities).

-export([init/0]).

-include("records.hrl").

init() ->
        {atomic, ok} = mnesia:create_table(entity,
                                           [{attributes, record_info(fields, entity)},
                                            {index, [name]}]),
        ok.
