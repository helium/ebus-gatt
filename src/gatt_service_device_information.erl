-module(gatt_service_device_information).

-behavior(gatt_service).

-export([init/1, uuid/0]).

uuid() ->
    "180A".

init([Info]) ->
    Keys = [
            {manufacturer_name, "2A29"},
            {model_number, "2A24"},
            {serial_number, "2A25"},
            {hardware_revision, "2A27"},
            {firmware_revision, "2A26"}
           ],
    Chars = lists:foldl(fun({{Key, UUID}, Index}, Acc) ->
                                case maps:get(Key, Info, false) of
                                    false -> Acc;
                                    Value ->
                                        Char = {gatt_characteristic_string,
                                                Index, [{uuid, UUID}, {value, Value}]},
                                        [Char | Acc]
                                end
                        end, [], lists:zip(Keys, lists:seq(0, length(Keys) - 1))),
    {ok, Chars, []}.
