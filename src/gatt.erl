-module(gatt).

-type uuid() :: string().
-type characteristic() :: gatt_characteristic:characteristic().
-type characteristic_spec() :: gatt_characteristic:spec().
-type descriptor() :: gatt_descriptor:descriptor().
-type descriptor_spec() :: gatt_descriptor:spec().
-type service() :: gatt_service:service().
-type service_spec() :: gatt_service:spec().
-type advertisement() :: ble_advertisement:advertisement().

-export_type([uuid/0,
              descriptor/0, descriptor_spec/0,
              characteristic/0, characteristic_spec/0,
              service/0, service_spec/0,
              advertisement/0]).

-export([start_application/1, start_application/2]).

start_application(Module) ->
    start_application(Module, []).

start_application(Module, Args) ->
    gatt_sup:start_application(Module, Args).
