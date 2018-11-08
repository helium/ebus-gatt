-module(gatt_object).

-type properties() :: map().
-export_type([properties/0]).


-callback uuid(State::any()) -> gatt:uuid().
-callback properties(State::any()) -> properties().
