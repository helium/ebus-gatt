-define(BLUEZ_SERVICE, "org.bluez").

-define(GATT_MANAGER_IFACE, "org.bluez.GattManager1").
-define(GATT_MANAGER(M), ?GATT_MANAGER_IFACE ++ "." ++ M).

-define(GATT_CHARACTERISTIC_IFACE, "org.bluez.GattCharacteristic1").
-define(GATT_DESCRIPTOR_IFACE, "org.bluez.GattDescriptor1").
-define(GATT_SERVICE_IFACE, "org.bluez.GattService1").
