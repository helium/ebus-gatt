-define(BLUEZ_SERVICE, "org.bluez").
-define(BLE_ADVERTISEMENT_IFACE, "org.bluez.LEAdvertisement1").

-define(GATT_ADAPTER_IFACE, "org.bluez.Adapter1").
-define(GATT_ADAPTER(M), ?GATT_ADAPTER_IFACE ++ "." ++ M).

-define(GATT_MANAGER_IFACE, "org.bluez.GattManager1").
-define(GATT_MANAGER(M), ?GATT_MANAGER_IFACE ++ "." ++ M).

-define(GATT_CHARACTERISTIC_IFACE, "org.bluez.GattCharacteristic1").
-define(GATT_CHARACTERISTIC(M), ?GATT_CHARACTERISTIC_IFACE ++ "." ++ M).

-define(GATT_DESCRIPTOR_IFACE, "org.bluez.GattDescriptor1").
-define(GATT_DESCRIPTOR(M), ?GATT_DESCRIPTOR_IFACE ++ "." ++ M).

-define(GATT_SERVICE_IFACE, "org.bluez.GattService1").

-define(GATT_ADVERTISING_MANAGER_IFACE, "org.bluez.LEAdvertisingManager1").
-define(GATT_ADVERTISING_MANAGER(M), ?GATT_ADVERTISING_MANAGER_IFACE ++ "." ++ M).

-define(GATT_ERROR_NOT_SUPPORTED, "org.bluez.Error.NotSupported").
-define(GATT_ERROR_NOT_AUTHORIZED, "org.bluez.Error.NotAuthorized").
-define(GATT_ERROR_FAILED, "org.bluez.Error.Failed").
