# Erlang BLE GATT

The `ebus-gatt` library offers facilities for standing up an Erlang BLE GATT server on an embedded Linux system. Onboarding a connected consumer device typically requires sending down a home Wi-Fi SSID and passphrase. Nowadays, users expect to be able to onboard a new device from a mobile app running on their smartphone. Running a BLE GATT server on the target device makes that mobile app style of onboarding possible over Bluetooth.

* [Features](#features)
* [Installation](#installation)
* [Usage](#usage)

## Features

Issue BLE advertisements so smartphones can discover and pair with a device.

* [Advertising](#advertising)

Construct a GATT tree with all of the following types of objects.

* [Applications](#applications)
* [Services](#services)
* [Characteristics](#characteristics)

This GATT tree defines the BLE application layer (signals and slots) that a mobile app uses to communicate with a device.

## Installation

Since `ebus-gatt` is an Erlang/OTP library we recommend that you develop your BLE GATT server with a recent release (OTP 21.1 or later) of Erlang. To install the latest stable Erlang release on Ubuntu or some other Debian-based Linux distro see Erlang Solutions' [instructions](https://www.erlang-solutions.com/resources/download.html) under "Installation using repository".

Add `gatt` to the `deps` section of your `rebar.config`:

```erlang
{deps, [
        {gatt, ".*", {git, "https://github.com/helium/ebus-gatt", {branch, "master"}}}
       ]}
```

Adding `ebus-gatt` to an Erlang/OTP application this way requires that the library be built using rebar3. Since `ebus-gatt` depends on [`ebus`](https://github.com/helium/ebus) that means rebar3 has to build `ebus` as well. Install the following dependencies on Ubuntu 18.04 so rebar3 can build `ebus-gatt`:

```bash
$ sudo apt install make gcc libdbus-1-dev
```

To ensure that `gatt` is started when your application starts also add it to the `applications` section of your application's `.app.src` file. For example:

```erlang
{application, my_app,
 [{description, "An application using gatt"},
  {vsn, "git"},
  {registered, []},
  {applications,
   [kernel,
    stdlib,
    gatt
   ]},
  {env,[]}
 ]}.
```

D-Bus and BlueZ also need to be installed and running on the target device for `ebus-gatt` to work. Since running a GATT server on Linux is still somewhat uncommon make sure you have version 5.45 or later of BlueZ installed:

```bash
$ bluetoothd -v
5.48
```

To restart BlueZ on a Linux distro that uses `systemd` for its init system:

```bash
$ sudo service bluetooth restart
```

## Usage

Design patterns for the various types of GATT tree objects are defined by `ebus-gatt` as OTP behaviors. You construct a concrete GATT server application in Erlang by implementing these behaviors. See [`gateway-config`](https://github.com/helium/gateway-config) for an example of how to structure an Erlang/OTP application based on `ebus-gatt`. There you will find modules that implement the `ble_advertisement`, `gatt_application`, `gatt_service` and `gatt_characteristic` behaviors.

## Supervision

At the root of `gateway_config`'s GATT tree is a `gatt_application` also known as a "profile". An OTP supervisor starts this `gatt_application` and restarts it in the event that it crashes. See [`gateway_config_app.erl`](https://github.com/helium/gateway-config/blob/master/src/gateway_config_app.erl) and [`gateway_config_sup.erl`](https://github.com/helium/gateway-config/blob/master/src/gateway_config_sup.erl) for how to start and configure an OTP supervisor for our `example_gatt_application`:

```erlang
ChildSpecs = [
              #{
                id => example_gatt_application,
                restart => permanent,
                type => supervisor,
                start => {gatt_application_sup, start_link, [example_gatt_application, []]}
               }
             ]
```

## Advertising

Once an `example_ble_advertisement` and `example_gatt_application` are implemented and the `example_gatt_application` is started you can begin advertising so that a nearby mobile phone can discover and pair with it:

```erlang
{ok, Bus} = example_gatt_application:bus(),
{ok, AdvPid} = ble_advertisement:start_link(Bus, example_gatt_application:path(), 0,
                                            example_ble_advertisement, []).
```

Once a connection is established with a generic BLE mobile app like [nRF Connect](https://play.google.com/store/apps/details?id=no.nordicsemi.android.mcp) you can then stop advertising:

```erlang
ble_advertisement:stop(AdvPid, normal).
```

## Applications

A `gatt_application` is a collection of one or more `gatt_service[s]`. The following `example_gatt_application` module only has only one `gatt_service` named `example_gatt_service`:

```erlang
-module(example_gatt_application).

-behavior(gatt_application).

-export([bus/0, adapter_path/0, path/0, init/1]).

-record(state, {}).

init([]) ->
    DeviceInfo = #{
                   manufacturer_name => <<"Acme">>,
                   firmware_revision => <<"1.2.3">>,
                   serial_number => <<"A7654321">>
                  },
    Services = [
                {example_gatt_service, 0, true},
                {gatt_service_device_information, 1, true, [DeviceInfo]}
               ],
    {ok, Services, #state{}}.

bus() ->
    ebus:system().

adapter_path() ->
    %% NOTE: The Bluetooth adapter path for your device may vary
    "/org/bluez/hci0".

path() ->
    "/com/acme/onboard".
```

Note that D-Bus requires a `.conf` file so that other local D-Bus services like [`connmand`](https://wiki.archlinux.org/index.php/ConnMan) can communicate with this GATT server. The `.conf` file in this example should be named `com.acme.Onboard.conf` based on the object `path()` above and placed somewhere like `/etc/dbus-1/system.d` depending on your Linux distro. The contents of `com.acme.Onboard.conf` should be:

```xml
<!DOCTYPE busconfig PUBLIC
 "-//freedesktop//DTD D-BUS Bus Configuration 1.0//EN"
 "http://www.freedesktop.org/standards/dbus/1.0/busconfig.dtd">
<busconfig>
  <policy user="root">
    <allow own="com.acme.Onboard"/>
    <allow send_interface="com.acme.Onboard"/>
  </policy>
  <policy context="default">
     <allow send_destination="com.acme.Onboard"/>
  </policy>
</busconfig>
```

## Services

A `gatt_service` is comprised of of one or more `gatt_characteristics[s]`. The following `example_gatt_service` module has three `gatt_characteristics[s]` named `example_gatt_char_wifi_connect`, `example_gatt_char_wifi_ssid` and `example_gatt_char_wifi_passphrase` respectively:

```erlang
-module(example_gatt_service).
-include("example_gatt.hrl").

-behavior(gatt_service).

-export([init/1, uuid/0, handle_info/2]).

-record(state, {
                connect_result_char=undefined :: undefined | ebus:object_path()
               }).

uuid() ->
    ?UUID_EXAMPLE_GATT_SERVICE.

init(_) ->
    Characteristics =
        [
         {example_gatt_char_wifi_connect, 1, []},
         {example_gatt_char_wifi_ssid, 2, []},
         {example_gatt_char_wifi_passphrase, 3, []}
        ],
    {ok, Characteristics, #state{}}.

handle_info({connect, _Ssid, _Passphrase}=Msg, State=#state{}) ->
    %% TODO: Attempt to connect to given Wi-Fi SSID with given passphrase
    {noreply, State}
```

GATT services and their characteristics must have distinct UUIDs. Define all the UUIDs for a `gatt_service` inside a common Erlang header file like `example_gatt.hrl` for convenience:

```erlang
-define(UUID_EXAMPLE_GATT_SERVICE, "b027ee62-ee7f-4048-b912-b5e49cab4839").
-define(UUID_EXAMPLE_GATT_CHAR_WIFI_CONNECT, "fbc273af-590d-4885-b8e2-9a58adda69bf").
-define(UUID_EXAMPLE_GATT_CHAR_WIFI_SSID, "7f797fe1-9b61-4cc1-80f1-b03add5a0c92").
-define(UUID_EXAMPLE_GATT_CHAR_WIFI_PASSPHRASE, "c0fce586-fc9c-468c-99f1-1713285096e0").
```

OTP behaviors are like inheritance in an object-oriented programming language. If you trace the lineage of `gatt_service` you will find that every `gatt_service` is an `ebus_object` and every `ebus_object` is a `gen_server`. The `gen_server` behavior defines a `handle_info/2` callback to deal with messages sent directly to the `example_gatt_service` with the `!` operator. That includes messages like `self ! {connect, Ssid, Passphrase}` which originate from the `gen_server` itself.

The above `example_gatt_service` includes a stub for an unimplemented `handle_info({connect, Ssid, Passphrase}, State)` callback. To actually connect when that callback is invoked you need some way to pass the incoming `Ssid` and `Passphrase` down to your device's Wi-Fi network interface. Consider adding [`connman`](https://github.com/helium/ebus-connman) to your `rebar.config` if you want to use [`connmand`](https://wiki.archlinux.org/index.php/ConnMan) to negotiate your device's Wi-Fi connection. If your device is running [Nerves](https://github.com/nerves-project) check out [`nerves_network`](https://github.com/nerves-project/nerves_network).

## Characteristics

Each `gatt_service` is comprised of `gatt_characteristic[s]` with optional "descriptors" attached to them. Think of `gatt_characteristic[s]` as value slots that a BLE client can read from, write to and subscribe to. Here is an example of a very simple `gatt_characteristic` that supports reading from and writing to a `utf8_string` value.

```erlang
-module(example_gatt_char_ssid).
-include("example_gatt.hrl").

-behavior(gatt_characteristic).

-export([init/2,
         uuid/1,
         flags/1,
         read_value/2,
         write_value/2]).

-record(state, {path :: ebus:object_path()}).

uuid(_) ->
    ?UUID_EXAMPLE_GATT_CHAR_WIFI_SSID.

flags(_) ->
    [read, write].

init(Path, _) ->
    Descriptors =
        [
         {gatt_descriptor_cud, 0, ["Wi-Fi SSID"]},
         {gatt_descriptor_pf, 1, [utf8_string]}
        ],
    {ok, Descriptors, #state{path=Path}}.

read_value(State=#state{}, _) ->
    {ok, State#state.value, State}.

write_value(State=#state{}, Bin) ->
    {ok, State#state{value=Bin}}.
```

Sometimes you don't want an unknown BLE client to be able to read the value of a `gatt_characteristic`. A cleartext Wi-Fi passphrase characteristic should be write-only for example. You can define the `flags(_)` for that characteristic as `[read]` instead of `[read, write]` to protect it from hackers.

There is also a `notify` flag to enable push notifications of value changes to connected BLE clients. This is useful for characteristics that represent status changes like `offline -> connecting -> online`. The [nRF Connect](https://play.google.com/store/apps/details?id=no.nordicsemi.android.mcp) mobile app has support for these push notifications and is a great way to explore GATT services and peek and poke at GATT characteristics. 
