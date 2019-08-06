# Erlang BLE GATT

The `ebus-gatt` library offers facilities for standing up an Erlang BLE GATT server on an embedded Linux system. Onboarding a connected consumer device typically requires sending down a home Wi-Fi SSID and passphrase. Nowadays, users expect to be able to onboard a new device from a mobile app running on their smartphone. Running a BLE GATT server on the target device makes that mobile app style of onboarding possible over Bluetooth.

* [Features](#features)
* [Installation](#installation)
* [Usage](#usage)

## Features

Issue BLE advertisements so smartphones can discover and pair with a device.

* Register a BLE advertisement.
* Start advertising over BLE.
* Stop advertising over BLE.

Construct a GATT tree with all of the following types of objects.

* Applications (also known as Profiles)
* Services
* Characteristics
* Descriptors

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

D-Bus and BlueZ also need to be installed and running on the target device for `ebus-gatt` to work. Since running a GATT server on Linux is still considered somewhat unusual make sure you have version 5.45 or later of BlueZ installed:

```bash
$ bluetoothd -v
5.48
```

To restart BlueZ on a Linux distro that uses `systemd` for its init system:

```bash
$ sudo service bluetooth restart
```

## Usage

Design patterns for the various types of GATT tree objects are defined by `ebus-gatt` as OTP behaviours. You construct a concrete GATT server application in Erlang by implementing these behaviours. See [`gateway-config`](https://github.com/helium/gateway-config) for an example of how to structure an Erlang/OTP application based on `ebus-gatt`. There you will find modules that implement the `ble_advertisement`, `gatt_application`, `gatt_service` and `gatt_characteristic` behaviours.

## Start Profile

At the root of `gateway_config`'s GATT tree is a `gatt_application` also known as a "profile". An OTP supervisor starts this `gatt_application` and restarts it in the event that it crashes. See `gateway-config/src/gateway_config_app.erl` and `gateway-config/src/gateway_config_sup.erl` for how to start and configure an OTP supervisor for an `example_gatt_application` like the following:

```erlang
ChildSpecs = [
              #{
                id => gateway_gatt_application,
                restart => permanent,
                type => supervisor,
                start => {gatt_application_sup, start_link, [example_gatt_application, []]}
               }
             ]
```

## Register Advertisement and Start Advertising

Once your `example_gatt_application` is started you can begin advertising so that a nearby mobile phone can discover and pair with it:

```erlang
{ok, Bus} = example_gatt_application:bus(),
{ok, AdvPid} = ble_advertisement:start_link(Bus, example_gatt_application:path(), 0,
                                            gateway_ble_advertisement, []).
```

## Stop Advertising

Once a BLE connection is established with a generic BLE mobile app like [nRF Connect](https://play.google.com/store/apps/details?id=no.nordicsemi.android.mcp) you can then stop advertising:

```erlang
ble_advertisement:stop(AdvPid, normal).
```

## Writable Characteristics

A `gatt_application` is a collection of one or more `gatt_service[s]`. Each `gatt_service` is comprised of `gatt_characteristic[s]` which can have "descriptors" attached to them.
