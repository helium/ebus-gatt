# ebus-gatt

The `ebus-gatt` library offers facilities for standing up an Erlang BLE GATT server on an embedded Linux system. Onboarding a connected consumer device typically requires sending down a home Wi-Fi SSID and passphrase. Nowadays, users expect to be able to onboard a new device from a mobile app running on their smartphone. Running a BLE GATT server on the target device makes that mobile app style of onboarding possible over Bluetooth.

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

Add `ebus-gatt` to the `deps` section of your `rebar.config`:

```erlang
{deps, [
        {gatt, ".*", {git, "https://github.com/helium/ebus-gatt", {branch, "master"}}}
       ]}
```

Adding `ebus-gatt` to an Erlang/OTP application this way requires that the library be built using rebar3. Since `ebus-gatt` depends on [`ebus`](https://github.com/helium/ebus) that means rebar3 has to build `ebus` as well. Install the following dependencies on Ubuntu 18.04 so rebar3 can build `ebus-gatt`:

```bash
$ sudo apt install make gcc libdbus-1-dev
```

D-Bus and BlueZ also need to be installed and running on the target device for `ebus-gatt` to work. Since running a GATT server on Linux is considered somewhat unusual make sure you have version 5.45 or later of BlueZ installed:

```bash
$ bluetoothd -v
5.48
```

To restart BlueZ on a Linux distro that uses `systemd` for its init system:

```bash
$ sudo service bluetooth restart
```
