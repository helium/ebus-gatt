# ebus-gatt

The `ebus-gatt` library offers facilities for standing up a BLE GATT server on an embedded Linux system with Erlang/OTP and BlueZ already on it. Onboarding a connected consumer device typically requires sending down a home Wi-Fi SSID and passphrase. Nowadays, users expect to be able to onboard a new device from a mobile app running on their smartphone. Running a BLE GATT server on the target device makes that mobile app style of onboarding possible over Bluetooth.

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

## Usage
