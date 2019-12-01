---
title: Installing OpenWrt on the Mikrotik hap-ac
author: Collin J. Doering
date: 2019-12-01
description: Use dnsmasq to provide a dhcp+tftp server in order to netboot OpenWrt on Mikrotik hap-ac routers
tags: general, hardware, networking, dhcp, tftp, OpenWrt, mikrotik
---

Recently I purchased a [Mikrotik hap-ac
router](https://mikrotik.com/product/RB962UiGS-5HacT2HnT) as I am in the process of moving away
from the unify access points I currently use in my home network. The main reasons for this
is the numerous [GPL
violations](https://sfconservancy.org/blog/2019/oct/02/cambium-ubiquiti-gpl-violations/), the
introduction of a [call home feature](https://news.ycombinator.com/item?id=21430997) without
telling users, as well as the lack of deeper control that only comes with open source software.
In any case, the end goal is to leverage [Openwisp](http://openwisp.org/) as the wireless
access point controller, and OpenWrt powered access points everywhere in the house. One nice
thing about using OpenWrt is that my older [Unify
AC-LR](https://store.ui.com/products/unifi-ac-lr), also [supports
it](https://openwrt.org/toh/ubiquiti/unifiac), so I can use a similar process to the one I will
describe in this article to leverage my older equipment in the case I do not sell it right
away.

This article will detail how to install OpenWrt on the Mikrotik hap-ac router, but will save
setup of the access points as well as Openwisp to another article.

<!--more-->

Though there is [a guide provided within the OpenWrt
wiki](https://openwrt.org/toh/mikrotik/common) regarding installation onto Mikrotik (and other)
devices, I found that it was not as clear as it could be, mainly due the many options
mentioned, but none clearly demonstrated. Here I would like provide clear and concise
instructions on how to install OpenWrt on the Mikrotik hap-ac, though the process could be used
for any netboot installation of OpenWrt (with some modification).

The high-level process is quiet simple, and the OpenWrt does do a good job of describing this
in its wiki. Here is the TLDR:

1. Ensure router/device will utilize netboot upon start. This will vary per router/device.
2. Run local `dhcp` server and `tftp` server, ensuring the appropriate firmware file is
   referenced as `dhcp` `option 66`.
3. Ensure router/device is plugged into computer running the `dhcp+tftp` server.
4. Reboot the router and keep an eye on the `dhcp`/`tftp` server logs to confirm the router
   gets an ip address from the local dhcp server, as well as downloads the firmware binary
   referenced by `dhcp option 66`.

Now luckily [dnsmasq](http://www.thekelleys.org.uk/dnsmasq/doc.html) provides both a `dhcp` and
`tftp` server, which is easy to configure and use. First, ensure `dnsmasq` is installed, and
place the following in `/etc/dnsmasq.conf`.

```
port=0
interface=enp0s20f0u2u4
bind-interfaces
dhcp-range=192.168.0.11,192.168.0.150,12h
dhcp-boot=openwrt-18.06.5-ar71xx-mikrotik-rb-nor-flash-16M-ac-initramfs-kernel.bin
dhcp-option-force=66,192.168.0.10
enable-tftp
tftp-root=/srv/tftp
```

Make sure to change the `interface` to match which one you are using to connect to the router.
Additionally, create a folder to store `tftp` files and download the appropriate firmware binaries
to this directory, referencing them correctly in the `dnsmasq` configuration `dhcp-boot` and
`tftp-root`


```shell
mkdir /srv/tftp
chown dnsmasq:dnsmasq /srv/tftp
```

Finally, we need to start the `dnsmasq` service, power cycle the router and watch the log
output from the `dnsmasq` service.

Assuming you are using `systemd`, you would do this like so:

```shell
sudo systemctl start dnsmasq
sudo journalctl -fu dnsmasq
```

Below is a sample of the log output I received when netbooting my Mikrotik hap-ac:

```
Nov 20 21:02:04 rekahsoft-work dnsmasq[23837]: dnsmasq: syntax check OK.
Nov 20 21:02:04 rekahsoft-work systemd[1]: Started A lightweight DHCP and caching DNS server.
Nov 20 21:02:04 rekahsoft-work dnsmasq[23839]: started, version 2.80 DNS disabled
Nov 20 21:02:04 rekahsoft-work dnsmasq[23839]: compile time options: IPv6 GNU-getopt DBus i18n IDN2 DHCP DHCPv6 no-Lua TFTP conntrack ipset auth DNSSEC loop-detect inotify dumpfile
Nov 20 21:02:04 rekahsoft-work dnsmasq[23839]: DBus support enabled: connected to system bus
Nov 20 21:02:04 rekahsoft-work dnsmasq-dhcp[23839]: DHCP, IP range 192.168.0.11 -- 192.168.0.150, lease time 12h
Nov 20 21:02:04 rekahsoft-work dnsmasq-dhcp[23839]: DHCP, sockets bound exclusively to interface enp0s20f0u2u4
Nov 20 21:02:04 rekahsoft-work dnsmasq-tftp[23839]: TFTP root is /srv/tftp
Nov 20 21:02:17 rekahsoft-work dnsmasq-dhcp[23839]: DHCPDISCOVER(enp0s20f0u2u4) 74:4d:28:f0:40:28
Nov 20 21:02:17 rekahsoft-work dnsmasq-dhcp[23839]: DHCPOFFER(enp0s20f0u2u4) 192.168.0.108 74:4d:28:f0:40:28
Nov 20 21:02:17 rekahsoft-work dnsmasq-dhcp[23839]: DHCPREQUEST(enp0s20f0u2u4) 192.168.0.108 74:4d:28:f0:40:28
Nov 20 21:02:17 rekahsoft-work dnsmasq-dhcp[23839]: DHCPACK(enp0s20f0u2u4) 192.168.0.108 74:4d:28:f0:40:28
Nov 20 21:02:19 rekahsoft-work dnsmasq-tftp[23839]: sent /srv/tftp/openwrt-18.06.5-ar71xx-mikrotik-rb-nor-flash-16M-ac-initramfs-kernel.bin to 192.168.0.108
Nov 20 21:02:42 rekahsoft-work dnsmasq-dhcp[23839]: DHCPDISCOVER(enp0s20f0u2u4) 74:4d:28:f0:40:28
Nov 20 21:02:42 rekahsoft-work dnsmasq-dhcp[23839]: DHCPOFFER(enp0s20f0u2u4) 192.168.0.108 74:4d:28:f0:40:28
Nov 20 21:02:42 rekahsoft-work dnsmasq-dhcp[23839]: DHCPREQUEST(enp0s20f0u2u4) 192.168.0.108 74:4d:28:f0:40:28
Nov 20 21:02:42 rekahsoft-work dnsmasq-dhcp[23839]: DHCPACK(enp0s20f0u2u4) 192.168.0.108 74:4d:28:f0:40:28
```

You'll notice that first the router receives an IP address via `DHCP` followed by the
appropriate firmware binary being sent to the device. At this point, the router will be running
OpenWrt, though completely ephemerally until it is permanently installed (either via the web
interface or via ssh). I was unable to do the installation via the web interface as when net
booting, device board auto-recognition did not work correctly (with a message saying
"Sysupgrade is not yet supported on unknown."), which required me dropping the appropriate
board version into `/some/path/to/the/board/file/openwrt` (see [this form
post](https://forum.openwrt.org/t/mikrotik-rb952ui-5ac2nd/33635)). Since I was already there, I
proceeded to complete the installation via command-line, though it would have been just as easy
to go back to the web ui.

There we have it! OpenWrt installed onto a Mikrotik hap-ac router. Easy peasy, thanks to `dnsmasq`!
