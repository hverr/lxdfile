lxdfile
=======

A Dockerfile-like file format for LXD containers.

Manage LXD containers using a Dockerfile-like file format, with support to store metadata.

Installing
----------

Download the latest release, or use

```sh
stack build
stack exec lxdfile -- -h
```

Usage
-----

Example lxdfiles can be found in [lxdfile-examples](https://github.com/hverr/lxdfile-examples).

Using lxdfiles to power LXD containers involves two steps:

  1. Build the reusable image using `lxdfile build`
  2. Launch a container using a prebuilt image using `lxdfile launch`

### Building images

Take for example the [gogs example](https://github.com/hverr/lxdfile-examples/tree/master/gogs).

Running `lxdfile build gogs` will create an image in your local LXD image repository

```
$ lxc image list local:
+-------------------------+--------------+--------+---------------------------------------+--------+----------+------------------------------+
|          ALIAS          | FINGERPRINT  | PUBLIC |              DESCRIPTION              |  ARCH  |   SIZE   |         UPLOAD DATE          |
+-------------------------+--------------+--------+---------------------------------------+--------+----------+------------------------------+
| gogs                    | 6199407c377b | no     | Gogs - Go Git Service                 | x86_64 | 177.56MB | Nov 14, 2016 at 1:56pm (UTC) |
+-------------------------+--------------+--------+---------------------------------------+--------+----------+------------------------------+
| ubuntu/yakkety (3 more) | 16b6d53f6dd1 | no     | Ubuntu yakkety amd64 (20161114_03:49) | x86_64 | 80.20MB  | Nov 14, 2016 at 1:44pm (UTC) |
+-------------------------+--------------+--------+---------------------------------------+--------+----------+------------------------------+
```

### Launching containers
You can launch containers using `lxdfile launch`, e.g. `lxdfile launch gogs gogs`.

To configure launched containers, you can use init scripts.

A possible use of init scripts is to configure the network. You can pass multiple init scripts to `lxdfile launch` using the `-i` flag. They will be executed in the order they are passed on the command line.

```
$ cat configure-network.lxdfile
RUN echo "iface lan inet dhcp" > /etc/network/interfaces
RUN chmod 0600 /etc/network/interfaces

$ lxdfile launch gogs gogs -i configure-network
```

These configuration scripts can be container-specific or reusable for multiple images. They should be included in source control.

Metadata
--------
Container and image metadata is stored in `/etc/lxdfile`. It contains JSON representations of the used lxdfile and the used init scripts.
