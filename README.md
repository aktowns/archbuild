# archbuild

Rewrite of my shell-script based arch repository build system.   

This is mostly used to build my local package repos for my systems at home  

Uses pacstrap to build a rootfs then systemd-nspawn with ephemeral instances to 
make the packages. 

WIP

## Setup

```sh 
stack build
stack exec sudo archbuilder-exe
```
