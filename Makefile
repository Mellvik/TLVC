
ifndef TOPDIR
$(error TOPDIR is not defined; did you mean to run './build.sh' instead?)
endif

include $(TOPDIR)/Make.defs

.PHONY: all clean libc kconfig defconfig config menuconfig

all: .config include/autoconf.h
	$(MAKE) -C libc all
	$(MAKE) -C libc DESTDIR='$(TOPDIR)/cross' install
	$(MAKE) -C tlvc all
	$(MAKE) -C bootblocks all
	$(MAKE) -C tlvccmd all
	$(MAKE) -C image all

image:
	$(MAKE) -C image

images:
	$(MAKE) -C image images

kimage: kernel image

kernel:
	$(MAKE) -C tlvc

kclean:
	$(MAKE) -C tlvc kclean

clean:
	$(MAKE) -C libc clean
	$(MAKE) -C libc DESTDIR='$(TOPDIR)/cross' uninstall
	$(MAKE) -C tlvc clean
	$(MAKE) -C bootblocks clean
	$(MAKE) -C tlvccmd clean
	$(MAKE) -C image clean
	@echo
	@if [ ! -f .config ]; then \
	    echo ' * This system is not configured. You need to run' ;\
	    echo ' * `make config` or `make menuconfig` to configure it.' ;\
	    echo ;\
	fi

libc:
	$(MAKE) -C libc DESTDIR='$(TOPDIR)/cross' uninstall
	$(MAKE) -C libc all
	$(MAKE) -C libc DESTDIR='$(TOPDIR)/cross' install

tlvc/arch/i86/drivers/char/KeyMaps/config.in:
	$(MAKE) -C tlvc/arch/i86/drivers/char/KeyMaps config.in

kconfig:
	$(MAKE) -C config all

defconfig:
	$(RM) .config
	@yes '' | ${MAKE} config

include/autoconf.h: .config
	@yes '' | config/Configure -D config.in

config: tlvc/arch/i86/drivers/char/KeyMaps/config.in kconfig
	config/Configure config.in

menuconfig: tlvc/arch/i86/drivers/char/KeyMaps/config.in kconfig
	config/Menuconfig config.in
