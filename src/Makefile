include ../support/include.mk

all: ebin_dir compile appfile

ebin_dir:
	mkdir -p $(EBIN_DIR)

compile: $(EBIN_FILES)

appfile:
	cp erlbrake.app.src $(EBIN_DIR)/erlbrake.app

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
