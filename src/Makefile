include ../support/include.mk

all: ebin_dir compile

ebin_dir:
	mkdir -p $(EBIN_DIR)

compile: $(EBIN_FILES)

debug:
	$(MAKE) DEBUG=-DDEBUG

clean:
	rm -rf $(EBIN_FILES)
