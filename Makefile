.PHONY: test install

test:
	bb test

install:
	bbin install . --as builder --main-opts '["-m" "builder.core"]'
	bbin install . --as detect-duplicates --main-opts '["-m" "builder.detect-duplicates"]'
