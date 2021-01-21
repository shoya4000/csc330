default: babies-image.x86-linux

babies-image.x86-linux: src/babies.sml build.cm src/csc330.sml
	 ml-build build.cm Babies.main babies-image

run: babies-image.x86-linux
	sml @SMLload  babies-image.x86-linux data/babies.txt 1920

test-01: babies-image.x86-linux
	sml @SMLload  babies-image.x86-linux data/babies.txt 1920 < tests/test-01.in | diff tests/test-01.expected -

test-02: babies-image.x86-linux
	sml @SMLload  babies-image.x86-linux data/babies.txt 1920 < tests/test-02.in | diff tests/test-02.expected -

test-03: babies-image.x86-linux
	sml @SMLload  babies-image.x86-linux data/babies.txt 1920 < tests/test-03.in | diff tests/test-03.expected -

test-04: babies-image.x86-linux
	sml @SMLload  babies-image.x86-linux data/babies.txt 1920 < tests/test-04.in | diff tests/test-04.expected -

clean:
	rm -f babies-image.x86-linux
