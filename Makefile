clean:
	rm -f *~ */*~ */*/*~ */*/*/*~

check:
	cargo clippy
	cargo test -q
	./run_tests

publish: check
	cargo publish
