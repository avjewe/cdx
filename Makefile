clean:
	rm -rf *~ */*~ */*/*~ */*/*/*~ tmp

update:
	cargo update
	cargo audit

check:
	cargo clippy
	cargo test -q
	./run_tests

publish: update check
	cargo publish
