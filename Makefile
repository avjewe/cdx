clean:
	rm -rf *~ */*~ */*/*~ */*/*/*~ tmp

update:
	cargo upgrade --verbose
	cargo audit

check:
	cargo clippy
	cargo test -q -- --no-capture
	./run_tests

publish: update check
	cargo publish
