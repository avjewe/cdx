clean:
	rm -rf *~ */*~ */*/*~ */*/*/*~ tmp

update:
	cargo update

check:
	cargo clippy
	cargo test -q
	./run_tests

publish: update check
	cargo publish
