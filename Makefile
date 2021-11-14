clean:
	rm -f *~ */*~ */*/*~ */*/*/*~

publish:
	cargo test
	./run_tests
	cargo publish
