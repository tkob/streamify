check: streamifytest
	./streamifytest

streamifytest: streamifytest.sml streamifytest.mlb streamify-sig.sml streamify.sml streamify.mlb mlb-path-map
	mlton -mlb-path-map mlb-path-map streamifytest.mlb

clean:
	rm -f streamifytest
	rm -rf .cm/
