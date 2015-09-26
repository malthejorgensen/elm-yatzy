test: elm-stuff *.elm
	elm-test TestRunner.elm

elm-stuff:
	elm package install -y
