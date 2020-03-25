build:
	elm make --output=docs/script.js src/Main.elm

serve:
	pushd docs ; python -m http.server ; popd
