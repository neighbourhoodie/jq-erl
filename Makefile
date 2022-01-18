.PHONY: all clean test

all:
	rebar3 compile

test: all
	rebar3 shell

clean:
	rm -rf _build priv c_src/*.{a,d,o,so} compile_commands.json
