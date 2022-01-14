ERL_PATH := /opt/homebrew/Cellar/erlang/24.2/lib/erlang
ERL_INC  := $(ERL_PATH)/usr/include
HB_INC   := /opt/homebrew/include
JQ_LIBS  := /opt/homebrew/lib/libjq.a /opt/homebrew/lib/libonig.a

.PHONY: all clean test

all: jq_nif.so jq.beam

clean:
	rm -f *.{a,so,beam} myjq

test: all
	erl -noshell -s jq run -s init stop

%.beam: %.erl
	erlc $^

jq_nif.so: $(JQ_LIBS) jq_nif.c
	$(CC) -I $(ERL_INC) -I $(HB_INC) -fpic -shared -flat_namespace -undefined suppress -o $@ $^

myjq: $(JQ_LIBS) myjq.c
	$(CC) -I $(HB_INC) -o $@ $^
