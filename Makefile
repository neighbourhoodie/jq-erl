ERL_PATH := /opt/homebrew/Cellar/erlang/24.2/lib/erlang
ERL_INC  := $(ERL_PATH)/usr/include
HB_INC   := /opt/homebrew/include
HB_LIB   := /opt/homebrew/lib

.PHONY: all clean test

all: jq_nif.so jq.beam

clean:
	rm -f *.{a,so,beam} myjq

test: all
	erl -noshell -s jq run -s init stop

%.beam: %.erl
	erlc $^

jq_nif.so: jq_nif.c
	$(CC) -Wall -Wextra \
		-I $(ERL_INC) -I $(HB_INC) -L $(HB_LIB) -l jq \
		-fpic -shared -flat_namespace -undefined suppress \
		-o $@ $^

myjq: $(JQ_LIBS) myjq.c
	$(CC) -I $(HB_INC) -L $(HB_LIB) -l jq -o $@ $^
