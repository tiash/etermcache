REBAR=rebar
DIALYZER=dialyzer
ERL=erl
.PHONY : all
all: compile

.PHONY : deps
deps: 
	@${REBAR} get-deps update-deps

.PHONY : clean
clean:
	@${REBAR} clean

.PHONY : compile
compile: 
	@${REBAR} compile


.PHONY : xref
xref: compile
	@${REBAR} xref # don't scan dependencies...

.PHONY : check
check: compile xref dialyze test

.PHONY : doc
doc:
	@${REBAR} doc

.PHONY : test
test: compile
	@${REBAR} eunit

plts=erts.plt kernel.plt stdlib.plt syntax_tools.plt

$(plts): %.plt:
	@${DIALYZER} --build_plt --output_plt $@ --apps $*

.PHONY : plt
plt: $(plts)

.PHONY : dialyze
dialyze:
	@${DIALYZER} --plts $(plts) -r ebin
.PHONY : benchmark
benchmark:
	@${ERL} -compile benchmark/exemell_benchmark.erl
	@${ERL}	-pa ebin deps/*/ebin benchmark -noshell -run exemell_benchmark run



