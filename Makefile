# Copyright 2013 Erlware, LLC. All Rights Reserved.
#
# BSD License see COPYING

ERL = $(shell which erl)
ERL_VER = $(shell erl -eval 'erlang:display(erlang:system_info(otp_release)), halt().'  -noshell)

ERLFLAGS= -pa $(CURDIR)/.eunit -pa $(CURDIR)/ebin -pa $(CURDIR)/*/ebin

REBAR=$(shell which rebar)

ifeq ($(REBAR),)
#$(error "Rebar not available on this system, try running make get-rebar")
REBAR=$(CURDIR)/rebar
endif

ERLWARE_COMMONS_PLT=$(CURDIR)/.erlware_commons_plt

.PHONY: all compile doc clean test shell distclean pdf get-deps rebuild #dialyzer typer #fail on Travis.

all: deps compile

$(REBAR):
	wget https://github.com/rebar/rebar/wiki/rebar
	chmod a+x rebar

get-rebar: $(REBAR)

deps:
	$(REBAR) get-deps
	$(REBAR) compile

get-deps: $(REBAR)
	$(REBAR) get-deps
	$(REBAR) compile

compile: $(REBAR)
	$(REBAR) compile

doc: compile
	- $(REBAR) skip_deps=true doc

test: compile
	$(REBAR) skip_deps=true eunit

shell: compile
# You often want *rebuilt* rebar tests to be available to the
# shell you have to call eunit (to get the tests
# rebuilt). However, eunit runs the tests, which probably
# fails (thats probably why You want them in the shell). This
# runs eunit but tells make to ignore the result.
	- @$(REBAR) skip_deps=true eunit
	@$(ERL) $(ERLFLAGS)

clean: $(REBAR)
	$(REBAR) skip_deps=true clean
	- rm -rf $(CURDIR)/doc/*.html
	- rm -rf $(CURDIR)/doc/*.css
	- rm -rf $(CURDIR)/doc/*.png

clean-deps: clean
	rm -rvf $(CURDIR)/deps/*
	rm -rf $(ERLWARE_COMMONS_PLT).$(ERL_VER)

distclean: clean-deps
	rm -rf $(CURDIR)/rebar

rebuild: clean-deps get-deps all
