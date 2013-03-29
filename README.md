README
======
Author: Jordan Wilberding <jwilberding@gmail.com>

Website: http://blog.erlware.org

eauthnet is a an Erlang API for processing payments through Authorize.net (http://authorize.net)

Quick Start
-----------

* Dependencies

```bash
$ ./rebar get-deps
```

* Compile

```bash
$ ./rebar compile
```

* Test

```bash
$ erl +K true +A30 -pa ebin -env ERL_LIBS lib:deps -config config/sys.config
Erlang R15B03 (erts-5.9.3.1) [source] [smp:4:4] [async-threads:30] [hipe] [kernel-poll:true]

Eshell V5.9.3.1  (abort with ^G)
1> hackney:start().
ok
2> eauthnet:charge(<<"1234123412341234">>, <<"1234">>, <<"123">>, <<"1.00">>).
{authnet_result,<<"3">>,<<"2">>,<<"13">>,
                <<"The merchant login ID or password is invalid or the account is inactive.">>,
                <<>>,<<"P">>,<<"0">>,<<>>,<<>>,<<"1.00">>,<<>>,
                <<"auth_capture">>,<<>>,<<>>,<<>>,<<>>,<<>>,<<>>,<<>>,<<>>,
                <<>>,<<>>,<<>>,<<>>,<<>>,<<>>,<<>>,<<>>,...}
```

Examples
--------

TODO
