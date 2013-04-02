README
======
Authors: Tristan Sloughter <tsloughter@gmail.com>
         Jordan Wilberding <jwilberding@gmail.com>

Website: http://blog.erlware.org

dikdik is a an Erlang interface for Heroku's PostgreSQL

Quick Start
-----------

* Dependencies

```bash
$ make get-deps
```

* Compile

```bash
$ make compile
```

* Test

```bash
$ make shell
Erlang R15B03 (erts-5.9.3.1) [source] [smp:4:4] [async-threads:30] [hipe] [kernel-poll:true]

Eshell V5.9.3.1  (abort with ^G)
(dikdik@five9s-MacBook-Air-2.local)1> dikdik:create_table(<<"wutang">>).
{{create,table},[]}
(dikdik@five9s-MacBook-Air-2.local)2> dikdik:create(<<"wutang">>, <<"doc1">>, <<"{\"type\":{\"test\":\"testmore\"},\"name\":5}">>).
{{insert,0,1},[]}
(dikdik@five9s-MacBook-Air-2.local)3> dikdik:find(<<"wutang">>, <<"doc1">>).
<<"{\"type\":{\"test\":\"testmore\"},\"name\":5}">>
```

Examples
--------

TODO
