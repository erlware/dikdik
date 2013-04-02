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
1> dikdik:create_table(<<"test">>).
{{create,table},[]}
```

Examples
--------

TODO
