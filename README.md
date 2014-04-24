Epool
=====

`This lib is intended to allow you to use your gen_server as a pool of workers.`

`Idea from http://learnyousomeerlang.com/building-applications-with-otp#a-pools-tree`


Example
=======

To start and use examples.

```
  $ git clone https://github.com/stofel/epool.git
  $ cd epool
  $ make
  $ make install
  $ make start
  $ make attach
  (epool@127.0.0.1)1> epool_example:start().
  {ok,<0.55.0>}
  (epool@127.0.0.1)2> epool:list().
  [test_pool]
  (epool@127.0.0.1)3> epool:status_pool(test_pool).
  {ok,[{tasks,0},
       {sum_task_length,0},
       {sum_answers_length,0},
       {size,4},
       {free_workers,4},
       {errors,0}]}
  (epool@127.0.0.1)4> epool_example:call().        
  {ok,[{pong,1},{pong,2}]}
  (epool@127.0.0.1)5> epool:stop_pool(test_pool).
  ok
  (epool@127.0.0.1)6> epool:list().
  []
```

See epool api src/epool.erl.

See examples and example worker src/epool_example.erl and src/epool_example_worker.erl


**NOTE**: Use resize command with caution.

**NOTE 2**: When use epool in embedded mode, add epool to your_project.app.src to 
applications list
