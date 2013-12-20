e0
==

e0 is a transactional database based loosely on Google F1 concepts.

If you want to try it out ... build, start your nodes and join them:

    make && make devrel

    for d in dev/dev*; do $d/bin/e0 start; done
    for d in dev/dev*; do $d/bin/e0 ping; done
    for d in dev/dev{2,3}; do $d/bin/e0-admin join e01@127.0.0.1; done
    ./dev/dev1/bin/e0-admin ringready

NOTE: this will start a 3 e0 node test cluster, but the nodes doesn't really cooperate (yet)!

To get a taste of the API which offers both optimistic and pessimistic
transactional mechanisms, follow the short intro below where we will

    1) create a new e0_obj
    2) write the e0_obj optimistically, it will fail if there is a conflict
    3) try write it optimistically again, but that will fail now due to that
       it would conflict as the new value is not based on the old one.
       e0 will _never_ let you write data without basing it on the previous
       version!
    4) Now lets try the "pessimistic read" r4 "read for update/delete" the 
       e0_obj we wrote in 2) with a timeout of 3000 milliseconds. If we succeed
       we are guaranteed to be able to write an updated version of the e0_obj.
    5) Here we update the e0_obj we read in 4)
    6) As we have read the object with r4, we are guaranteed not to get a 
       conflicting write. 

Lets go ...

    ./dev/dev1/bin/e0 attach
    (e01@127.0.0.1)1> E0 = e0_obj:new(<<"co-session">>, integer_to_binary(1), {funky, stuff}).
    (e01@127.0.0.1)2> e0:w(E0).
    [ok]
    (e01@127.0.0.1)3> e0:w(E0).
    {error,{conflicts,[{e0_obj,<<"co-session">>,<<"1">>,
                               {funky,stuff},
                               {1387,538422,276141},
                               undefined,undefined,undefined,undefined}]}}
    (e01@127.0.0.1)4> [E01] = e0:r4(<<"co-session">>,<<"1">>, 3000). 
    [{e0_obj,<<"co-session">>,<<"1">>,
             {funky,stuff},
             {1387,538422,276141},
             {1387,538446,320402},
             undefined,undefined,
             {bitcask_entry,<<131,104,2,109,0,0,0,10,99,111,45,115,101,
                              115,115,105,111,...>>,
                            1,159,0,1387538446}}]
    (e01@127.0.0.1)5> E02 = e0_obj:set_val(E01, "winter is coming").
    {e0_obj,<<"co-session">>,<<"1">>,"winter is coming",
            {1387,538422,276141},
            {1387,538446,320402},
            undefined,undefined,
            {bitcask_entry,<<131,104,2,109,0,0,0,10,99,111,45,115,101,
                             115,115,105,111,110,...>>,
                           1,159,0,1387538446}}
    (e01@127.0.0.1)6> e0:w(E02).
    [ok]

From that, note that all e0 operations takes lists as arguments, meaning that
you can e.g. invoke e0:r4(...) with a number of boxes/keys that you want to update
as a whole. An example would be a web service that involves multiple actors that
might update data concurrently.
