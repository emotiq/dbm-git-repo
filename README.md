# dbm-git-repo
DBM's legacy support code for blockchain

For now the dbm-legacy-support folder contains the Lisp source for all the support routines beneath new Emotiq code. It is only partially ported to ACL at this time. Work on porting contines here.

The Emotiq folder contains new code written expressly for the Emotiq blockchain effort.

***

Regarding Lisp ports: for the following, ACL and Clozure have been
tested and work. Lispworks has been partially tested.

***

Here are som basic load instructions:

Start Lisp. Then, 

```
(load "/path/to/load.lisp")
(asdf :cosi)
```

where "/path/to/" is the path of this repository containing the
load.lisp file, which you load first.

***

Simple system test:

```
(in-package :cosi)
(tst 100)
```

Typically runs in about 10 seconds on a Mac, puts out 1/2 page of output.
