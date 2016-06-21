ExecSQL
========

This is a simple app I wrote to keep track of all the various database servers I use, the databases on those servers, and the username/password combinations needed to access those databases.

You'll need a Common LISP environment to use this app, along with ASDF 3.x or better.

I did my development using SBCL and the included build script uses the `buildapp` utility from SBCL to generate an executable. (Although honestly, the generated executable is huge because it has to package up the LISP environment!)

You can also use the cl-launch application if you would rather not compile a native executable. The `execSql.sh` script is an example of how to do that.

To use this application for real work you will need to create a configuration file called `~/.execSql/config.lisp`. There is an example provided that you can modify for your needs.
