<!-- © 2019 Serokell
   -
   - SPDX-License-Identifier: MPL-2.0
   -->


# Overcight

This project is a collection of tools for overseeing the execution of programs,
tracking their access to the file system, and analysing the results.

Imagine that you are working on a project, and this project has a CI system.
It also has a bunch of shell-scripts, some of them are used frequently,
others – extremely rarely. To be sure that codebase changes do not break these
shell-scripts, you might want to call them in your CI pipelines. But how do you
know _all_ scripts are exercised on the CI? Well, that’s how.


## Simple use (`overc`)

The `overc` utility runs a program, oversees it, and outputs a file containing
a list (a set) of files that it accessed for _reading_ or _execution_.

```
overc run -- ls -l /tmp
```

will create a file `overc-ls.json` in the current directory.
(You can override the location with the `-o` option.)

Another way to create an output file is to run:

```
overc list path/to/directory
```

which will produce an output as if someone tried to _read_ every file
in the filesystem subtree corresponding to the directory and _execute_ every
file in the subtree that has at least one of the executable bits set.

Then you can use the `overc set` family of commands to perform various
set-theoretic operations on resulting output files:

* `overc set union` merges multiple output files into one
* `overc set subtract` leaves files that exist only in the first output file
* `overc set view` prints an output file in a nice way


## Use on the CI (`overci`)

The `overci` utility is optimised for being used as part of a CI pipeline.
Here is how:

* Prefix all commands in your CI configuration with `overci run $job_name`
  (assuming that you have multiple independent jobs and they have names).
  The command will place its output file at `.overci.out/$job_name.json`.
* Make sure the `.overci.out` directory is being collected as a build artefact.
* Have the last job that collects outputs from all `.overci.out` directories
  from all the preceding jobs and runs `overci check`.

The `overci check` command from the last step above will exit with a non-zero
exit code, if there are files in the repository that were not read
or executable files that were not executed and it will print the list of them.

Of course, there might be files that you would like it to ignore (for example,
you might have documentation, like specifications, that are not processed on
the CI) – you can exclude them with the help of the `.overci.yaml` file in the
root of your repository.
