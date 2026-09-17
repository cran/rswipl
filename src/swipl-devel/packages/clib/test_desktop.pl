/*  Part of SWI-Prolog

    Author:        Jan Wielemaker
    E-mail:        jan@swi-prolog.org
    WWW:           http://www.swi-prolog.org
    Copyright (c)  2026, SWI-Prolog Solutions b.v.
    All rights reserved.

    Redistribution and use in source and binary forms, with or without
    modification, are permitted provided that the following conditions
    are met:

    1. Redistributions of source code must retain the above copyright
       notice, this list of conditions and the following disclaimer.

    2. Redistributions in binary form must reproduce the above copyright
       notice, this list of conditions and the following disclaimer in
       the documentation and/or other materials provided with the
       distribution.

    THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
    FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
    COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
    INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING,
    BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
    LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
    CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
    LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
    ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
    POSSIBILITY OF SUCH DAMAGE.
*/


:- module(test_desktop,
	  [ test_desktop/0
	  ]).

:- asserta(user:file_search_path(foreign, '.')).
:- asserta(user:file_search_path(library, '.')).
:- asserta(user:file_search_path(library, '../plunit')).

:- use_module(library(plunit)).
:- use_module(library(apply)).
:- use_module(library(lists)).
:- use_module(library(option)).
:- use_module(library(readutil)).
:- use_module(library(yall)).
:- use_module(library(desktop)).

test_desktop :-
    run_tests([ desktop_open
	      ]).

/* Tests desktop_open/2 using a Prolog process that records the arguments
   it receives rather than a real document opener.
*/

:- begin_tests(desktop_open,
	       [ sto(rational_trees),
		 setup(create_test_files),
		 cleanup(delete_test_files)
	       ]).

test(url, Args == ['https://www.swi-prolog.org']) :-
    open_args('https://www.swi-prolog.org', [], Args).
test(mailto, Args == ['mailto:bugs@example.com']) :-
    open_args('mailto:bugs@example.com', [], Args).
test(directory, Args == [Dir]) :-
    tmp_dir(Tmp),
    os_file(Tmp, Dir),
    open_args(Tmp, [], Args).
test(alias, Args == [OsFile]) :-
    absolute_file_name(library('lists.pl'), File, [access(read)]),
    os_file(File, OsFile),
    open_args(library('lists.pl'), [], Args).
test(plain_file, Args == [OsFile]) :-
    test_file('plain.txt', File),
    os_file(File, OsFile),
    open_args(File, [], Args).
test(special_characters, Args == OsFiles) :-
    maplist(test_file, ['with space.txt', 'with\'quote.txt'], Files),
    maplist(os_file, Files, OsFiles),
    maplist([F,A]>>open_args(F, [], [A]), Files, Args).
test(wildcard, Args == OsFiles) :-
    test_file('glob*.txt', Pattern),
    maplist(test_file, ['glob1.txt', 'glob2.txt'], Files),
    maplist(os_file, Files, OsFiles),
    open_args(Pattern, [], Args).
test(pre_arguments, Args == ['--first', OsFile]) :-
    test_file('plain.txt', File),
    os_file(File, OsFile),
    probe_opener(Exe-Argv),
    append(Argv, ['--first'], Argv1),
    open_args(File, [opener(Exe-Argv1)], Args).
test(no_file, error(existence_error(source_sink, _))) :-
    open_args('no/such/file.txt', [], _).
test(no_opener, error(existence_error(program, _))) :-
    open_args('.', [opener('no-such-opener-4711')], _).
test(failed, error(process_error(_, exit(2)))) :-
    prolog_command('halt(2)', Exe, Argv),
    append(Argv, ['--'], Argv1),
    desktop_open('.', [opener(Exe-Argv1), wait(true)]).

:- end_tests(desktop_open).

%!  open_args(+Document, +Options, -Arguments) is det.
%
%   Call desktop_open/2 using our recording  process and unify Arguments
%   with the arguments it received.

open_args(Document, Options, Arguments) :-
    probe_log(Log),
    (   exists_file(Log)
    ->  delete_file(Log)
    ;   true
    ),
    (   option(opener(_), Options)
    ->  Options1 = Options
    ;   probe_opener(Opener),
        Options1 = [opener(Opener)|Options]
    ),
    desktop_open(Document, [wait(true)|Options1]),
    read_file_to_string(Log, String, []),
    split_string(String, "\n", "\r", Lines0),
    exclude(==(""), Lines0, Lines),
    maplist(atom_string, Arguments, Lines).

%!  probe_opener(-Opener) is det.
%
%   Opener for desktop_open/2 that appends its arguments to probe_log/1.

probe_opener(Exe-Argv) :-
    probe_log(Log),
    format(atom(Goal),
	   "current_prolog_flag(argv, Argv),\c
	    setup_call_cleanup(open(~q, append, Out),\c
			       forall(member(Arg, Argv),\c
				      format(Out, '~~w~~n', [Arg])),\c
			       close(Out))",
	   [Log]),
    prolog_command(Goal, Exe, Argv0),
    append(Argv0, ['--'], Argv).

%!  prolog_command(+Goal, -Exe, -Argv) is det.
%
%   Run Goal using a fresh Prolog process.  Used to create a process
%   without relying on the tools installed on the target platform.

prolog_command(Goal, Exe, ['-f', none, '-g', Goal, '-t', halt]) :-
    current_prolog_flag(executable, Exe).

probe_log(Log) :-
    test_file('log', Log).

tmp_dir(Dir) :-
    current_prolog_flag(tmp_dir, Dir).

test_file(Name, Path) :-
    tmp_dir(Tmp),
    atomic_list_concat([Tmp, /, 'pl_desktop_open_', Name], Path).

%!  os_file(+File, -OsFile) is det.
%
%   OsFile is the absolute OS file name desktop_open/2 hands to the
%   opener for File.

os_file(File, OsFile) :-
    absolute_file_name(File, Absolute),
    prolog_to_os_filename(Absolute, OsFile).

test_files([ 'plain.txt', 'with space.txt', 'with\'quote.txt',
	     'glob1.txt', 'glob2.txt'
	   ]).

create_test_files :-
    test_files(Names),
    maplist(create_test_file, Names).

create_test_file(Name) :-
    test_file(Name, Path),
    setup_call_cleanup(
	open(Path, write, Out),
	format(Out, 'test~n', []),
	close(Out)).

delete_test_files :-
    test_files(Names),
    forall(( member(Name, ['log'|Names]),
	     test_file(Name, Path),
	     exists_file(Path)
	   ),
	   delete_file(Path)).
