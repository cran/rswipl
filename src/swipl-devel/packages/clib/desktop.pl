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

:- module(desktop,
          [ desktop_open/1,             % +Document
            desktop_open/2              % +Document, +Options
          ]).
:- autoload(library(apply),[maplist/2,maplist/3]).
:- autoload(library(error),[must_be/2,existence_error/2]).
:- autoload(library(lists),[append/3]).
:- autoload(library(option),[option/2,option/3]).
:- autoload(library(process),[process_create/3,process_which/2]).
:- autoload(library(uri),[uri_is_global/1]).

/** <module> Interact with the desktop environment

This library provides access to the  desktop   environment  of  the user.
Currently it only provides desktop_open/1,2, which  hands a document over
to the application the desktop associates with it.
*/

%!  desktop_open(+Document) is det.
%!  desktop_open(+Document, +Options) is det.
%
%   Open Document using the application  the   desktop  associates  with
%   it, e.g., a PDF viewer for a `.pdf` file or a file browser if
%   Document is a directory.  Document is one of
%
%     - A URL, i.e., text for which uri_is_global/1 is true, such as
%       ``https://www.swi-prolog.org`` or ``mailto:bugs@example.com``.
%       The URL is passed to the _opener_ unmodified.
%     - A file or directory.  This is either a plain file name, a term
%       Dir/File or a file _alias_ such as library(lists).  If a plain
%       file name does not exist as given it is expanded using
%       expand_file_name/2, i.e., ``~``, ``$var`` and wildcards are
%       expanded and all matching files are opened.
%
%   Options processed:
%
%     - opener(+Command)
%       Use Command instead of the platform default.  Command is a
%       specification for process_which/2, a term `Command-Args` to
%       pass the arguments Args before Document or `win_shell` to use
%       the Windows ShellExecute() API.
%     - wait(+Boolean)
%       If `true`, wait for the _opener_ to complete and raise an
%       exception if it fails.  This does __not__ wait for the
%       application: openers such as ``xdg-open`` merely hand the
%       document to the desktop and complete immediately, normally with
%       success regardless of what the desktop does with it.  Default is
%       `false`, which starts the opener using the detached(true) option
%       of process_create/3.  The option is ignored if `win_shell` is
%       used, which never waits.
%
%   The command to use is taken from  the   first  of  these that yields
%   an available command:
%
%     1. The option opener(Command)
%     2. The Prolog flag `desktop_opener` if it is not `default`.  It
%        uses the same syntax as the opener(Command) option.
%     3. The Windows ShellExecute() API (see win_shell/2)
%     4. ``open`` on MacOS
%     5. ``cygstart`` (Cygwin), ``termux-open`` (Android/Termux) or
%        ``wslview`` (WSL without a Linux desktop)
%     6. ``xdg-open`` (freedesktop.org), followed by the desktop
%        specific ``gio``, ``gnome-open``, ``kde-open``, ``kde-open5``
%        and ``exo-open``
%     7. ``wslview``, ``handlr``, ``mimeopen`` or ``run-mailcap``
%     8. ``open``, unless we are on Linux, where ``open`` is an alias
%        for openvt(1) rather than a document opener.
%
%   @error existence_error(source_sink, Document) if Document does not
%   exist.
%   @error existence_error(config, desktop_opener) if no command to open
%   documents is available.
%   @error process_error(Command, exit(Status)) if wait(true) is used
%   and Command does not exit successfully.

:- predicate_options(desktop_open/2, 2,
                     [ opener(any),
                       wait(boolean)
                     ]).

:- create_prolog_flag(desktop_opener, default,
                      [ type(term),
                        keep(true)
                      ]).

desktop_open(Document) :-
    desktop_open(Document, []).

desktop_open(Document, Options) :-
    document_arguments(Document, Arguments),
    opener(Command, Options),
    maplist(open_argument(Command, Options), Arguments).

%!  document_arguments(+Document, -Arguments) is det.
%
%   Arguments is the list of URLs and file(File) terms to pass to the
%   opener, one invocation per element.

document_arguments(Document, [Document]) :-
    atomic(Document),
    uri_is_global(Document),
    !.
document_arguments(Document, Arguments) :-
    document_files(Document, Files),
    maplist(file_argument, Files, Arguments).

file_argument(File, file(File)).

%!  document_files(+Document, -Files) is det.
%
%   Files is the list of existing files or directories denoted by
%   Document.  Compound  terms are handed  to absolute_file_name/3,
%   which deals with both aliases and Dir/File terms.

document_files(Spec, [File]) :-
    compound(Spec),
    !,
    (   absolute_file_name(Spec, File,
                           [ access(exist),
                             file_type(directory),
                             file_errors(fail)
                           ])
    ->  true
    ;   absolute_file_name(Spec, File,
                           [ access(exist),
                             file_errors(error)
                           ])
    ).
document_files(Spec, Files) :-
    must_be(atomic, Spec),
    atom_string(Name, Spec),
    (   access_file(Name, exist)                % also true for directories
    ->  Expanded = [Name]                       % do not expand ``$`` and ``*``
    ;   catch(expand_file_name(Name, Expanded), error(_,_),
              existence_error(source_sink, Spec)),
        Expanded \== []
    ->  true
    ;   existence_error(source_sink, Spec)
    ),
    maplist(existing_file, Expanded, Files).

existing_file(File, Absolute) :-
    (   access_file(File, exist)                % also true for directories
    ->  absolute_file_name(File, Absolute)
    ;   existence_error(source_sink, File)
    ).

%!  opener(-Command, +Options) is det.
%
%   Find the command to open a document.  Command is either `win_shell`
%   or a term Exe-Args, where Exe is the absolute path of the executable
%   and Args are arguments that precede the document.

opener(Command, Options) :-
    option(opener(Spec), Options),
    !,
    (   opener_command(Spec, Command0),
        resolve_command(Command0, Command)
    ->  true
    ;   existence_error(program, Spec)
    ).
opener(Command, _Options) :-
    (   current_prolog_flag(desktop_opener, Spec),
        Spec \== default,
        opener_command(Spec, Command0),
        resolve_command(Command0, Command)
    ->  true
    ;   default_opener(Command0),
        resolve_command(Command0, Command)
    ->  true
    ;   existence_error(config, desktop_opener)
    ).

opener_command(win_shell, Command) =>
    Command = win_shell.
opener_command(Exe-Args, Command) =>
    must_be(list, Args),
    Command = Exe-Args.
opener_command(Exe, Command) =>
    Command = Exe-[].

:- if(current_predicate(win_shell/2)).
resolve_command(win_shell, Command) =>
    Command = win_shell.
:- endif.
resolve_command(Exe-Args, Command) =>
    opener_executable(Exe, Path),
    Command = Path-Args.
resolve_command(_, _) =>
    fail.

%!  opener_executable(+Command, -Path) is semidet.
%
%   Path is the absolute file name of Command.  A plain name is searched
%   for on ``$PATH``.

opener_executable(Command, Path) :-
    (   atom(Command),
        file_base_name(Command, Command)        % plain name
    ->  process_which(path(Command), Path)
    ;   process_which(Command, Path)
    ).

%!  default_opener(-Command) is nondet.
%
%   Enumerate commands that may be able to  open a document in the order
%   we prefer them.  Note that we must not use ``open`` on Linux, where
%   this is an alias for openvt(1) rather than a document opener.

:- if(current_predicate(win_shell/2)).
default_opener(win_shell).                      % Windows ShellExecute()
:- endif.
default_opener(open-[]) :-                      % MacOS
    current_prolog_flag(apple, true).
default_opener(cygstart-[]).                    % Cygwin
default_opener('termux-open'-[]).               % Android (Termux)
default_opener(wslview-[]) :-                   % WSL without Linux desktop
    getenv('WSL_DISTRO_NAME', _),
    \+ getenv('DISPLAY', _),
    \+ getenv('WAYLAND_DISPLAY', _).
default_opener('xdg-open'-[]).                  % freedesktop.org
default_opener(gio-[open]).                     % Gnome (GIO)
default_opener('gnome-open'-[]).                % Gnome (deprecated)
default_opener('kde-open'-[]).                  % KDE
default_opener('kde-open5'-[]).
default_opener('exo-open'-[]).                  % XFCE
default_opener(wslview-[]).                     % WSL (wslu)
default_opener(handlr-[open]).                  % handlr
default_opener(mimeopen-['-n']).                % Perl File::MimeInfo
default_opener('run-mailcap'-[]).               % mailcap(5)
default_opener(open-[]) :-                      % Haiku and friends
    \+ linux.

linux :-
    current_prolog_flag(arch, Arch),
    sub_atom(Arch, _, _, _, linux),
    !.

%!  open_argument(+Command, +Options, +Argument) is det.
%
%   Run Command on a single URL or file name.

:- if(current_predicate(win_shell/2)).
open_argument(win_shell, _Options, Argument) :-
    !,
    win_shell_argument(Argument, OsArgument),
    win_shell(open, OsArgument).
:- endif.
open_argument(Exe-Args, Options, Argument) :-
    append(Args, [Argument], Argv),
    (   option(wait(Wait), Options, false),
        must_be(boolean, Wait),
        Wait == true
    ->  process_create(Exe, Argv,
                       [ stdin(null),
                         stdout(null)
                       ])
    ;   process_create(Exe, Argv,
                       [ detached(true),
                         stdin(null),
                         stdout(null),
                         stderr(null)
                       ])
    ).

:- if(current_predicate(win_shell/2)).
win_shell_argument(file(File), OsFile) :-
    !,
    prolog_to_os_filename(File, OsFile).
win_shell_argument(URL, URL).
:- endif.
