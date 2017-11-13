#!/usr/bin/env  swipl
:- set_prolog_flag(verbose, silent).
:- consult('cwhile').
:- initialization main.

debug_flag([debug|Rest], true, Rest).
debug_flag(Rest, false, Rest).
main:-
    current_prolog_flag(argv, Argv),
    debug_flag(Argv, Debug, Argv2),
    Argv2=[File|Argv3],
    maplist(term_to_atom, InitState, Argv3),
    phrase_from_file((spaces, metastatment(AT), spaces), File),
    cs(AT, Code),
    format('compiled to code: ~n~w~n~n', [Code]),
    run(Code, InitState, Result, Debug),
    format('Terminated in state:~n~w', [Result]),
    halt.
main:-
    write('error'),
    halt.
