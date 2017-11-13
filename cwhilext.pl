:- use_module(library(assoc)).
:- use_module(library(dcg/basics)).
:- use_module(library(pure_input)).
%############# UTILS ################

throw_error(Message) -->
    spaces,nonblanks(S),
    lazy_list_location(file(F, L, C, _)),
    {
        format('In ~w at ~w:~w:~n~|',[F, L, C]),
        format('   syntax error: ~s, found: ~s', [Message, S]),
        halt
    }.
throw_error(Message) -->
    {
        write(Message),
        halt
    }.
%############# PARSER CODE ########################
digit(N)--> [S],{char_type(S, digit(N))}.

n(N)    --> digit(D), n(D, N).
n(A, N) --> digit(D), {A1 is A*10+D}, n(A1, N).
n(N, N) --> [].


spaces--> " ", spaces.
spaces--> "\n", spaces.
spaces--> [].

lcl(C)      --> [C], {char_type(C, lower)}.
var(A)      --> lcl(C), var([C], L), {atom_to_chars(A, L)}.
var(L, N)   --> lcl(C), {append(L, [C], L2)}, var(L2, N). 
var(N, N)   --> [].

a(num(N))   -->  n(N).
a(var(X))   --> var(X).
a(add(X, Y))-->"(",spaces, a(X),spaces, "+",spaces, a(Y), spaces,")".
a(mul(X, Y))-->"(",spaces, a(X),spaces, "*",spaces, a(Y),spaces, ")".
a(sb(X, Y)) -->"(",spaces, a(X),spaces, "-",spaces, a(Y),spaces, ")".
a(div(X, Y))-->"(",spaces, a(X),spaces, "/",spaces, a(Y),spaces, ")".

b(true)     --> "TRUE".
b(false)    --> "FALSE".
b(not(X))   --> "!",!, spaces, (b(X); throw_error('expected boolian expresion')),!.
b(and(X, Y))--> "(", spaces, b(X),spaces, "&", spaces, b(Y),spaces, ")".
b(eq(X, Y)) --> "(", spaces, a(X),spaces, "=", spaces, a(Y),spaces, ")".
b(leq(X, Y))--> "(", spaces, a(X),spaces, "<=",spaces, a(Y),spaces, ")".

statment(asign(Var, Val))   --> 
    var(Var), 
    spaces,
    ":=",
    (
        spaces, a(Val)%;
        % throw_error('expected arethmetic exp!')
    ).
statment(skip)              --> "SKIP".
statment(cond(B, S1, S2))   --> "IF",!,
    (   
        spaces, b(B); 
        throw_error('expecting bool exp!')
    ),!,
    (   
        spaces,"THEN"; 
        throw_error('expected \'THEN\'')
    ),!,
    spaces,
    metastatment(S1), 
    (   
        spaces,"ELSE";
        throw_error('expected \'ELSE\'')
    ),!,
    spaces,
    metastatment(S2), 
    (
        spaces,"END";
        throw_error('expected \'END\'') 
    ),!.

statment(loop(B, S))--> 
    "WHILE",!, 
    spaces, 
    (
        b(B);
        throw_error('expected bool exp')
    ),!,
    spaces, 
    (
        "DO";
        throw_error('expected \'DO\'')
    ),!,
    spaces, 
    metastatment(S),
    spaces, 
    (
        "END";
        throw_error('expected \'END\'')
    ),!.
metastatment(try(S1, S2))   --> spaces, "TRY", spaces, metastatment(S1), spaces, "CATCH", spaces, metastatment(S2), spaces, "END".
metastatment(comp(S1, S2))  --> spaces, statment(S1),spaces,";",spaces, metastatment(S2),spaces.
metastatment(X)             --> statment(X).

%################# COMPILER CODE ###################################

ca(num(X), [puch(X)]).
ca(var(X), [fetch(X)]).
ca(add(A1, A2), Code):-
    ca(A1, Code1),
    ca(A2, Code2),
    append([Code2, Code1, [add]], Code).

ca(mul(A1, A2), Code):-
    ca(A1, Code1),
    ca(A2, Code2),
    append([Code2, Code1, [mult]], Code).

ca(sb(A1, A2), Code):-
    ca(A1, Code1),
    ca(A2, Code2),
    append([Code2, Code1, [subt]], Code).

ca(div(A1, A2), Code):-
    ca(A1, Code1),
    ca(A2, Code2),
    append([Code2, Code1, [div]], Code).

cb(true, [true]).
cb(false, [false]).

cb(eq(A1, A2), Code):-
    ca(A1, Code1),
    ca(A2, Code2),
    append([Code2, Code1, [eq]], Code).

cb(leq(A1, A2), Code):-
    ca(A1, Code1),
    ca(A2, Code2),
    append([Code2, Code1, [le]], Code).

cb(not(X), Code):-
    cb(X, Code1),
    append([Code1, [neg]], Code).

cb(and(A1, A2), Code):-
    cb(A1, Code1),
    cb(A2, Code2),
    append([Code2, Code1, [and]], Code).

cs(asign(X, A), Code):-
    ca(A, ACode),
    append([ACode, [store(X)]], Code).

cs(skip, [noop]).

cs(comp(S1, S2), Code):-
    cs(S1, Code1),
    cs(S2, Code2),
    append([Code1,Code2], Code).

cs(cond(B, S1, S2), Code):-
    cb(B, Bcode),
    cs(S1, S1code),
    cs(S2, S2code),
    append([Bcode,[branch(S1code, S2code)]], Code).

cs(loop(B, S), [loop(Bcode, Scode)]):-
    cb(B, Bcode),
    cs(S, Scode).

cs(try(S1, S2), Code):-
    cs(S1, S1code),
    cs(S2, S2code),
    append([[try], S1code, [catch(S2code)]], Code).

%#################### VM CODE ######################

evl([[puch(X)|C], E, S, norm, I],        [C,[X|E], S, norm, I]).


evl([[add|C], [err,_|E], S, norm, I],    [C,[err|E],S, norm, I]).
evl([[add|C], [_,err|E], S, norm, I],    [C,[err|E],S, norm, I]).
evl([[add|C], [Z1,Z2|E], S, norm, I],    [C,[Sum|E],S, norm, I]):-
    Sum is Z1+Z2.

evl([[mult|C], [err,_|E], S, norm, I],   [C,[err|E],S, norm, I]).
evl([[mult|C], [_,err|E], S, norm, I],   [C,[err|E],S, norm, I]).
evl([[mult|C], [Z1,Z2|E], S, norm, I],   [C,[Sum|E],S, norm, I]):-
    Sum is Z1*Z2.

evl([[subt|C], [_,err|E], S,norm, I],   [C,[err|E],S,norm, I]).
evl([[subt|C], [err,_|E], S,norm, I],   [C,[err|E],S,norm, I]).
evl([[subt|C], [Z1,Z2|E], S,norm, I],   [C,[Sum|E],S,norm, I]):-
    Sum is Z1-Z2.

evl([[div|C], [_,0|E], S,norm, I],   [C,[err|E],S,norm, I]).
evl([[div|C], [_,err|E], S,norm, I],   [C,[err|E],S,norm, I]).
evl([[div|C], [err,_|E], S,norm, I],   [C,[err|E],S,norm, I]).
evl([[div|C], [Z1,Z2|E], S,norm, I],   [C,[Res|E],S,norm, I]):-
    Res is div(Z1,Z2).

evl([[true|C], E, S, norm, I],           [C,[tt|E],S, norm, I]).

evl([[false|C], E, S, norm, I],          [C,[ff|E],S, norm, I]).


evl([[eq|C], [err, _| E] , S, norm, I],  [C,[err|E],S, norm, I]).
evl([[eq|C], [_, err| E] , S, norm, I],  [C,[err|E],S, norm, I]).
evl([[eq|C], [Z, Z| E] , S, norm, I],  [C,[tt|E],S, norm, I]).
evl([[eq|C], [_, _| E] , S, norm, I],  [C,[ff|E],S, norm, I]).


evl([[le|C], [err, _| E] , S, norm, I],  [C,[err|E],S, norm, I]).
evl([[le|C], [_, err| E] , S, norm, I],  [C,[err|E],S, norm, I]).
evl([[le|C], [Z1, Z2| E] , S, norm, I],  [C,[tt|E],S, norm, I]):-
    Z1=<Z2.
evl([[le|C], [_, _| E] , S, norm, I],  [C,[ff|E],S, norm, I]).

evl([[and|C], [err, _| E] , S, norm, I], [C,[err|E],S, norm, I]).
evl([[and|C], [_, err| E] , S, norm, I], [C,[err|E],S, norm, I]).
evl([[and|C], [tt, tt| E] , S, norm, I], [C,[tt|E],S, norm, I]).
evl([[and|C], [ff, tt| E] , S, norm, I], [C,[ff|E],S, norm, I]).
evl([[and|C], [tt, ff| E] , S, norm, I], [C,[ff|E],S, norm, I]).

evl([[neg|C], [err| E] , S, norm, I],     [C,[err|E],S, norm, I]).
evl([[neg|C], [tt| E] , S, norm, I],     [C,[ff|E],S, norm, I]).
evl([[neg|C], [ff| E] , S, norm, I],     [C,[tt|E],S, norm, I]).

evl([[fetch(X)|C], E , S, norm, I],      [C,[RES|E],SNext, norm, I]):-
    (
        get_assoc(X, S, RES);
        format('error: fetching undefined variable ~w !~n', [X]), halt
    ), SNext=S.

evl([[store(_)|C], [err|E], S, norm, _], [C, E, S, abnorm, 0]). 
evl([[store(X)|C], [Z|E] , S, norm, I],  [C,E,SNext, norm, I]):-
    number(Z), 
    put_assoc(X, S, Z, SNext).

evl([[noop|C], E , S, norm, I],    [C,E,S, norm, I]).

evl([[branch(_, _)|C], [err|E], S, norm, I],     [C, E, S, abnorm, I]).
evl([[branch(C1, C2)|C], [T|E] , S, norm, I],   [Cres,E,S, norm, I]):-
    T=tt, append([C1, C], Cres);
    T=ff, append([C2, C], Cres).

evl([[loop(C1, C2)|C], E , S, norm, I],  [ResCode,E,S, norm, I]):-
    append([C2, [loop(C1, C2)]], BranchBody),
    append([C1, [branch(BranchBody, [noop])], C], ResCode).

evl([[try|C], E, S, abnorm, I], [C, E, S, abnorm, In]):-
    In is I+1.
evl([[try|C], E , S, norm, I],    [C,E,S, norm, I]).

evl([[catch(X)|C], E, S, abnorm, 0], [ResCode, E, S, norm, 0]):-
    append([X, C], ResCode).

evl([[catch(_)|C], E, S, abnorm, I], [C, E, S, abnorm, In]):-
    In is I-1.

evl([[catch(_)|C], E , S, norm, I],    [C,E,S, norm, I]).
evl([[W|C], E, S, abnorm, I], [C, E, S, abnorm, I]):-
    \+ W=catch(_),
    \+ W=try.



run([[],E, S, N, I], [[], E, S, N, I], _).

run([C, E, S, N, I], Result, Debug):-
    (
        Debug, 
        assoc_to_list(S, State), 
        (   
            N=norm,format('State',[]);
            N=abnorm, format('Abnormal State')
        ),
        format(': ~w ~nStack: ~w~n~n', [State, E]) , 
        format('Executing: ~w ~n', C),
        get_single_char(_);
        true
    ),
    evl([C, E, S, N, I], NextConf),
    run(NextConf, Result, Debug).

run(Code, InitState, FinalState, Debug):-
    list_to_assoc(InitState,S),
    run([Code, [], S, norm, 0], [[], _,FS,_,_], Debug),
    assoc_to_list(FS, FinalState).
