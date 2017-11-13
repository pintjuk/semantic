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

%#################### VM CODE ######################

evl([[puch(X)|C], E, S],        [C,[X|E], S]).

evl([[add|C], [Z1,Z2|E], S],    [C,[Sum|E],S]):-
   Sum is Z1+Z2. 

evl([[mult|C], [Z1,Z2|E], S],   [C,[Sum|E],S]):-
   Sum is Z1*Z2.

evl([[subt|C], [Z1,Z2|E], S],   [C,[Sum|E],S]):-
   Sum is Z1-Z2.

evl([[true|C], E, S],           [C,[tt|E],S]).

evl([[false|C], E, S],          [C,[ff|E],S]).

evl([[eq|C], [Z1, Z2| E] , S],  [C,[RES|E],S]):-
    Z1=Z2, RES=tt;
    RES=ff.

evl([[le|C], [Z1, Z2| E] , S],  [C,[RES|E],S]):-
    Z1=<Z2, RES=tt;
    RES=ff.

evl([[and|C], [T1, T2| E] , S], [C,[RES|E],S]):-
    T1=tt, T2=tt, RES=tt;
    RES=ff.

evl([[neg|C], [T1| E] , S],     [C,[RES|E],S]):-
    T1=tt, RES=ff;
    T1=ff, RES=tt.

evl([[fetch(X)|C], E , S],      [C,[RES|E],SNext]):-
    (
        get_assoc(X, S, RES);
        format('error: fetching undefined variable ~w !~n', [X]), halt
    ), SNext=S.

evl([[store(X)|C], [Z|E] , S],  [C,E,SNext]):-
    number(Z), 
    put_assoc(X, S, Z, SNext).

evl([[noop|C], E , S],    [C,E,S]).

evl([[branch(C1, C2)|C], [T|E] , S],    [Cres,E,S]):-
    T=tt, append([C1, C], Cres);
    T=ff, append([C2, C], Cres).

evl([[loop(C1, C2)|C], E , S],  [ResCode,E,S]):-
    append([C2, [loop(C1, C2)]], BranchBody),
    append([C1, [branch(BranchBody, [noop])], C], ResCode).

run([[],E, S], [[], E, S], _).

run([C, E, S], Result, Debug):-
    (
        Debug, 
        assoc_to_list(S, State), 
        format('State: ~w ~nStack: ~w~n~n', [State, E]) , 
        format('Executing: ~w ~n', C),  
        get_single_char(_);
        true
    ),
    evl([C, E, S], NextConf),
    run(NextConf, Result, Debug).
run(Code, InitState, FinalState, Debug):-
    list_to_assoc(InitState,S),
    run([Code, [], S], [[], _,FS], Debug),
    assoc_to_list(FS, FinalState).
