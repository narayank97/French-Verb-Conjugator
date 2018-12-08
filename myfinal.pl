% potential french pronouns in different perspectives
french([je],[firstperson,singular,pronoun]).
french([tu],[secondperson,singular,pronoun]).
french(["il/elle"],[thirdperson,singular,pronoun]).
french([nous],[firstperson,plural,pronoun]).
french([vous],[secondperson,plural,pronoun]).
french(["ils/elles"],[thirdperson,plural,pronoun]).

% -ir,-er,-re are the three most common verb endings in the French Language

% these are the -ir conjugation endings
french([i,s],[firstperson,singular,verb,ir]).
french([i,s],[secondperson,singular,verb,ir]).
french([i,t],[thirdperson,singular,verb,ir]).
french([i,s,s,o,n,s],[firstperson,plural,verb,ir]).
french([i,s,s,e,z],[secondperson,plural,verb,ir]).
french([i,s,s,e,n,t],[thirdperson,plural,verb,ir]).

% these are the -er conjugation endings
french([e],[firstperson,singular,verb,er]).
french([e,s],[secondperson,singular,verb,er]).
french([e],[thirdperson,singular,verb,er]).
french([o,n,s],[firstperson,plural,verb,er]).
french([e,z],[secondperson,plural,verb,er]).
french([e,n,t],[thirdperson,plural,verb,er]).

% these are the -re conjugation endings
french([s],[firstperson,singular,verb,re]).
french([s],[secondperson,singular,verb,re]).
french([],[thirdperson,singular,verb,re]).
french([o,n,s],[firstperson,plural,verb,re]).
french([e,z],[secondperson,plural,verb,re]).
french([e,n,t],[thirdperson,plural,verb,re]).

% these are the -irregular conjugation endings for etre (to be)
french([s,u,i,s],[firstperson,singular,verb,etre]).
french([e,s],[secondperson,singular,verb,etre]).
french([e,s,t],[thirdperson,singular,verb,etre]).
french([s,o,m,m,e,s],[firstperson,plural,verb,etre]).
french([e,t,e,s],[secondperson,plural,verb,etre]).
french([s,o,n,t],[thirdperson,plural,verb,etre]).

% these are the -irregular conjugation endings for aller (to be)
french([v,a,i,s],[firstperson,singular,verb,aller]).
french([v,a,s],[secondperson,singular,verb,aller]).
french([v,a],[thirdperson,singular,verb,aller]).
french([a,l,l,o,n,s],[firstperson,plural,verb,aller]).
french([a,l,l,e,z],[secondperson,plural,verb,aller]).
french([v,o,n,t],[thirdperson,plural,verb,aller]).

% This function checks to see if two elements are next to each other
adjacent(X, Y, [X,Y|_]).
adjacent(X, Y, [_|Tail]) :-
    adjacent(X, Y, Tail).

adjacentirreg(W, X, Y, Z, [W,X,Y,Z|_]).
adjacentirreg(W, X, Y, Z, [_|Tail]) :-
    adjacentirreg(W, X, Y, Z, Tail).

adjacentirreg1(V, W, X, Y, Z, [V,W,X,Y,Z|_]).
adjacentirreg1(V, W, X, Y, Z, [_|Tail]) :-
    adjacentirreg1(V, W, X, Y, Z, Tail).

% this function removes the last two elements from a list
without_last_n(Old, N, New) :-
    length(Tail, N),
    append(New, Tail, Old).

% this function allows me to print out my output in a friendly way
print( [ ] ).
print( [ X | Y ] ):-
   write(X),
   print( Y ).

% this is the function used to conjugate -er verbs
er_conjugator(Infinitive):-
    % checks to see if it ends with -er
    adjacent(e, r, Infinitive),
    % removes -er suffix
    without_last_n(Infinitive, 2, Root),

    % first person singular conjugation
    french(A,[firstperson,singular,pronoun]),
    french(B,[firstperson,singular,verb,er]),
    print(A),
    % appends first person verb ending
    append(Root,B,Iverb),
    write(' '),
    print(Iverb),
    nl,

%%%%%%%%%%%%%% THE REST OF THE CODE FOLLOWS A SIMILAR PATTERN %%%%%%%%%%%%%%%%%%

    % second person singular conjugation
    french(C,[secondperson,singular,pronoun]),
    french(D,[secondperson,singular,verb,er]),
    print(C),
    append(Root,D,Youverb),
    write(' '),
    print(Youverb),
    nl,

    % third person singular conjugation
    french(E,[thirdperson,singular,pronoun]),
    french(F,[thirdperson,singular,verb,er]),
    print(E),
    append(Root,F,HEverb),
    write(' '),
    print(HEverb),
    nl,

    % First person plural conjugation
    french(G,[firstperson,plural,pronoun]),
    french(H,[firstperson,plural,verb,er]),
    print(G),
    append(Root,H,USverb),
    write(' '),
    print(USverb),
    nl,

    % second person singular conjugation
    french(I,[secondperson,plural,pronoun]),
    french(J,[secondperson,plural,verb,er]),
    print(I),
    append(Root,J,YOUALLverb),
    write(' '),
    print(YOUALLverb),
    nl,

    % third person singular conjugation
    french(K,[thirdperson,plural,pronoun]),
    french(L,[thirdperson,plural,verb,er]),
    print(K),
    append(Root,L,THEYverb),
    write(' '),
    print(THEYverb),
    nl. % This creates a new line in the terminal, makes it easier to see

% this is the function used to conjugate -ir verbs
ir_conjugator(Infinitive):-
    adjacent(i, r, Infinitive),
    without_last_n(Infinitive, 2, Root),

    % first person singular conjugation
    french(A,[firstperson,singular,pronoun]),
    french(B,[firstperson,singular,verb,ir]),
    print(A),
    append(Root,B,Iverb),
    write(' '),
    print(Iverb),
    nl,

    % second person singular conjugation
    french(C,[secondperson,singular,pronoun]),
    french(D,[secondperson,singular,verb,ir]),
    print(C),
    append(Root,D,Youverb),
    write(' '),
    print(Youverb),
    nl,

    % third person singular conjugation
    french(E,[thirdperson,singular,pronoun]),
    french(F,[thirdperson,singular,verb,ir]),
    print(E),
    append(Root,F,HEverb),
    write(' '),
    print(HEverb),
    nl,

    % First person plural conjugation
    french(G,[firstperson,plural,pronoun]),
    french(H,[firstperson,plural,verb,ir]),
    print(G),
    append(Root,H,USverb),
    write(' '),
    print(USverb),
    nl,

    % second person singular conjugation
    french(I,[secondperson,plural,pronoun]),
    french(J,[secondperson,plural,verb,ir]),
    print(I),
    append(Root,J,YOUALLverb),
    write(' '),
    print(YOUALLverb),
    nl,

    % third person singular conjugation
    french(K,[thirdperson,plural,pronoun]),
    french(L,[thirdperson,plural,verb,ir]),
    print(K),
    append(Root,L,THEYverb),
    write(' '),
    print(THEYverb),
    nl.

% this is the function used to conjugate -re verbs
re_conjugator(Infinitive):-
    adjacent(r, e, Infinitive),
    without_last_n(Infinitive, 2, Root),

    % first person singular conjugation
    french(A,[firstperson,singular,pronoun]),
    french(B,[firstperson,singular,verb,re]),
    print(A),
    append(Root,B,Iverb),
    write(' '),
    print(Iverb),
    nl,

    % second person singular conjugation
    french(C,[secondperson,singular,pronoun]),
    french(D,[secondperson,singular,verb,re]),
    print(C),
    append(Root,D,Youverb),
    write(' '),
    print(Youverb),
    nl,

    % third person singular conjugation
    french(E,[thirdperson,singular,pronoun]),
    french(F,[thirdperson,singular,verb,re]),
    print(E),
    append(Root,F,HEverb),
    write(' '),
    print(HEverb),
    nl,

    % First person plural conjugation
    french(G,[firstperson,plural,pronoun]),
    french(H,[firstperson,plural,verb,re]),
    print(G),
    append(Root,H,USverb),
    write(' '),
    print(USverb),
    nl,

    % second person singular conjugation
    french(I,[secondperson,plural,pronoun]),
    french(J,[secondperson,plural,verb,re]),
    print(I),
    append(Root,J,YOUALLverb),
    write(' '),
    print(YOUALLverb),
    nl,

    % third person singular conjugation
    french(K,[thirdperson,plural,pronoun]),
    french(L,[thirdperson,plural,verb,re]),
    print(K),
    append(Root,L,THEYverb),
    write(' '),
    print(THEYverb),
    nl.
%%%%%%%%%%%%%%%% BEGINNING OF IRREGULAR VERBS %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this is the function used to conjugate the verb etre
irregular_etre_conjugator(Infinitive):-
    % check to see if verb entered is etre
    adjacentirreg(e,t,r,e, Infinitive),
    % delete the entire word since its irregular
    without_last_n(Infinitive, 4, Root),

    % replace the infinitive aller with its correct conjugation
    % first person singular conjugation
    french(A,[firstperson,singular,pronoun]),
    french(B,[firstperson,singular,verb,etre]),
    print(A),
    append(Root,B,Iverb),
    write(' '),
    print(Iverb),
    nl,

    % second person singular conjugation
    french(C,[secondperson,singular,pronoun]),
    french(D,[secondperson,singular,verb,etre]),
    print(C),
    append(Root,D,Youverb),
    write(' '),
    print(Youverb),
    nl,

    % third person singular conjugation
    french(E,[thirdperson,singular,pronoun]),
    french(F,[thirdperson,singular,verb,etre]),
    print(E),
    append(Root,F,HEverb),
    write(' '),
    print(HEverb),
    nl,

    % First person plural conjugation
    french(G,[firstperson,plural,pronoun]),
    french(H,[firstperson,plural,verb,etre]),
    print(G),
    append(Root,H,USverb),
    write(' '),
    print(USverb),
    nl,

    % second person singular conjugation
    french(I,[secondperson,plural,pronoun]),
    french(J,[secondperson,plural,verb,etre]),
    print(I),
    append(Root,J,YOUALLverb),
    write(' '),
    print(YOUALLverb),
    nl,

    % third person singular conjugation
    french(K,[thirdperson,plural,pronoun]),
    french(L,[thirdperson,plural,verb,etre]),
    print(K),
    append(Root,L,THEYverb),
    write(' '),
    print(THEYverb),
    nl.

% this is the function used to conjugate the verb aller
irregular_aller_conjugator(Infinitive):-
    % check to see if verb entered is aller
    adjacentirreg1(a,l,l,e,r, Infinitive),
    % delete the entire word since its irregular
    without_last_n(Infinitive, 5, Root),

    % replace the infinitive aller with its correct conjugation
    % first person singular conjugation
    french(A,[firstperson,singular,pronoun]),
    french(B,[firstperson,singular,verb,aller]),
    print(A),
    append(Root,B,Iverb),
    write(' '),
    print(Iverb),
    nl,

    % second person singular conjugation
    french(C,[secondperson,singular,pronoun]),
    french(D,[secondperson,singular,verb,aller]),
    print(C),
    append(Root,D,Youverb),
    write(' '),
    print(Youverb),
    nl,

    % third person singular conjugation
    french(E,[thirdperson,singular,pronoun]),
    french(F,[thirdperson,singular,verb,aller]),
    print(E),
    append(Root,F,HEverb),
    write(' '),
    print(HEverb),
    nl,

    % First person plural conjugation
    french(G,[firstperson,plural,pronoun]),
    french(H,[firstperson,plural,verb,aller]),
    print(G),
    append(Root,H,USverb),
    write(' '),
    print(USverb),
    nl,

    % second person singular conjugation
    french(I,[secondperson,plural,pronoun]),
    french(J,[secondperson,plural,verb,aller]),
    print(I),
    append(Root,J,YOUALLverb),
    write(' '),
    print(YOUALLverb),
    nl,

    % third person singular conjugation
    french(K,[thirdperson,plural,pronoun]),
    french(L,[thirdperson,plural,verb,aller]),
    print(K),
    append(Root,L,THEYverb),
    write(' '),
    print(THEYverb),
    nl.
