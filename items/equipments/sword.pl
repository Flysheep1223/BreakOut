:- dynamic equipment/4. % equipment(Type, Name, X, Y)

% Initialize sword
init_sword(X, Y) :-
    assertz(equipment(sword, 'Iron Sword', X, Y)).

% Effect when picked up
apply_sword_effect :-
    player_atk(Atk),
    NewAtk is Atk + 10,
    retract(player_atk(Atk)),
    assert(player_atk(NewAtk)),
    format('~n[ITEM] You equipped an Iron Sword! Attack +10 (Total: ~w)~n', [NewAtk]).
