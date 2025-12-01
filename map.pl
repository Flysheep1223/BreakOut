:- dynamic wall/2, location/3, game_over/0, health/1.
:- use_module(library(readutil)).

map_size(60, 34).
exit_pos(50, 32).

health_zone(46, 14, 56, 20).
health_zone(6, 26, 16, 32).

map_segment([22, 4], [22, 6]).
map_segment([22, 6], [38, 6]).
map_segment([38, 6], [38, 4]).
map_segment([24, 14], [24, 10]).
map_segment([24, 10], [28, 10]).
map_segment([32, 10], [36, 10]).
map_segment([36, 10], [36, 14]).
map_segment([36, 18], [36, 22]).
map_segment([36, 22], [24, 22]).
map_segment([24, 22], [24, 18]).
map_segment([44, 4], [44, 8]).
map_segment([48, 2], [48, 8]).
map_segment([52, 2], [52, 8]).
map_segment([56, 2], [56, 8]).
map_segment([16, 12], [4, 12]).
map_segment([4, 12], [4, 4]).
map_segment([4, 4], [16, 4]).
map_segment([16, 4], [16, 12]).
map_segment([4, 22], [18, 22]).
map_segment([18, 22], [18, 16]).
map_segment([18, 16], [4, 16]).
map_segment([4, 16], [4, 22]).
map_segment([32, 26], [36, 26]).
map_segment([36, 26], [36, 30]).
map_segment([28, 26], [24, 26]).
map_segment([24, 26], [24, 34]).
map_segment([56, 34], [56, 30]).
map_segment([56, 30], [50, 30]).
map_segment([50, 30], [50, 26]).
map_segment([50, 26], [60, 26]).
map_segment([0, 0], [0, 34]).
map_segment([0, 34], [60, 34]).
map_segment([60, 34], [60, 0]).
map_segment([60, 0], [0, 0]).

fill_horizontal_line(X1, Y, X2) :-
    min_max(X1, X2, X_Min, X_Max),
    between(X_Min, X_Max, X),
    assertz(wall(X, Y)),
    fail.
fill_horizontal_line(_, _, _).

fill_vertical_line(X, Y1, Y2) :-
    min_max(Y1, Y2, Y_Min, Y_Max),
    between(Y_Min, Y_Max, Y),
    assertz(wall(X, Y)),
    fail.
fill_vertical_line(_, _, _).

min_max(A, B, Min, Max) :-
    (A =< B -> Min = A, Max = B ; Min = B, Max = A).

generate_walls :-
    retractall(wall(_, _)),
    map_segment([X1, Y1], [X2, Y2]),
    (   Y1 = Y2 -> fill_horizontal_line(X1, Y1, X2)
    ;   X1 = X2 -> fill_vertical_line(X1, Y1, Y2)
    ;   true
    ),
    fail.
generate_walls :-
    format('~nWall generation complete.~n').

show_map :-
    (   location(player, PlayerX, PlayerY), health(H)
    ->  format('~n[HEALTH: ~w]~n', [H]),
        draw_map(PlayerX, PlayerY)
    ;   format('~nERROR: Player position is not set. Run "start."~n')
    ).

start :-
    generate_walls,
    retractall(game_over),
    retractall(location(player, _, _)),
    retractall(health(_)),
    assert(health(100)),
    assert(location(player, 30, 2)),
    format('~nGame started. Player spawned at (30, 2).~n'),
    format('Initial Health: 100~n'),
    format('Your Exit is at (50, 32), marked by "$".~n'),
    format('Recovery Zones are marked by H.~n'), 
    format('Commands: WASD to move, Q to quit. (Real-time mode)~n'),
    show_map,
    game_loop.

w :- move(up).
s :- move(down).
a :- move(left).
d :- move(right).
q :- handle_choice(e).

game_loop :-
    game_over, !.
game_loop :-
    get_single_char(Code),
    handle_input(Code),
    game_loop.

handle_input(119) :- move(up), !.   % w
handle_input(87)  :- move(up), !.   % W
handle_input(115) :- move(down), !. % s
handle_input(83)  :- move(down), !. % S
handle_input(97)  :- move(left), !. % a
handle_input(65)  :- move(left), !. % A
handle_input(100) :- move(right), !. % d
handle_input(68)  :- move(right), !. % D
handle_input(113) :- handle_choice(e). % q
handle_input(81)  :- handle_choice(e). % Q
handle_input(_).


is_wall(X, Y) :- wall(X, Y).

get_health_zone_char(X, Y, Char) :-
    health_zone(X_Min, Y_Min, X_Max, Y_Max),
    X >= X_Min, X =< X_Max,
    Y >= Y_Min, Y =< Y_Max,
    Char = 'H',
    !.

print_map_char(X, Y, X, Y) :- 
    format('@'), !.

print_map_char(X, Y, _, _) :- 
    exit_pos(X, Y),
    format('$'), !.

print_map_char(X, Y, _, _) :- 
    get_health_zone_char(X, Y, Char), 
    format('~w', [Char]), 
    !.

print_map_char(X, Y, _, _) :- 
    wall(X, Y),
    (   (is_wall(X, Y+1); is_wall(X, Y-1)),
        \+ (is_wall(X+1, Y); is_wall(X-1, Y)) -> format('|')
    ;   (is_wall(X+1, Y); is_wall(X-1, Y)),
        \+ (is_wall(X, Y+1); is_wall(X, Y-1)) -> format('-')
    ;   format('+')
    ), !.

print_map_char(_, _, _, _) :- 
    format('.'), !.

draw_map(PlayerX, PlayerY) :-
    map_size(MaxX, MaxY),
    format('~n+--- Map (Current Position: @) ---+~n'),
    between(0, MaxY, Y_index),
    Y is MaxY - Y_index,
    between(0, MaxX, X),
    print_map_char(X, Y, PlayerX, PlayerY),
    (   X = MaxX -> format('~n')
    ;   true
    ),
    fail.
draw_map(_, _) :- 
    format('+----------------------------------+~n').

ask_restart :-
    format('Would you like to restart the game or exit? (R/E): '),
    read_line_to_string(user_input, ChoiceRaw),
    (   ChoiceRaw = "" ->
            read_line_to_string(user_input, Choice)
    ;   Choice = ChoiceRaw
    ),
    normalize_choice(Choice, Normalized),
    handle_choice(Normalized),
    !.

normalize_choice(C, N) :-
    string_lower(C, L),
    (L = "r" -> N = r ; L = "e" -> N = e ; N = invalid).

handle_choice(r) :-
    format('\n--- Performing Full Game Restart ---\n\n'),
    start,
    !.

handle_choice(e) :-
    format('\nThank you for playing! Goodbye.\n'),
    halt.

handle_choice(_) :-
    format('Invalid choice. Please enter R or E.\n'),
    ask_restart.

decrease_health :-
    health(H),
    H > 0,
    H_New is H - 1,
    retract(health(H)),
    assert(health(H_New)),
    (H_New = 0 -> end_game_low_health ; true).

end_game_low_health :-
    \+ game_over,
    format('~n!!! HEALTH DEPLETED !!!~n'),
    format('*** GAME OVER: Your health reached 0. ***~n'),
    end_game.

restore_health :-
    location(player, X, Y),
    health_zone(X_Min, Y_Min, X_Max, Y_Max),
    X >= X_Min, X =< X_Max,
    Y >= Y_Min, Y =< Y_Max,
    !, 
    health(H),
    H_Max = 100, 
    Recovery = 5,
    H_Candidate is H + Recovery,
    (H_Candidate > H_Max -> H_New = H_Max ; H_New = H_Candidate),
    (H_New > H -> 
        retract(health(H)),
        assert(health(H_New)),
        format('~n[Recovery!] +5 HP (Current: ~w/~w)~n', [H_New, H_Max])
    ;
        true
    ).
restore_health.

update_location(NewX, NewY) :-
    \+ wall(NewX, NewY),
    retract(location(player, _, _)),
    assert(location(player, NewX, NewY)),
    restore_health, 
    check_exit(NewX, NewY),
    !.
update_location(_, _) :- 
    fail.

move(_) :- 
    game_over,
    !,
    format('~n!!! Game Over! You cannot move further. !!!~n'),
    fail.

move(Direction) :-
    location(player, X, Y),
    map_size(MaxX, MaxY),
    (   Direction = up,     Y1 is Y + 1, Y1 =< MaxY, update_location(X, Y1)
    ;   Direction = down,   Y1 is Y - 1, Y1 >= 0, update_location(X, Y1)
    ;   Direction = left,   X1 is X - 1, X1 >= 0, update_location(X1, Y)
    ;   Direction = right,  X1 is X + 1, X1 =< MaxX, update_location(X1, Y)
    ),
    location(player, NewX, NewY),
    format('~nYou moved to (~w, ~w).~n', [NewX, NewY]),
    show_map, 
    !. 

move(_) :- 
    format('~nCannot move in that direction (Invalid direction, blocked, or out of bounds)!~n'),
    show_map.

tp(_, _) :- 
    game_over,
    !,
    format('~n!!! Game Over! Cannot teleport. !!!~n'),
    fail.

tp(NewX, NewY) :-
    map_size(MaxX, MaxY),
    NewX >= 0, NewX =< MaxX,
    NewY >= 0, NewY =< MaxY,
    update_location(NewX, NewY),
    format('~n--- TELEPORT SUCCESSFUL ---~n'),
    location(player, NewX_Actual, NewY_Actual),
    format('You are now at (~w, ~w).~n', [NewX_Actual, NewY_Actual]),
    show_map, 
    !. 

tp(NewX, NewY) :- 
    format('~n--- TELEPORT FAILED ---~n'),
    format('Target (~w, ~w) is outside boundaries or occupied by a wall.~n', [NewX, NewY]),
    show_map. 

check_exit(X, Y) :- 
    exit_pos(X, Y),
    \+ game_over,
    format('~n**************************************************~n'),
    format('*** CONGRATULATIONS! You found the exit at (~w, ~w)! ***~n', [X, Y]),
    format('**************************************************~n'),
    end_game.

check_exit(_, _) :- 
    true.

end_game :- 
    \+ game_over,
    assert(game_over),
    ask_restart.
