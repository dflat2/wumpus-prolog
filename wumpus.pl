%
library(tty).

% Messages.
message(observation(stench)) :- 
    write_ln('[!] It''s smelly there, the Wumpus might be nearby...').

message(observation(breeze)) :- write_ln('[!] There''s a pit around there.').
message(observation(glitter)) :- write_ln('Are you walking on gold?').
message(observation(bump)) :- write_ln('Ouch! That was a wall!').

message(observation(scream)) :-
    write_ln('* Aaaargh! *'),
    write_ln('Yes! You killed the Wumpus!').

message(observation(forward)) :-
    write_ln('You stepped forward.').

message(observation(turn(left))) :-
    write_ln('You turned 90 degrees left.').

message(observation(turn(right))) :-
    write_ln('You turned 90 degrees right.').

message(observation(grab)) :-
    write_ln('You grabbed some gold.').

message(observation(shot)) :-
    write_ln('You just shot your only arrow.').

message(game_over) :- write_ln('Game over.').

message(status(lose(fell_in_pit))) :-
    write_ln('Ahhhhhhhh!'),
    write_ln('You fell in a pit and died.').

message(status(lose(eaten_by_wumpus))) :-
    write_ln('* Yum yum *'),
    write_ln('What was that?'),
    write_ln('You got eaten by the Wumpus and died.').

message(status(lose(climb_with_no_gold))) :-
    write_ln('You exited the cave without gold.').

message(status(win)) :-
    write_ln('You win, congratulations!').

message(action(turn(left))) :- write_ln('Turn 90 degrees left.'). 
message(action(turn(right))) :- write_ln('Turn 90 degrees right.'). 
message(action(forward)) :- write_ln('Move one step forward.'). 
message(action(grab)) :- write_ln('Grab gold.'). 
message(action(shot)) :- write_ln('Shoot an arrow (you have only one).'). 
message(action(climb)) :- write_ln('Climb the ladders and end the game.'). 

message(wumpus_game(position(X, Y), direction(Direction), _, _, _)) :-
    write('Your position on the grid is ('), write(X), write(', '), write(Y), write(').'),
    write_ln(''),
    write('You are facing '), write(Direction), write('.'),
    write_ln('').

actions_choice([], _).
actions_choice([Action | Rest], N) :-
    write('('), write(N), write(') '),
    message(Action),
    M is N + 1,
    actions_choice(Rest, M).

% Map description
pit(position(3, 1)).
pit(position(1, 4)).
pit(position(4, 4)).

wumpus(position(4, 2)).

gold(position(4, 1)).

ladders(position(1,1)).

% List of all actions
action(turn(left)).
action(turn(right)).
action(forward).
action(grab).
action(shot).
action(climb).

% List of all game status
status(lose(fell_in_pit)).
status(lose(eaten_by_wumpus)).
status(lose(climb_with_no_gold)).
status(ongoing).
status(win).

% List of all possible observations
observation(forward).
observation(turn(left)).
observation(turn(right)).
observation(grab).
observation(shot).
observation(stench).
observation(breeze).
observation(glitter).
observation(bump).
observation(scream).

% Predicates for moving
neighbor(position(X1, Y1), position(X2, Y2)) :-
    X2 is X1,
    Y2 is Y1 + 1.

neighbor(position(X1, Y1), position(X2, Y2)) :-
    Y2 is Y1,
    X2 is X1 + 1.

neighbor(position(X1, Y1), position(X2, Y2)) :-
    X2 is X1,
    Y2 is Y1 - 1.

neighbor(position(X1, Y1), position(X2, Y2)) :-
    Y2 is Y1,
    X2 is X1 - 1.

findall_neighbors(C, L) :-
    findall(CN, neighbor(C, CN), L).

in_bounds(position(X, Y)) :-
    1 =< X,
    X =< 4,
    1 =< Y,
    Y =< 4.

go(position(X1, Y), direction(up), position(X2, Y)) :-
    X2 is X1 - 1, in_bounds(position(X2, Y)), !.

go(Position, direction(up), Position).

go(position(X1, Y), direction(down), position(X2, Y)) :-
    X2 is X1 + 1, in_bounds(position(X2, Y)), !.

go(Position, direction(down), Position).

go(position(X, Y1), direction(left), position(X, Y2)) :-
    Y2 is Y1 - 1, in_bounds(position(X, Y2)), !.

go(Position, direction(left), Position).

go(position(X, Y1), direction(right), position(X, Y2)) :-
    Y2 is Y1 + 1, in_bounds(position(X, Y2)), !.

go(Position, direction(right), Position).

targets_wumpus(Position, Direction) :-
    go(Position, Direction, NewPosition),
    wumpus(NewPosition), !.

targets_wumpus(Position, Direction) :-
    go(Position, Direction, NewPosition),
    Position \= NewPosition,
    targets_wumpus(NewPosition, Direction).

only_pits([], []).

only_pits([Pit | Rest], [Pit | Rest2]) :-
    pit(Pit), !,
    only_pits(Rest, Rest2).

only_pits([_ | Rest], Pits) :-
    only_pits(Rest, Pits).

% Should return only one wumpus
only_wumpus([], []).

only_wumpus([Wumpus | Rest], [Wumpus | Rest2]) :-
    wumpus(Wumpus), !,
    only_wumpus(Rest, Rest2).

only_wumpus([_ | Rest], Wumpus) :-
    only_wumpus(Rest, Wumpus).

% Actions.

% Transition functions
% All of them are of the form transition(S, A, S')

% turn(left) and turn(right).
rotate(direction(up), turn(left), direction(left)).
rotate(direction(left), turn(left), direction(down)).
rotate(direction(down), turn(left), direction(right)).
rotate(direction(right), turn(left), direction(up)).

rotate(direction(up), turn(right), direction(right)).
rotate(direction(left), turn(right), direction(up)).
rotate(direction(down), turn(right), direction(left)).
rotate(direction(right), turn(right), direction(down)).

transition(
    wumpus_game(Position, Direction1, Wumpus, Gold, Arrow),
    action(turn(X)),
    wumpus_game(Position, Direction2, Wumpus, Gold, Arrow)
) :-
    rotate(Direction1, turn(X), Direction2).

% grab
transition(
    wumpus_game(Position, Direction, Wumpus, gold(no), Arrow),
    action(grab),
    wumpus_game(Position, Direction, Wumpus, gold(yes), Arrow)
).

% forward
transition(
    wumpus_game(Position1, Direction, Wumpus, Gold, Arrow),
    action(forward),
    wumpus_game(Position2, Direction, Wumpus, Gold, Arrow)
) :-
    go(Position1, Direction, Position2).

% shot
transition(
    wumpus_game(Position, Direction, wumpus(alive), Gold, arrow(yes)),
    action(shot),
    wumpus_game(Position, Direction, wumpus(dead), Gold, array(no))
) :-
    targets_wumpus(Position, Direction), !.

transition(
    wumpus_game(Position, Direction, Wumpus, Gold, arrow(yes)),
    action(shot),
    wumpus_game(Position, Direction, Wumpus, Gold, arrow(no))
).

% climb
transition(
    wumpus_game(_, Direction, Wumpus, Gold, Arrow),
    action(climb),
    wumpus_game(position(0,1), Direction, Wumpus, Gold, Arrow)
).

% preconditions for actions

% It is always possible to go forward, if there is a wall in front of you it does nothing
precondition(action(forward), _).

% It is always possible to turn around
precondition(action(turn(left)), _).
precondition(action(turn(right)), _).

% To grab gold, there must be gold where you are
precondition(
    action(grab),
    wumpus_game(Position, _, _, gold(no), _)
) :-
    gold(Position).

% You need an arrow to shot it
precondition(action(shot), wumpus_game(_, _, _, _, arrow(yes))).

% To climb ladders, you need ladders
precondition(action(climb), wumpus_game(Position, _, _, _, _)) :-
    ladders(Position).

% Observation functions, of the form observe(S, A, S', O)
observe(_, action(forward), _, observation(forward)).
observe(_, action(turn(left)), _, observation(turn(left))).
observe(_, action(turn(right)), _, observation(turn(right))).
observe(_, action(grab), _, observation(grab)).
observe(_, action(shot), _, observation(shot)).

% The wumpus is smelly on a four-neighborhood
observe(
    _,
    _,
    wumpus_game(Position, _, wumpus(alive), _, _),
    observation(stench)
) :-
    findall_neighbors(Position, N),
    only_wumpus(N, W),
    member(_, W).

% There is breeze around holes
observe(
    _,
    _,
    wumpus_game(Position, _, _, _, _),
    observation(breeze)
) :-
    findall_neighbors(Position, N),
    only_pits(N, Pits),
    member(_, Pits).

% Walking on gold
observe(
    _,
    _,
    wumpus_game(Position, _,  _, gold(no), _),
    observation(glitter)
) :-
    gold(Position).

% Bump observation when hitting a wall
observe(
    wumpus_game(Position, _, _, _, _),
    action(forward),
    wumpus_game(Position, _, _, _, _),
    observation(bump)
).

% Wumpus dies
observe(
    wumpus_game(_, _, wumpus(alive), _, _),
    _,
    wumpus_game(_, _, wumpus(dead), _, _),
    observation(scream)
).

% No more arrows
observe(
    wumpus_game(_, _, _, _, arrow(yes)),
    _,
    wumpus_game(_, _, wumpus(dead), _, arrow(no)),
    observation(no_more_arrow)
).

% End game conditions
ended(wumpus_game(position(0, 1), _, _, gold(yes), _), status(win)).
ended(wumpus_game(position(0, 1), _, _, gold(no), _), status(lose(climb_with_no_gold))).

ended(wumpus_game(Position, _, _, _, _), status(lose(fell_in_pit))) :-
    pit(Position).

ended(
    wumpus_game(
        Position,
        _,
        wumpus(alive),
        _,
        _
    ), 
    status(lose(eaten_by_wumpus))
) :-
    wumpus(Position).

initial_state(
    wumpus_game(
        position(1, 1),
        direction(right),
        wumpus(alive),
        gold(no),
        arrow(yes)
    )
).

findall_possible_actions(Game, L) :-
    findall(Action, precondition(Action, Game), L).

show_observations([]).
show_observations([Observation | Rest]) :-
    message(Observation),
    show_observations(Rest).

prompt_action(Actions, Action) :-
    write_ln('Please provide an action among the following.'),
    actions_choice(Actions, 1),
    read_line_to_string(user_input, ActionNumberString),
    number_codes(ActionNumberInteger, ActionNumberString),
    ActionIndexInteger is ActionNumberInteger - 1,
    nth0(ActionIndexInteger, Actions, Action),
    !.

% Ask for action again if user didn't provide a valid input
prompt_action(Actions, Action) :-
    write_ln(''),
    prompt_action(Actions, Action).

iterate(Game) :-
    ended(Game, Status), !,
    message(Status),
    message(game_over).

iterate(Game) :-
    findall_possible_actions(Game, Actions),
    prompt_action(Actions, Action),
    tty_clear,
    transition(Game, Action, NewGame),
    findall(O, observe(Game, Action, NewGame, O), Observations),
    sleep(0.5),
    show_observations(Observations),
    nl, message(NewGame), nl,
    iterate(NewGame).

manual :-
    write_ln('Welcome to Wumpus!'),
    write_ln('- You are in a 4x4 grid. This is a cave where a dangerous Wumpus lives.'),
    write_ln('- Fortunately, the Wumpus cannot move. But you don''t know where it is...'),
    write_ln('- Your mission: find the gold and get yourself out of here.'),
    write_ln('- Beware: you have no light, and only one arrow (if you ever dare to shot...).'),
    write_ln('- You start in the top left corner of the grid, facing right.'),
    write_ln('- Good luck, and watch your steps, there are holes!'),
    write_ln('').

start :-
    tty_clear,
    manual,
    initial_state(InitialState),
    iterate(InitialState).
