% name surname
% studentid
% compiling: no
% complete: no
:- ['cmpecraft.pro'].

:- init_from_map.

% 10 points
% manhattan_distance(+A, +B, -Distance) :- .
manhattan_distance(A, B, Distance) :-
    [X1, Y1] = A,
    [X2, Y2] = B,
    Distance is abs(X1-X2) + abs(Y1-Y2).


% 10 points
% minimum_of_list(+List, -Minimum) :- .

minimum_of_list_helper([], Minimum, Minimum).

minimum_of_list_helper([H|T], Temp, Minimum) :-
    H<Temp,!,
    minimum_of_list_helper(T, H, Minimum).

minimum_of_list_helper([_|T], Temp, Minimum) :-
    minimum_of_list_helper(T, Temp, Minimum).

minimum_of_list(List, Minimum) :-
    [H|T] = List,
    minimum_of_list_helper([H|T], H, Minimum).



% 10 points
% find_nearest_type(+State, +ObjectType, -ObjKey, -Object, -Distance) :- .

find_nearest_type(State, ObjectType, ObjKey, Object, Distance) :- 
    find_nearest_type_helper(State, ObjectType, [], _, _, ObjKey, Object, Distance).

% creates a list -ObjectList- that contains all the objects of the given type
% calls create_dist_list and get_the_nearest
find_nearest_type_helper([AgentDict, ObjectDict, _], ObjectType, TempList, DistList, ObjectList, ObjKey, Object, Min):-
    findall(X, ObjectDict.X.type=ObjectType, ObjectList),
    get_dict(x, AgentDict, X1),
    get_dict(y, AgentDict, Y1),
    create_dist_list([X1, Y1], ObjectDict, ObjectList, TempList, DistList),
    get_the_nearest(DistList, ObjectList, ObjectDict, ObjKey, Object, Min).

% calculates the manhattan distances of the objects in the list and creates a new list of distances
create_dist_list(_, _, [], TempList, DistList) :-
    DistList = TempList.

create_dist_list(AgentLoc, ObjectDict, [H|T], TempList, DistList):-
    get_dict(H, ObjectDict, Object),
    get_dict(x, Object, X2),
    get_dict(y, Object, Y2),
    manhattan_distance(AgentLoc, [X2, Y2], Dist),
    append(TempList, [Dist], NewDistList),
    create_dist_list(AgentLoc, ObjectDict, T, NewDistList, DistList).

% finds the object with the minimum distance
get_the_nearest(DistList, ObjectList, ObjectDict, ObjKey, Object, Min) :-
    minimum_of_list(DistList, Min),
    get_object_infos(DistList, ObjectList, ObjectDict, ObjKey, Object, Min).

get_object_infos([H|_], [H2|_], ObjectDict, ObjKey, Object, MinDist) :-
    H = MinDist, !,
    ObjKey is H2,
    get_dict(ObjKey, ObjectDict, Object).

get_object_infos([_|T], [_|T2], ObjectDict, ObjKey, Object, MinDist) :-
    get_object_infos(T, T2, ObjectDict, ObjKey, Object, MinDist).
    


% 10 points
% navigate_to(+State, +X, +Y, -ActionList, +DepthLimit) :- .
navigate_to(State, X, Y, ActionList, DepthLimit) :-
    [AgentDict, _, _] = State,
    get_dict(x, AgentDict, AgentX),
    get_dict(y, AgentDict, AgentY),
    add_action(AgentX, AgentY, X, Y, [], 0, DepthLimit, ActionList).

% if the agent is in the given coordinates, the ActionList is complete
add_action(AgentX, AgentY, X, Y, Actions, Count, DepthLimit, ActionList) :-
    DepthLimit >= Count,
    AgentX = X,
    AgentY = Y,
    ActionList = Actions.

% if the agent is in the left of the given coordinat, s/he must go right
add_action(AgentX, AgentY, X, Y, Actions, Count, DepthLimit, ActionList) :-
    DepthLimit >= Count,
    AgentX < X,
    append(Actions, [go_right], NewActions),
    NewAgentX is AgentX+1,
    NewCount is Count+1,
    add_action(NewAgentX, AgentY, X, Y, NewActions, NewCount, DepthLimit, ActionList).

% if the agent is in the right of the given coordinat, s/he must go left
add_action(AgentX, AgentY, X, Y, Actions, Count, DepthLimit, ActionList) :-
    DepthLimit >= Count,
    AgentX > X,
    append(Actions, [go_left], NewActions),
    NewAgentX is AgentX-1,
    NewCount is Count+1,
    add_action(NewAgentX, AgentY, X, Y, NewActions, NewCount, DepthLimit, ActionList).

add_action(AgentX, AgentY, X, Y, Actions, Count, DepthLimit, ActionList) :-
    DepthLimit >= Count,
    AgentY < Y,
    append(Actions, [go_down], NewActions),
    NewAgentY is AgentY+1,
    NewCount is Count+1,
    add_action(AgentX, NewAgentY, X, Y, NewActions, NewCount, DepthLimit, ActionList).

add_action(AgentX, AgentY, X, Y, Actions, Count, DepthLimit, ActionList) :-
    DepthLimit >= Count,
    AgentY > Y,
    append(Actions, [go_up], NewActions),
    NewAgentY is AgentY-1,
    NewCount is Count+1,
    add_action(AgentX, NewAgentY, X, Y, NewActions, NewCount, DepthLimit, ActionList).
    



% 10 points
% chop_nearest_tree(+State, -ActionList) :- .
chop_nearest_tree(State, ActionList) :- 
    find_nearest_type(State, tree, _, Object, Distance),
    get_dict(x, Object, ObjX), get_dict(y, Object, ObjY),
    navigate_to(State, ObjX, ObjY, Actions, Distance),
    append(Actions, [left_click_c, left_click_c, left_click_c, left_click_c], ActionList).

% 10 points
% mine_nearest_stone(+State, -ActionList) :- .
mine_nearest_stone(State, ActionList) :- 
    find_nearest_type(State, stone, _, Object, Distance),
    get_dict(x, Object, ObjX), get_dict(y, Object, ObjY),
    navigate_to(State, ObjX, ObjY, Actions, Distance),
    append(Actions, [left_click_c, left_click_c, left_click_c, left_click_c], ActionList).


% 10 points
% gather_nearest_food(+State, -ActionList) :- .
gather_nearest_food(State, ActionList) :- 
    find_nearest_type(State, food, _, Object, Distance),
    get_dict(x, Object, ObjX), get_dict(y, Object, ObjY),
    navigate_to(State, ObjX, ObjY, Actions, Distance),
    append(Actions, [left_click_c], ActionList).


% 10 points
% collect_requirements(+State, +ItemType, -ActionList) :- .


%item_info(stick, reqs{log: 2}, 4).
%item_info(stone_pickaxe, reqs{log: 3, stick: 2, cobblestone: 3}, 100).
%item_info(stone_axe, reqs{log: 3, stick: 2, cobblestone: 3}, 100).

get_needs(State, stick, ActionList, NewCollectActions) :-
    chop_nearest_tree(State, Actions),
    append(ActionList, Actions, NewCollectActions).

get_needs(State, stone_pickaxe, ActionList, NewCollectActions) :-
    chop_nearest_tree(State, Actions),
    append(ActionList, Actions, ActionList2),
    execute_actions(State, Actions, NextState),
    chop_nearest_tree(NextState, Actions2),
    append(ActionList2, Actions2, ActionList3),
    append(ActionList3, [craft_stick], ActionList4),
    execute_actions(NextState, Actions2, NextState2),
    mine_nearest_stone(NextState2, Actions3),
    append(ActionList4, Actions3, NewCollectActions).

get_needs(State, stone_axe, ActionList, NewCollectActions) :-
    chop_nearest_tree(State, Actions),
    append(ActionList, Actions, ActionList2),
    execute_actions(State, Actions, NextState),
    chop_nearest_tree(NextState, Actions2),
    append(ActionList2, Actions2, ActionList3),
    append(ActionList3, [craft_stick], ActionList4),
    execute_actions(NextState, Actions2, NextState2),
    mine_nearest_stone(NextState2, Actions3),
    append(ActionList4, Actions3, NewCollectActions).


check_if_collect_necessary([AgentDict, _, _], stick, ActionList) :-
    has(log, 2, AgentDict.inventory),
    ActionList = [].

check_if_collect_necessary([AgentDict, _, _], stone_pickaxe, ActionList) :-
    has(log, 3, AgentDict.inventory),
    has(stick, 2, AgentDict.inventory),
    has(cobblestone, 3, AgentDict.inventory),
    ActionList = [].

check_if_collect_necessary([AgentDict, _, _], stone_axe, ActionList) :-
    has(log, 3, AgentDict.inventory),
    has(stick, 2, AgentDict.inventory),
    has(cobblestone, 3, AgentDict.inventory),
    ActionList = [].

check_and_get_needs(State, stick, ActionList) :-
    check_if_collect_necessary(State, stick, ActionList).

check_and_get_needs(State, stick, ActionList) :-
    get_needs(State, stick, [], ActionList).

check_and_get_needs(State, stone_pickaxe, ActionList) :-
    check_if_collect_necessary(State, stone_pickaxe, ActionList).

check_and_get_needs(State, stone_pickaxe, ActionList) :-
    get_needs(State, stone_pickaxe, [], ActionList).

check_and_get_needs(State, stone_axe, ActionList) :-
    check_if_collect_necessary(State, stone_axe, ActionList).

check_and_get_needs(State, stone_axe, ActionList) :-
    get_needs(State, stone_axe, [], ActionList).

collect_requirements(State, ItemType, ActionList) :- 
    check_and_get_needs(State, ItemType, ActionList).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%item_info(stick, reqs{log: 2, stick: 0, cobblestone:0}, 4).
%required(stick, [tree-1, stone-0]).
%required(stone_pickaxe, [tree-2, stone-1]).
%required(stone_axe, [tree-2, stone-1]).
%
%collect_requirements(State, ItemType, ActionList) :- 
%    [AgentDict, _, _] = State,
%    check_and_get_needs(State, ItemType, AgentDict.inventory, ActionList).
%
%%check_if_collect_necessary([AgentDict, _, _], ItemType, ActionList) :-
%    item_info(ItemType, Reqs, _),
%    get_dict(log, Reqs, LogVal),
%    get_dict(stick, Reqs, StickVal),
%    get_dict(cobblestone, Reqs, CobblestoneVal),
%    has(log, LogVal, AgentDict.inventory),
%    has(stick, StickVal, AgentDict.inventory),
%    has(cobblestone, CobblestoneVal, AgentDict.inventory),
%    ActionList = [].
%
%%check_and_get_needs(State, ItemType, Inv, Actions) :-
%    check_if_collect_necessary(State, ItemType, Actions).
%
%%check_and_get_needs(State, ItemType, Inv, Actions) :-
%    required(ItemType, [_-TreeVal, _-StoneVal]),
%    get_needs(State, TreeVal, StoneVal, [], WholeActions),
%    ItemType = stick, !,
%    Actions = WholeActions.
%
%%check_and_get_needs(State, ItemType, Inv, Actions) :-
%    required(ItemType, [_-TreeVal, _-StoneVal]),
%    get_needs(State, TreeVal, StoneVal, [], WholeActions),
%    craft_stick(State, NextState),
%    craft_stick(NextState, FinalState),
%    State = FinalState,
%    Actions = WholeActions.
%
%%get_needs(State, 0, 0, CollectActions, WholeActions):-
%    WholeActions=CollectActions.
%
%get_needs(State, NumTree, NumStone, CollectActions, WholeActions):-
%    NumTree>0,
%    chop_nearest_tree(State, Actions),
%    append(CollectActions, Actions, NewCollectActions),
%    execute_actions(State, Actions, NextState),
%    NewNumTree is NumTree-1,
%    get_needs(NextState, NewNumTree, NumStone, NewCollectActions, WholeActions).
%
%get_needs(State, NumTree, NumStone, CollectActions, WholeActions):-
%    NumStone>0,
%    mine_nearest_stone(State, Actions),
%    append(CollectActions, Actions, NewCollectActions),
%    execute_actions(State, Actions, NextState),
%    NewNumStone is NumStone-1,
%    get_needs(NextState, NumTree, NewNumStone, NewCollectActions, WholeActions).

% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .

my_tile_occupied(X, Y, State) :-
    State = [_, StateDict, _],
    get_dict(_, StateDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
    X = Ox, Y = Oy.

find_castle_location_helper(State, X, Y, Width, Height, XMin, YMin) :-
    X<Width-1, Y<Height-1,
    X1 is X+1, X2 is X+2, Y1 is Y+1, Y2 is Y+2,
    not(my_tile_occupied(X, Y, State)), not(my_tile_occupied(X1, Y, State)), not(my_tile_occupied(X2, Y, State)), 
    not(my_tile_occupied(X, Y1, State)), not(my_tile_occupied(X1, Y1, State)), not(my_tile_occupied(X2, Y1, State)),
    not(my_tile_occupied(X, Y2, State)), not(my_tile_occupied(X1, Y2, State)), not(my_tile_occupied(X2, Y2, State)),
    XMin = X, YMin = Y.
 
walk_through_the_map(State, X, Y, Width, Height, XMin, YMin) :-
    find_castle_location_helper(State, X, Y, Width, Height, XMin, YMin) -> true; (X1 is X+1, find_castle_location_helper(State, X1, Y, Width, Height, XMin, YMin) -> true; (Y1 is Y+1,find_castle_location_helper(State, X, Y1, Width, Height, XMin, YMin))).

walk_through_the_map(State, X, Y, Width, Height, XMin, YMin) :-
    X<Width-1, Y<Height-1,
    X1 is X+1, Y1 is Y+1,
    walk_through_the_map(State, X1, Y1, Width, Height, XMin, YMin).


find_castle_location(State, XMin, YMin, XMax, YMax) :-
    width(W), Width is W-2,
    height(H), Height is H-2,
    walk_through_the_map(State, 1, 1, Width, Height, K, L),
    XMin is K, YMin is L,
    XMax is XMin+2, YMax is YMin+2.

% 15 points
% make_castle(+State, -ActionList) :- .
