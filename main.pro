% zeynep buse aydin
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

% if the agent is in the left of the given coordinat, adds go_right to the ActionList
add_action(AgentX, AgentY, X, Y, Actions, Count, DepthLimit, ActionList) :-
    DepthLimit >= Count,
    AgentX < X,
    append(Actions, [go_right], NewActions),
    NewAgentX is AgentX+1,
    NewCount is Count+1,
    add_action(NewAgentX, AgentY, X, Y, NewActions, NewCount, DepthLimit, ActionList).

% if the agent is in the right of the given coordinat, adds go_left to the ActionList
add_action(AgentX, AgentY, X, Y, Actions, Count, DepthLimit, ActionList) :-
    DepthLimit >= Count,
    AgentX > X,
    append(Actions, [go_left], NewActions),
    NewAgentX is AgentX-1,
    NewCount is Count+1,
    add_action(NewAgentX, AgentY, X, Y, NewActions, NewCount, DepthLimit, ActionList).

% if the agent is above the given coordinat, adds go_down to the ActionList
add_action(AgentX, AgentY, X, Y, Actions, Count, DepthLimit, ActionList) :-
    DepthLimit >= Count,
    AgentY < Y,
    append(Actions, [go_down], NewActions),
    NewAgentY is AgentY+1,
    NewCount is Count+1,
    add_action(AgentX, NewAgentY, X, Y, NewActions, NewCount, DepthLimit, ActionList).

% if the agent is below the given coordinat, adds go_up to the ActionList
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

% this predicate is used in collect_requirements
mine_nearest_cobblestone(State, ActionList) :- 
    find_nearest_type(State, cobblestone, _, Object, Distance),
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

% "mine" is the general process of getting 3 cobblestones
% this can be done by either mining 1 stone or 3 cobblestones
mine(State, Actions) :-
    (mine_nearest_stone(State, WillAppendActions); (
        mine_nearest_cobblestone(State, Actions1),
        execute_actions(State, Actions1, NextState),
        mine_nearest_cobblestone(NextState, Actions2),
        execute_actions(NextState, Actions2, NextState2),
        append(Actions1, Actions2, Actions3),
        mine_nearest_cobblestone(NextState2, Actions4),
        append(Actions3, Actions4, WillAppendActions)
        )
    ),
    append([], WillAppendActions, Actions).

% for stick agent must have 2 logs, hence s/he must chop a tree
get_needs(State, stick, ActionList, NewCollectActions) :-
    chop_nearest_tree(State, Actions),
    append(ActionList, Actions, NewCollectActions).

% for stone_pickaxe, agent must chop 2 trees, craft a stick and then collect 3 cobblestones
% for cobblestones, the agent can either mine 1 stone or mine 3 cobblestones
get_needs(State, ItemType, ActionList, NewCollectActions) :-
    (ItemType = stone_axe; ItemType = stone_pickaxe),
    chop_nearest_tree(State, Actions),
    append(ActionList, Actions, ActionList2),
    execute_actions(State, Actions, NextState),
    chop_nearest_tree(NextState, Actions2),
    append(ActionList2, Actions2, ActionList3),
    append(ActionList3, [craft_stick], ActionList4),
    execute_actions(NextState, Actions2, NextState2),
    mine(NextState2, Actions3),
    append(ActionList4, Actions3, NewCollectActions).

% for a castle, 9 cobblestones are needed
% these cobblestones can be collected by mining 3 stones or 9 cobblestones
get_needs(State, castle, ActionList, NewCollectActions) :-
    mine(State, Actions1),
    append(ActionList, Actions1, TempActions),
    execute_actions(State, Actions1, NextState),
    mine(NextState, Actions2),
    append(TempActions, Actions2, TempActions2),
    execute_actions(NextState, Actions2, FinalState),
    mine(FinalState, Actions3),
    append(TempActions2, Actions3, NewCollectActions).

% if there are 2 logs in the inventory, there is no need to collect anything for a stick
% so the ActionList is empty
collect_not_necessary([AgentDict, _, _], stick, ActionList) :-
    has(log, 2, AgentDict.inventory),
    ActionList = [].

% if there are 3 logs, 2 sticks and 3 cobblestones in the inventory, 
% there is no need to collect anything for a stone_axe or a stone_pickaxe
% so the ActionList is empty
collect_not_necessary([AgentDict, _, _], ItemType, ActionList) :-
    (ItemType = stone_axe; ItemType = stone_pickaxe),
    has(log, 3, AgentDict.inventory),
    has(stick, 2, AgentDict.inventory),
    has(cobblestone, 3, AgentDict.inventory),
    ActionList = [].

% if there are 9 cobblestones in the inventory, there is no need to collect anything for a castle
% so the ActionList is empty
collect_not_necessary([AgentDict, _, _], castle, ActionList) :-
    has(cobblestone, 9, AgentDict.inventory),
    ActionList = [].

% if agent already has the required items, the list of actions is empty
collect_requirements(State, ItemType, ActionList) :-
    collect_not_necessary(State, ItemType, ActionList).

collect_requirements(State, ItemType, ActionList) :-
    get_needs(State, ItemType, [], ActionList).


% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .

% the modified version of the tile_occcupied without the blocking check
my_tile_occupied(X, Y, State) :-
    State = [_, StateDict, _],
    get_dict(_, StateDict, Object),
    get_dict(x, Object, Ox),
    get_dict(y, Object, Oy),
    X = Ox, Y = Oy.

% X and Y are XMin and YMin respectively, if the 9 tiles
% (X,Y), (X+1,Y), (X+2,Y), (X,Y+1), (X+1,Y+1), (X+2,Y+1), (X,Y+2), (X+1,Y+2), (X+2,Y+2) are not full
find_castle_location_helper(State, X, Y, Width, Height, XMin, YMin) :-
    X<Width-1, Y<Height-1,
    X1 is X+1, X2 is X+2, Y1 is Y+1, Y2 is Y+2,
    not(my_tile_occupied(X, Y, State)), not(my_tile_occupied(X1, Y, State)), not(my_tile_occupied(X2, Y, State)), 
    not(my_tile_occupied(X, Y1, State)), not(my_tile_occupied(X1, Y1, State)), not(my_tile_occupied(X2, Y1, State)),
    not(my_tile_occupied(X, Y2, State)), not(my_tile_occupied(X1, Y2, State)), not(my_tile_occupied(X2, Y2, State)),
    XMin = X, YMin = Y.
 
% if X and Y cannot be XMin and YMin, then check the other 3x3 tiles one by one
% first go right and then go down while checking 
walk_through_the_map(State, X, Y, Width, Height, XMin, YMin) :-
    find_castle_location_helper(State, X, Y, Width, Height, XMin, YMin) -> 
        true; 
        ((X1 is X+1, find_castle_location_helper(State, X1, Y, Width, Height, XMin, YMin)) -> 
            true; 
            (Y1 is Y+1,find_castle_location_helper(State, X, Y1, Width, Height, XMin, YMin))
        ).

% now go diagonal
walk_through_the_map(State, X, Y, Width, Height, XMin, YMin) :-
    X<Width-1, Y<Height-1,
    X1 is X+1, Y1 is Y+1,
    walk_through_the_map(State, X1, Y1, Width, Height, XMin, YMin).

find_castle_location(State, XMin, YMin, XMax, YMax) :-
    width(W), Width is W-2,
    height(H), Height is H-2,
    walk_through_the_map(State, 1, 1, Width, Height, XMin, YMin),
    XMax is XMin+2, YMax is YMin+2.

% 15 points
% make_castle(+State, -ActionList) :- .

make_castle(State, ActionList) :-
    collect_requirements(State, castle, ActionList2),
    execute_actions(State, ActionList2, NextState),
    find_castle_location(NextState, XMin, YMin, XMax, YMax),
    go_and_place_cobbles(NextState, ActionList2, ActionList, XMin, YMin, XMax, YMax).

% X and Y are the coordinates that a single cobblestone will be placed
% we need to place 3 cobblestones for each row so if the current coordinate is in the right edge (i.e. =XMax),
% jumps to the next row by incrementing Y
go_and_place_cobbles(State, OldActions, Actions, X, Y, XMax, YMax) :-
    X=XMax, Y=<YMax,
    [AgentDict,_,_] = State,
    get_dict(x, AgentDict, Ax), get_dict(y, AgentDict, Ay),
    manhattan_distance([X,Y], [Ax, Ay], Dist),
    navigate_to(State, X, Y, ActionList, Dist),
    append(OldActions, ActionList, ActionList2),
    append(ActionList2, [place_c], ActionList3),
    execute_actions(State, ActionList, NextState),
    Y1 is Y+1, NewX is X-2, 
    go_and_place_cobbles(NextState, ActionList3, Actions, NewX, Y1, XMax, YMax).

% a row of 3 cobblestones are placed
go_and_place_cobbles(State, OldActions, Actions, X, Y, XMax, YMax) :-
    X<XMax, Y=<YMax,
    [AgentDict,_,_] = State,
    get_dict(x, AgentDict, Ax), get_dict(y, AgentDict, Ay),
    manhattan_distance([X,Y], [Ax, Ay], Dist),
    navigate_to(State, X, Y, ActionList, Dist),
    append(OldActions, ActionList, ActionList2),
    append(ActionList2, [place_c], ActionList3),
    execute_actions(State, ActionList, NextState),
    X1 is X+1, 
    go_and_place_cobbles(NextState, ActionList3, Actions, X1, Y, XMax, YMax).

go_and_place_cobbles(_, OldActions, Actions, X, Y, XMax, YMax) :-
    Xx is XMax-2, Yy is YMax+1,
    X=Xx, Y=Yy, !,
    Actions = OldActions.
        