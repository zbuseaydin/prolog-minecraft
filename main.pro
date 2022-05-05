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

temp_min([], Minimum, Minimum).

temp_min([H|T], Temp, Minimum) :-
    H<Temp,!,
    temp_min(T, H, Minimum).

temp_min([_|T], Temp, Minimum) :-
    temp_min(T, Temp, Minimum).

minimum_of_list(List, Minimum) :-
    [H|T] = List,
    temp_min([H|T], H, Minimum).



% 10 points
% find_nearest_type(+State, +ObjectType, -ObjKey, -Object, -Distance) :- .

find_nearest_type(State, ObjectType, ObjKey, Object, Distance) :- 
    find_nearest_type_helper(State, ObjectType, [], _, _, ObjKey, Object, Distance).

find_nearest_type_helper([AgentDict, ObjectDict, _], ObjectType, TempList, DistList, ObjectList, ObjKey, Object, Min):-
    findall(X, ObjectDict.X.type=ObjectType, ObjectList),
 %   write(ObjectList),
    get_dict(x, AgentDict, X1),
    get_dict(y, AgentDict, Y1),
    create_dist_list([X1, Y1], ObjectDict, ObjectList, TempList, DistList),
    get_the_nearest(DistList, ObjectList, ObjectDict, ObjKey, Object, Min).

create_dist_list(_, _, [], TempList, DistList) :-
    DistList = TempList.

create_dist_list(AgentLoc, ObjectDict, [H|T], TempList, DistList):-
    get_dict(H, ObjectDict, Object),
    get_dict(x, Object, X2),
    get_dict(y, Object, Y2),
    manhattan_distance(AgentLoc, [X2, Y2], Dist),
    append(TempList, [Dist], NewDistList),
    create_dist_list(AgentLoc, ObjectDict, T, NewDistList, DistList).

get_the_nearest(DistList, ObjectList, ObjectDict, ObjKey, Object, Min) :-
    minimum_of_list(DistList, Min),
    get_object_infos(DistList, ObjectList, ObjectDict, ObjKey, Object, Min).

get_object_infos([H|_], [H2|_], ObjectDict, ObjKey, Object, MinDist) :-
    H = MinDist, !,
    ObjKey is H2,
    get_dict(ObjKey, ObjectDict, Object).

get_object_infos([_|T], [_|T2], ObjectDict, ObjKey, Object, MinDist) :-
    get_object_infos(T, T2, ObjectDict, ObjKey, Object, MinDist).
    

    
%T: 523
%Agent: agent_dict{hp:10,hunger:96573,inventory:bag{cobblestone:0,fruits:0,log:0,stick:4},x:8,y:1}
%State: object_dict{0:object{hp:0,type:stone,x:2,y:1},8:object{hp:1,type:tree,x:5,y:3},9:object{hp:2,type:tree,x:8,y:3},10:object{hp:3,type:cobblestone,x:8,y:4},11:object{hp:0,type:cobblestone,x:1,y:5},12:object{hp:3,type:stone,x:8,y:5},1000:object{hp:3,type:cobblestone,x:9,y:1},1001:object{hp:2,type:cobblestone,x:8,y:0},1002:object{hp:1,type:cobblestone,x:7,y:0},1003:object{hp:3,type:cobblestone,x:7,y:2}}
    
%agent_dict{hp:10,hunger:96573,inventory:bag{},x:8,y:1}
%object_dict{8:object{hp:1,type:tree,x:5,y:3},9:object{hp:2,type:tree,x:8,y:3},10:object{hp:3,type:cobblestone,x:8,y:4},12:object{hp:3,type:stone,x:8,y:5},1001:object{hp:2,type:cobblestone,x:8,y:0}}




% 10 points
% navigate_to(+State, +X, +Y, -ActionList, +DepthLimit) :- .
navigate_to(State, X, Y, ActionList, DepthLimit) :-
    [AgentDict, _, _] = State,
    get_dict(x, AgentDict, AgentX),
    get_dict(y, AgentDict, AgentY),
    add_action(AgentX, AgentY, X, Y, [], 0, DepthLimit, ActionList).

add_action(AgentX, AgentY, X, Y, Actions, Count, DepthLimit, ActionList) :-
    DepthLimit >= Count,
    AgentX = X,
    AgentY = Y,
    ActionList = Actions.

add_action(AgentX, AgentY, X, Y, Actions, Count, DepthLimit, ActionList) :-
    DepthLimit >= Count,
    AgentX < X,
    append(Actions, [go_right], NewActions),
    NewAgentX is AgentX+1,
    NewCount is Count+1,
    add_action(NewAgentX, AgentY, X, Y, NewActions, NewCount, DepthLimit, ActionList).

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

item_info(stick, reqs{log: 2, stick: 0, cobblestone:0}, 4).
required(stick, [tree-1, stone-0]).
required(stone_pickaxe, [tree-2, stone-1]).
required(stone_axe, [tree-2, stone-1]).

collect_requirements(State, ItemType, ActionList) :- 
    [AgentDict, _, _] = State,
    check_and_get_needs(State, ItemType, AgentDict.inventory, ActionList).

check_if_collect_necessary([AgentDict, _, _], ItemType, ActionList) :-
    item_info(ItemType, Reqs, _),
    get_dict(log, Reqs, LogVal),
    get_dict(stick, Reqs, StickVal),
    get_dict(cobblestone, Reqs, CobblestoneVal),
    has(log, LogVal, AgentDict.inventory),
    has(stick, StickVal, AgentDict.inventory),
    has(cobblestone, CobblestoneVal, AgentDict.inventory),
    ActionList = [].

check_and_get_needs(State, ItemType, Inv, Actions) :-
    check_if_collect_necessary(State, ItemType, Actions).

check_and_get_needs(State, ItemType, Inv, Actions) :-
    required(ItemType, [_-TreeVal, _-StoneVal]),
    get_needs(State, TreeVal, StoneVal, [], WholeActions),
    Actions = WholeActions.

get_needs(State, 0, 0, CollectActions, WholeActions):-
    WholeActions=CollectActions.

get_needs(State, NumTree, NumStone, CollectActions, WholeActions):-
    NumTree>0,
    chop_nearest_tree(State, Actions),
    append(CollectActions, Actions, NewCollectActions),
    execute_actions(State, Actions, NextState),
    NewNumTree is NumTree-1,
    get_needs(NextState, NewNumTree, NumStone, NewCollectActions, WholeActions).

get_needs(State, NumTree, NumStone, CollectActions, WholeActions):-
    NumStone>0,
    mine_nearest_stone(State, Actions),
    append(CollectActions, Actions, NewCollectActions),
    execute_actions(State, Actions, NextState),
    NewNumStone is NumStone-1,
    get_needs(NextState, NumTree, NewNumStone, NewCollectActions, WholeActions).

% 5 points
% find_castle_location(+State, -XMin, -YMin, -XMax, -YMax) :- .



% 15 points
% make_castle(+State, -ActionList) :- .
