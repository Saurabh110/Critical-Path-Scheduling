
duration(a, 10).
duration(b, 15).
duration(c, 4).
duration(d, 3).
duration(e, 2).
duration(f, 10).
duration(g, 19).
duration(h, 21).
duration(i, 18).
duration(j, 26).
duration(k, 3).
duration(l, 2).
duration(m, 4).
duration(n, 2).
duration(o, 1).
duration(p, 9).


prerequisite(b, a).
prerequisite(i, a).
prerequisite(c, b).
prerequisite(d, c).
prerequisite(g, c).
prerequisite(e, d).
prerequisite(h, g).
prerequisite(e, h).
prerequisite(f, e).

prerequisite(j, i).
prerequisite(k, j).
prerequisite(n, j).
prerequisite(f, h).
prerequisite(l, k).
prerequisite(m, l).
prerequisite(f, m).
prerequisite(o, n).
prerequisite(p, o).
prerequisite(f, p).


%Finds or Maps True if Task is goal state
findLast(Task):-
	not(prerequisite(Z,Task)).

%Finds or Maps True if Task is start state	
findFirst(Task):-
	not(prerequisite(Task,Z)).
	
%Base condition for the recursion
prereqs([Head|Tail], List):-
	findFirst(Head),
	List = [Head|Tail].

%Finds prerequisite of Head and appends to list
prereqs([Head|Tail], List):-
	prerequisite(Head, Pre_Head),
	append([Pre_Head], [Head|Tail], New_List),
	prereqs(New_List, List).

%Finds list of nodes till goal state from Task
findParents(Task, List):-
	prereqs([Task], List).

%Base condition for the recursive calc_ef function
calc_ef([], Rec_Value, Ret_Total):-
	Ret_Total = Rec_Value.

%Calculates earlyFinish of Head and appends to a list
calc_ef([Head|Tail], Rec_Value, Ret_Total):-
	duration(Head, Value),
	Total is Rec_Value + Value,
	calc_ef(Tail, Total, Ret_Total).

%Used to call the recursive calc_ef function
calc_ef_first([Head|Tail],Ret_Total):-
	duration(Head,Value),
	calc_ef(Tail, Value, Ret_Total).

%Used to call the calc_ef_first function.
calc_earlyFinish(Task, Temp_Value):-
	findParents(Task, List),
	calc_ef_first(List, Temp_Value).

%earlyFinish Function, Finds MaxValue for given task.
earlyFinish(Task, MaxValue):-
	findall(X, calc_earlyFinish(Task, X), L),
	findMax(L, MaxValue).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Base condition for recursive find_path_to_start funciton
find_path_to_start([Head|Tail], List):-
	findLast(Head),
	List = [Head|Tail].

%Finds path to the starting node and appends to list
find_path_to_start([Head|Tail], List):-
	prerequisite(Pre_Head, Head),
	append([Pre_Head], [Head|Tail], New_List),
	find_path_to_start(New_List, List).

%Function to find path to the starting node
findChildren(Task, List):-
	find_path_to_start([Task], List).

%Used to call calc_ls_first funciton
calc_lateStart(Task, Temp_Value, EF_Value):-
	findChildren(Task, [Head|Tail]),
	earlyFinish(Head, EF_Value),
	calc_ls_first([Head|Tail], Temp_Value, EF_Value).

%Base condition for the recursive calc_ls function
calc_ls([], Rec_Value, Ret_Total):-
	Ret_Total = Rec_Value.

%Calculates lateStart of Head and appends to a list
calc_ls([Head|Tail], Rec_Value, Ret_Total):-
	duration(Head, Value),
	Total is Rec_Value - Value,
	calc_ls(Tail, Total, Ret_Total).

%Used to call the recursive calc_ls function
calc_ls_first([Head|Tail],Ret_Total,EF_Value):-
	duration(Head,Value),
	LS_of_goal is EF_Value - Value,
	calc_ls(Tail, LS_of_goal, Ret_Total).

%lateStart function
lateStart(Task, Late_Start_Value):-
	findall(X, calc_lateStart(Task, X, EF_Value), L),
	findMin(L, Late_Start_Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%To calculate criticalPath to a Task
criticalPath(Task, Path):-
	findParents(Task, List),
	removeLast(List, Path),
	isCritical(Path,Task).

%Base condition for isCritical function
isCritical([], _).

%Function to check if Head is on the criticalPath
isCritical([Head|Tail], Defined_Task):-
	duration(Head, Duration),
	earlyFinish(Head, EF_Value),
	lateStart1(Head, LS_Value, Defined_Task),
	EF_Value is LS_Value + Duration,
	isCritical(Tail, Defined_Task).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Based condition to stop recursion
find_path_to_start1([Head|Tail], List, Defined_Task):-
	Head = Defined_Task,
	List = [Head|Tail].

%Calculates path till the Task Passed to criticalPath
find_path_to_start1([Head|Tail], List, Defined_Task):-
	prerequisite(Pre_Head, Head),
	append([Pre_Head], [Head|Tail], New_List),
	find_path_to_start1(New_List, List, Defined_Task).


%To call function that Calculates path till the Task Passed to criticalPath
findChildren1(Task, List, Defined_Task):-
	find_path_to_start1([Task], List, Defined_Task).

%Calculated using the node passed to criticalPath function as the goal node
lateStart1(Task, Late_Start_Value, Defined_Task):-
	findall(X, calc_lateStart1(Task, X, EF_Value, Defined_Task), L),
	findMin(L, Late_Start_Value).

%Calculates path till the Task Passed to criticalPath
calc_lateStart1(Task, Temp_Value, EF_Value, Defined_Task):-
	findChildren1(Task, [Head|Tail], Defined_Task),
	earlyFinish(Head, EF_Value),
	calc_ls_first([Head|Tail], Temp_Value, EF_Value).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Function to calculate maxSlack
maxSlack(Task, Slack):-
	findall(X, prerequisite(X,_), L),
	find_slack(L, List, []), %L contains all the nodes except goal node
	findMax(List, MaxValue), %Max slack is calculated
	earlyFinish(Task, EF_Value),
	lateStart(Task, LS_Value),
	duration(Task, Duration),
	LateFinish is  LS_Value + Duration,
	Slack is LateFinish - EF_Value,
	Slack is MaxValue. %Slack of Task is calculated

%Base condition for find_slack function
find_slack([],List,Li1):- List = Li1.

%Finds and appends slck of head to a list
find_slack([Head|Tail], List, Li) :-
	earlyFinish(Head, EF_Value),
	lateStart(Head, LS_Value),
	duration(Head, Duration),
	LateFinish is  LS_Value + Duration,
	Slack is LateFinish - EF_Value,
	append([Slack], Li, List1),
	find_slack(Tail, List, List1).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%Function to find MaxValue from passed list
findMax([], R, R). %end
findMax([Head|Tail], WK, R):- Head >  WK, findMax(Tail, Head, R).
findMax([Head|Tail], WK, R):- Head =< WK, findMax(Tail, WK, R).
findMax([Head|Tail], R):- findMax(Tail, Head, R).

%Function to find MinValue from passed list
findMin([], R, R). %end
findMin([Head|Tail], WK, R):- Head <  WK, findMin(Tail, Head, R). 
findMin([Head|Tail], WK, R):- Head >= WK, findMin(Tail, WK, R).
findMin([Head|Tail], R):- findMin(Tail, Head, R). 

%Function to append to a list.
append( [], X, X).                      
append( [X | Y], Z, [X | W]) :- append( Y, Z, W).

%Function to remove last of a list
removeLast([Head|Tail], Returned_list) :-             
   list_butlast_prev(Tail, Returned_list, Head).    

list_butlast_prev([], [], _).
list_butlast_prev([X1|Tail], [X0|Returned_list], X0) :-  
   list_butlast_prev(Tail, Returned_list, X1).   
