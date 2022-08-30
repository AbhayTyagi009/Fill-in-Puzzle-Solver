%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Fill-in Puzzle Solver %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Submitted By - Abhay Tyagi
% Student Number - 1088116
% For this project, we have to design a fill-in puzzle solver. A fill-in puzzle 
% is like a crossword puzzle, except that instead of being given obscure clues 
% telling us which words go where, you are given a list of all the words to 
% place in the puzzle, but not told where they go. A fillin puzzle will be
% represented as a list of lists, each of the same length and each representing
% a single row of the puzzle. Each element in each of these lists is either a:
% '#', denoting a solid, unfillable square; an underscore (_), representing a
% fillable square; or a single, lower case letter (e.g., h), denoting a
% pre-filled square. The output should be a solved puzzle.

% The approach on this project is divided into 3 parts, and is based on Hint 4 -
% 1) Preprocessing the list.
% 2) Finding all the viable the slots for binding to the wordlist.
% 3) Filling the puzzle.

% Loading the library because the SWI Prolog library provides two different, 
% incompatible transpose/2 predicates, and usually autoloads the wrong one by 
% default.
:- ensure_loaded(library(clpfd)).

% puzzle_solution(+Puzzle, +WordList) is the driver predicate7 of this project.
% This follows the basic structure discussed above.
puzzle_solution(Puzzle, WordList) :-
        sort_wordlist(WordList, WLSorted),
        find_slots(Puzzle, Slots), !,
        fill(WLSorted, Slots), !.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Processing the List %%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sort_wordlist(+WordList, -WLSorted) sorts the wordlist so that -
% 1) Longer words appear before shorter words, if they have the same frequency.
% 2) Words of the same length are kept together.
% 3) Words of rarer lengths appear first in the wordlist.
% First, we sort the list in descending order of their length. Then we make
% nested lists in which all lists of the same lists are put in a list. Then we
% sort the sublists in ascending order of their lengths. This will put wordlists
% of rarer lengths first, and the most common words are put near the end. Then 
% we remove the nested lists to get a structure similar to the original
% wordlist.
sort_wordlist(WordList, WLSorted) :-
        sort(WordList, 0, WordList_Sorted),
        fragment(WordList_Sorted, WordList_Split),
        sort(WordList_Split, 1, WordList_Split_Sorted),
        defragment(WordList_Split_Sorted, WLSorted).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Quick Sort %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% sort(+WordList, +Indicator, -Sorted) sorts the list of words in ascending or
% descending order of their lengths using Quick Sort according to the value of
% the Indicator. If the indicator is 0, we use descending sorting and if the
% indicator is 1, we use ascending sorting. Quick sort is used as it is much
% faster than merge sort that is used by the built-in sort/2 predicate.
sort(WordList, Ind, Sorted) :-
        (Ind = 0 -> 
                sort_d(WordList, [], Sorted)
        ;
                sort_a(WordList, [], Sorted)
        ).

% sort_d(+WordList, -Sorted) runs a descending quick sort algorithm.
sort_d([], Sorted, Sorted).
sort_d([Head|Tail], Temp, Sorted) :-
        pivot_d(Head, Tail, List1, List2),
        sort_d(List1, Temp, Sorted1),
        sort_d(List2, [Head|Sorted1], Sorted).
    
pivot_d(_, [], [], []).
pivot_d(Head, [X|Tail], [X|List1], List2) :-
        length(Head, Length1),
        length(X, Length2),
        Length2 =< Length1,
        pivot_d(Head, Tail, List1, List2).
pivot_d(Head, [X|Tail], List1, [X|List2]) :-
        length(Head, Length1),
        length(X, Length2),
        Length2 > Length1,
        pivot_d(Head, Tail, List1, List2).

% sort_a(+WordList, -Sorted) runs an ascending quick sort algorithm. 
sort_a(WordList, Sorted) :-
        sort_a(WordList, [], Sorted).

sort_a([], List, List).
sort_a([Head|Tail], Temp, Sorted) :-
        pivot_a(Head, Tail, List1, List2),
        sort_a(List1, Temp, Sorted1),
        sort_a(List2, [Head|Sorted1], Sorted).

pivot_a(_, [], [],[]).
pivot_a(Head, [X|Tail], [X|List1], List2) :-
        length(Head, Length1),
        length(X, Length2),
        Length2 >= Length1,
        pivot_a(Head, Tail, List1, List2).
pivot_a(Head, [X|Tail], List1, [X|List2]) :-
        length(Head, Length1),
        length(X, Length2),
        Length2 < Length1,
        pivot_a(Head, Tail, List1, List2).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Splitting the List %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% fragment(+WordList, -WordList_Split) segregates the list by seperating the
% list into smaller lists of words of similar length. Words of same lengths are
% kept in a small lengths. This is done to sort the words by the frequency of
% lengths so we can use that to sort later.
fragment([Word|WordList], WordList_Split) :-
        length(Word, Length),
        fragment([Word|WordList], Length, [], [], WordList_Split).

fragment([], _, [], WordList_Split, WordList_Split) :- !.
fragment([], _, Accumulator1, Accumulator2, WordList_Split) :-
        not(length(Accumulator1, 0)),
        append(Accumulator2, [Accumulator1], Accumulator3),
        fragment([], _, [], Accumulator3, WordList_Split).
fragment([Word|WordList], Prev_Length, Accumulator1, Accumulator2, 
        WordList_Split) :-
        length(Word, Curr_Length),
        (Curr_Length =:= Prev_Length ->
                append(Accumulator1, [Word], Accumulator3),
                fragment(WordList, Curr_Length, Accumulator3, Accumulator2, 
                        WordList_Split)
        ;
                append(Accumulator2, [Accumulator1], Accumulator4),
                fragment(WordList, Curr_Length, [Word], Accumulator4, 
                        WordList_Split)
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%% Reconstructing the WordList %%%%%%%%%%%%%%%%%%%%%%%%%

% defragment(+NestedList, -WordList) removes the nested lists we got from
% fragment/2 to get a list of words, similar to the input wordlist.
defragment(NestedList, WordList) :- 
        defragment(NestedList, [], WordList).

defragment([], List, List) :- !.
defragment([Element|List], Accumulator, WordList) :-
        append(Accumulator, Element, Accumulator1),
        defragment(List, Accumulator1, WordList).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%% Finding Fillable Slots %%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% find_slots(+Puzzle, -Slots) finds all the bindable slot sequences in the 
% puzzle (horizontally and vertically), so that they can be unified with the
% word list easily later on. First, we find the horizontal slots in the puzzle.
% Then we transpose the puzzle and get the vertical slots. 
find_slots(Puzzle, Slots) :-
        find_slots_hor(Puzzle, Slots1, Puzzle1),
        transpose(Puzzle1, Puzzle1_Flipped),
        find_slots_ver(Puzzle1_Flipped, Slots1, Slots).


%%%%%%%%%%%%%%%%%%%%%%% Finding all the Horizontal Slots %%%%%%%%%%%%%%%%%%%%%%%

% find_slots_hor(+Puzzle, -Slots, -Sol) traverses horizontally across the puzzle
% to get a list of slots (empty variables) that can be used to unify with the
% wordlist.
find_slots_hor(Puzzle, Slots, Sol) :-
        find_slots_hor(Puzzle, [], [], Slots, Sol).

find_slots_hor([], Slots, Sol, Slots, Sol).
find_slots_hor([Row|Puzzle], Temp_Slots, Temp_Puzzle, Slots, Sol) :-
        append(Temp_Puzzle, [Row], Temp_Puzzle1),
        get_slots(Row, Bound),
        append(Temp_Slots, Bound, Temp_Slots1),
        find_slots_hor(Puzzle, Temp_Slots1, Temp_Puzzle1, Slots, Sol).


%%%%%%%%%%%%%%%%%%%%%%%% Finding all the Vertical Slots %%%%%%%%%%%%%%%%%%%%%%%%

% find_slots_ver(+Puzzle, -Slots, -Solution) traverses vertically across the
% puzzle to get a list of slots (empty variables) that can be used to unify with
% the wordlist.
find_slots_ver([], Slots, Slots).
find_slots_ver([Column|Puzzle], Accumulator, Slots) :-
        get_slots(Column, TempSlots),
        append(Accumulator, TempSlots, Accumulator1),
        find_slots_ver(Puzzle, Accumulator1, Slots).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% Getting Free Slots %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% get_slots(+Row, -Slots) will get all sequences of unassigned slots (_) and
% preallocated slots (Lowercase letters) with a length greater than 1.
get_slots(Row, Slots) :-
        get_slots(Row, [], [], Slots).

get_slots([], [], Sol, Sol) :- !.
get_slots([], Accumulator1, Accumulator2, SlotList) :-
        length(Accumulator1, Length),
        (Length < 2 ->
                get_slots([], [], Accumulator2, SlotList)
        ;
                append(Accumulator2, [Accumulator1], Accumulator3),
                get_slots([], [], Accumulator3, SlotList)
        ).
get_slots([Slot|Row], Accumulator1, Accumulator2, SlotList) :-
        (Slot == # ->
                length(Accumulator1, Length),
                (Length < 2 ->
                        get_slots(Row, [], Accumulator2, SlotList)
                ;
                        append(Accumulator2, [Accumulator1], Accumulator3),
                        get_slots(Row, [], Accumulator3, SlotList)
                )
        ;
                append(Accumulator1, [Slot], Accumulator3),
                get_slots(Row, Accumulator3, Accumulator2, SlotList)
        ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%% Unifying the Logical Variables %%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% fill(+WordList, -BindedSlots) will unify the sots we got previously with the
% get_slots/2 predicate. This will bind each variable (memory address) to an
% alphabet in the wordlist, automatically updating the puzzle as well.
fill([], _) :- !.
fill([Word|WordList], Slots):-
        bind(Word, Slots, BindedSlots1),
        fill(WordList, BindedSlots1).

bind(Word, Slots, BindedSlots) :-
        bind(Word, Slots, [], BindedSlots).

bind(_, [], BindedSlots, BindedSlots).
bind(Word, [WordSlot|SlotList], Temp, Slots) :-
        (Word = WordSlot,
        append(Temp, [WordSlot], Temp1),
        append(Temp1, SlotList, Temp2),
        bind(_, [], Temp2, Slots))
        ;
        (append(Temp, [WordSlot], Temp1),
        bind(Word, SlotList, Temp1, Slots)),
        not(length(SlotList, 0)).
