% Author: Wenjun Wang <www4@student.unimelb.edu.au>
% Purpose: Provides two useful funtions in Cribbage card game:
%     hand_value(): Calculate the points of given hand card and start card.
%     select_hand(): Choose the best four cards among the dealt cards.
%
% The whole project can be divided into 3 parts:
% 1. Util predicates that help making the judging predicates easier.(mapping, etc.)
% 2. Five judging predicates to calculate the points seperately according to the rules.
% 3. Two target predicates which make use of other predicates.
%
% The hand_value() predicate will first call the judging predicate flushed() and 
% ofhn() that only consider the suit of hand and start card rather than the actual
% value. Then the hand cards and start card will be combine together to form a list,
% note that the non-numric rank will be mapped to numeric value now and get sorted.
% After that, the rest predicates will be called to calculated the points value and 
% all of the five partial points will be added and the sum points will be returned.
%
% The select_hand() predicate will first generate possible start cards and use the
% keep_four() predicate to get all of the combinations of hand cards. Then it will
% call the accumulate() predicate to calculate the total possible value of given 
% four cards. After calculating all of the combinations, it sorts the list and return
% the keep-discard combination with the greatest expected value.


% reduce_value(+List1, +List2, -Result)
% 
% Purpose: An util predicate to map the elements with value above 10 to 10.
reduce_value([], Result, Result).
reduce_value([Elt|Tail1], List, Result):-
    (Elt > 10 ->
        append(List, [10], NewList)
    ;
        append(List, [Elt], NewList)
    ),
    reduce_value(Tail1, NewList, Result).
        
% sublist(?Sub, +List)
% 
% Purpose: An util predicate to judge if a list is the sublist of another one, or
% get all the sublists of a given List. 
sublist([], []).
sublist(Xs, [_|Ys]):-
    sublist(Xs, Ys).
sublist([X|Xs], [X|Ys]):-
    sublist(Xs, Ys).

% trans_rank(+List1, +List2, -Result)
% 
% Purpose: An util predicate to map non-numeric rank to numeric value.
trans_rank([], Result, Result).
trans_rank([Elt|Tail1], List, Result):-
    (Elt == jack ->
        append(List, [11], NewList)
    ;Elt == queen ->
        append(List, [12], NewList)
    ;Elt == king ->
        append(List, [13], NewList)
    ;Elt == ace ->
        append(List, [1], NewList)
    ;
        append(List, [Elt], NewList)
    ),
    trans_rank(Tail1, NewList, Result).

% clumped(+List1, +List2, -Result)
% 
% Purpose: An util predicate to transform a list to value-count pairs list.
clumped([], Result, Result).
clumped([Elt|Tail], List, Result):-
    length(List, N),
    (N >= 1 ->
        Index is N - 1,
        nth0(Index, List, Last),
        Value-Count = Last,
        (Elt == Value ->
            Count0 is Count + 1,
            append(Rest, [Last], List),
            append(Rest, [Elt-Count0], NewList)
        ; 
            append(List, [Elt-1], NewList)
        )
    ;
        NewList = [Elt-1]
    ),
    clumped(Tail, NewList, Result).

% count_pairs(+List, +Temp, -Result)
% 
% Purpose: An util predicate to count pairs in a value-count list.
count_pairs([], Result, Result).
count_pairs([_-Count|List], Sum, Result):-
    (Count == 2 ->
        Sum0 is Sum + 1
    ; Count == 3 ->
        Sum0 is Sum + 3
    ; Count == 4 ->
        Sum0 is Sum + 6
    ;Sum0 is Sum),
    count_pairs(List, Sum0, Result).

% get_len_and_multi(+List, +TempValue, +TempLength, +TempMulti, -ResultLength, -ResultMulti)
% 
% Purpose: An util predicate to calculate the longest sequence and its multiplier,
% the multiplier here indicate the number of duplicate elements in the longest sequence.
get_len_and_multi([], _, Result1, Result2, Result1, Result2).
get_len_and_multi([Elt-Count|List], Temp, Length, Multi, Result1, Result2):-
    (Temp == [] ->
        NewTemp = [Elt],
        NewLength = 1,
        NewMulti = Count
    ;
        nth0(0, Temp, Value),
        Target is Value + 1,
        (Elt == Target ->
            NewTemp = [Elt],
            NewLength is Length + 1,
            NewMulti is Multi*Count
        ;
            (Length < 3 ->
                NewTemp = [Elt],
                NewLength = 1,
                NewMulti = Count
            ;
                NewTemp = [14],
                NewLength = Length,
                NewMulti = Multi
            )
        )
    ),
    get_len_and_multi(List, NewTemp, NewLength, NewMulti, Result1, Result2).

% keep_four(+List, -Keep, -NotKeep)
% 
% Purpose: An util predicate to choose four cards from five/six to keep, and left
% the others to discard.
keep_four(List, Keep, NotKeep):-
    length(List, N),
    ( N == 5 ->
        nth0(_, List, Elt),
        select(Elt, List, Keep),
        NotKeep = [Elt]
    ;
        length(NotKeep, 2),
        sublist(NotKeep,List),
        NotKeep = [Elt1, Elt2],
        select(Elt1, List, Temp),
        select(Elt2, Temp, Keep)
    ).

% generate_startcards(+List, -Result)
% 
% Purpose: An util predicate to generate possible start cards based on initial cards.
generate_startcards(List, Result):-
    Ranks = [ace, 2, 3, 4, 5, 6, 7, 8, 9, 10, jack, queen, king],
    Suits = [clubs, diamonds, hearts, spades],
    setof(card(Rank, Suit), 
          (member(Rank, Ranks), 
          member(Suit, Suits), 
          \+ member(card(Rank, Suit), List)),
          Result).

% flushes(+Hand, +Startcard, -Points)
% 
% Purpose: The predicate to judge if all the cards in the hand are of the same suit
% and return the points.
flushes(Hand, Startcard, Points):-
    Hand = [card(_,Suit1),card(_,Suit2),card(_,Suit3),card(_,Suit4)],
    Startcard = card(_,Suit5),
    sort([Suit1,Suit2,Suit3,Suit4], List),
    length(List, Len),
    (Len is 1 ->
        (member(Suit5, List) ->
            Points = 5
        ;Points = 4)
    ;Points = 0).

% ofhn(+Hand, +Startcard, -Points)
% 
% Purpose: The predicate to judge if the hand contains the jack of the same suit 
% as the start card and return the points.
ofhn(Hand, Startcard, Points):-
    Startcard = card(_,Suit),
    (member(card(jack, Suit), Hand) ->
        Points = 1
    ;Points = 0).

% fifteens(+List, -Points)
%
% Purpose: The predicate to count the number of combinations of cards that have the
% sum of 15 and return the points
fifteens(List, Points):-
    reduce_value(List, [], TransList),
    (bagof(Part, (sublist(Part, TransList), sumlist(Part,15)), Parts) ->
        length(Parts, Length)
    ;
        Length = 0
    ),
    Points is Length*2.

% pairs(+List, -Points)
%
% Purpose: The predicate to count the number of pairs in hand and start card and 
% return the points.
pairs(List, Points):-
    clumped(List, [], R),
    count_pairs(R, 0, Pairs),
    Points is Pairs*2.

% runs(+List, -Points)
% 
% Purpose: The predicate to calculate the longest run and its multiplier and return
% the points.
runs(List, Points):-
    clumped(List, [], R),
    get_len_and_multi(R, [], 0, 1, Length, Multi),
    (Length >= 3 ->
        Points is Length*Multi
    ;
        Points = 0
    ).

% hand_value(+Hand, +Startcard, -Points)
%
% Purpose: The predicate calculate the total points of the hand and the start card
% by calling the related predicates and return the total points.
hand_value(Hand, Startcard, Points):-
    flushes(Hand, Startcard, FlushesPoint),
    ofhn(Hand, Startcard, OfhnPoint),
    Hand = [card(Value1, _),card(Value2, _),card(Value3, _),card(Value4, _)],
    Startcard = card(Value5, _),
    trans_rank([Value1, Value2, Value3, Value4, Value5], [], ValueList),
    msort(ValueList,List),
    fifteens(List, FifteensPoint),
    pairs(List, PairsPoint),
    runs(List, RunsPoint),
    Points is FifteensPoint + PairsPoint + RunsPoint + FlushesPoint + OfhnPoint.

% accumulate(+Hand, +Startcards, -Total)
% 
% Purpose: The predicate is to calculate the total possible points of a given hand
% so that it can be used to assess the expected value.
accumulate(Hand, Startcards, Total):-
    findall(Points, 
          (member(Start, Startcards), hand_value(Hand, Start, Points)),
          Result),
    sumlist(Result, Total).

% select_hand(+Card, -Hand, -Cribcards)
%
% Purpose: The predicate is to choose the best four cards to keep by considering
% the start card may bring to hand and return the best four cards with greatest 
% expected value.
select_hand(Card, Hand, Cribcards):-
    generate_startcards(Card, Startcards),
    setof(Keep-NotKeep, keep_four(Card, Keep, NotKeep), Hands),
    setof(Total-Keep-NotKeep, 
          (member(Keep-NotKeep,Hands), accumulate(Keep, Startcards, Total)),
          Result),
    keysort(Result, SortedResult),
    append(_, [_-Hand-Cribcards], SortedResult).
