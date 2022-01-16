/*
 * MCO3 Travel Advisor Agent
 * Palpallatoc, Aaron
 * Mangoba, Michael
 * Tiongquico, Erik
*/
% input(Question) :-

input(Question) :-

write('Trip Advisor Agent (TAA) Israel').
write('Would you like to travel to Israel?'),
read(answer), nl,
    ((answer == yes; answer  == y)
    -> 

    assert(yes(Question));
    assert(no(Question), fail).

    )

:- dynamic yes/1,no/1.

verify(Order) :-
    (yes(Order)) :-
        -> true;
    (no(Order)) :-
        -> fail;