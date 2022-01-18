/*
 * MCO3 Travel Advisor Agent
 * Palpallatoc, Aaron
 * Mangoba, Michael
 * Tiongquico, Erik
*/

welcome:- 
    write('Trip Advisor Agent (TAA) Israel'),
    ask('Would you like to travel to Israel?').

ask(Question) :-
    write(Question),
    write('(yes/no)'),
    read(Response),
    nl,
    ((Response == yes; Response == y) -> assert(yes(Question));
     (Respones ==  no; Response == n) -> assert(no(Question));
    write('Sorry. I do not recognize this input.'),fail).

% % Fixes argumentation of yes/no dynamics
% :- dynamic yes/1, no/1.

% verify(Order) :-
%     (yes(Order) :-
%         ->
%         true;
%     (no(Order)
%         ->
%         fail;
%     ask(Order))).

% --------------- Everything below is the knowledge base --------------- %


% input(Question) :-

traveler(X).

traveldate(
    startdate(Month, Day, Year),
    enddate(Month, Day, Year)
    ).

% Profile of User
% Genders
male(X) :- \+female(X).
female(X) :- \+male(X).

% Purposes
