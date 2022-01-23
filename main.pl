/*
 * MCO3 Travel Advisor Agent
 * Palpallatoc, Aaron
 * Mangoba, Michael
 * Tiongquico, Erik
*/

welcome:- 
    write('Trip Advisor Agent (TAA) Israel'),
    ask('Would you like to travel to Israel?').

% This will be used for yes/no questions only.
ask(Question) :-
    write(Question),
    write('(yes/no)'),
    read(Response),
    nl,
    ((Response == yes; Response == y) -> assert(yes(Question));
     (Respones ==  no; Response == n) -> assert(no(Question));
    write('Sorry. I do not recognize this input.'),fail).

% This will be used for questions that prompt specific words or answers e.g. nationality
% edit: Might be better to create a separate function for each type of prompt? We need a proper fact declaration.

% prompt(Question) :-
%     write(Question),
%     read(Response),
%     nl,
%     ((Response == yes; Response == y) -> assert(yes(Question));
%      (Respones ==  no; Response == n) -> assert(no(Question));
%     write('Sorry. I do not recognize this input.'),fail).

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

% COVID tests

vaccine(
    Brand,
    Doses,
    Days
).

booster(
    Brand,
    Days
).

% Applies both to normal vac or booster
% validbrand(brand, days(min, max))
validbrand(pfizer, days(7, 180)).
validbrand(moderna, days(14, 180)).
validbrand(asterzeneca, days(14, 180)).
validbrand(sinovac, days(14, 180)).
validbrand(sinopharm, days(14, 180)).
validbrand(jj, days(14, 180)).


% If recovered from COVID
% naatTest(X) :-
