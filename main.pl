/*
 * MCO3 Travel Advisor Agent
 * Palpallatoc, Aaron
 * Mangoba, Michael
 * Tiongquico, Erik
*/


write('Trip Advisor Agent (TAA) Israel').
write('Would you like to travel to Israel?').


% --------------- Everything below is the knowledge base --------------- %

% input(Question) :-

traveler(X).

date(
    startdate(Month, Day, Year),
    enddate(Month, Day, Year)
    ).

% Profile of User
% Genders
male(X) :- \+female(X).
female(X) :- \+male(X).

% Purposes
