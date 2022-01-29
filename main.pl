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

has(X,Y).
travel(X,Y).

% Profile of User
% Genders
male(X) :- \+female(X).
female(X) :- \+male(X).

% Purposes

% COVID tests

vaccine(
    Brand,
    Days
).

% booster(
%     Brand,
%     Days
% ).

vaccinated(X, vaccine(Brand,Days)).
% vaccinated(X, booster(Brand,Days)).
days_vaccinated(X, Y).

% Rules

% X is isolated because of Y
% isolated(X,Y) :-
%     travel(X,Y),
%     redlist(Y).

isolated(X) :-
    list_of_red_countries(L),
    travel(X,L).


% Applies both to normal vac or booster
% validbrand(brand, days(min, max))
validbrand(Brand, days(Min, Max)).
validbrand(pfizer, days(7, 180)).
validbrand(moderna, days(14, 180)).
validbrand(astrazeneca, days(14, 180)).
validbrand(sinovac, days(14, 180)).
validbrand(sinopharm, days(14, 180)).
validbrand(jj, days(14, 180)).

list_of_valid_brands(ValidBrand) :- findall(Vaccine, validbrand(Vaccine,_), ValidBrand).
list_of_valid_vaccinations(ValidVaccine, days(X,Y)) :- findall(Vaccine, validbrand(Vaccine, days(X,Y)), ValidVaccine).

has_validvaccine(X) :-
    list_of_valid_brands(L),
    vaccinated(X, vaccine(L,_)),
    
   has(X, vaccine(Brand, Doses, _)).
    


% For who will be isolated
% Red List Countries (as of December 31, 2021)

redlist(botswana).
redlist(canada).
redlist(ethiopia).
redlist(france).
redlist(hungary).
redlist(malawi).
redlist(mexico).
redlist(nigeria).
redlist(portugal).
redlist(southafrica).
redlist(spain).
redlist(switzerland).
redlist(tanzania).
redlist(trukey).
redlist(uae).
redlist(uk).
redlist(us).

list_of_red_countries(RedCountry) :- findall(Country, redlist(Country), RedCountry).


% If recovered from COVID
% naatTest(X) :-


% TBH WE DONT KNOW THESE BAKA DI NEED? kasi eemail directly based on purpose nagiiba eh
% If traveling for work


% If traveling for academics

    