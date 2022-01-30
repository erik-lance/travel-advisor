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
     (Response ==  no; Response == n) -> assert(no(Question));
    write('Sorry. I do not recognize this input.'),fail).

% bioprofile(Traveller) :-
%     write('Are you male?').

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

% traveler(X).

traveldate(
    startdate(Month, Day, Year),
    enddate(Month, Day, Year)
).

% has(X,Y).
% travel(X,Y).
list_of_travels(X,TravelList) :- findall(Country, travel(X,Country), TravelList).

% Profile of User

% Purposes

% COVID tests

% vaccine(
%     Brand,
%     Days
% ).

% booster(
%     Brand,
%     Days
% ).

% vaccinated(X, vaccine(Brand,Days)).
% vaccinated(X, booster(Brand,Days)).
% days_vaccinated(X, Y).

% Rules

% X is isolated because of Y
% isolated(X,Y) :-
%     travel(X,Y),
%     redlist(Y).

% isolated(X) :-
%     list_of_travels(X,ListTravels),
%     list_of_red_countries(ListRed),
%     member(ListTravels,ListRed)
    


% Applies both to normal vac or booster
% validbrand(Brand, days(Min, Max)).
validbrand(pfizer, days(7, 180)).
validbrand(moderna, days(14, 180)).
validbrand(astrazeneca, days(14, 180)).
validbrand(sinovac, days(14, 180)).
validbrand(sinopharm, days(14, 180)).
validbrand(jj, days(14, 180)).

% For checking
list_of_valid_brands(ValidBrand) :- findall(Vaccine, validbrand(Vaccine,_), ValidBrand).
list_of_valid_vaccinations(ValidVaccine, days(X,Y)) :- findall(Vaccine, validbrand(Vaccine, days(X,Y)), ValidVaccine).

% X has a validvaccine IF
% X is vaccinated with a valid brand
has_validvaccine(X) :-
    vaccinated(X,vaccine(VaccineBrand,Days)),
    validbrand(Vaccinebrand,days(Min, Max)),
    Days > Min-1,
    Days < Max+1.


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
redlist(turkey).
redlist(uae).
redlist(uk).
redlist(us).

% For checking
list_of_red_countries(RedCountry) :- findall(Country, redlist(Country), RedCountry).

isolated(X) :-
    not(has_validvaccine(X)),
    travel(X,Y),
    redlist(Y).

% If recovered from COVID
% naatTest(X) :-


% TBH WE DONT KNOW THESE BAKA DI NEED? kasi eemail directly based on purpose nagiiba eh
% If traveling for work


% If traveling for academics

% PSEUDODATA

traveler(mark).
male(mark).
vaccinated(mark, vaccine(pfizer,7)).
% travel(mark,philippines).
travel(mark,switzerland).
