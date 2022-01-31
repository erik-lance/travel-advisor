/*
 * MCO3 Travel Advisor Agent
 * Palpallatoc, Aaron
 * Mangoba, Michael
 * Tiongquico, Erik
*/

:- dynamic traveler/1, partysize/1,
        male/1, female/1,
        vaccinated/2, vaccine/2.


welcome:- 
    write('Trip Advisor Agent (TAA) Israel'),
    nl,
    % ask('Would you like to travel to Israel?').
    write('What is the size of your party? '),
    read(PartySize), nl,
    partysize(PartySize),
    profile,
    
    write('Thank you.').



% This will be used for yes/no questions only.
ask(Question) :-
    write(Question),
    write('(yes/no)'),
    read(Response),
    nl,
    (
        (Response == yes; Response == y) -> assert(yes(Question));
        (Response ==  no; Response == n) -> assert(no(Question));
        write('Sorry. I do not recognize this input.'),
        fail
    ).

% ---- Questions ---- %

profile :-
    write('What\'s your name? '),
    read(Name),
    assert(traveler(Name)),
    askpurpose(Name).

askpurpose(Traveler) :-
    write('What is the purpose of your travel?'),
    write('(t) Touring/Visiting'),
    write('(w) Work'),
    write('(s) School'),
    read(Response),
    nl,
    (
        (Response == t) -> (assert(purpose(Traveler, visiting)), bioprofile(Traveler));
        (Response == w) -> assert(purpose(Traveler, work));
        (Response == s) -> assert(purpose(Traveler, school))
    ).
    

bioprofile(Traveler) :-
    write('Are you male or female? (m/f) '),
    read(Response),
    nl,
    (
        (Response == male;   Response == m) -> (assert(male(Traveler)),   askvaccinated(Traveler));
        (Response == female; Response == f) -> (assert(female(Traveler)), askvaccinated(Traveler));
        write('Sorry. I do not recognize this input. '), 
        bioprofile(Traveler)
    ).
    
askvaccinated(Traveler) :-
    write('Are you vaccinated? (y/n) '),
    read(VacResponse),
    nl,
    (
        (VacResponse == yes; VacResponse == y) -> askvaccine(Traveler);
        write('Edi okay')
    ).

% Asks if have been tested positive in the past and recovered
askPositive(Traveler) :-
    write('Have you tested positive in the recently (10 days)?'),
    read(Responsepositive),
	nl,
    (
		(
			(Responsepositive == yes) -> 
				assert(recentlyPositive(Traveler)),
				write('have you recieved a health maintenance organization issued Certificate of Recovery'),
				read(Responsecertificate),
				nl,
				((Responsecertificate == no) ->
					write('Please acquire a HMO issued Certificate of Recovery before going to Israel to be recognized as recovered'),
					assert(notRecovered(Traveler))
				);
				((Responsecertificate == yes) ->
					assert(recovered(Traveler)),
				)
		);
		
		((Responsepositive == no) ->
			assert(negative(Traveler)),
		)
	).
 
% Asks for brand and days since last vaccination of traveler
% Note: edit for booster eventually.
askvaccine(Traveler) :-
    write('What is your most recent vaccine brand? (pfizer/moderna/astrazeneca/sinovac/sinopharm/jj) '),
    read(ResponseBrand),
    nl,
    write('How many days since your last vaccination? '),
    read(ResponseDays),
    nl,
    assert(vaccinated(Traveler,vaccine(ResponseBrand,ResponseDays))).


% ------------------- IGNORE EVERYTHING BETWEEN FOR NOW ------------------- %

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

% ---- RULES ---- %

% X has a validvaccine IF
% X is vaccinated with a valid brand
has_validvaccine(Traveler) :-
    vaccinated(Traveler,vaccine(VaccineBrand,Days)),
    validbrand(Vaccinebrand,days(Min, Max)),
    Days > Min-1,
    Days < Max+1.

% Isolation due to invalid vaccine (be it by brand or days vaccinated)
isolated(Traveler) :- not(has_validvaccine(Traveler)).

% Isolation due to invalid vaccine AND traveled to a red list country.
isolated(Traveler) :-
    not(has_validvaccine(Traveler)),
    travel(Traveler,Country),
    redlist(Country).

% Is traveler considered Recovered?
considered_recovered(Traveler) :-
    not()

% ---- DICTIONARY ---- %

traveldate(
    startdate(Month, Day, Year),
    enddate(Month, Day, Year)
).

list_of_travels(X,TravelList) :- findall(Country, travel(X,Country), TravelList).

% Profile of User

% Purposes

% COVID tests

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
