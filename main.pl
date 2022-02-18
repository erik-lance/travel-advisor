/*
 * MCO3 Travel Advisor Agent
 * Palpallatoc, Aaron
 * Mangoba, Michael Jhullian
 * Tiongquico, Erik
*/

:- dynamic traveler/1, travel/2,
        partysize/1, partyindex/1, memberNum/2,
        citizen/1, purpose/1,
        vaccinated/2, vaccine/2,
        booster/2, boosted/2,
        recentlyPositive/1,
        hasCertificate/1,
        minor/1, partyindex/1.


welcome:- 
    write('Trip Advisor Agent (TAA) Israel'),
    nl,
    % ask('Would you like to travel to Israel?').
    write('What is the size of your party? '),
    read(PartySize), nl,
    (
      (PartySize > 0, PartySize < 5) -> (
        assert(partysize(PartySize)),
        assert(partyindex(1)),
        profile,    
        write('Thank you.')                           
      );
        write('Sorry, but your party must be at the size of 1-4 only.'),
        welcome
    ).
    
% This will be used for yes/no questions only.
ask(Question, Desc) :-
    write(Question),
    write('(yes/no)'),
    read(Response),
    nl,
    (
        (Response == yes; Response == y) -> assert(yes(Desc));
        (Response ==  no; Response == n) -> assert(no(Desc));
        write('Sorry. I do not recognize this input.'),
        fail
    ).

% ---- Questions for all ---- %
flight :-
    write('What\s the date of your flight?'),
    write('Month: '),
    read(FlightM),
    write('Day: '),
    read(FlightD),
    write('Year: '),
    read(FlightY).

arrival :-
    write('What\s the date of your arrival?'),
    write('Month: '),
    read(FlightM),
    write('Day: '),
    read(FlightD),
    write('Year: '),
    read(FlightY).

% ---- Note by erik: I think we can use ask() prompt for these. ---- %

israeliCitizenship :-
    write('Does your party have an Israeli citizenship?'),
    read(Response),
    nl,
    (
        (Response == yes; Response == y) -> assert(yes('citizen'))
    ).

return :-
    write('Do you intend to stay longer than 90 days?'),
    read(Response),
    nl,
    (
        (Response == yes; Response == y) -> assert(yes('stay'))
    ).

% ---- Questions ---- %

profile :-
    partyindex(Index),
    write('What\'s your name? '),
    read(Name),
    assert(traveler(Name)),
    assert(memberNum(Name,Index)),
    askpurpose,
    bioprofile(Name).

 askpurpose :-
    write('What is the purpose of your travel?'),
    write('(t) Touring/Visiting'),
    write('(w) Work'),
    write('(s) School'),
    read(Response),
    nl,
    (
        (Response == t) -> (assert(purpose('visiting')));
        (Response == w) -> assert(purpose('work'));
        (Response == s) -> assert(purpose('school'));
        write('Invalid Input.'),
        askpurpose
    ).   

bioprofile(Traveler) :-
    write('Are you an Israeli citizen?'),
    read(Response),
    nl,
    (
        (Response == yes;   Response == y) -> assert(citizen(Traveler)),
        askvaccinated(Traveler)
    ).
    
askMinor(Traveler) :-
    write('What is your current age?'),
    read(AgeResponse),
    ( 
        (AgeResponse < 18) -> 
        write('Are you accompanied by a Parent?'),
        read(PResponse),
        (
            (PResponse == 'no'; PResponse == 'n') ->
            assert(minor(Traveler))
        )
    ).

askvaccinated(Traveler) :-
    write('Are you vaccinated? (y/n) '),
    read(VacResponse),
    nl,
    (
        (VacResponse == yes; VacResponse == y) -> (
            askvaccine(Traveler), 
            write('Have you taken booster shots? (y/n)'),
            read(BoostResponse), nl,
            (
                (BoostResponse == yes; BoostResponse == y) -> askbooster(Traveler)
            )
        );
        write('Edi okay'), nl,
        (VacResponse == yes; VacResponse == y) -> askvaccine(Traveler);
        write('Edi okay'),
        nl
    ),
    askPositive(Traveler).

% Asks if have been tested positive in the past and recovered
askPositive(Traveler) :-
    write('Have you tested positive in the recently (10 days)?'),
    read(Responsepositive),
	nl,
    (
        (Responsepositive == yes) -> (
            assert(recentlyPositive(Traveler)),
            write('have you recieved a health maintenance organization issued Certificate of Recovery'),
            read(Responsecertificate),
            nl,
            (
                (Responsecertificate == no) -> write('Please acquire a HMO issued Certificate of Recovery before going to Israel to be recognized as recovered');
                (Responsecertificate == yes) -> assert(hasCertificate(Traveler))
            )
        );
        nl
	),
    checkParty(Traveler).
 
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

askbooster(Traveler) :-
    write('What is your most recent booster brand? (pfizer/moderna/astrazeneca/sinovac/sinopharm/jj)'),
    read(ResponseBrand),nl,
    write('How many days since you last booster shot? '),
    read(ResponseDays),nl,
    assert(boosted(Traveler, booster(ResponseBrand, ResponseDay))).

checkParty(Traveler) :-
    partysize(Capacity),
    memberNum(Traveler,PartyNum),
    (
        (PartyNum < Capacity) -> (
                                    write('How about the next person?'),
                                    retract(partyindex(PartyNum)),
                                    assert(partyindex(PartyNum+1)),
                                    nl,
                                    profile
                                 );
        write('All members have been checked.')
    ).

listRequirements(Traveler) :-  
    write('The Requirements for '), write(Traveler), write(': '), nl, nl,
    ( 
        not(hasCertificate(Traveler)) -> 
        write('HMO issued Certificate of Recovery') 
    ),
    (
        minor(Traveler) -> 
        write('Letter of Consent to Travel to Israel from Parents'), nl,
        write('Letter and Proof of Adult supervision while in Israel'), nl
    ),
    (
       not(hasRoundTrip(Traveler)) ->     
       write('Travel plans after stay (Return Ticket)'), nl
    ).

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
    validbrand(VaccineBrand,days(Min, Max)),
    Days > Min-1,
    Days < Max+1.

% Isolation due to invalid vaccine (be it by brand or days vaccinated)
isolated(Traveler) :- not(has_validvaccine(Traveler)).

% % Isolation due to invalid vaccine AND traveled to a red list country.
isolated(Traveler) :-
    % not(has_validvaccine(Traveler)),
    travel(Traveler,Country),
    redlist(Country).

% Isolated due to Recently positive AND no Vaccine AND no HMO recovery Certificate
isolated(Traveler) :-
    recentlyPositive(Traveler),
    not(hasCertificate(Traveler)).

% Considered Recovered due to being recently positive AND has HMO recovery Certificate
recovered(Traveler) :-
    hasCertificate(Traveler),
    recentlyPositive(Traveler).

% ---- DICTIONARY ---- %

traveldate(
    flightdate(Month, Day, Year),
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

% % Isolated due to red country
% traveler(mark).
% male(mark).
% vaccinated(mark, vaccine(pfizer,7)).
% travel(mark,philippines).
% travel(mark,switzerland).

% % Not isolated, has valid vaccine and is considered recovered
% traveler(vincent).
% male(vincent).
% vaccinated(vincent, vaccine(moderna,15)).
% hasCertificate(vincent).
% recentlyPositive(vincent).

% % Isolated due to invalid vaccine (exceeded max days)
% traveler(bulleros).
% male(bulleros).
% vaccinated(bulleros, vaccine(moderna, 189)).

% % Considered Recovered, has cert while positive
% traveler(martelino).
% male(martelino).
% hasCertificate(martelino).
% recentlyPositive(martelino).

% % Isolated, no valid vaccine
% traveler(kate).
% female(kate).
% vaccinated(kate, vaccine(pfizer,3)).

% % Isolated, no vaccine, no cert, and is only recently positive.
% % Also not recovered
% traveler(belo).
% female(belo).
% recentlyPositive(belo).

% % Vaccine is not part of database of valid vaccines.
% traveler(yuri).
% male(yuri).
% vaccinated(yuri, vaccine(sputnik,14)).