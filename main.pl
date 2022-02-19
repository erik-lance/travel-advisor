/*
 * MCO3 Travel Advisor Agent
 * Palpallatoc, Aaron
 * Mangoba, Michael Jhullian
 * Tiongquico, Erik
*/

:- dynamic traveler/1, travel/2,
        partysize/1, partyindex/1, memberNum/2,
        citizen/1, purpose/1, clergy/1,
        phpassport/1, ilpassport/1,
        a1visa/1, b1visa/1, a3visa/1,
        vaccinated/2, vaccine/2,
        booster/2, boosted/2,
        noTravel/1, yesTravel/1,
        hasCertificate/1,
        minor/1, partyindex/1,
        flightDays/1, returnDays/1.

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
        flight,
        return,
        ((not(purpose('v'))) -> purpose;
         true
        ),
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
    write('How many days until your flight?'),
    read(Days),
    assert(flightDays(Days)).

% ---- Note by erik: I think we can use ask() prompt for these. ---- %

return :-
    write('How many days do you plan to stay?'),
    read(Days),
    assert(returnDays(Days)),
    nl,
    (
        ( Days =< 90 ) -> assert(purpose(v));
        true
    ).

purpose :-
    write('What is the purpose of your travel?'), nl,
    write('(w) work'),nl,
    write('(r) returning resident'),nl,
    read(Response), 
    nl,
    (
        (Response == w; Response == r) -> assert(purpose(Response));
        write('Invalid input. Please try again.'),
        purpose
    ).

% ---- Questions ---- %

profile :-
    partyindex(Index),
    write('What\'s your name? '),
    read(Name),
    assert(traveler(Name)),
    assert(memberNum(Name,Index)),
    askMinor(Name),
    bioprofile(Name),
    passport(Name),
    (
        (purpose('r'), not(noTravel(Name))) -> (
            ((citizen(Name)) -> askILPassport(Name);
             askA1VISA(Name)
            )
        );
        (purpose('w'), not(noTravel(Name))) -> (
            askClergy(Name),
            ((clergy(Name)) -> askA3VISA(Name);
             askB1VISA(Name)
            )
        );
        (purpose('v'), not(noTravel(Name))) -> (
            write('You\'re all clear!')
        );
        (purpose('v'), noTravel(Name)) -> (printVisitorVisa);
        format('I am sorry ~w, but you can not travel', [Traveler])
        % write('I am sorry, but you can not travel.')
    ),
    checkParty(Name).

askMinor(Traveler) :-
    write('What is your current age?'),
    read(AgeResponse),
    ( 
        (AgeResponse < 18) -> (write('Are you accompanied by a Parent?'),
            read(PResponse),
            (
                (PResponse == 'no'; PResponse == 'n') -> assert(minor(Traveler))
                % hi gian ano ibig sabihin nito thx
            )
        );
        true
    ).

bioprofile(Traveler) :-
    write('Do you have an Israeli citizenship?'),
    read(Response),
    nl,
    (
        (Response == yes;   Response == y) -> assert(citizen(Traveler));
        true
    ).
    
passport(Traveler) :-
    write('Do you have a Philippine Passport?'),
    read(Response),
    nl,
    (
        (Response == yes; Response == y) -> assert(phpassport(Traveler));
        true
    ),
    ((not(citizen(Traveler))) -> covidFlow(Traveler);
     true
     ).

askvaccinated(Traveler) :-
    write('Are you vaccinated at 2nd dose? (y/n) '),
    read(VacResponse),
    nl,
    (
        (VacResponse == yes; VacResponse == y) -> (
            askvaccine(Traveler), 
            write('Have you taken booster shots? (y/n)'),
            read(BoostResponse), nl,
            (
                (BoostResponse == yes; BoostResponse == y) -> askbooster(Traveler);
                true
            )
        );

        (VacResponse == no; VacResponse == n) -> (
            write('Do you plan to get your second dose in the future days?'),
            read(FutureVac),
            (
                (FutureVac == yes; FutureVac == y) -> (
                    askfutureVaccine(Traveler),
                    write('Do you also plan to get a booster?'),
                    read(FutureBoost),
                    nl,
                    (
                        (FutureBoost == yes; FutureBoost == y) -> askfutureBooster(Traveler);
                        write('Alright! Note, if you plan to travel half a year or so from your vaccination, you might need a booster.'),
                        true
                    )
                );
                format('I am sorry ~w, but you can not travel', [Traveler]),
                % write('I am sorry, but you are not allowed to travel'),
                true
            )
        );

        true
    ).
% Duplicate to 
% Asks if have been tested positive in the past and recovered
/* askCertificate(Traveler) :-
    assert(recentlyPositive(Traveler)),
    write('have you recieved a health maintenance organization issued Certificate of Recovery from the european union'),
    read(Responsecertificate),
    nl,
    (
        (Responsecertificate == yes) -> assert(hasCertificate(Traveler))
    ).
*/
 
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
    write('How many days since your last booster shot? '),
    read(ResponseDays),nl,
    assert(boosted(Traveler, booster(ResponseBrand, ResponseDays))).

askfutureVaccine(Traveler) :-
    write('What vaccine brand do you plan to take?'),
    write('Note: if J&J, type jj.'),
    read(ResponseBrand), nl,
    write('How many days from now do you plan to take it?'),
    read(ResponseDays), nl,
    assert(vaccinated(Traveler, vaccine(ResponseBrand,-ResponseDays))).

askfutureBooster(Traveler) :-
    write('What booster brand do you plan to take?'),
    write('Note: if J&J, type jj.'),
    read(ResponseBrand), nl,
    write('How many days from now do you plan to take it?'),
    read(ResponseDays), nl,
    assert(boosted(Traveler, booster(ResponseBrand,-ResponseDays))).

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

% ---- Purpose Requirements ---- %

% Returning
askILPassport(Traveler) :-
    write('Do you have your Israeli Passport?'),
    read(Response),
    nl,
    (
        (Response == yes; Response == y) -> assert(ilpassport(Traveler))
    ).

askA1VISA(Traveler) :-
    write('Do you have an A/1 VISA?'),
    read(Response),
    nl,
    (
        (Response == yes; Response == y) -> assert(a1visa(Traveler));
        printTemporaryResidentVisa
    ).

% Work
askClergy(Traveler) :-
    write('Are you working as a Clergy?'),
    read(Response),
    nl,
    (
        (Response == yes; Response == y) -> assert(clergy(Traveler));
        true
    ).

askB1VISA(Traveler) :-
    write('Do you have a B/1 VISA?'),
    read(Response),
    nl,
    (
        (Response == yes; Response == y) -> assert(b1visa(Traveler));
        printWorkVisa
    ).

askA3VISA(Traveler) :-
    write('Do you have an A/3 VISA?'),
    read(Response),
    nl,
    (
        (Response == yes; Response == y) -> assert(a3visa(Traveler));
        printClergyVisa
    ).

% Visit (No requirements for now)



listRequirements(Traveler) :-  
    write('The Requirements for '), write(Traveler), write(': '), nl, nl,
    ( 
        not(hasCertificate(Traveler)) -> 
        write('European HMO issued Certificate of Recovery') 
    ),
    (
        minor(Traveler) -> 
        write('Letter of Consent to Travel to Israel from Parents'), nl,
        write('Letter and Proof of Adult supervision while in Israel'), nl
    ).

covidFlow(Traveler) :- 
    (
        redListPrompt(Traveler),
        (   has_travelredlist(Traveler) -> askExemption(Traveler);
            true
        )
    ),
    (
        (not(noTravel(Traveler)), not(yesTravel(Traveler))) -> askvaccinated(Traveler),
        (
            (not(has_validvaccine(Traveler))) -> askCertificate(Traveler);
            true
        );
        (   (has_validvaccine(Traveler)) ->assert(yesTravel(Traveler));
            true
        );
        true
    ).

% Asks if have been tested positive in the past and recovered
askCertificate(Traveler) :-
    assert(recentlyPositive(Traveler)),
    write('have you recieved a health maintenance organization issued Certificate of Recovery from the european union'), nl,
    read(Responsecertificate),
    nl,
    (
        (Responsecertificate == yes) -> assert(yesTravel(Traveler));
        (Responsecertificate == no) -> askExemption(Traveler);
        true
    ).

askExemption(Traveler) :-
    write('[yes/no]Do you have exceptional entry permission from the Population and Immigration Authority of Israel?'), nl,
    read(Response),
    (
        (Response == yes) -> assert(yesTravel(Traveler));
        (Response == no) -> assert(noTravel(Traveler));
        (
            write('Wrong input, valid inputs are [yes/no]'), nl,
            askExemption(Traveler),
            true
        )
    ).

redListPrompt(Traveler) :-
    write('How many countries have you visited or plan to visit within 14 days before your flight to Israel? (0 if none)'), nl,
    read(Number),
    ( (Number > 0) -> listCountry(Traveler, Number);
      true
    ).

listCountry(Traveler, Number) :-
    write('Input Said Country: '), nl,
    read(Country),
    assert(travel(Traveler, Country)),
    (   (Number - 1 > 0) -> listCountry(Traveler, Number - 1);
        true
    ).

printWorkVisa() :-
    write('Requirements for Working Visa: '), nl,
    write('     Completed Application for Work Visa found in Government website'), nl,
    write('     Verified Certificate of good Conduct '), nl,
    write('     Medical Certificate from recognized institutions'), nl,
    write('     Consent for Finger print scand and Photograph'), nl,
    write('     Payment Free for Visa'), nl,
    write('Please email the Israel Ministry of Interior for possible other requirements'), nl.

printVisitorVisa() :-
    write('Requirements for Visitor Visa:'), nl,
    write('     Completed Application for Visitor Visa found in Government website'), nl,
    write('     A photocopy of travel documents'), nl,
    write('     Proof of financial means (bank statements from last 3 months)'), nl,
    write('     Order for round trip airline tickets from and to Israel'), nl,
    write('     Two passport pictures (5x5)'), nl,
    write('     Payment Fee for Visa'), nl.

printClergyVisa() :-
    write('Requirements for Clergy Visa'), nl,
    write('     Completed Application for Clergy Visa found in Government website'), nl,
    write('     Payment Fee for Visa'), nl,
    write('     Two passport picture (5x5)'), nl,
    write('     Invitation from recognized religious Institute'), nl.

printTemporaryResidentVisa() :-
    write('To be applicable for a temporary Resident Visa one the following must be met'), nl,
    write('     A jew returning to Israel after being away or whose ancestors were away from Israel'), nl,
    write('     A person born from a Jewish Mother'), nl,
    write('     A convert to Judaism and not a member of any other religion'), nl,
    write('For more requirements, please contact the Israel Ministry of Interior').
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

has_travelredlist(Traveler) :-
    travel(Traveler,Country),
    redlist(Country).

% Isolation due to invalid vaccine (be it by brand or days vaccinated)
isolated(Traveler) :- not(has_validvaccine(Traveler)).

% % Isolation due to invalid vaccine AND traveled to a red list country.
isolated(Traveler) :-
    % not(has_validvaccine(Traveler)),
    travel(Traveler,Country),
    redlist(Country).

% Isolated due to Recently positive AND no Vaccine AND no HMO recovery Certificate
isolated(Traveler) :- 
    not(hasCertificate(Traveler)).

% Considered Recovered due to being recently positive AND has HMO recovery Certificate
recovered(Traveler) :-
    hasCertificate(Traveler),
    has_validvaccine(Traveler).

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