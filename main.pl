/*
 * MCO3 Travel Advisor Agent
 * Palpallatoc, Aaron
 * Mangoba, Michael Jhullian
 * Tiongquico, Erik
*/

:- dynamic traveler/1, travel/2, purpose/1,
        partysize/1, partyindex/1, memberNum/2,
        vaccinated/2, vaccine/2,
        booster/2, boosted/2,
        noTravel/1, yesTravel/1,
        hasCertificate/1,
        minor/1, partyindex/1,
        flightDays/1, returnDays/1,
        
        yes/2, no/2.

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
        (
            not(purpose('v')) -> purpose;
               (purpose('v')) -> true
        ),
        profile,    
        write('Thank you.')                           
      );
        write('Sorry, but your party must be at the size of 1-4 only.'),
        welcome
    ).
    
% This will be used for yes/no questions only.
ask(Traveler, Question, Desc) :-
    write(Question),
    write(' (yes/no) or (y/n)'),
    read(Response),
    nl,
    (
        (Response == yes; Response == y) -> assert(yes(Traveler, Desc));
        (Response ==  no; Response == n) -> assert(no(Traveler, Desc));
        (
            write('Sorry. I do not recognize this input.'),
            ask(Traveler, Question, Desc)
        )
    ).

% ---- Questions for all ---- %
flight :-
    write('How many days until your flight?'),
    read(Days),
    assert(flightDays(Days)).

return :-
    write('How many days do you plan to stay?'),
    read(Days),
    assert(returnDays(Days)),
    nl,
    (
        ( Days =< 90 ) -> assert(purpose(v));
        ( Days  > 90 ) -> write('This means you are no longer elligible for a tourist VISA. It is only elligible for those staying below 90 days.'), nl
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
    ask(Name,'Do you have an Israeli citizenship?','citizen'),
    ask(Name,'Do you have a Philippine Passport?','phpassport'),
    (
         (no(Name,'citizen')) -> covidFlow(Name);
        (yes(Name,'citizen')) -> true
    ),
    (
        (purpose('r'), covid_result(Name)) -> (
            (
                (yes(Name,'citizen')) -> ask(Name, 'Do you have your Israeli Passport?', 'ilpassport'),
                                         ask(Name, 'Do you have an A/1 VISA?',           'a1visa')
            )
        );

        (purpose('w'),  covid_result(Name)) -> 
        (
            ask(Name, 'Are you working as a Clergy?','clergy'),
            (
                (yes(Name,'clergy')) -> ask(Name, 'Do you have an A/3 VISA?', 'a3visa'),
                                        ask(Name, 'Do you have a B/1 VISA?',  'b1visa')
            )
        );

        (purpose('v'),  covid_result(Name)) -> (
            write('You\'re all clear!')
        );

        (purpose('v'), not(covid_result(Name))) -> (
            printVisitorVisa,
            format('I am sorry ~w, but you can not travel', [Traveler])
        )
    ),
    checkParty(Name).

% Returning

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
        (AgeResponse >= 18) -> true
    ).

askvaccinated(Traveler) :-
    ask(Traveler, 'Are you vaccinated at 2nd dose?', 'vaccinated'),
    nl,
    (
        (yes(Traveler,'vaccinated')) -> (
            askvaccine(Traveler), 
            ask(Traveler, 'Have you taken booster shots?', 'booster'),
            (
                yes(Traveler,'booster') -> askbooster(Traveler);
                no(Traveler,'booster') -> (
                    ask(Traveler, 'Do you plan to get a booster in the future days?', 'futurebooster'),
                    nl,
                    (
                        (yes(Traveler,'futurebooster')) -> askfutureBooster(Traveler);
                        (no(Traveler,'futurebooster')) -> write('Alright! Note, if you plan to travel half a year or so from your vaccination, you might need a booster.'), nl
                    )
                )
            )
        );

        (no(Traveler,'vaccinated')) -> (
            ask(Traveler,'Do you plan to get your second dose in the future days?', 'futurevac'),
            (
                (yes(Traveler,'futurevac')) -> (
                    askfutureVaccine(Traveler),
                    ask(Traveler,'Do you also plan to get a booster?', 'futurebooster'),
                    nl,
                    (
                        (yes(Traveler,'futurebooster')) -> askfutureBooster(Traveler);
                        (no(Traveler,'futurebooster')) -> write('Alright! Note, if you plan to travel half a year or so from your vaccination, you might need a booster.'), nl
                    )
                );
                (no(Traveler,'futurevac')) -> (
                    format('I am sorry ~w, but you can not travel', [Traveler])
                )
            )
        )
    ).
 
% Asks for brand and days since last vaccination of traveler
% Note: edit for booster eventually.
askvaccine(Traveler) :-
    write('What is your most recent vaccine brand? (pfizer/moderna/astrazeneca/sinovac/sinopharm/jj) '), nl,
    read(ResponseBrand),
    nl,
    write('How many days since your last vaccination? '),
    read(ResponseDays),
    nl,
    assert(vaccinated(Traveler,vaccine(ResponseBrand,ResponseDays))).

askbooster(Traveler) :-
    write('What is your most recent booster brand? (pfizer/moderna/astrazeneca/sinovac/sinopharm/jj)'), nl,
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
        (   has_travelredlist(Traveler) ->
                ask(Traveler, 'Do you have exceptional entry permission from the Population and Immigration Authority of Israel?', 'exemption');
            not(has_travelredlist(Traveler)) -> (
                askvaccinated(Traveler),
                (not(has_validvaccine(Traveler))) -> (
                    ask(Traveler, 'Have you recieved a health maintenance organization issued Certificate of Recovery from the european union', 'certificate')
                );
                (has_validvaccine(Traveler)) -> write('Your COVID documents appear in order')
            )
        )
    ).

redListPrompt(Traveler) :-
    write('How many countries have you visited or plan to visit within 14 days before your flight to Israel? (0 if none)'), nl,
    read(Number),
    ( (Number > 0) -> listCountry(Traveler, Number);
      (Number =< 0) -> true
    ).

listCountry(Traveler, Number) :-
    write('Input Said Country: '), nl,
    read(Country),
    assert(travel(Traveler, Country)),
    (   (Number - 1 > 0) -> listCountry(Traveler, Number - 1);
        (Number - 1 =<0) -> write('All countries listed.')
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

% --------------- Everything below is the knowledge base --------------- %

% ---- RULES ---- %

% X has a validvaccine IF
% X is vaccinated with a valid brand
has_validvaccine(Traveler) :-
    vaccinated(Traveler,vaccine(VaccineBrand,Days)),
    validbrand(VaccineBrand,days(Min, Max)),
    Days > Min-1,
    Days < Max+1.

% Checks if traveler has visited a red list country
has_travelredlist(Traveler) :-
    travel(Traveler,Country),
    redlist(Country).

% No red list coutnries and has valid vaccine
covid_result(Traveler) :-
    not(has_travelredlist(Traveler)),
    has_validvaccine(Traveler).

% No red list, no valid vaccine, but has a certificate of recovery
covid_result(Traveler) :-
    not(has_travelredlist(Traveler)),
    not(has_validvaccine(Traveler)),
    yes(Traveler,'certificate').

% No red list, no valid vaccine, no certificate of recovery, but has an entry of permission exemption
covid_result(Traveler) :-
    not(has_travelredlist(Traveler)),
    not(has_validvaccine(Traveler)),
    no(Traveler,'certificate'),
    yes(Traveler,'exemption').

% Has red list, but has an exemption.
covid_result(Traveler) :-
    has_travelredlist(Traveler),
    yes(Traveler, 'exemption').

% ---- DICTIONARY ---- %

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