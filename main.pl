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
        minor/1, partyindex/1,
        flightDays/1, returnDays/1,
        asknum/2,
        yes/1, no/1,
        yes/2, no/2.

welcome:- 
    write('Trip Advisor Agent (TAA) Israel'),
    nl,
    % ask('Would you like to travel to Israel?').
    asknum('What is the size of your party? ', PartySize), nl,
    (
      (PartySize > 0, PartySize < 5) -> (
        assert(partysize(PartySize)),
        assert(partyindex(1)),
        flight,
        return,
        profile,
        minors,    
        write('Thank you.')                           
      );
        write('Sorry, but your party must be at the size of 1-4 only.'),nl,
        welcome
    ).
    
% Thes will be used for yes/no questions only.
ask(Question, Desc) :-
    write(Question), nl,
    write(' (yes/no) or (y/n)'),
    read(Response),
    nl,
    (
        (Response == yes; Response == y) -> assert(yes(Desc));
        (Response ==  no; Response == n) -> assert(no(Desc));
        (
            write('Sorry. I do not recognize this input. '),
            ask(Question, Desc)
        )
    ).

ask(Traveler, Question, Desc) :-
    write(Question), nl,
    write(' (yes/no) or (y/n)'),
    read(Response),
    nl,
    (
        (Response == yes; Response == y) -> assert(yes(Traveler, Desc));
        (Response ==  no; Response == n) -> assert(no(Traveler, Desc));
        (
            write('Sorry. I do not recognize this input. '),
            ask(Traveler, Question, Desc)
        )
    ).

% This will be used for numerical questions only.
% The number inputted will be stored to Answer variable.
asknum(Question, Answer) :-
    write(Question), nl,
    read(Response), nl,
    (
        (integer(Response)) -> Response = Answer;
        (
            write('Please input a number.'),nl,
            asknum(Question, Answer)
        )   
    ).

% ---- Questions for all ---- %
flight :-
    asknum('How many days until your flight? ', Days),
    assert(flightDays(Days)).

return :-
    ask('Is your stay temporary?','stay'),
    (   
        yes('stay') -> (
            asknum('How many days do you approximately plan to stay?', Days),
            assert(returnDays(Days)),
            nl,
            (
                ( Days =< 90 ) -> assert(purpose(v));
                ( Days  > 90 ) -> write('This means you are no longer elligible for a tourist VISA. It is only elligible for those staying below 90 days.'), nl, assert(purpose(w))
            )
        );
        no('stay') -> assert(purpose(r))
    ).

minors :- 
    aggregate_all(count, minor(_), X),
    partysize(Y),
    (
      (X == Y) -> write('Unfortunately, a group of minors are not allowed to travel.'), nl;
      true
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
        % Returning Citizens
        (purpose('r'), has_validCOVID_documents(Name)) -> (
            (yes(Name,'citizen')) -> (
                ask(Name, 'Do you have your Israeli Passport?', 'ilpassport'),
                (    can_travel(Name))  -> write('You are all set! '), nl;
                (not(can_travel(Name))) -> format('I am sorry, ~w, but you are missing requirements.', [Name]), nl,
                                           write('You need to provide a valid proof of your Israeli Citizenship'), nl,
                                           write('to comply with the law of return.'), nl
            );
            ( no(Name,'citizen')) -> (
                ask(Name, 'Do you have an A/1 VISA?','a1visa'),
                (
                    can_travel(Name) -> write('You are all set!'), nl;
                    not(can_travel(Name)) -> format('I am sorry, ~w, but you are missing requirements.', [Name]), nl, printTemporaryResidentVisa()
                )
            )
        );

        % Travel for Work / Official Business
        % (Working, and is either a citizen or passed their covid results)
        (purpose('w'), has_validCOVID_documents(Name)) -> 
        (
            (no(Name,'citizen')) -> (
                ask(Name, 'Are you working as a Clergy?','clergy'),
                (
                    (yes(Name,'clergy')) -> ask(Name, 'Do you have an A/3 VISA?', 'a3visa');
                    ( no(Name,'clergy')) -> ask(Name, 'Do you have a B/1 VISA?',  'b1visa')
                ),
                (
                    can_travel(Name) -> write('You are all set!'), nl;
                    (not(can_travel(Name))) -> format('I am sorry, ~w, but you are missing requirements.', [Name]), nl,
                    (
                        (yes(Name,'clergy'), no(Name,'a3visa')) -> printClergyVisa;
                        ( no(Name,'clergy'), no(Name,'b1visa')) -> printWorkVisa
                    )    
                )
            );
            (yes(Name,'citizen')) -> (
                ask(Name, 'Do you have your Israeli Passport?', 'ilpassport'), nl,
                (
                    (     can_travel(Name)) -> write('You are all set!'), nl;
                    ( not(can_travel(Name))) -> format('I am sorry, ~w, but you are missing requirements.', [Name]), nl,
                                               write('You need to provide a valid proof of your Israeli Citizenship'), nl,
                                               write('to comply with the law of return.'), nl
                )
            )
        );

        % Travel for Visiting or Touring
        % (Visiting, and is either a citizen or passed their covid results)
        (purpose('v')) -> (
            has_validCOVID_documents(Name) -> (
                (no(Name,'citizen')) -> write('You are all set!'), nl;
                (yes(Name,'citizen')) -> (
                    ask('Do you have your Israeli passport?', 'ilpassport'),
                    (
                        (yes(Name,'ilpassport')) -> write('You are all set!'), nl;
                        ( no(Name,'ilpassport'))  -> format('I am sorry ~w, but you can not travel. ~n', [Name]),  nl,
                                                    write('You need to provide a valid proof of your Israeli Citizenship'), nl,
                                                    write('to comply with the law of return.'), nl
                    )
                )
            );
            not(has_validCOVID_documents(Name)) -> format('I am sorry ~w, but you can not travel. ~n', [Name]),  nl,
                                                   printVisitorVisa
        );

        % For those who failed the COVID tests or documents.
        (not(covid_result(Name)); not(can_travel(Name))) -> format('I am sorry ~w, but you do not have the valid requirements. ~n', [Name]), nl
    ),
    checkParty(Name).

askMinor(Traveler) :-
    asknum('What is your current age? ', AgeResponse),
    ( 
         (AgeResponse < 18) -> assert(minor(Traveler));
        (AgeResponse >= 18) -> true
    ).

askvaccinated(Traveler) :-
    ask(Traveler, 'Are you vaccinated at 2nd dose? (or 1 for J&J)', 'vaccinated'),
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
        % This will then ask if they plan to get vaccines in the future.
        (no(Traveler,'vaccinated')) -> (
            ask(Traveler,'Do you plan to get your second dose (or 1st for J&J) in the future days?', 'futurevac'),
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
                    format('I am sorry ~w, but you can not travel. ~n', [Traveler])
                )
            )
        )
    ).
 
% Asks for brand and days since last vaccination of traveler
% Note: edit for booster eventually.
askvaccine(Traveler) :-
    write('What is your most recent vaccine brand? (pfizer/moderna/astrazeneca/sinovac/sinopharm/jj) '), nl,
    read(ResponseBrand), nl,
    asknum('How many days since your last vaccination? ', ResponseDays), nl,
    assert(vaccinated(Traveler,vaccine(ResponseBrand,ResponseDays))).

askbooster(Traveler) :-
    write('What is your most recent booster brand? (pfizer/moderna/astrazeneca/sinovac/sinopharm/jj)'), nl,
    read(ResponseBrand), nl,
    asknum('How many days since your last booster shot? ',ResponseDays), nl,
    assert(boosted(Traveler, booster(ResponseBrand, ResponseDays))).

askfutureVaccine(Traveler) :-
    write('What vaccine brand do you plan to take? '),
    write('Note: if J&J, type jj.'), nl,
    read(ResponseBrand), nl,
    asknum('How many days from now do you plan to take it? ', ResponseDays), nl,
    assert(vaccinated(Traveler, vaccine(ResponseBrand,-ResponseDays))).

askfutureBooster(Traveler) :-
    write('What booster brand do you plan to take? '),
    write('Note: if J&J, type jj.'), nl,
    read(ResponseBrand), nl,
    asknum('How many days from now do you plan to take it? ',
    ResponseDays), nl,
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
        (PartyNum >= Capacity) -> write('All members have been checked. ')
    ).

covidFlow(Traveler) :- 
    (
        redListPrompt(Traveler),
        (   has_travelredlist(Traveler) ->
                ask(Traveler, 'Do you have exceptional entry permission from the Population and Immigration Authority of Israel?', 'exemption');
            not(has_travelredlist(Traveler)) -> (
                askvaccinated(Traveler),
                (not(has_validvaccine(Traveler))) -> (
                    ask(Traveler, 'Have you recieved a health maintenance organization issued Certificate of Recovery from the European Union?', 'certificate'),
                    (no(Traveler, 'certificate')) -> (
                        ask(Traveler, 'Do you have exceptional entry permission from the Population and Immigration Authority of Israel?', 'exemption')
                    );
                    true
                );
                (has_validvaccine(Traveler)) -> write('Your COVID documents appear in order.'),nl
            )
        ),
        % This prints if traveler failed the COVID reqs.
        (no(Traveler,'exemption')) -> printCOVIDReqs;
        true
    ).

redListPrompt(Traveler) :-
    asknum('How many countries have you visited or plan to visit within 14 days before your flight to Israel? (0 if none)', Number),
    ( (Number > 0) -> listCountry(Traveler, Number);
      (Number =< 0) -> true
    ).

listCountry(Traveler, Number) :-
    write('Input Said Country: '), nl,
    read(Country),
    assert(travel(Traveler, Country)),
    (   (Number - 1 > 0) -> listCountry(Traveler, Number - 1);
        (Number - 1 =<0) -> write('All countries listed. '),nl
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
    write('To be applicable for an A/1 Temporary Resident visa, the following must be met'), nl,
    write('     A jew returning to Israel after being away or whose ancestors were away from Israel'), nl,
    write('     A person born from a Jewish Mother'), nl,
    write('     A convert to Judaism and not a member of any other religion'), nl,
    write('For more requirements, please contact the Israel Ministry of Interior'), nl.

printCOVIDReqs() :-
    write('COVID Test Requirements'), nl,
    write('     Travelers must be vaccinated with a WHO (World Health Organization) approved vaccine'), nl,
    write('     or recovery confirmed by a country from the European Union with a certificate.'), nl,
    write('     Vaccines valid: (Pfizer/Moderna/AstraZeneca/Sinovac/Sinopharm/J&J)'), nl,
    write('         - Vaccinated with two doses (or 1 for J&J)'), nl,
    write('         - However, for Pfizer vaccines, 7 days is the minimum.'), nl,
    write('         - 14 days or more have passed since date of second dose on flight.'), nl,
    write('         - but not more than 180 days on the day of leaving Israel.'), nl,
    write('     You must also have not traveled to a red listed country the past 14 days before boarding.'), nl,
    write('     However, if you can present an exemption certificate, you will not need any of the above.'), nl.

% --------------- Everything below is the knowledge base --------------- %

% ---- RULES ---- %

% X has a validvaccine IF
% X is vaccinated with a valid brand
has_validvaccine(Traveler) :-
    vaccinated(Traveler,vaccine(VaccineBrand,Days)),
    validbrand(VaccineBrand,days(Min, Max)),
    flightDays(FDays),
    returnDays(RDays),
    Days + FDays > Min-1,
    Days + FDays + RDays < Max+1.

has_validvaccine(Traveler) :-
    boosted(Traveler,booster(VaccineBrand,Days)),
    validbrand(VaccineBrand,days(Min, _)),
    flightDays(FDays),
    Days + FDays > Min-1.

% This is for cases where only pre-flight is relevant.
has_valid_preflightvaccine(Traveler) :-
    vaccinated(Traveler,vaccine(VaccineBrand,Days)),
    validbrand(VaccineBrand,days(Min,_)),
    flightDays(FDays),
    FDays + Days > Min-1.

has_valid_preflightvaccine(Traveler) :-
    boosted(Traveler,booster(VaccineBrand,Days)),
    validbrand(VaccineBrand,days(Min,_)),
    flightDays(FDays),
    FDays + Days > Min-1.

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

% Travel Summary
has_validCOVID_documents(Traveler) :-
    purpose('r'),
    no(Traveler,'citizen'),
    covid_result(Traveler),
    has_valid_preflightvaccine(Traveler).

has_validCOVID_documents(Traveler) :-
    purpose('r'),
    yes(Traveler,'citizen').

has_validCOVID_documents(Traveler) :-
    (purpose('w') ; purpose('v')),
    no(Traveler,'citizen'),
    covid_result(Traveler).

has_validCOVID_documents(Traveler) :-
    (purpose('w') ; purpose('v')),
    yes(Traveler,'citizen').


% Can travel as a returning citizen if owns an IL passport OR owns an A1 VISA
can_travel(Traveler) :-
    purpose('r'),
    yes(Traveler,'ilpassport'); yes(Traveler,'a1visa').

% Can travel as a working citizen if owns an IL passport
can_travel(Traveler) :-
    purpose('w'),
    yes(Traveler,'citizen'),
    yes(Traveler,'ilpassport').

% Can travel as for work if clergy with a3 VISA OR non clergy with b1 VISA
can_travel(Traveler) :-
    purpose('w'),
    (
        yes(Traveler,'clergy'),
        yes(Traveler,'a3visa')
    ) ;
    (
        no(Traveler,'clergy'),
        yes(Traveler,'b1visa')
    ).

% Can travel for visit if not a citizen and has valid COVID documents
can_travel(Traveler) :-
    purpose('v'),
    no(Traveler,'citizen'),
    has_validCOVID_documents(Traveler).

% Can travel for visit if citizen and has passport
can_travel(Traveler) :-
    purpose('v'),
    yes(Traveler,'citizen'),
    yes(Traveler,'ilpassport').

% ---- DICTIONARY ---- %

list_of_travels(X,TravelList) :- findall(Country, travel(X,Country), TravelList).

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