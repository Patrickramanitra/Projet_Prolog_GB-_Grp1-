% ============================================================
%  Projet IA – Enquête policière en PROLOG (Phase 2 : Modélisation)
%  + Interface Web (SWI‑Prolog HTTP)
%  Fichier unique : faits + règles + cas spéciaux + utilitaires + serveur HTTP
%  SWI-Prolog recommandé (swipl)
% ============================================================

:- module(enquete_policiere, [
    % Requêtes principales (moteur logique)
    is_guilty/2,
    innocent/2,
    explain_guilt/3,
    find_all_guilty/2,
    % Serveur HTTP
    server/1,
    stop_server/1
]).

/* -----------------------------------------------------------
   LIBRAIRIES HTTP (SWI-Prolog)
----------------------------------------------------------- */
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/html_write)).
:- use_module(library(http/http_json)).

:- multifile http:location/3.
http:location(api, '/api', []).  % /api/... pour endpoints JSON

/* -----------------------------------------------------------
   DÉCLARATIONS DYNAMIQUES (facultatif : permet d’ajouter/supprimer à chaud)
----------------------------------------------------------- */

:- discontiguous has_motive/2.
:- discontiguous was_near_crime_scene/2.
:- discontiguous has_fingerprint_on_weapon/2.
:- discontiguous temoin/2.
:- discontiguous eyewitness_identification/2.
:- discontiguous fiable/1.
:- discontiguous non_fiable/1.
:- discontiguous utilise_avant_crime/2.
:- discontiguous arme_du_crime/2.
:- discontiguous alibi/3.
:- discontiguous crime/3.
:- discontiguous build_suspect_links/2.

:- dynamic suspect/1.
:- dynamic crime_type/1.
:- dynamic has_motive/2.
:- dynamic was_near_crime_scene/2.
:- dynamic has_fingerprint_on_weapon/2.
:- dynamic has_bank_transaction/2.
:- dynamic owns_fake_identity/2.
:- dynamic temoin/2.
:- dynamic eyewitness_identification/2.
:- dynamic fiable/1.
:- dynamic non_fiable/1.
:- dynamic utilise_avant_crime/2.
:- dynamic arme_du_crime/2.
:- dynamic alibi/3.
:- dynamic crime/3.


/* -----------------------------------------------------------
   FAITS (dataset fourni)
----------------------------------------------------------- */

% Types de crimes
crime_type(assassinat).
crime_type(vol).
crime_type(escroquerie).

% Suspects
suspect(john).
suspect(mary).
suspect(alice).
suspect(bruno).
suspect(sophie).

% Indices (faits)
% VOL – John
has_motive(john, vol).
was_near_crime_scene(john, vol).
has_fingerprint_on_weapon(john, vol).

% ASSASSINAT – Mary
has_motive(mary, assassinat).
was_near_crime_scene(mary, assassinat).
has_fingerprint_on_weapon(mary, assassinat).

% ESCROQUERIE – Alice / Bruno / Sophie
has_motive(alice, escroquerie).
has_bank_transaction(alice, escroquerie).

has_bank_transaction(bruno, escroquerie).
owns_fake_identity(sophie, escroquerie).

% Cas spéciaux – Témoins
temoin(paul, assassinat).
eyewitness_identification(mary, assassinat).   % Paul dit avoir vu Mary
fiable(paul).

temoin(julie, vol).
eyewitness_identification(john, vol).
non_fiable(julie).

% Faux positifs (empreintes présentes avant le crime)
utilise_avant_crime(john, couteau).
arme_du_crime(couteau, assassinat).

% Alibi & contexte du crime (exemple pour illustrer la règle d’innocence)
alibi(alice, restaurant, '20:00').
crime(escroquerie, '20:00', bureau).

/* -----------------------------------------------------------
   AIDES LOGIQUES (qualité du témoin, faux positifs)
----------------------------------------------------------- */

% Un témoin "valide" est un témoin déclaré fiable pour un type de crime
temoin_valide(T, Type) :-
    temoin(T, Type),
    fiable(T).

% Faux positif : empreinte sur l’arme mais laissée avant le crime
faux_positif(Suspect, CrimeType) :-
    has_fingerprint_on_weapon(Suspect, CrimeType),
    utilise_avant_crime(Suspect, Arme),
    arme_du_crime(Arme, CrimeType).

/* -----------------------------------------------------------
   INNOCENCE (alibi simple basé sur lieu/heure du crime)
----------------------------------------------------------- */

% Règle d’innocence : si, à l’heure du crime (Type, Heure, Lieu),
% le suspect a un alibi dans un autre lieu, il est innocent.
innocent(Suspect, CrimeType) :-
    crime(CrimeType, Heure, LieuCrime),
    alibi(Suspect, LieuAlibi, Heure),
    LieuAlibi \= LieuCrime.

/* -----------------------------------------------------------
   CULPABILITÉ (règles par type de crime)
----------------------------------------------------------- */

% VOL : mobile + présence + (empreinte OU témoin fiable) + pas d’innocence + pas de faux positif
is_guilty(Suspect, vol) :-
    has_motive(Suspect, vol),
    was_near_crime_scene(Suspect, vol),
    ( has_fingerprint_on_weapon(Suspect, vol)
    ; ( eyewitness_identification(Suspect, vol),
        temoin_valide(_, vol)  % au moins un témoin fiable pour ce type
      )
    ),
    \+ innocent(Suspect, vol),
    \+ faux_positif(Suspect, vol).

% ASSASSINAT : mobile + présence + (empreinte OU témoin fiable) + pas d’innocence + pas de faux positif
is_guilty(Suspect, assassinat) :-
    has_motive(Suspect, assassinat),
    was_near_crime_scene(Suspect, assassinat),
    ( has_fingerprint_on_weapon(Suspect, assassinat)
    ; ( eyewitness_identification(Suspect, assassinat),
        temoin_valide(_, assassinat)
      )
    ),
    \+ innocent(Suspect, assassinat),
    \+ faux_positif(Suspect, assassinat).

% ESCROQUERIE : mobile (optionnel) + (transaction OU fausse identité) + pas d’innocence
% NB : on peut exiger le mobile selon le niveau de sévérité voulu.
is_guilty(Suspect, escroquerie) :-
    ( has_bank_transaction(Suspect, escroquerie)
    ; owns_fake_identity(Suspect, escroquerie)
    ),
    \+ innocent(Suspect, escroquerie).

/* -----------------------------------------------------------
   EXPLICATIONS (justifications des décisions)
----------------------------------------------------------- */

% explain_guilt(S, Type, Evidence) : liste les éléments à charge retenus
explain_guilt(S, Type, Evidence) :-
    findall(E, evidence_true(S, Type, E), EvList),
    sort(EvList, Evidence).

% Énumération des étiquettes d’indices "vrais"
evidence_true(S, T, motive)           :- has_motive(S, T).
evidence_true(S, T, near_scene)       :- was_near_crime_scene(S, T).
evidence_true(S, T, fingerprint_wpn)  :- has_fingerprint_on_weapon(S, T).
evidence_true(S, T, bank_txn)         :- has_bank_transaction(S, T).
evidence_true(S, T, fake_identity)    :- owns_fake_identity(S, T).
evidence_true(S, T, eyewitness)       :- eyewitness_identification(S, T), temoin_valide(_, T).
evidence_true(S, T, no_innocence)     :- \+ innocent(S, T).
evidence_true(S, T, no_false_positive):- \+ faux_positif(S, T).

/* -----------------------------------------------------------
   OUTIL : liste des coupables par type
----------------------------------------------------------- */

% find_all_guilty(Type, Suspects) : renvoie tous les suspects jugés coupables
find_all_guilty(Type, Suspects) :-
    crime_type(Type),
    findall(S, (suspect(S), is_guilty(S, Type)), L),
    sort(L, Suspects).

/* -----------------------------------------------------------
   PROGRAMME PRINCIPAL – INTERFACE CONSOLE (inchangé)
----------------------------------------------------------- */

% main/0 : menu interactif
main :-
    writeln("=== Systeme Expert : Enquete Policiere ==="),
    writeln("1. Vérifier si un suspect est coupable d'un crime"),
    writeln("2. Lister tous les suspects coupables d'un type de crime"),
    writeln("3. Quitter"),
    writeln("Entrez votre choix (1/2/3) : "),
    read(Choice),
    handle_choice(Choice).

% Gestion des choix
handle_choice(1) :-
    writeln("Entrez le suspect (ex: john.) : "),
    read(Suspect),
    writeln("Entrez le type de crime (ex: vol.) : "),
    read(Type),
    ( is_guilty(Suspect, Type) ->
        format(" ~w est coupable de ~w.~n", [Suspect, Type])
    ;   format(" ~w n'est pas coupable de ~w.~n", [Suspect, Type])
    ),
    nl, main.

handle_choice(2) :-
    writeln("Entrez le type de crime (ex: vol.) : "),
    read(Type),
    writeln("=== Liste des coupables ==="),
    find_all_guilty(Type, Suspects),
    ( Suspects = [] -> writeln("Aucun suspect trouvé.")
    ;   print_suspects(Suspects)
    ),
    nl, main.

handle_choice(3) :-
    writeln("Au revoir !"), !.

handle_choice(_) :-
    writeln("Choix invalide, réessayez."), nl,
    main.

% Affichage des suspects
print_suspects([]).
print_suspects([H|T]) :-
    writeln(H),
    print_suspects(T).

/* ------------------- PRÉDICATS UTILITAIRES POUR LE SERVEUR HTTP ------------------- */

guilty_links([], []).
guilty_links([Type|Rest], [a([href=Href], Type)|RestLinks]) :-
    format(atom(Href), '/guilty?type=~w', [Type]),
    guilty_links(Rest, RestLinks).

/* -----------------------------------------------------------
   SERVEUR HTTP – HANDLERS & PAGES HTML
----------------------------------------------------------- */

% Handlers déclarés
:- http_handler(root(.),          home_handler,     []).
:- http_handler(root(suspects),   suspects_handler, []).
:- http_handler(root(suspect),    suspect_handler,  []).
:- http_handler(root(guilty),     guilty_handler,   []).
:- http_handler(root(check),      check_handler,    []).
:- http_handler(api(guilty),      api_guilty_handler, []).  % /api/guilty?type=...

% Démarrer / arrêter le serveur
server(Port) :-
    http_server(http_dispatch, [port(Port)]),
    format('  Serveur HTTP demarre sur http://localhost:~w/\n', [Port]).

stop_server(Port) :-
    http_stop_server(Port, []),
    format('  Serveur HTTP arrete (port ~w).\n', [Port]).

/* ---------------------- PAGES ---------------------- */

home_handler(_Request) :-
    findall(Type, crime_type(Type), Types),
    findall(S, suspect(S), Suspects),
    reply_html_page(
        title('Enquete Policiere'),
        [
          % Style général
          \style_block,
          div(class=container, [
              h1('Bienvenue sur le Systeme D\'Enquete'),
              hr([]),
              h2('Tester un suspect'),
              form([action='/check', method='get'], [
                  label([for=suspect], 'Suspect : '),
                  select([name=suspect], \options_list(Suspects)),
                  br([]),
                  label([for=type], 'Type de crime : '),
                  select([name=type], \options_list(Types)),
                  br([]),
                  % Bouton interactif
                  input([type=submit, value='Verifier', class='verify-button'])
              ])
          ])
        ]
    ).

coupables_links([]) --> [].
coupables_links([T|Ts]) -->
    { format(atom(Href), '/guilty?type=~w', [T]) },
    html(a([href=Href, class=button], T)),
    coupables_links(Ts).



guilty_handler(Request) :-
    http_parameters(Request, [ type(Type, [atom]) ]),
    ( crime_type(Type)
    -> find_all_guilty(Type, GuiltyList),
       Title = ['Coupables pour ', Type],
       reply_html_page(
           title(Title),
           [
               \style_block,
               h1(Title),
               \nav,
               hr([]),
               \guilty_list_block(Type, GuiltyList),
               \footer
           ]
       )
    ; reply_html_page(
          title('Type invalide'),
          [ \style_block, p('Type de crime inconnu.') ]
      )
    ).

% --------------------------------------------------
% DCG : bloc pour liste de coupables
% --------------------------------------------------
guilty_list_block(_Type, []) --> html(p('Aucun coupable trouvé.')).
guilty_list_block(_Type, Suspects) -->
    { build_suspect_links(Suspects, Links) },
    html(ul(Links)).

% Construction des <li>
build_suspect_links([], []).
build_suspect_links([H|T], [li([H, ' - ', a([href=HRef], 'Voir Details ')])|Rest]) :-
    format(atom(HRef), '/suspect?suspect=~w', [H]),
    build_suspect_links(T, Rest).




suspects_handler(_Request) :-
    findall(S, suspect(S), Suspects),
    reply_html_page(
        title('Suspects'),
        [ \style_block,
          h1('Suspects'),
          \nav,
          hr([]),
          \suspects_list(Suspects),
          \footer
        ]).

suspect_handler(Request) :-
    http_parameters(Request, [ suspect(Suspect, [atom]) ]),
    findall(T, crime_type(T), Types),
    reply_html_page(
        title(['Dossier de ', Suspect]),
        [ \style_block,
          h1(['Dossier de ', Suspect]),
          \nav,
          hr([]),
          \cards_suspect_types(Suspect, Types),
          \footer
        ]).


check_handler(Request) :-
    http_parameters(Request, [
        suspect(Suspect, [atom]),
        type(Type, [atom])
    ]),
    ( crime_type(Type), suspect(Suspect) -> true ; fail ),
    ( is_guilty(Suspect, Type) -> Verdict = true ; Verdict = false ),
    ( Verdict == true ->
        format(atom(Msg), '  ~w est coupable de ~w.', [Suspect, Type])
    ;
        format(atom(Msg), '  ~w n\'est pas coupable de ~w.', [Suspect, Type])
    ),
    ( explain_guilt(Suspect, Type, Evidence) -> true ; Evidence = [] ),
    format(atom(HRef), '/suspect?suspect=~w', [Suspect]),
    reply_html_page(
        title('Resultat'),
        [ \style_block,
          h1('Resultat'),
          \nav,
          hr([]),
          p(class=verdict, Msg),
          h3('Elements pris en compte :'),
          \evidence_block(Evidence),
          p(a([href=HRef, class=button], 'Voir la fiche du suspect')),
          \footer
        ]).


/* ------------------- COMPOSANTS HTML ------------------- */

style_block -->
    html(style([type='text/css'], '
body {
    font-family: Arial, sans-serif;
    background: linear-gradient(120deg, #314e75, #1f2d4c);
    color: #e6e9f3;
    margin: 0;
    padding: 0;
}

.container {
    max-width: 900px;
    margin: 50px auto;
    padding: 20px;
    text-align: center;
}

h1 {
    font-size: 3em;
    margin-bottom: 20px;
    text-shadow: 2px 2px 4px #1a2342;
}
h2,h3,h4 { margin:35px 0; }
form {
    background:#121832;
    border-radius:12px;
    padding:20px;
    display:inline-block;
    text-align:left;
}

input[type=submit] {
    padding:8px 16px;
    background:#1a2342;
    color:#e6f3e8;
    border:none;
    border-radius:8px;
    cursor:pointer;
    font-weight:bold;
    margin-top:10px;
}

input[type=submit]:hover {
    background:#2a3a6e;
}

div.coupables {
    margin-top:30px;
}

div.coupables a {
    display:inline-block;
    padding:6px 12px;
    background:#1a73e8;
    color:#fff;
    border-radius:8px;
    margin:6px;
    text-decoration:none;
}

div.coupables a:hover {
    background:#155ab6;
}

/* Bouton Verifier */
input.verify-button {
    padding:8px 16px;
    background:#1a2342;
    color:#e6f3e8;
    border:none;
    border-radius:8px;
    cursor:pointer;
    font-weight:bold;
    margin-top:10px;
}

input.verify-button:hover {
    background:#28a745;  
    color:#ffffff;
}

.card { background:#121832; border:1px solid #72747cff; border-radius:12px; padding:25px; margin:15px 0; }
.pill-guilty {
    display:inline-block;
    padding:2px 8px;
    border-radius:12px;
    background:#28a745;   % vert
    color: #ffffff;
    font-weight: bold;
}

.pill-innocent {
    display:inline-block;
    padding:2px 8px;
    border-radius:12px;
    background:#dc3545;   % rouge
    color: #ffffff;
    font-weight: bold;
}

nav {
    display: flex;
    justify-content: center;  
    gap: 90px;                
    background: rgba(0,0,0,0.4);
    padding: 10px;
    border-radius: 8px;
}

nav a {
    color: #e6e9f3;
    text-decoration: none;
    padding: 6px 12px;
    border-radius: 6px;
    transition: all 0.2s;
}

nav a:hover {
    background: #2a3a6e;
    cursor: pointer;
}
.button-folder {
    padding:6px 12px;
    background:#f0ad4e;   
    color:#121832;
    border:none;
    border-radius:8px;
    text-decoration:none;
    margin-left:10px;      
    font-weight:bold;
}

.button-folder:hover {
    background:#ec9f3b;   
}

.button-test {
    padding:6px 12px;
    background:#ffffff;   
    color:#000000;        
    border:none;
    border-radius:8px;
    text-decoration:none;
    font-weight:bold;
}

.button-test:hover {
    background:#e6e6e6;   
}


.button { padding:6px 12px; background:#1a2342; color:#e6f3e8; border:none; border-radius:8px; text-decoration:none; }
.button:hover { background:#2a3a6e; }
select { margin:4px 0; padding:4px; border-radius:6px; }
'))
.

nav_bar -->
    html(nav([
        a([href='/'], 'Accueil'),
        a([href='/suspects'], 'Suspects'),
        a([href='/guilty?type=vol'], 'Coupables: vol'),
        a([href='/guilty?type=assassinat'], 'Coupables: assassinat'),
        a([href='/guilty?type=escroquerie'], 'Coupables: escroquerie')
    ])).


nav --> nav_bar.

footer --> html(footer([hr([]), p(' Enquete policiere ')])).

options_suspects -->
    { findall
    (S, suspect(S), L) },
    options_list(L).

options_crimes -->
    { findall(T, crime_type(T), L) },
    options_list(L).

options_list([]) --> [].
options_list([H|T]) --> html(option([value=H], H)), options_list(T).

suspects_list([]) --> html(p('')).
suspects_list([H|T]) -->
    { format(atom(HRef), '/suspect?suspect=~w', [H]) },
    html(div(class=card, [
        strong(H),
        a([class='button-folder', href=HRef], ['Ouvrir le dossier '])
    ])),
    suspects_list(T).



cards_suspect_types(_Suspect, []) --> [].

cards_suspect_types(Suspect, [Type|Ts]) -->
    {
        ( is_guilty(Suspect, Type) -> Verdict = vrai ; Verdict = faux ),
        ( explain_guilt(Suspect, Type, E) -> Evid = E ; Evid = [] ),
        format(atom(HRef), '/check?suspect=~w&type=~w', [Suspect, Type])
    },
    html(div(class='card', [
        h3(['Crime : ', Type]),
        p(['Verdict : ', \verdict_badge(Verdict)]),
        h4('Elements :'),
        \evidence_block(Evid),
        a([class=button, href=HRef], 'Tester a part')
    ])),
    cards_suspect_types(Suspect, Ts).



verdict_badge(vrai)  --> html(span(class='pill-guilty', 'Coupable')).
verdict_badge(faux)  --> html(span(class='pill-innocent', 'Non coupable')).


% Cas où il n'y a aucun coupable
guilty_list(_, []) --> 
    html(p('Aucun coupable trouvé.')).

% Cas avec des coupables
guilty_list(Type, Suspects) -->
    { build_suspect_links(Suspects, Links) },
    html(ul(Links)).

% Construction des liens HTML pour chaque coupable
build_suspect_links([], []).
build_suspect_links([H|T], [li([H, ' — ', a([href=Href], 'Voir')])|Rest]) :-
    format(atom(Href), '/suspect?suspect=~w', [H]),
    build_suspect_links(T, Rest).



% Bloc d'évidences sous forme de liste à puces
evidence_block([]) --> html(p(em('Aucun indice retenu.'))).
evidence_block(L)  --> html(ul(\evidence_items(L))).

evidence_items([]) --> [].  
evidence_items([E|Es]) --> evidence_item(E), evidence_items(Es).

evidence_item(E) --> { (ev_label(E,L)->true; L=E) }, 
    html(li([style='margin:4px 0;'], L)).


% Libellés lisibles

ev_label(motive,            'Mobile').
ev_label(near_scene,        'Presence pres de la scene').
ev_label(fingerprint_wpn,   'Empreinte sur l\'arme').
ev_label(bank_txn,          'Transaction bancaire liee').
ev_label(fake_identity,     'Fausse identite').
ev_label(eyewitness,        'Temoin oculaire fiable').
ev_label(no_innocence,      'Aucun alibi exonerant').
ev_label(no_false_positive, 'Pas de faux positif detecte').

/* ===========================================================
   FIN DU FICHIER
   Utilisation :
   ?- [enquete_policiere_prolog].
   ?- server(8080).
   Naviguer sur http://localhost:8080/
=========================================================== */
