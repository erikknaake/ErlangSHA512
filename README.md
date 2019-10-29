hasher
=====

An OTP library

Build
-----

    $ rebar3 compile

This project was used as a challenge to learn Erlang:

# Learning Erlang
Door: Erik Knaake

Course: APP

Docent: Michel Portier

Versie: 1.0.0

Datum: 29 oktober 2019

## 1. Inleiding

Tijdens de course APP van de HAN krijg je de opdracht zelfstandig een nieuwe programmeertaal te leren, waarbij het de bedoeling is dat die programmeertaal in een nieuw paradigma valt.

Voor de opdracht Paradigma's van het vak APP heb ik gekozen om [Erlang](https://www.erlang.org/) te leren, dit document beschrijft mijn bevindingen die heb gedaan tijdens het leren van Erlang. 

### 1.1. Wat is Erlang

Erlang is een [concurrent functional programming language](https://en.wikipedia.org/wiki/Erlang_(programming_language). Erlang is in de jaren 80 gemaakt door Ericsson (Tate, 2010). Erlang richt zich op een aantal zaken, waaronder:

1. Concurrency
2. Fault tolerance
3. Distrubution

Vanwege deze features wordt Erlang onder andere gebruikt in systemen waarvan een hoge uptime of een grote throughput wordt verwacht, zoals [telecommunicatie systemen van Ericsson](http://erlang.org/faq/introduction.html) en [WhatsApp](https://codesync.global/media/successful-companies-using-elixir-and-erlang/). 
Maar ook afgeleide talen zoals [Elixir](https://elixir-lang.org/) worden gebruikt voor dezelfde use cases, bijvoorbeeld door [Discord](https://blog.discordapp.com/scaling-elixir-f9b8e1e7c29b).

De tekst hierboven riep bij mij de vraag op: "*Waarom hebben die afgeleide talen ook de voordelen van Erlang?*", dat komt doordat die talen op dezelfde VM draaien als erlang, namelijk de [BEAM](https://en.wikipedia.org/wiki/BEAM_(Erlang_virtual_machine). 

## 2. Getting Started

Nu we een korte introdcutie hebben gehad over Erlang is het tijd om wat hands on tijd te krijgen.

### 2.1. Installatie

Erlang is op Arch Linux eenvoudig te installeren met ```sudo pacman -S erlang``` hierbij wordt alles mee geleverd wat je nodig hebt om te beginnen met Erlang.

Maar een goede editor is ook van belang, ik heb twee editors gebruikt die goed werken, namelijk:

1. Sublime Text, deze is erg licht en heeft out of de box syntax highlighting en syntax completion. 
2. IntelliJ, deze heeft een [plugin](https://plugins.jetbrains.com/plugin/7083-erlang/) die alles biedt wat Sublime Text heeft, maar ook een aantal templates kan genereren.

Omdat ik voor de meeste talen IntelliJ gebruik, zal ik deze nu ook gebruiken. 

### 2.2. Hello World

De makkelijkste manier om te controleren of de installatie goed is gegaan is door een terminal op te starten en ```erl``` te runnen. Dit start een interactieve Erlang shell op. 
Alles wat je in deze shell intypt wordt geevalueerd door Erlang, hello world is zo simpel als: ```"Hello World".```.

Let hierbij op de ```.```, deze geeft namelijk het einde van het statement aan. Je ziet bij dit simpele programma meteen dat Erlang iets kent dat lijkt op strings, de quotes zijn namelijk nodig in dit programma, maar Erlang is slim genoeg om zelf te bepalen dat het een string is.

### 2.3. Erlang principles

Dit brengt ons bij het typing systeem van Erlang, [Erlang is dynamicly typed](https://learnyousomeerlang.com/types-or-lack-thereof). Dit neemt in het geval van Erlang de vorm aan dat er runtime errors kunnen worden gegooit over verkeerde types en dat typing information op runtime beschikbaar is.

Nu klinkt het gooien van runtime errors nooit als een goed idee, maar Erlang heeft hier wel speciale mogelijkheden in. Erlang heeft namelijk een [Let it Crash](http://verraes.net/2014/12/erlang-let-it-crash/) filosofie. 

Dit ligt wat gecompliceerder, dus neem even de tijd. In Erlang draait alles in processen (threads bestaan niet), hierdoor is er dus geen shared state. Dus het crashen van een proces heeft geen invloed op andere processen. 

Omdat alles in processen draait, zijn deze processen ontworpen om zo licht mogelijk te zijn. Het is niet onwaarschijnlijk om [duizenden processen](http://verraes.net/2014/12/erlang-let-it-crash/) in een Erlang applicatie te hebben. Dus het herstarten van een process is niet bijzonder duur, en kan bovendien volledig worden geautomatiseerd met een [supervision tree](http://erlang.org/documentation/doc-4.9.1/doc/design_principles/sup_princ.html).

Deze lightweight processen zijn ook een van de redenen dat Erlang goed kan schalen.

### 2.4. Language Constructs

Erlang kent variabelen, atoms en functies.

Een variable is net zoals in Java met een grote uitzondering: ze kunnen niet wijzigen, als een variabele een waarde krijgt zal dit altijd zijn waarde blijven. Anders gezegd variabelen zijn [immutable](https://en.wikipedia.org/wiki/Immutable_object). Een variabele begint met een hoofdletter.

Een atom lijkt het meest op een enum zoals in C++, maar is wel flink anders. Een atom is een woord dat begint met een lowercase letter. Een atom kan geen waarde bevatten, maar wordt veel gebruikt in [pattern matching](http://erlang.org/doc/reference_manual/patterns.html). 

De functie ```io:format``` geeft bijvoorbeeld de atom ```ok``` terug, je kunt hierop matchen om te kijken of de functie wel correct is uitgevoerd. ```he = io:format("hai ~p~n", ["He"]).``` geeft een error omdat het niet matchd, terwijl ```ok = io:format("hai ~p~n", ["He"]).``` geen error geeft omdat dat wel matchd. Ik leg in het hoofdstuk printen wat meer uit over ```io:format```.

Een functie is het belangrijkste deel van Erlang, het is eigenlijk hetzelfde als een method is in Java, maar dan zonder class. Een functie word beschreven als 
```erlang
functionName(Param) ->
    % code here
    ReturnValue.
```

Dit fragment laat ook een comment zien in Erlang, die beginnen met ```%```.

Een functie kan in een module zitten, dit wordt in een bestand aangegeven met ```-module(moduleName)```, code uit een ander module wordt aangeroepen met ```otherModule:functieNaam().```. De functie ```io:format``` zit dus in de module io en de functie heet format. Modules zijn pas pas van toepassing als je bestanden gebruikt, lees hiervoor het hoofdstuk compileren.

### 2.5. Printen

Ik ga niet ver in op functies die Erlang standaard heeft in zijn modules, maar omdat printen essentieel is voor debuggen geef ik een korte uitleg over de makkelijkste print functie.

Zoals al is verteld kun je in Erlang printen met ```io:format```. Deze functie kan veel meer dan een literal priten. Er zijn namelijk karakter combinaties met een speciale betekenins. Al deze combinaties beginnen met ```~``` en heten [control sequences](http://erlang.org/doc/man/io.html#format-2). De control sequences die je het meeste gebruikt zijn ```~p```, ```~w```, ```~n``` en ```~.2#```.

~p: Pretty print de variabele.

~w: Print de variabele ruw.

~.2#: Print de variabele in de base die achter de punt staat, dus deze print in base 2 AKA binary.

~n: Print een newline character.

Voor deze control sequences, behalve de ~n, moet de tweede parameter van io:format worden ingevuld. Dit is een lijst met alles dat geprint moet worden in de volgorde van control sequences. In deze lijst kunnen meerdere datatypes staan, hierover volgt meer in het hoofstuk datatypes. 

```erlang
Part = "World".
io:format("Hello ~p~n", [Part]).
```

## 3. Compileren

Op het moment dat je meer met Erlang wilt, is de interactieve shell al snel een probleem, je wilt code in bestanden kunnen zetten. Dit kan op een aantal manieren worden bereikt, de meest simpele manier is door code op te slaan in een ```.erl``` bestand en binnen de Erlang shell ```c(bestandsnaam).``` te runnen.

Om dit te laten werken moet in het bestand een module zijn gedefineerd met dezelfde naam als het bestand. De funtie ```c``` compileerd de code en genereerd een ```.beam``` bestand, dit is te vergelijken met een ```.class``` bestand van java.

Op deze manier moet je nog steeds alle bestanden los compileren, dit is niet praktisch als je meer bestanden hebt. Dit is waar [rebar3](https://www.rebar3.org/) een rol gaat spelen. 

Een andere optie is MakeFile van [erlang.mk](https://erlang.mk/guide/getting_started.html), omdat dit nu weinig waarde toe voegt ga ik hier niet mee verder.

### 3.1. rebar3

Rebar3 is een fully flegged compiler, maar kan nog meer dan alleen compilen.

#### 3.1.1 Installatie

Rebar3 is op Arch Linux te installeren met:
```shell
cd ~/.local/bin
curl -O https://s3.amazonaws.com/rebar3/rebar3
chmod +x rebar3
```

Dit gaat ervan uit dat de ```~/.local/bin``` al bestaat en in de PATH variabele staat. De path variabele kun je controleren met 
```shell
echo $PATH
```

#### 3.1.2. Een project starten

Rebar3 kan projecten generen op een vergelijkbare, maar simpelere manier, als maven: ```rebar3 new <template> <appname>```.

Voor een volledige [OTP](https://learnyousomeerlang.com/what-is-otp) applicatie gebruik je als template ```app```, voor functies en modules kun je het beste ```lib``` gebruiken. 

**Let op:** de appname moet beginnen met een kleine letter anders is het gegenereerde project onjuist.

#### 3.1.3. Een dependency toevoegen

Rebar3 is ook een package manager. In een rebar3 project staat een bestand ```rebar.config```, dit is een configuratie bestand geschreven in Erlang, waarin de dependencies staan. Om bijvoorbeeld [cowboy](https://ninenines.eu/docs/en/cowboy/2.6/guide/introduction/) (een HTTP server) toe te voegen heb je de volgende code nodig:

```erlang
{deps, [
    {cowboy, "2.6.3"}
]}.
```

Wanneer je een app zoals cowboy installeerd moet je in tegenstelling tot een library de app laten starten bij de programma startup. Dit doe je door in de ```appname.app.src``` onder applications cowboy toe te voegen. Dit kan er bijvoorbeeld zo uitzien:

```erlang
{application, appname,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, []},
  {mod, {appname_app, []}},
  {applications,
   [kernel,
    stdlib,
    cowboy,
    jiffy,
    mnesia
   ]},
  {env,[]},
  {modules, []},

  {licenses, ["TODO"]},
  {links, []}
 ]}.
```

#### 3.1.4. Runnen

Er zijn twee belangrijke manieren om Erlang te runnen met rebar3, de eerste optie is shell, bedoeld voor development en de andere is release.

##### 3.1.4.1. Shell

Voor shell moet er in de ```rebar.config``` de regel ```{apps, [appname]}``` worden vervangen met ```{apps, [{appname, [kernel]}]}```. De applicatie is dan te runnen met ```rebar3 shell```, hierbij runt het programma binnen de Erlang shell.

##### 3.1.4.2. Release

Voor release moet er in de ```rebar.config``` een release worden toegevoegd:
```erlang
{relx, [{release, {appname, "0.0.1"},
         ["appname"]},

        {dev_mode, false},
        {include_erts, false},

        {extended_start_script, true},
        {vm_args, "config/vm.args"},
        {sys_config, "config/sys.config"}
        ]}.
```
Hierbij zijn de velden ```vm_args``` en ```sys_config``` optioneel, maar wel aan te raden, zodat je in die bestanden nog een aantal parameters kunt zetten. Een project kan meerdere releases bevatten.

Dit is te runnen met: 
```shell
rebar3 release # Compile
_build/default/rel/appname/bin/appname foreground # Run
```
foreground zorgt ervoor dat het programma in de Bash shell blijft, waardoor het nog eenvoudig te stoppen is.

#### 3.1.5. Testen

Rebar3 heeft standaard support voor [eunit](http://erlang.org/doc/apps/eunit/chapter.html) voor het unit testen. Dit is te runnen met ```rebar3 eunit```. Hierover volgt later meer.

## 4. Erlang for beginners

### 4.1. Datatypes
Erlang heeft een aantal [datatypes](http://erlang.org/doc/reference_manual/data_types.html) die je regelmatig gebruikt:
1. Number, gewoon een nummer (integer of float)

2. Atom, een waarde begginend met een kleine letter. Het lijkt het meest op een enum uit andere talen.
Dit gebruik je bijvoorbeeld voor patternmatching.

3. Bitstring, wordt aangegeven met ```<<>>``` en zet het geen hierbinnen om naar een binary representatie. 

4. Binary, een bitstring, maar met een lengte die een meervoud van 8 is.

5. Fun, een functie

6. Pid, een processId

7. Tuple, lijkt een beetje op een struct, het is een verzameling van verschillende datatypen.

8. Map, is een tuple, maar dan heeft elk 'attribuut' een naam. Vergelijkbaar met de Map class uit java.

9. List, een verzameling van waarden. Mag in tegenstelling tot de meeste talen verschillende waarden bevatten.

10. String, zoals ook bekend in java.

11. Record, is een map met andere syntax, maar dan met een vast aantal waarden.

12. Boolean, true of false.

Zoals hierboven te zien is zitten tuples en lists erg dicht bij erlkaar in [Erlang](http://erlang.org/pipermail/erlang-questions/2006-February/018890.html) 
na wat [googlen](https://stackoverflow.com/questions/26228267/difference-between-a-list-and-a-tuple-in-erlang) lijkt het vooral een semantisch verschil te zijn, maar kunnen ze hetzelfde bereiken. Tuples hebben in tegenstelling
tot lists wel O(1) complexity, terwijl lists O(N) complexity hebben. 
De consensus lijkt te zijn om tuples te gebruiker
bij data dat bij elkaar hoort, zoals voornaam en achternaam, en lists voor een lijst van data die bij elkaar hoort, maar los van elkaar
gebruikt kan worden. Een goed iets om te onthouden is dat lists bedoeld zijn voor een onbekend aantal waarden, terwijl tuples vooral
zijn bedoeld voor fixed size.
### Pattern matching

Erlang ondersteund pattern matching, dit houdt in dat er gekeken wordt op welke statement matcht.

Op deze manier is het bijvoorbeeld mogelijk om conditioneel een bepaalde functie aan te roepen. De volgende code geeft een groet,
maar geeft aan je niet te kennen als je een lege string meegeeft.
```erlang
-module(pattern_matching).
-export([greeter/1]).

greeter("") ->
  io:format("I don't know you~n");
greeter(Str) ->
  io:format("Hello ~p~n", [Str]).
```

### Loops
Erlang kent geen for loops of while loops zoals de meeste talen. Maar je kunt for loops wel nabootsen met recursie.

#### Backwards for loop
```erlang                                                   
-module(for).
-export([loopFrom/1]).

loopFrom(0) ->
        io:format("end~n");
loopFrom(N) ->
        io:format("~p~n", [N]),
        loopFrom(N - 1).
```

#### For loop
```erlang    
-module(for).                                               
-export([loopTo/2]).

loopTo(I, N) when N =< I ->
        io:format("loop: ~p~n", [N]),
        loopTo(I, N + 1);
loopTo(I, N) ->
        io:format("end~n").
```

#### For each loop
```erlang
-module(foreach).
-export([foreach/1]).

foreach([]) ->
        io:format("end~n");
foreach([H|T]) ->
        io:format("H: ~p~n", [H]),
        foreach(T).
```
Hier zie je hoe het HEAD en TAIL principe wordt gebruikt met pattern matching om een lijst te splitsen
in het eerste item en de rest van de lijst. Ook wordt er een patternmatch gedaan op een lege lijst
om ervoor te zorgen dat de functie stopt na de laatste waarde.

Je kunt ook een generieke foreach schrijven, die zijn gedrag krijgt als parameter. Hiervoor kan je het ```fun``` datatype gebruiken.
```erlang
-module(foreach).
-export([foreach/2]).

foreach([], Fun) ->
        io:format("end~n");
foreach([H|T], Fun) ->
        Fun(H),
        foreach(T, Fun).
```

Dit is dan bijvoorbeeld aan te roepen met een lambda expression
```erlang
foreach:foreach([1, 3, 2, 6], fun(H) -> io:format("H: ~p~n", [H]) end).
```
Functies die een functie aannemen of retourneren worden ook wel [higher order functions](https://medium.com/javascript-scene/higher-order-functions-composing-software-5365cf2cbe99) genoemd.

### Ingebouwde list functies

Omdat veel acties die je op lijsten doet gegeneraliseerd kunnen worden heeft erlang de [lists module](http://erlang.org/doc/man/lists.html).

Een aantal van deze functie die je regelmatig zult gebruiken zijn:
1. map, voert een bewerking uit op elk element van de lijst en retourneert een lijst van de resultaten.
2. filter, retourneert een list van alle elementen waarvoor de meegegeven functie ```true``` retourneert.
3. foreach, roept voor elk element in de list de functie aan die je meegeeft
4. foldl, Accumuleert een waarde door een bereking te doen op elk element in de list hierin de accumulator mee te nemen.
De volgende code berekend bijvoorbeeld de som van alle items in de list:
```erlang
lists:foldl(fun(Item, SumSoFar) -> Item + SumSoFar end, 0, [1,2,3,4,5]).
```

### Shopping cart

Het boek Seven Languages In Seven Weeks geeft een aantal kleine opdrachten om te oefenen met de basics van Erlang (Tate, 2010), daarvan ga ik er nu een uitwerken.

#### Opdracht
Consider a shopping list that looks like ```[{item, quantity, price}, ...]```. Write a list comprehension that builds a list of
items of the form ```[{item, total_price}, ...]``` where total_price is quanity times price.

#### Oplossing
```erlang
cart(Cart) ->
  [{Item, Quantity * Price} || {Item, Quantity, Price} <- Cart].
```

#### Uitproberen
```erlang
shopping_cart:cart([{"Laptop", 1, 1200}, {"Compiler", 20, 4}]).
% [{"Laptop",1200},{"Compiler",80}]
```

#### Uitleg

De list comphrehension pakt elk element in de list Cart en maakt er een Tuple van waarop te matchen is met Item, Quantity en Price.
Door een nieuwe tuple aan te maken met deze gematchte variabelen kun je rekenen met bepaalde delen van de tuple die op dat moment wordt uitgerekend.

### Nog een oefening
Converteer een binary list naar een integer list
```erlang
-spec binaryListToIntegerList(list(binary())) -> list(integer()).
binaryListToIntegerList(BinaryList) ->
  lists:map(fun(Binary) ->
              <<Integer:64>> = Binary,
              Integer
            end, BinaryList).
```
Hier wordt gebruik gemaakt van pattern matching om een binary naar een integer om te zetten en wordt de map functie
gebruikt om dit voor de hele lijst te doen.

### Concurrency
Zoals al eerder beschreven is Erlang een taal die zich focust op concurrency. In dit hoodstuk laat ik zien hoe je simpele concurrency kunt maken in Erlang.

In Erlang start je geen nieuwe thread op maar een process om concurrency te bereiken, dit die je met de ```spawn``` functie
spawn neemt een functie aan om uit te voeren op een ander process en retourneert het process id van het gespawnde process.

Meestal maak je een process aan met een zogehete receive loop, dit is een standaard pattern in Erlang en ziet er uit als:
```erlang
loop() ->
    receive
        "Option 1" ->
            % Do stuff
            loop();
        "Option 2" ->
            % Do other stuff
            loop();
        terminate ->
            % do some exiting stuff
            ; % note not calling loop again, so the process terminates
        _ ->
          % Do something for any other option
          loop()
        end.
```

Dit process zou je kunnen spawnen met ```Pid = spawn(fun loop/0).```

Aan zo'n receive loop heb je natuurlijk pas iets als je er berichten heen kunt sturen, dit doe je met het PId:
```PId ! Message.```

*Note:* wanneer je distributed systemen hebt, moet je soms aangeven op welke node het process draait.

Met het versturen van berichten naar een process maak je iets dat ik al enigzinds vind lijken op OO programmeren, je loop is een soort
object dat gedrag heeft dat door andere kan worden aangesproken met behulp van een message. Om dit volledig te kunnen moet je alleen nog return waarden
toevoegen, en dit kan door in het process een message te sturen naar het originele process.
```erlang
receive
    {PId, Message} ->
        PId ! Message
        loop();
```

Er is nog veel meer te vertellen over concurrency in Erlang, maar ik beperk mij tot nog maar een ding en dat is het principe van [supervision trees](http://erlang.org/documentation/doc-4.9.1/doc/design_principles/sup_princ.html).
In erlang kun je een exit signal van een process trappen in het parent process, op deze manier kan de parent het child process opnieuw starten.

Op het moment dat je met deze zaken bezig gaat, moet je gaan kijken naar de [OTP](https://learnyousomeerlang.com/what-is-otp) libary, omdat deze een aantal van deze principes al erg goed heeft geimplementeerd voor jou.
Dit biedt mogelijkheden zoals start het child opnieuw op, totdat het in de afgelopen 10 seconden meer dan 5 keer gecrasht is.

Nu heb ik het nodige verteld over de basics van Erlang en is het tijd voor de challenge.

### Challenge
Implementeer het [SHA-512]((https://nvlpubs.nist.gov/nistpubs/FIPS/NIST.FIPS.180-4.pdf)) hashing algoritme in Erlang en maak hiervoor gebruik van features van Erlang.

Het SHA-512 algortime kan gebruikt worden als cryptografisch hashing algoritme, je zou het dus kunnen gebruiken voor
wachtwoord validatie. Je hoeft alleen de hash van het wacthwoord op te slaan, waaruit je nooit het originele wachtwoord kunt halen, terwijl je met het wachtwoord wel kunt valideren of die op dezelfde hash uitkomt.

#### Preprocessing
SHA-512 begint met een preprocess start, hierin wordt een padding toegepast: 
- er wordt een '1' bit toegevoegd aan het bericht.
```erlang
addBit(<<MessageToAppend/binary-unsigned-big>>) ->
  <<MessageToAppend/bitstring-unsigned-big, <<1:1>>/bitstring-unsigned-big>>.
```
- daarna worden er '0' bits toegevoegd totdat het bericht voldoet aan ```l + 1 + k = 896 mod 1024```, oftewel todat het 128 bits korter is dan een meervoud van 1024 bits.
```erlang
-spec paddZeroes(binary(), integer()) -> binary().
paddZeroes(<<UnpaddedMessage/bitstring-unsigned-big>>, MessageLength) ->
  <<UnpaddedMessage/bitstring-unsigned-big, <<0:(numberOfZeroesToAdd(MessageLength))>>/bitstring-unsigned-big>>.

-spec numberOfZeroesToAdd(integer()) -> integer().
numberOfZeroesToAdd(MessageLength) ->
  mod(896 - (MessageLength + 1), 1024). % 896 magic constant from chapter 5.1.2, + 1 because message is padded with a bit

-spec mod(integer(), integer()) -> integer().
mod(X, Y) when X > 0 ->
  X rem Y;
mod(X, Y) when X < 0 ->
  Y + X rem Y;
mod(0, _) ->
  0.
```
- als laatste stap van het padden wordt er in 128 bits de lengte van het bericht toegevoegd.
```erlang
lengthPadd(<<ZeroPaddedMessage/bitstring-unsigned-big>>, MessageLength) ->
  <<ZeroPaddedMessage/bitstring-unsigned-big, <<MessageLength:128>>/bitstring-unsigned-big>>.
```

Na deze padding stap is het bericht een meervoud van 1024 bits, nu moeten er per blok van 1024 bit stappen uitgevoerd.
Hiervoor moet je eerst het per 1024 bits van het bericht een functie kunnen aanspreken. Om dit te realiseren heeft mijn preprocess de volgende stap extra:
```erlang
-spec parse(binary()) -> list(list(binary())).
parse(<<PaddedMessage/binary-unsigned-big>>) ->
  [splitToNByteBlocks(X, 8) || X <- splitToNByteBlocks(PaddedMessage, 128)].
```
Die het resultaat splitst in lijsten van 1024 bit met daarin steeds lijsten van 64 bit. Achteraf kan dit waarschijnlijk efficienter door steeds op de binary een deel te matchen, in plaats van het helemaal te converteren en later weer terug te converteren.

#### Hash computation

Door de preprocess stap kan de volgende functie nu per 1024 bit de hash computation uitvoeren:
```erlang
-spec digest(list(list(binary())), list(integer())) -> binary().
digest(Message, InitialWorkers) ->
      lists:foldl(
        fun(MessageBlock, PreviousWorkers) ->
          hash_block(MessageBlock, PreviousWorkers)
        end,
        InitialWorkers,
        Message).
```
De hash computation bestaat uit een aantal stappen:

- bereken de message schedule (W)
    - Als T kleiner is dan 16, pak het "word" op de Tde plek van het message blok.
    - Anders ```sigma1(W[T - 2]) + W[T - 7] + sigma0(W[T - 15]) + W[T - 16]``` waarbij alle optellingen mod 2^64 gebeuren.

Dit is te vereenvoudigen door het message block in een binary te zetten en dan de berekening van de message schedule te starten bij 16:
```erlang
-spec calculateMessageSchedule(list(binary())) -> list(integer()).
calculateMessageSchedule(MessageBlock) ->
  calculateMessageSchedule(MessageBlock, [], 1).

-spec calculateMessageSchedule(list(binary()), list(binary()), integer()) -> list(integer()).
calculateMessageSchedule(_, W, 81) ->
  binaryListToIntegerList(W);
calculateMessageSchedule(MessageBlock, _, _) ->
  extend(binaryListToBinary(MessageBlock), 16).

-spec extend(binary(), integer()) -> binary().
extend(MessageSchedule, 80) ->
  MessageSchedule;
extend(MessageSchedule, T) ->
  W2 = getWordFromByteOffset(MessageSchedule, T - 2),
  W7 = getWordFromByteOffset(MessageSchedule, T - 7),
  W15 = getWordFromByteOffset(MessageSchedule, T - 15),
  W16 = getWordFromByteOffset(MessageSchedule, T - 16),
  S0 = sigma0(W15),
  S1 = sigma1(W2),
  Next = add64(W16 + S0 + W7, S1),
  extend(concatBinary(MessageSchedule, <<Next:?WORD_SIZE/big-unsigned>>), T + 1).
``` 
- Initialiseer worker variabelen op de vorige worker variabelen.
    - Als dit de eerste keer is, zet ze op de gedefineerde constanten
    
- Reken nieuwe workers uit. Deze stap wordt van T = 0 tot T = 79 gedaan
```
T1 = h + sum1(e) + ch(e, f, g) + K[T] + W[T]
h = g
g = f
f = e
e = d + T1
d = c
b = a
a = T1 + T2
```
in Erlang:
```erlang
-spec calculateWorkers(list(integer()), binary()) -> list(integer()).
calculateWorkers(InitialWorkers, MessageSchedule) ->
  calculateWorkers(MessageSchedule, InitialWorkers, InitialWorkers, 0).

-spec calculateWorkers(binary(), list(integer()), list(integer()), integer()) -> list(integer()).
calculateWorkers(_, Workers, Next, 80) ->
  calculateIntermediateHashValue(Workers, Next);
calculateWorkers(MessageSchedule, Workers, [A, B, C, D, E, F, G, H], T) ->
  S0 = sum0(A),
  Maj = maj(A, B, C),
  T2 = add64(S0, Maj),
  S1 = sum1(E),
  Ch = ch(E, F, G),
  K = getWordFromByteOffset(kConstants(), T),
  Wt = getWordFromByteOffset(MessageSchedule, T),
  T1 = add64(H + S1 + Ch + K, Wt),
  calculateWorkers(MessageSchedule,
    Workers,
    [add64(T1, T2), A, B, C, add64(D, T1), E, F, G],
    T + 1).
```
waarbij alle optellingen mod 2^64 gebeuren.
- Tel de nieuwe workers (a t/m h) op bij de vorige workers
```erlang
-spec calculateIntermediateHashValue(list(integer()), list(integer())) -> list(integer()).
calculateIntermediateHashValue(Workers, HashValues) ->
  lists:map(
    fun({HashValue, Worker}) ->
      add64(HashValue, Worker)
    end,
    lists:zip(HashValues, Workers)).
```

#### Compress

Als laatste stap van het SHA-512 algortime worden de laatste workers samengevoegd tot een geheel:
```erlang
-spec compress(list(integer())) -> integer().
compress(Workers) ->
  lists:foldl(fun appendBits/2, <<>>, Workers).
-spec appendBits(integer(), binary()) -> binary().
appendBits(Value, Accumulator) ->
  appendBits(Value, Accumulator, ?WORD_SIZE).
```

#### Toelichting
Omdat het heel makkelijk is om een klein foutje te maken bij een algoritme zoals SHA-512, heb ik veel gebruik
gemaakt van unit tests met behulp van [eunit](http://erlang.org/doc/apps/eunit/chapter.html).
Het is namelijk zo dat alle stappen in het algoritme als pure functies zijn te schrijven, waardoor het unit testen
erg makkelijk is. Met deze unit tests heb ik bijvoorbeeld weten te achterhalen dat ik een van mijn K constanten
niet goed had gekopieerd uit de specificatie.

Om goed te kunnen testen heb ik gebruik gemaakt van de preprocesser van erlang, deze maakt het namelijk mogelijk
om alleen in het geval van een test bepaalde functies te exporteren:
```erlang
-ifdef(TEST).
-export([padd/1]).
-endif.
```
Hierdoor is het mogelijk alle functies los te testen en toch een kleine export te houden wanneer je dit als library zou uitgeven.

De volledige code van de challenge is meegeleverd in hetzelfde zipbestand als dit bestand.

### Testen met eunit
Zoals net verteld, heb ik voor de challenge gebruik gemaakt van eunit om te unit testen, dit is met rebar makkelijk te doen,
eunit zit daar namelijk al ingebouwd.

De unit tests zijn te runnen met:
```shell script
rebar3 eunit
```

Het schrijven van een unit test is niet zo lastig, je moet een test folder aanmaken, met daarin bestanden die de
eunit header importeren
```erlang
-include_lib("eunit/include/eunit.hrl").
```
nu kun je simpele tests schrijven in de vorm:
```erlang
naam_test() ->
  [
    ?assertEqual(Expectation, Actual),
    ?assertEqual(Expectation, Actual)
  ].
```
Belangrijk hierbij is dat test functies eindigen op '_test'.

Als test voor de challenge heb ik bijvoorbeeld:
```erlang
sigma1_test() ->
  [
    ?assertEqual(2#1001101001011010010110100101101001011010010110100101101001011010, hasher:sigma1(2#1111000011110000111100001111000011110000111100001111000011110000))
  ].
```

Ik heb voor dit project geen before each of andere assertions nodig gehad, maar eunit heeft wel ondersteuning voor deze zaken. 

### Quircks

Tijdens het werken met Erlang ben ik een aantal 'gekke' dingen tegengekomen:

1. [Erlang kent geen modulo operator](https://stackoverflow.com/questions/353224/how-do-you-do-modulo-or-remainder-in-erlang), deze moet je zelf als functie met de rem operator (remainder operator) implementeren.
2. Lists zijn niet 0 indexed, maar beginnen bij 1.
3. Op een bepaald punt stopped eunit met het door hebben dat de tests zijn [geupdate](https://github.com/erlang/rebar3/issues/1998), hij blijft dan hetzelfde uitvoeren als de laatste uitvoering,
op te lossen met ```rm -rf _build```, dit is feitelijk een clean stap. Omdat je niet altijd door hebt dat je tests niet meer updaten verhoogd dit de kans op bugs enorm.
4. Wanneer je met een bitstring of een binary werkt, is het belangrijk dat je het exacte aantal bits vermeld dat je wild gebruiken,
omdat Erlang anders het minimum pakt en dat is niet voor alle algoritmes wenselijk.

### Conlcusie
De challenge die ik heb gemaakt was uitdagend, omdat het pittig was om te debuggen, dit gaat samen met twee van de grootste valkuilen van Erlang:
1. Geen makkelijk doorzoekbare documentatie
2. Erlang heeft van alle talen die ik heb gebruikt misschien wel de minst duidelijke foutmeldingen

In de challenge heb ik een aantal type conversies gedaan die waarschijnlijk weg te werken zijn, maar hierdoor heb ik
wel goed kunnen oefenen met map en fold functies.

Ik merkte dat ik in het begin van Erlang moeite had om te zien wanneer ik pattern matching of lists functies kon gebruiken, 
ik kan duidelijk merken dat ik steeds makkelijker zie wat ik wanneer kan gebruiken.

Ik denk ook zeker dat ik map en fold ga gebruiken in OO talen die dit ondersteunen, ook zal ik vaker immutable state gaan gebruiken
omdat ik gemerkt dat dat het testen van functionaliteit veel makkelijker maakt.

Ik denk dat Erlang een taal is die je pas moet gebruiken als je een duidelijk voordeel erbij hebt en ik denk dat je dan moet gaan kijken
of [elixir](https://elixir-lang.org/) niet een betere optie is, die mogelijk meer developer vriendelijk is.
Als ik hierna een andere taal zou moeten oppakken zou ik gaan kijken naar Scala, Kotlin of Elixir.

### Tot slot
In het begin dacht ik iets te gaan doen met concurrency in Erlang, door een HTTP server met een database te gaan bouwen,
hiervoor heb ik uitgezocht hoe je cowboy met mnesia kan gebruiken. Echter zou dit te veel tijd hebben gekost om dit te realiseren.

Hier heb ik wel een aantal metingen op kunnen doen en de resultaten verbaasden mij, Erlang leek helemaal niet zoveel meer
connecties aan te kunnen als dat geclaimd werd, dit is een van de redenen dat ik be geswitched naar het SHA algoritme als challenge.
Bovendien zou ik daarin alleen gebruik maken van pattern matching en supervision trees, maar niet van andere features die erlang heeft, zoals het functioneel
programmeren, wat ik als leerzamer heb beschouwd.

## Literatuurlijst
Tate B. A. (2010) *Seven Languages In Seven Weeks*. Dallas, Texas.
