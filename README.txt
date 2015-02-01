
                    Pracownia z Programowania Funkcyjnego
                      prowadzący: dr Dariusz Biernacki


                              Projekt końcowy:

                     +---------------------------------+
                     |                                 |
                     |              OLEK               |
                     |  PLATFORMOWA GRA EKSPLORACYJNA  |
                     |                                 |
                     +---------------------------------+

                           autor: Marcin Kaczmarek

1. Założenia projektu
---------------------

    Celem projektu była weryfikacja możliwości języka funkcyjnego OCaml
w dziedzinie gier komputerowych. Miało być to osiągnięte poprzez opracowanie
silnika wraz z edytorem map, a nastepnie wykonanie w nim prostej gry,
demonstrujacej możliwości silnika. Silnik powinien pozwalać na:

    *   tworzenie plansz w postaci różnorodnych kafelków rozmieszczonych
        na kwadratowej siatce;

    *   plansze z wieloma warstwami dla urozmaicenia wizualnego; oprócz
        kafelków powinny być dostępne warstwy (tła) z jednolitym kolorem oraz
        warstwy z obrazami;

    *   zarządzanie obiektami: tworzenie, usuwanie, wyświetlanie, informowanie
        o wydarzeniach (np. klawiatura), kolizje;
    
    *   komunikację między obiektami;

    *   łatwe dodawanie do gry nowych rodzajów obiektów i kafelków;

    *   zapisywanie i odczytywanie stanu gry.

    Edytor dołączony do silnika powinien dawac dostęp do wszystkich ustawień
w sposób graficzny, aby nie było konieczne ręczne edytowanie plików
konfiguracyjnych. Powinien też dynamicznie kompilować i ładować rodzaje obiektów
i kafelków.

    Dodatkowym założeniem było także stosowanie funkcyjnego paradygmatu
programowania oraz trwałych struktur danych tam, gdzie to tylko możliwe. Nie
jest to może praktyczne, ale służy celom edukacyjnym

2. Realizacja
-------------

    Architektura wynikowych aplikacji pomija obiektowe możliwości języka OCaml
i bazuje wyłącznie na systemie modułów. Poniżej wymieniono kilka kluczowych
decyzji projektowych.

    W języku OCaml nie ma koercji typów, dlatego dla elastyczności obiektów,
zastosowano podział na dwie części: fizyczną (body) i logiczną (mind). Część
fizyczna każdego obiektu jest taka sama dla każdego rodzaju obiektu -- jest
po prostu instancja typu Body.t (src/body.ml), który zawiera informacje
o pozycji, rozmiarach i obrazku (tzw. sprajcie lub duszku), który go
reprezentuje. Częśc logiczna jest realizowana za pomocą modułów pierwszej
kategorii (first class modules), które udostępnia OCaml. Ogólnie rzecz biorąc,
dodanie nowego rodzaju obiektu sprowadza się to stworzenia modułu, który spełnia
pewną sygnaturę (sygnatura MIND w pliku src/mind.ml)

    Dzięki podziałowi, obiekty mogą odczytywać swoje wzajemne informacje
o położeniu, rozmiarach itp. ale niestety, ze względu na brak koercji typów, nie
mają wglądu w informacje szczególne, zaimplementowane w ramach części mind. Ten
kompromis między abstrakcja a elastycznością okazał się jednak wystarczający do
realizacji nietrywialnej gry.

    Ponieważ stosowane struktury danych są trwałe, do identyfikacji obiektów
wprowadzono system uchwytów (typ Env.handle w pliku src/env.ml). Dzięki temu
unika sie problemu utrzymywania przez biekty nieaktualnych referencji do innych
obiektów, w miejsce których znalazły sie zawsze aktualne uchwyty). Uchwyty
służą do wysyłania wiadomości do innych obiektów a także pobierania informacji
o ich części fizycznej.

    Do serializacji użyto s-wyrażeń. Jest to szczególnie łatwe, ponieważ
biblioteka Core dostarcza preprocesor automatycznie generujący fukcje
konwertujące większość typów do i z s-wyrażenia. Ręcznie napisana została
garstka takich funkcji. Kolejną korzyścią tego sposobu realizacji jest to, że
wynikowe pliki są czytelne dla człowieka i można je też ręcznie edytować w razie
potrzeby.

3. Co się nie udało
-------------------

    Właściwie wszystkie założenia zostału spełnione, poza kilkoma wyjątkami.

    Struktura danych utrzymująca dwuwymiarową siatkę kafelek jest
zaimplementowana przy użyciu typu array, którego wartości modyfikuje się
w miejscu. Jest to podyktowanie ograniczeniami czasowymi. Nie mniej jednak
interfejs struktury Grid (src/grid.mli) jest taki, jakby była ona trwała
i pozwala na łatwą zmiane implementacji. Do jej realizacji rozważane jest
drzewo czwórkowe, które zapewnia swobodny dostęp do elementów w czasie
logarytmicznym.

    Moduły obiektów i kafelków nie sa dynamicznie ładowane, lecz wkompilowane
w program. Ingerencja ich w resztę projektu jest jednak minimalna i ich
oddzielenie nie powinno nastręczać wielu problemów. Jeśli to będzie
zrealizowane, silnik będzie można wykorzystac do wielu rodzajów gier.

    Edytor właściwie pozwala właściwie na konfigurowanie wszystkiego poza
plikiem rozruchowym (data/saves/new.ml). Niektóre opcje są dostępne jednak
jedynie za pośrednictwem wbudowanego "terminala", który wprawdzie okazał się
bardzo praktyczny, jednak nie jest do końca w duchu aplikacji z GUI.

4. Gra demonstracyjna
---------------------

    Gra demonstracyjna pokazuje wiele istotnych możliwości silnika.

    *   dwa pokoje, między którymi mozna sie przemieszczać, co pokazuje
        teoretyczny brak ograniczen na rozmiar świata gry;

    *   różne rodzaje wrogów, które mogą zabić postać, lub być przez nią zapite,
        co pokazuje komunikację między obiektami;

    *   obiekty, które strzelają, czyli tworzą dynamicznie nowe instancje
        obiektów;

    *   checkpointy, które pozwalają na zapisanie i wznowienie gry;

    *   związki przyczynowo-skutkowe: aby otworzyć zablokowane drzwi w pierwszym
        pokoju, trzeba zdobyc klucz w drugim pokoju;

    *   różne rodzaje warstw: tło porusza się z paralaksą, niektóre obiekty są
        przed postacią natomiast inne za nią;

    *   różne rodzaje kafelków: niektóre zabijają, przez inne można przejść od
        dołu, a przez inne w ogóle nie można przejść.

5. Kompilacja
-------------

    Do skompilowania potrzebne sa następujące narzędzia:

    *   OCaml w wersji 4.01.0 z narzędziami ocamlopt, ocamlbuild, ocamlfind,
        ocamlp4;

    *   pakiety do OCamla: core (111.28.01), ocamlsdl (0.9.1);

    *   biblioteka SDL (koniecznie w wersji 1.2, a nie 2.x) wraz z dodatkami
        SDL_ttf i SDL_image.

    Podczas instalacji paietu ocamlsdl przez program opam, należy najpierw mieć
w systemie bibliotekę SDL wraz z wymienionymi dodatkami. W przeciwnym razie
pakiet sie nie doda albo doda się bez modułów sdl.image, sdl.ttf. W takim
wystarczy odinstalować pakiet ocamlsdl, dodac brakujące biblioteki systemowe
i zainstalować go ponownie.

    Program był testowany na systemie Linux i OS X. Kompatybilność z systemem
Windows nie jest znana. Oczywiście nie jest wykluczone, że kompilator
i biblioteki w innej wersji też działają. 

    Jeśli wszystkie zależności sa spełnione, do kompilacji wystarczy polecenie

        ./make game                                   ( nie mylić z 'make game' )

wywołane w katalogu głównym projektu (tym, w którym znajduje się niniejszy
plik). Polecenie to skompiluje grę demonstracyjną do pliku wykonywalnego 'game'.
Analogicznie kompiluje się edytor do pliku wykonywalnego 'editor':

        ./make editor

natomiast aby usunąć pliki pośrednie kompilacji należy użyć polecenia

        ./make clean

Można także zrobić wszystko za jesnym razem poleceniem

        ./make game editor clean

6. Obsługa gry i edytora
------------------------
    
    Sterowanie w grze odbywa się za pomoca kilku klawiszy:

    *   strzałki: lewo, prawo -- poruszanie postacią;

    *   spacja -- skok (im dłużej trzymany, tym wyższy).

    Sterowanie w edytorze:

    *   klawisze: W, A, S, D -- zmiana aktywnego kafelka;

    *   strzałki: góra, dół -- zmiana aktywnej warstwy;

    *   Shift + strzałki: góra, dół -- zmiana kolejności warstw;

    *   klawisz E -- wyświetl wyłącznie aktywną warstwę;

    *   klawisz Enter -- pokaż terminal;

    *   prawy przycisk myszy -- postaw kafelek w miejscu kursora;

    *   ruch myszy + prawy przycisk myszy -- przesuń obraz tła;

    *   Shift + ruch myszy + prawy przycisk myszy -- przesuń widok;

    Wiele operacji jest dostępnych wyłącznie z terminala, który pokazuje się
klawiszem Enter i tym samym klawiszem zatwierdza się polecenie. Klawisz Esc
wychodzi z terminala bez zaakceptowania polecenia:

    *   new <nazwa_pokoju> <liczba_wierszy> <liczba kolumn> -- tworzy nowy
        pokój;

    *   load <nazwa_pokoju> -- wczytuje istniejący pokój;

    *   save [<nazwa_pokoju>] -- zapisuje poków pod podana nazwą lub pod
        aktualną nazwą, jeśli nowa nie zostanie podana;

    *   mode obj lub mode world -- przełącza między trybem edycji świata
        i trybem edycji obiektów;

    *   show tiles lub hide tiles -- pokazuje lub chowa warstwę fizycznych
        kafelków;

    *   set tileset <nazwa_tilesetu> -- ustawia zestaw kafelków;

    *   add uniform layer <r> <g> <b> -- dodaje warstwę jednolita o kolorze
        (r, g, b);

    *   add image layer <nazwa_obrazu> -- dodaje warstwę z obrazem
        data/backgrounds/<nazwa_obrazu>.png;

    *   add tiled layer -- dodaje warstwę kafelek;

    *   rem layer -- usuwa aktywna warstwę;

    *   set repeat <powtarzanie> -- ustawia powtarzanie aktywnego obrazka;
        dostępne opcje to: xy, x, y, -;

    *   set parallax <liczba> -- ustawia paralaksę aktywnago obrazka;

    *   add obj <rodzaj_obiektu> -- dodaje obiekt danego rodzaju;

    *   set name <nazwa> -- ustawia nazwę aktywnego obiektu;

    *   edit init -- włącza tryb edycji struktury inicjującej aktywny obiekt;

    *   rem obj -- usuwa aktywny obiekt.

    Niektóre polecenia działają tylko w określonym trybie (obj lub world) lub
    dla określonych rodzajów aktywnej warstwy. Podział powienien byc intuicyjny. 


