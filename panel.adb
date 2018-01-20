-- panel.adb
--
-- materiały dydaktyczne
-- 2016
-- (c) Jacek Piwowarczyk
--

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Float_Text_IO;
use Ada.Float_Text_IO;

with Ada.Calendar;
use Ada.Calendar;
with Ada.Numerics.Float_Random;

with Ada.Strings;
use Ada.Strings;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Ada.Exceptions;
use Ada.Exceptions;

procedure Panel is

  Koniec : Boolean := False with Atomic;

  type CruiseControlStates is (On, Off);
  CruiseControlState : CruiseControlStates := Off with Atomic;

  type Atrybuty is (Czysty, Jasny, Podkreslony, Negatyw, Migajacy, Szary);

  protected Ekran  is
    procedure Pisz_XY(X,Y: Positive; S: String; Atryb : Atrybuty := Czysty);
    procedure Pisz_Float_XY(X, Y: Positive;
                            Num: Float;
                            Pre: Natural := 3;
                            Aft: Natural := 2;
                            Exp: Natural := 0;
                            Atryb : Atrybuty := Czysty);
    procedure Czysc;
    procedure Tlo;
  end Ekran;

  protected body Ekran is
    -- implementacja dla Linuxa i macOSX
    function Atryb_Fun(Atryb : Atrybuty) return String is
      (case Atryb is
       when Jasny => "1m", when Podkreslony => "4m", when Negatyw => "7m",
       when Migajacy => "5m", when Szary => "2m", when Czysty => "0m");

    function Esc_XY(X,Y : Positive) return String is
      ( (ASCII.ESC & "[" & Trim(Y'Img,Both) & ";" & Trim(X'Img,Both) & "H") );

    procedure Pisz_XY(X,Y: Positive; S: String; Atryb : Atrybuty := Czysty) is
      Przed : String := ASCII.ESC & "[" & Atryb_Fun(Atryb);
    begin
      Put( Przed);
      Put( Esc_XY(X,Y) & S);
      Put( ASCII.ESC & "[0m");
    end Pisz_XY;

    procedure Pisz_Float_XY(X, Y: Positive;
                            Num: Float;
                            Pre: Natural := 3;
                            Aft: Natural := 2;
                            Exp: Natural := 0;
                            Atryb : Atrybuty := Czysty) is

      Przed_Str : String := ASCII.ESC & "[" & Atryb_Fun(Atryb);
    begin
      Put( Przed_Str);
      Put( Esc_XY(X, Y) );
      Put( Num, Pre, Aft, Exp);
      Put( ASCII.ESC & "[0m");
    end Pisz_Float_XY;

    procedure Czysc is
    begin
      Put(ASCII.ESC & "[2J");
    end Czysc;

    procedure Tlo is
    begin
      Ekran.Czysc;
      Ekran.Pisz_XY(1,1,"+=========== Automatyczny tempomat ===========+");
      Ekran.Pisz_XY(3,5,"Aktualna prędkość =");
      Ekran.Pisz_XY(9,7,"Stan tempomatu:");
      Ekran.Pisz_XY(1,10,"+======== Instrukcja ========+");
      Ekran.Pisz_XY(1,11,"Space - Start");
      Ekran.Pisz_XY(1,12,"W - przyspiesz");
      Ekran.Pisz_XY(1,13,"S - zwolnij");
      Ekran.Pisz_XY(1,14,"E - włącz/wyłącz tempomat");
      Ekran.Pisz_XY(1,15,"A - Zwiększ prędkość tempomatu o 10 km/h");
      Ekran.Pisz_XY(1,15,"D - Zmiejsz prędkość tempomatu o 10 km/h");

    end Tlo;

  end Ekran;

  task Przebieg;

  task body Przebieg is
    use Ada.Numerics.Float_Random;

    Nastepny     : Ada.Calendar.Time;
    Okres        : conCruiseControlStatet Duration := 0.8; -- sekundy
    Przesuniecie : conCruiseControlStatet Duration := 0.5;

    Gen : Generator;
    function Los_Fun return Float is
        (Random(Gen) * (if CruiseControlState=On then 80.0 else 20.0) - 20.0);
    Wartosc : Float := Los_Fun;
  begin
    Reset(Gen);
    Nastepny := Clock + Przesuniecie;
    loop
      delay until Nastepny;
      Wartosc := Los_Fun;
      Ekran.Pisz_XY(19 ,5, 20*' ', Atryb=>Czysty);
      Ekran.Pisz_Float_XY(19, 5, Wartosc, Atryb=>Negatyw);
      Ekran.Pisz_XY(15 ,7, CruiseControlState'Img, Atryb=>Podkreslony);
      exit when Koniec;
      Nastepny := Nastepny + Okres;
    end loop;
    Ekran.Pisz_XY(1,11,"");
    exception
      when E:others =>
        Put_Line("Error: Zadanie Przebieg");
        Put_Line(Exception_Name (E) & ": " & Exception_Message (E));
  end Przebieg;

  Zn : Character;

begin
  -- inicjowanie
  Ekran.Tlo;
  loop
    Get_Immediate(Zn);
    exit when Zn in 'q'|'Q';
    CruiseControlState := (if Zn in 'D'|'d' then On elsif Zn in 'M'|'m' then Off else CruiseControlState);
  end loop;
  Koniec := True;
end Panel;
