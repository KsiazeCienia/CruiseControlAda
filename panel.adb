--Marcin Włoczko
--Michał Mielus
--Tempomat

with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Float_Text_IO;
use Ada.Float_Text_IO;
with Ada.Real_Time;
use Ada.Real_Time;
with System;
with Ada.Calendar;
use Ada.Calendar;
with Ada.Numerics.Discrete_Random;

with Ada.Strings;
use Ada.Strings;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Ada.Exceptions;
use Ada.Exceptions;

procedure Panel is

  Quit : Boolean := False with Atomic;
  ShouldSlowDown : Boolean := False with Atomic;
  IncrementValue : Integer := 0 with Atomic;
  Distance : Float := 0.0;

  subtype Do5 is Integer range 1..5;
  package Losuj is new Ada.Numerics.Discrete_Random(Do5);

  type Stany is (Wlaczony, Wylaczony);
  Stan : Stany := Wylaczony with Atomic;

  type Atrybuty is (Czysty, Jasny, Podkreslony, Negatyw, Migajacy, Szary);

  protected Screen  is
    procedure Write_XY(X,Y: Positive; S: String; Atryb : Atrybuty := Czysty);
    procedure Write_Number(X, Y: Positive;
                            Num: Float;
                            Pre: Natural := 3;
                            Aft: Natural := 2;
                            Exp: Natural := 0;
                            Atryb : Atrybuty := Czysty);
    procedure Clear;
    procedure Background;
  end Screen;

  protected body Screen is
    -- implementacja dla Linuxa i macOSX
    function Atryb_Fun(Atryb : Atrybuty) return String is
      (case Atryb is
       when Jasny => "1m", when Podkreslony => "4m", when Negatyw => "7m",
       when Migajacy => "5m", when Szary => "2m", when Czysty => "0m");

    function Esc_XY(X,Y : Positive) return String is
      ( (ASCII.ESC & "[" & Trim(Y'Img,Both) & ";" & Trim(X'Img,Both) & "H") );

    procedure Write_XY(X,Y: Positive; S: String; Atryb : Atrybuty := Czysty) is
      Przed : String := ASCII.ESC & "[" & Atryb_Fun(Atryb);
    begin
      Put( Przed);
      Put( Esc_XY(X,Y) & S);
      Put( ASCII.ESC & "[0m");
    end Write_XY;

    procedure Write_Number(X, Y: Positive;
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
    end Write_Number;

    procedure Clear is
    begin
      Put(ASCII.ESC & "[2J");
    end Clear;

    procedure Background is
    begin
      Screen.Clear;
      Screen.Write_XY(1,1,"+=========== Panel ===========+");
      Screen.Write_XY(3,3,"Aktualna prędkość =");
      Screen.Write_XY(29,3,"km/h");
      Screen.Write_XY(3,5,"Pokonany dystans =");
      Screen.Write_XY(29,5, " km");
      Screen.Write_XY(4,7,"Stan tempomatu:");
      Screen.Write_XY(1,10,"+========= Instrukcja ========+");
      Screen.Write_XY(1,11,"Spacja - start");
      Screen.Write_XY(1,12,"W - przyspiesz");
      Screen.Write_XY(1,13,"S - zwlonij");
      Screen.Write_XY(1,14,"E - włącz/wyłącz tempomat");
      Screen.Write_XY(1,15,"R - zwiększ predkość tempomatu o 10km/h");
      Screen.Write_XY(1,16,"F - zmniejsz prędkość tempomatu o 10km/h");
      Screen.Write_XY(1,17,"T - zwiększ prędkość tempomatu o 1 km/h");
      Screen.Write_XY(1,18,"G - zmniejsz prędkość tempomatu o 1 km/h");
      Screen.Write_XY(1,19,"Q - wyjście");
    end Background;

  end Screen;

  protected Event is
    entry Wait(Ok: out Natural);
    procedure Make(Ok: in Natural);
  private
    pragma Priority (System.Default_Priority+4);
    Okres : Natural := 0;
    isEvent : Boolean := False;
  end Event;

  protected body Event is
    entry Wait(Ok: out Natural) when isEvent is
    begin
      isEvent := False;
      Ok := Okres;
    end Wait;
    procedure Make(Ok: in Natural) is
    begin
      isEvent := True;
      Okres := Ok;
    end Make;
  end Event;

  function CalculateDistance(Speed : Integer) return Float;

  function CalculateDistance(Speed : Integer) return Float is
      Distance : Float;
  begin
      Distance := Float(Speed) / 3600.0;
      return Distance;
  end;

  function SpeedControl(Speed : Integer) return Integer;

  function SpeedControl(Speed : Integer) return Integer is
      use Losuj;
      G : Generator;
      NewSpeed : Integer;
      SpeedIncrease : Integer;
  begin
      Reset(G);
      SpeedIncrease := Random(G);
      if Stan = Wylaczony then
          if shouldSlowDown then
              NewSpeed := Speed - SpeedIncrease;
          else
              NewSpeed := Speed + SpeedIncrease;
          end if;
      else
          if IncrementValue < -5  then
              IncrementValue := IncrementValue + SpeedIncrease;
              NewSpeed := Speed - SpeedIncrease;
         else if IncrementValue > 5 then
             IncrementValue := IncrementValue - SpeedIncrease;
             NewSpeed := Speed + SpeedIncrease;
         else
             NewSpeed := Speed + IncrementValue;
             IncrementValue := 0;
         end if;
        end if;
      end if;
      if NewSpeed < 0 then
          NewSpeed := 0;
      else if NewSpeed > 240 then
          NewSpeed := 240;
      end if;
      end if;

      return NewSpeed;
  end SpeedControl;

  task Drive;

  task body Drive is
      use Losuj;
      Next : Ada.Real_Time.Time;
      Interval : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(1000);
      Speed: Integer := 0;
      Ok : Natural;
  begin
      Event.Wait(Ok);
      Next := Ada.Real_Time.Clock;
      loop
          delay until Next;
          Speed := SpeedControl(Speed);
          Distance := Distance + CalculateDistance(Speed);
          Screen.Write_Number(23, 3, Float(Speed), Atryb=>Negatyw);
          Screen.Write_Number(23, 5, Distance, Atryb=>Negatyw);
          Screen.Write_XY(20,7,Stan'Img, Atryb=>Podkreslony);
          exit when Quit;
          Next := Next + Interval;
      end loop;
  exception
      when others => null;
  end Drive;


-- Panel body
  Zn : Character;
begin
  -- inicjowanie
  Screen.Background;
  loop
      Get_Immediate(Zn);
      case Zn is
          when ' ' => Event.Make(0);
          when 'w' => ShouldSlowDown := False;
          when 's' => ShouldSlowDown := True;
          when 'e' => Stan := (if Stan = Wlaczony then Wylaczony else Wlaczony);
          when 'r' => IncrementValue := 10;
          when 'f' => IncrementValue := -10;
          when 't' => IncrementValue := 1;
          when 'g' => IncrementValue := -1;
          when 'q' => exit;
          when others  => null;
      end case;
  end loop;
  Quit := True;
end Panel;
