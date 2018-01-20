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
with Ada.Real_Time;
use Ada.Real_Time;
with System;
with Ada.Calendar;
use Ada.Calendar;
-- with Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;

with Ada.Strings;
use Ada.Strings;
with Ada.Strings.Fixed;
use Ada.Strings.Fixed;

with Ada.Exceptions;
use Ada.Exceptions;

procedure Panel is

  Koniec : Boolean := False with Atomic;
  IsCruiseControlActive : Boolean := False;
  ShouldSlowDown : Boolean := False;
  IncrementValue : Integer := 0;

  subtype Do5 is Integer range 0..5;
  package Losuj is new Ada.Numerics.Discrete_Random(Do5);

  type Stany is (Duzo, Malo);
  Stan : Stany := Malo with Atomic;

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
      Ekran.Pisz_XY(1,1,"+=========== Panel ===========+");
      Ekran.Pisz_XY(3,5,"Ostatni wynik =");
      Ekran.Pisz_XY(9,7,"Stan:");
      Ekran.Pisz_XY(1,10,"+= Q-koniec, D-dużo, M-mało =+");
    end Tlo;

  end Ekran;

  protected Zdarzenie is
    entry Czekaj(Ok: out Natural);
    procedure Wstaw(Ok: in Natural);
  private
    pragma Priority (System.Default_Priority+4);
    Okres : Natural := 0;
    Jest_Zdarzenie : Boolean := False;
  end Zdarzenie;

  protected body Zdarzenie is
    entry Czekaj(Ok: out Natural) when Jest_Zdarzenie is
    begin
      Jest_Zdarzenie := False;
      Ok := Okres;
    end Czekaj;
    procedure Wstaw(Ok: in Natural) is
    begin
      Jest_Zdarzenie := True;
      Okres := Ok;
    end Wstaw;
  end Zdarzenie;

  function SpeedControl(Speed : Integer) return Integer;

  function SpeedControl(Speed : Integer) return Integer is
      use Losuj;
      G : Generator;
      NewSpeed : Integer;
  begin
      Reset(G);
      if not IsCruiseControlActive then
          if shouldSlowDown then
              NewSpeed := Speed - Random(G);
          else
              NewSpeed := Speed + Random(G);
          end if;
      else
          NewSpeed := Speed;
      end if;
      return NewSpeed;
  end SpeedControl;

  task Drive;

  task body Drive is
      use Losuj;
      Next : Ada.Real_Time.Time;
      Interval : constant Ada.Real_Time.Time_Span := Ada.Real_Time.Milliseconds(1200);
      Speed: Integer := 0;
      G : Generator;
      Ok : Natural;
  begin
      Zdarzenie.Czekaj(Ok);
      Reset(G);
      Next := Ada.Real_Time.Clock;
      loop
          delay until Next;
          Speed := SpeedControl(Speed);
          Ekran.Pisz_Float_XY(19, 5, Float(Speed), Atryb=>Negatyw);
          exit when Koniec;
          Next := Next + Interval;
      end loop;
  exception
      when others => null;
  end Drive;

-- Panel body
  Zn : Character;
begin
  -- inicjowanie
  Ekran.Tlo;
  loop
      Get_Immediate(Zn);
      case Zn is
          when ' ' => Zdarzenie.Wstaw(0);
          when 'w' => ShouldSlowDown := False;
          when 's' => ShouldSlowDown := True;
          when 'e' => IsCruiseControlActive := not IsCruiseControlActive;
          when 'a' => IncrementValue := 10;
          when 'd' => IncrementValue := -10;
          when 'q' => exit;
          when others  => null;
      end case;
    -- exit when Zn in 'q'|'Q';
    -- Stan := (if Zn in 'D'|'d' then Duzo elsif Zn in 'M'|'m' then Malo else Stan);
  end loop;
  Koniec := True;
end Panel;
