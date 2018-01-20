with System;
with Ada.Numerics.Discrete_Random;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Real_Time;
use Ada.Real_Time;


procedure Main is

    subtype Do5 is Integer range 0..5;
    package Losuj is new Ada.Numerics.Discrete_Random(Do5);

    IsCruiseControlActive : Boolean := False;
    ShouldSlowDown : Boolean := False;
    IncrementValue : Integer := 0;


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
            Speed := Main.SpeedControl(Speed);
            -- Panel.Ekran.Pisz_Float_XY(23, 5, Speed, Panel.Atryb=>Negatyw);
            Put_Line("Aktualna prędkośc wynosi" & Speed'Img & "km\h");

            Next := Next + Interval;
        end loop;
    exception
        when others => null;
    end Drive;

    task WaitingForKeyboardEvent;

    task body WaitingForKeyboardEvent is
        Answer : Character;
    begin
        loop
            Ada.Text_IO.Get_Immediate (Answer);
            case Answer is
                when ' ' => Zdarzenie.Wstaw(0);
                when 'w' => ShouldSlowDown := False;
                when 's' => ShouldSlowDown := True;
                when 'e' => IsCruiseControlActive := not IsCruiseControlActive;
                when 'a' => IncrementValue := 10;
                when 'd' => IncrementValue := -10;
                when others  => null;
            end case;
            Put_Line("Wciśnięto " & Answer'Img);
        end loop;
    end WaitingForKeyboardEvent;

begin
    Put_Line("Start");

end Main;
