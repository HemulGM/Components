unit HGM.Common.DateUtils;

interface

uses
  System.SysUtils, System.DateUtils;

type
  TDatePeriod = record
  private
    function GetDaysBetween: Integer;
  public
    DateBegin: TDateTime;
    DateEnd: TDateTime;
    property DaysBetween: Integer read GetDaysBetween;
    procedure SetValue(ADateBegin, ADateEnd: TDateTime);
    class function Create(DateBegin, DateEnd: TDateTime): TDatePeriod; static;
  end;

function HumanDateTime(Value: TDateTime; ShowTime: Boolean = True; WeekDay: Boolean = False): string;

function SecondsToTime(Value: Integer): TTime;

function HumanTime(Value: TTime): string;

function HumanDate(Value: TDate): string;

implementation

function HumanTime(Value: TTime): string;
var
  H, M, S, Ms: Word;
begin
  DecodeTime(Value, H, M, S, Ms);
  Result := '';
  if H > 0 then
    Result := Result + H.ToString + ' ч. ';
  if M > 0 then
    Result := Result + M.ToString + ' мин. ';
  if S > 0 then
    Result := Result + S.ToString + ' сек. ';
end;

function HumanDate(Value: TDate): string;
var
  D: Integer;
begin
  Result := '';
  D := Trunc(Value);
  if D > 0 then
    Result := Result + D.ToString + ' д. ';
end;

function SecondsToTime(Value: Integer): TTime;
begin
  Result := Value / SecsPerDay;
end;

function HumanDateTime(Value: TDateTime; ShowTime: Boolean; WeekDay: Boolean): string;

  function AddWeekDay: string;
  begin
    if WeekDay then
      Result := FormatDateTime(', ddd', Value)
    else
      Result := '';
  end;

begin
  if IsSameDay(Value, Today + 2) then
    Result := 'Послезавтра' + AddWeekDay
  else if IsSameDay(Value, Today + 1) then
    Result := 'Завтра' + AddWeekDay
  else if IsSameDay(Value, Today) then
    Result := 'Сегодня' + AddWeekDay
  else if IsSameDay(Value, Yesterday) then
    Result := 'Вчера' + AddWeekDay
  else if IsSameDay(Value, Yesterday - 1) then
    Result := 'Позавчера' + AddWeekDay
  else if YearOf(Value) = YearOf(Now) then
    Result := FormatDateTime('DD mmm', Value) + AddWeekDay
  else
    Result := FormatDateTime('DD mmm YYYY', Value) + AddWeekDay;

  if ShowTime then
    Result := Result + FormatDateTime(' в HH:NN:SS', Value);
end;

{ TDatePeriod }

class function TDatePeriod.Create(DateBegin, DateEnd: TDateTime): TDatePeriod;
begin
  Result.DateBegin := DateBegin;
  Result.DateEnd := DateEnd;
end;

function TDatePeriod.GetDaysBetween: Integer;
begin
  Result := System.DateUtils.DaysBetween(DateBegin, DateEnd);
end;

procedure TDatePeriod.SetValue(ADateBegin, ADateEnd: TDateTime);
begin
  DateBegin := ADateBegin;
  DateEnd := ADateEnd;
end;

end.

