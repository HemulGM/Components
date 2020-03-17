unit HGM.Common.DateUtils;

interface

uses
  System.SysUtils, Vcl.Graphics, Vcl.Dialogs, System.DateUtils;

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

implementation

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

