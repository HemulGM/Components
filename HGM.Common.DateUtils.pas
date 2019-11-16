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

implementation

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

