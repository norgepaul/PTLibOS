unit PTLib.XSuperObject.ASync;

interface

uses
  System.Threading, System.SysUtils, System.Classes,

  XSuperObject;

type
  TObjectSuccessProc = reference to procedure(X: ISuperObject);
  TArraySuccessProc = reference to procedure(X: ISuperArray);
  TFailedProc = reference to procedure(e: Exception);

  TXSuperObjectASync = class
    class procedure SO(const JSON: String; const OnSuccess: TObjectSuccessProc; const OnFailure: TFailedProc);
    class procedure SA(const JSON: String; const OnSuccess: TArraySuccessProc; const OnFailure: TFailedProc);
  end;

implementation

{ TXSuperObjectASync }

class procedure TXSuperObjectASync.SA(const JSON: String; const OnSuccess: TArraySuccessProc; const OnFailure: TFailedProc);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      X: ISuperArray;
    begin
      try
        X := TSuperArray.Create(JSON);

        if Assigned(OnSuccess) then
        begin
          TThread.Synchronize(
            nil,
            procedure
            begin
              OnSuccess(X);
            end);
        end;
      except
        on e: Exception do
        begin
          TThread.Synchronize(
            nil,
            procedure
            begin
              OnFailure(Exception(AcquireExceptionObject));
            end);
        end;
      end;
    end
  ).Start;
end;

class procedure TXSuperObjectASync.SO(const JSON: String; const OnSuccess: TObjectSuccessProc; const OnFailure: TFailedProc);
begin
  TThread.CreateAnonymousThread(
    procedure
    var
      X: ISuperObject;
    begin
      try
        X := TSuperObject.Create(JSON);

        if Assigned(OnSuccess) then
        begin
          TThread.Synchronize(
            nil,
            procedure
            begin
              OnSuccess(X);
            end);
        end;
      except
        on e: Exception do
        begin
          TThread.Synchronize(
            nil,
            procedure
            begin
              OnFailure(Exception(AcquireExceptionObject));
            end);
        end;
      end;
    end
  ).Start;
end;

end.
