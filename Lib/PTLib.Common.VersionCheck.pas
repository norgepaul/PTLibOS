unit PTLib.Common.VersionCheck;

interface

uses
  System.SysUtils, System.Classes, System.Threading, System.Net.HTTPClient,
  System.IOUtils,

  PTLib.Common.Strings;

type
  TOnVersionCheckSuccess = reference to procedure(const DownloadURL: String; const Version: String);
  TOnVersionCheckFail = reference to procedure(const e: Exception);

  TPTLibVersionCheck = class
  public
    class procedure CheckVersion(const VersionString: String; const BaseURL: String; const BaseFilename: String; const OnSuccess: TOnVersionCheckSuccess; const OnFail: TOnVersionCheckFail);
  end;

implementation

{ TPTLibVersionCheck }

class procedure TPTLibVersionCheck.CheckVersion(const VersionString, BaseURL, BaseFilename: String; const OnSuccess: TOnVersionCheckSuccess;
  const OnFail: TOnVersionCheckFail);
var
  Thread: TThread;
begin
  Thread := TThread.CreateAnonymousThread(
    procedure
    var
      HTTPClient: THTTPClient;
      FileVersionURL, RemoteVersion: String;
      FileURL: String;
      HTTPResponse: IHTTPResponse;
      ex: Exception;
    begin
      HTTPClient := THTTPClient.Create;
      try
        FileVersionURL :=
          IncludeTrailingURLBackslash(BaseURL) +
          BaseFilename +
          '.version';

         try
           HTTPResponse := HTTPClient.Get(FileVersionURL);

           RemoteVersion := Trim(HTTPResponse.ContentAsString);

           if RemoteVersion = VersionString then
           begin
             if Assigned(OnSuccess) then
             begin
               OnSuccess('', '');
             end;
           end
           else
           begin
             FileURL :=
               IncludeTrailingURLBackslash(BaseURL) +
               BaseFilename +
               'SetupV' +
               RemoteVersion +
               '.exe';

             if Assigned(OnSuccess) then
             begin
               OnSuccess(FileURL, RemoteVersion);
             end;
           end;
         except
           on e: Exception do
           begin
             if Assigned(OnFail) then
             begin
               ex := Exception(AcquireExceptionObject);

               OnFail(ex);
             end;
           end;
         end;
      finally
        FreeAndNil(HTTPClient);
      end;
    end
  );
  Thread.Start;
end;

end.
