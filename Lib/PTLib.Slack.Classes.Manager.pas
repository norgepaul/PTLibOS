unit PTLib.Slack.Classes.Manager;

interface

uses
  System.Classes, System.SysUtils, System.Threading,
  System.Net.URLClient, System.Net.HttpClient, System.Net.HttpClientComponent,

  PTLib.Slack.Classes,
  PTLib.Slack.Interfaces;

type
  TSlackManager = class
  private
    class procedure SendWebhookRequestAsync(SlackWebhookRequest: ISlackWebhookRequest); static;
    class procedure SendWebhookRequest(SlackWebhookRequest: ISlackWebhookRequest); static;
  public
    class procedure SendMessage(SlackWebhook: ISlackWebhook;
      const SuccessCallback: TSlackWebhookOnSuccessCallback = nil;
      const ErrorCallback: TSlackWebhookOnErrorCallback = nil);
    class function NewSlackMessage(const WebhookURL: String): ISlackMessage;
  end;

implementation

{ TSlackManager }

class function TSlackManager.NewSlackMessage(
  const WebhookURL: String): ISlackMessage;
begin
  Result := TSlackMessage.Create;
  Result.WebHookURL := WebhookURL;
end;

class procedure TSlackManager.SendMessage(SlackWebhook: ISlackWebhook;
  const SuccessCallback: TSlackWebhookOnSuccessCallback;
  const ErrorCallback: TSlackWebhookOnErrorCallback);
var
  SlackWebhookRequest: ISlackWebhookRequest;
begin
  SlackWebhookRequest := TSlackWebhookRequest.Create;
  SlackWebhookRequest.SlackWebhook := SlackWebhook;
  SlackWebhookRequest.SuccessCallback := SuccessCallback;
  SlackWebhookRequest.ErrorCallback := ErrorCallback;

  SendWebhookRequestASync(SlackWebhookRequest);
end;

class procedure TSlackManager.SendWebhookRequest(SlackWebhookRequest: ISlackWebhookRequest);
var
  Stream: TStringStream;
  Response: TMemoryStream;
  NetHTTPClient: TNetHTTPClient;
begin
  Response := TMemoryStream.Create;
  Stream := TStringStream.Create(SlackWebhookRequest.SlackWebhook.GetJSON, TEncoding.UTF8);
  NetHTTPClient := TNetHTTPClient.Create(nil);
  try
    try
      NetHTTPClient.Post(
        SlackWebhookRequest.SlackWebhook.WebHookURL,
        Stream,
        Response);

      if Assigned(SlackWebhookRequest.SuccessCallback) then
      begin
        SlackWebhookRequest.SuccessCallback(SlackWebhookRequest.SlackWebhook, Response);
      end;
    except
      on e: Exception do
      begin
        if Assigned(SlackWebhookRequest.ErrorCallback) then
        begin
          SlackWebhookRequest.ErrorCallback(SlackWebhookRequest.SlackWebhook, e);
        end;
      end;
    end;
  finally
    FreeAndNil(NetHTTPClient);
    FreeAndNil(Stream);
    FreeAndNil(Response);
  end;
end;

class procedure TSlackManager.SendWebhookRequestASync(SlackWebhookRequest: ISlackWebhookRequest);
begin
  TTask.Run(
    procedure
    begin
      SendWebhookRequest(SlackWebhookRequest);
    end
  );
end;

end.
