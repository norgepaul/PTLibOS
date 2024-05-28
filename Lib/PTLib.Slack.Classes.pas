unit PTLib.Slack.Classes;

interface

uses
  System.Classes, System.SysUtils, System.UITypes, System.JSON.Types,
  System.JSON.Builders, System.DateUtils,

  PTLib.Common.Interfaces,
  PTLib.Common.Classes,
  PTLib.Common.JSON,

  PTLib.Slack.Types,
  PTLib.Slack.Interfaces;

type
  TSlackBase = class(TInterfacedObject, ISlackBase)
  end;

  TSlackWebhook = class(TSlackBase, ISlackWebhook)
  strict private
    FWebHookURL: String;
  private
    function GetWebHookURL: String;
    procedure SetWebHookURL(const Value: String);
    function GetJSON: String;
  protected
    procedure DoGetJSON(out JSON: String); virtual; abstract;

    property WebHookURL: String read GetWebHookURL write SetWebHookURL;
  end;

  TSlackWebhookRequest = class(TSlackBase, ISlackWebhookRequest)
  strict private
    FSuccessCallback: TSlackWebhookOnSuccessCallback;
    FErrorCallback: TSlackWebhookOnErrorCallback;
    FSlackWebhook: ISlackWebhook;
  private
    function GetSuccessCallback: TSlackWebhookOnSuccessCallback;
    function GetSlackWebhook: ISlackWebhook;
    procedure SetSuccessCallback(const Value: TSlackWebhookOnSuccessCallback);
    procedure SetSlackWebhook(const Value: ISlackWebhook);
    function GetErrorCallback: TSlackWebhookOnErrorCallback;
    procedure SetErrorCallback(const Value: TSlackWebhookOnErrorCallback);
  protected
    property SuccessCallback: TSlackWebhookOnSuccessCallback read GetSuccessCallback write SetSuccessCallback;
    property ErrorCallback: TSlackWebhookOnErrorCallback read GetErrorCallback write SetErrorCallback;
    property SlackWebhook: ISlackWebhook read GetSlackWebhook write SetSlackWebhook;
  end;

  TSlackMessageAttachmentField = class(TSlackBase, ISlackMessageAttachmentField)
  strict private
    FTitle: String;
    FValue: String;
    FShort: Boolean;
  private
    function GetShort: Boolean;
    function GetTitle: String;
    function GetValue: String;
    procedure SetShort(const Value: Boolean);
    procedure SetTitle(const Value: String);
    procedure SetValue(const Value: String);
  protected
    property Title: String read GetTitle write SetTitle;
    property Value: String read GetValue write SetValue;
    property Short: Boolean read GetShort write SetShort;
  end;

  TSlackMessageAttachmentAction = class(TSlackBase, ISlackMessageAttachmentAction)
  strict private
    FActionType: TSlackActionType;
    FText: String;
    FURL: String;
    FStyle: TSlackActionStyle;
  private
    function GetActionType: TSlackActionType;
    function GetStyle: TSlackActionStyle;
    function GetText: String;
    function GetURL: String;
    procedure SetActionType(const Value: TSlackActionType);
    procedure SetStyle(const Value: TSlackActionStyle);
    procedure SetText(const Value: String);
    procedure SetURL(const Value: String);
  protected
    property ActionType: TSlackActionType read GetActionType write SetActionType;
    property Text: String read GetText write SetText;
    property URL: String read GetURL write SetURL;
    property Style: TSlackActionStyle read GetStyle write SetStyle;
  end;

  TSlackMessageAttachment = class(TSlackBase, ISlackMessageAttachment)
  strict private
    FFallback: String;
    FColor: TColor;
    FPreText: String;
    FTitle: String;
    FTitleLink: String;
    FText: String;
    FImageURL: String;
    FThumbURL: String;
    FFooter: String;
    FFooterIcon: String;
    FTimestamp: TDateTime;
    FAuthorName: String;
    FAuthorLink: String;
    FAuthorIcon: String;
    FFields: IList<ISlackMessageAttachmentField>;
    FActions: IList<ISlackMessageAttachmentAction>;
  private
    function GetColor: TColor;
    function GetFallback: String;
    function GetFields: IList<ISlackMessageAttachmentField>;
    function GetFooter: String;
    function GetFooterIcon: String;
    function GetImageURL: String;
    function GetText: String;
    function GetThumbURL: String;
    function GetTitle: String;
    function GetTitleLink: String;
    function GetTimestamp: TDateTime;
    procedure SetColor(const Value: TColor);
    procedure SetFallback(const Value: String);
    procedure SetFooter(const Value: String);
    procedure SetFooterIcon(const Value: String);
    procedure SetImageURL(const Value: String);
    procedure SetText(const Value: String);
    procedure SetThumbURL(const Value: String);
    procedure SetTitle(const Value: String);
    procedure SetTitleLink(const Value: String);
    procedure SetTimestamp(const Value: TDateTime);
    function GetPreText: String;
    procedure SetPreText(const Value: String);
    function GetAuthorIcon: String;
    function GetAuthorLink: String;
    function GetAuthorName: String;
    procedure SetAuthorIcon(const Value: String);
    procedure SetAuthorLink(const Value: String);
    procedure SetAuthorName(const Value: String);
    function GetActions: IList<ISlackMessageAttachmentAction>;
  protected
    property Fallback: String read GetFallback write SetFallback;
    property Color: TColor read GetColor write SetColor;
    property PreText: String read GetPreText write SetPreText;
    property Title: String read GetTitle write SetTitle;
    property TitleLink: String read GetTitleLink write SetTitleLink;
    property Text: String read GetText write SetText;
    property ImageURL: String read GetImageURL write SetImageURL;
    property ThumbURL: String read GetThumbURL write SetThumbURL;
    property Footer: String read GetFooter write SetFooter;
    property FooterIcon: String read GetFooterIcon write SetFooterIcon;
    property Timestamp: TDateTime read GetTimestamp write SetTimestamp;
    property AuthorName: String read GetAuthorName write SetAuthorName;
    property AuthorLink: String read GetAuthorLink write SetAuthorLink;
    property AuthorIcon: String read GetAuthorIcon write SetAuthorIcon;
    property Fields: IList<ISlackMessageAttachmentField> read GetFields;
    property Actions: IList<ISlackMessageAttachmentAction> read GetActions;
  public
    constructor Create; virtual;

    function AddField(const Title: String; const Value: String; const Short: Boolean = True): ISlackMessageAttachmentField;
    function AddAction(const Text: String; const URL: String; const Style: TSlackActionStyle = TSlackActionStyle.sasPrimary; const ActionType: TSlackActionType = TSlackActionType.satButton): ISlackMessageAttachmentAction;
  end;

  TSlackMessage = class(TSlackWebhook, ISlackMessage)
  strict private
    FText: String;
    FMarkdown: Boolean;
    FAttachments: IList<ISlackMessageAttachment>;
  private
    function GetAttachments: IList<ISlackMessageAttachment>;
    function GetText: String;
    procedure SetText(const Value: String);
    function GetMarkdown: Boolean;
    procedure SetMarkdown(const Value: Boolean);
  protected
    procedure DoGetJSON(out JSON: String); override;

    function AddAttachment: ISlackMessageAttachment;

    property Text: String read GetText write SetText;
    property Markdown: Boolean read GetMarkdown write SetMarkdown;
    property Attachments: IList<ISlackMessageAttachment> read GetAttachments;
  public
    constructor Create; virtual;
  end;

implementation

{ TSlackWebhook }

function TSlackWebhook.GetJSON: String;
begin
  DoGetJSON(Result);
end;

function TSlackWebhook.GetWebHookURL: String;
begin
  Result := FWebHookURL;
end;

procedure TSlackWebhook.SetWebHookURL(const Value: String);
begin
  FWebHookURL := Value;
end;

{ TSlackMessageAttachmentField }

function TSlackMessageAttachmentField.GetShort: Boolean;
begin
  Result := FShort;
end;

function TSlackMessageAttachmentField.GetTitle: String;
begin
  Result := FTitle;
end;

function TSlackMessageAttachmentField.GetValue: String;
begin
  Result := FValue;
end;

procedure TSlackMessageAttachmentField.SetShort(const Value: Boolean);
begin
  FShort := Value;
end;

procedure TSlackMessageAttachmentField.SetTitle(const Value: String);
begin
  FTitle := Value;
end;

procedure TSlackMessageAttachmentField.SetValue(const Value: String);
begin
  FValue := Value;
end;

{ TSlackMessageAttachment }

function TSlackMessageAttachment.AddAction(const Text, URL: String;
  const Style: TSlackActionStyle;
  const ActionType: TSlackActionType): ISlackMessageAttachmentAction;
begin
  Result := TSlackMessageAttachmentAction.Create;
  Result.ActionType := ActionType;
  Result.Text := Text;
  Result.URL := URL;
  Result.Style := Style;
  FActions.Add(Result);
end;

function TSlackMessageAttachment.AddField(const Title, Value: String;
  const Short: Boolean): ISlackMessageAttachmentField;
begin
  Result := TSlackMessageAttachmentField.Create;
  Result.Title := Title;
  Result.Value := Value;
  Result.Short := Short;

  FFields.Add(Result);
end;

constructor TSlackMessageAttachment.Create;
begin
  inherited;

  FFields := TList<ISlackMessageAttachmentField>.Create;
  FActions := TList<ISlackMessageAttachmentAction>.Create;
end;

function TSlackMessageAttachment.GetActions: IList<ISlackMessageAttachmentAction>;
begin
  Result := FActions;
end;

function TSlackMessageAttachment.GetAuthorIcon: String;
begin
  Result := FAuthorIcon;
end;

function TSlackMessageAttachment.GetAuthorLink: String;
begin
  Result := FAuthorLink;
end;

function TSlackMessageAttachment.GetAuthorName: String;
begin
  Result := FAuthorName;
end;

function TSlackMessageAttachment.GetColor: TColor;
begin
  Result := FColor;
end;

function TSlackMessageAttachment.GetFallback: String;
begin
  Result := FFallback;
end;

function TSlackMessageAttachment.GetFields: IList<ISlackMessageAttachmentField>;
begin
  Result := FFields;
end;

function TSlackMessageAttachment.GetFooter: String;
begin
  Result := FFooter;
end;

function TSlackMessageAttachment.GetFooterIcon: String;
begin
  Result := FFooterIcon;
end;

function TSlackMessageAttachment.GetImageURL: String;
begin
  Result := FImageURL;
end;

function TSlackMessageAttachment.GetPreText: String;
begin
  Result := FPreText;
end;

function TSlackMessageAttachment.GetText: String;
begin
  Result := FText;
end;

function TSlackMessageAttachment.GetThumbURL: String;
begin
  Result := FThumbURL;
end;

function TSlackMessageAttachment.GetTitle: String;
begin
  Result := FTitle;
end;

function TSlackMessageAttachment.GetTitleLink: String;
begin
  Result := FTitleLink;
end;

function TSlackMessageAttachment.GetTimestamp: TDateTime;
begin
  Result := FTimestamp;
end;

procedure TSlackMessageAttachment.SetAuthorIcon(const Value: String);
begin
  FAuthorIcon := Value;
end;

procedure TSlackMessageAttachment.SetAuthorLink(const Value: String);
begin
  FAuthorLink := Value;
end;

procedure TSlackMessageAttachment.SetAuthorName(const Value: String);
begin
  FAuthorName := Value;
end;

procedure TSlackMessageAttachment.SetColor(const Value: TColor);
begin
  FColor := Value;
end;

procedure TSlackMessageAttachment.SetFallback(const Value: String);
begin
  FFallback := Value;
end;

procedure TSlackMessageAttachment.SetFooter(const Value: String);
begin
  FFooter := Value;
end;

procedure TSlackMessageAttachment.SetFooterIcon(const Value: String);
begin
  FFooterIcon := Value;
end;

procedure TSlackMessageAttachment.SetImageURL(const Value: String);
begin
  FImageURL := Value;
end;

procedure TSlackMessageAttachment.SetPreText(const Value: String);
begin
  FPreText := Value;
end;

procedure TSlackMessageAttachment.SetText(const Value: String);
begin
  FText := Value;
end;

procedure TSlackMessageAttachment.SetThumbURL(const Value: String);
begin
  FThumbURL := Value;
end;

procedure TSlackMessageAttachment.SetTitle(const Value: String);
begin
  FTitle := Value;
end;

procedure TSlackMessageAttachment.SetTitleLink(const Value: String);
begin
  FTitleLink := Value;
end;

procedure TSlackMessageAttachment.SetTimestamp(const Value: TDateTime);
begin
  FTimestamp := Value;
end;

{ TSlackMessage }

function TSlackMessage.AddAttachment: ISlackMessageAttachment;
begin
  Result := TSlackMessageAttachment.Create;
  FAttachments.Add(Result);
end;

constructor TSlackMessage.Create;
begin
  FAttachments := TList<ISlackMessageAttachment>.Create;
  FMarkdown := True;
end;

procedure TSlackMessage.DoGetJSON(out JSON: String);

  function ColorToHex(const Color: TColor): String;
  begin
    Result :=
      '#' +
      IntToHex(Color and $000000FF, 2) +
      IntToHex((Color and $0000FF00) shr 8, 2) +
      IntToHex((Color and $00FF0000) shr 16, 2);
  end;

var
  JSONBuilder: IJSONBuilder;
  i: Integer;
  SlackMessage, Attachment, Action: TJSONCollectionBuilder.TPairs;
  AttachmentArray, FieldArray, ActionArray: TJSONCollectionBuilder.TElements;
  n: Integer;
begin
  JSONBuilder := NewJSONBuilder(TJsonFormatting.None);

  SlackMessage := JSONBuilder.Build.BeginObject;
  SlackMessage.Add('text', Text);
  SlackMEssage.Add('mrkdwn', Markdown);

  if Attachments.Count > 0 then
  begin
    AttachmentArray := SlackMessage.BeginArray('attachments');

    for i := 0 to pred(Attachments.Count) do
    begin
      Attachment := AttachmentArray.BeginObject;

      Attachment.
        Add('fallback', Attachments[i].Fallback).
        Add('pretext', Attachments[i].PreText).
        Add('title', Attachments[i].Title).
        Add('title_link', Attachments[i].TitleLink).
        Add('text', Attachments[i].Text).
        Add('color', ColorToHex(Attachments[i].Color)).
        Add('image_url', Attachments[i].ImageURL).
        Add('thumb_url', Attachments[i].ThumbURL).
        Add('footer', Attachments[i].Footer).
        Add('footer_icon', Attachments[i].FooterIcon).
        Add('ts', DateTimeToUnix(Attachments[i].Timestamp)).
        Add('author_name', Attachments[i].AuthorName).
        Add('author_link', Attachments[i].AuthorLink).
        Add('author_icon', Attachments[i].AuthorIcon);

      if Attachments[i].Fields.Count > 0 then
      begin
        FieldArray := Attachment.BeginArray('fields');

        for n := 0 to pred(Attachments[i].Fields.Count) do
        begin
          FieldArray.
            BeginObject.
              Add('title', Attachments[i].Fields[n].Title).
              Add('value', Attachments[i].Fields[n].Value).
              Add('short', Attachments[i].Fields[n].Short).
            EndObject;
        end;

        FieldArray.EndArray;
      end;

      if Attachments[i].Actions.Count > 0 then
      begin
        ActionArray := Attachment.BeginArray('actions');

        for n := 0 to pred(Attachments[i].Actions.Count) do
        begin
          ActionArray.
            BeginObject.
              Add('type',  SlackActionTypeValues[Attachments[i].Actions[n].ActionType]).
              Add('text', Attachments[i].Actions[n].Text).
              Add('url', Attachments[i].Actions[n].URL).
              Add('style', SlackActionStyleValues[Attachments[i].Actions[n].Style]).
            EndObject;
        end;

        ActionArray.EndArray;
      end;

      Attachment.EndObject;
    end;

    AttachmentArray.EndArray;
  end;

  SlackMessage.EndObject;

  JSON := JSONBuilder.ToJSONString;
end;

function TSlackMessage.GetAttachments: IList<ISlackMessageAttachment>;
begin
  Result := FAttachments;
end;

function TSlackMessage.GetMarkdown: Boolean;
begin
  Result := FMarkdown;
end;

function TSlackMessage.GetText: String;
begin
  Result := FText;
end;

procedure TSlackMessage.SetMarkdown(const Value: Boolean);
begin
  FMarkdown := Value;
end;

procedure TSlackMessage.SetText(const Value: String);
begin
  FText := Value;
end;

{ TSlackWebhookRequest }

function TSlackWebhookRequest.GetSuccessCallback: TSlackWebhookOnSuccessCallback;
begin
  Result := FSuccessCallback;
end;

function TSlackWebhookRequest.GetErrorCallback: TSlackWebhookOnErrorCallback;
begin
  Result := FErrorCallback;
end;

function TSlackWebhookRequest.GetSlackWebhook: ISlackWebhook;
begin
  Result := FSlackWebhook;
end;

procedure TSlackWebhookRequest.SetSuccessCallback(const Value: TSlackWebhookOnSuccessCallback);
begin
  FSuccessCallback := Value;
end;

procedure TSlackWebhookRequest.SetErrorCallback(
  const Value: TSlackWebhookOnErrorCallback);
begin
  FErrorCallback := Value;
end;

procedure TSlackWebhookRequest.SetSlackWebhook(const Value: ISlackWebhook);
begin
  FSlackWebhook := Value;
end;

{ TSlackMessageAttachmentAction }

function TSlackMessageAttachmentAction.GetActionType: TSlackActionType;
begin
  Result := FActionType;
end;

function TSlackMessageAttachmentAction.GetStyle: TSlackActionStyle;
begin
  Result := FStyle;
end;

function TSlackMessageAttachmentAction.GetText: String;
begin
  Result := FText;
end;

function TSlackMessageAttachmentAction.GetURL: String;
begin
  Result := FURL;
end;

procedure TSlackMessageAttachmentAction.SetActionType(
  const Value: TSlackActionType);
begin
  FActionType := Value;
end;

procedure TSlackMessageAttachmentAction.SetStyle(
  const Value: TSlackActionStyle);
begin
  FStyle := Value;
end;

procedure TSlackMessageAttachmentAction.SetText(const Value: String);
begin
  FText := Value;
end;

procedure TSlackMessageAttachmentAction.SetURL(const Value: String);
begin
  FURL := Value;
end;

end.
