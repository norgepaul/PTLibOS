unit PTLib.Slack.Interfaces;

interface

uses
  System.Classes, System.UITypes, System.SysUtils,

  PTLib.Common.Interfaces,

  PTLib.Slack.Types;

type
  ISlackWebhook = interface;

  TSlackWebhookOnSuccessCallback = reference to procedure(SlackWebhook: ISlackWebhook; const Response: TStream);
  TSlackWebhookOnErrorCallback = reference to procedure(SlackWebhook: ISlackWebhook; const e: Exception);

  ISlackBase = interface
    ['{25F70372-B429-4E92-991E-ECE8F4B0E62F}']
  end;

  ISlackWebhook = interface(ISlackBase)
    ['{6B5C6DC6-5399-49C8-9C8F-978788D1613C}']
    function GetJSON: String;
    function GetWebHookURL: String;
    procedure SetWebHookURL(const Value: String);
    property WebHookURL: String read GetWebHookURL write SetWebHookURL;
  end;

  ISlackWebhookRequest = interface(ISlackBase)
    ['{0E733D43-8AEA-475F-8E46-C98144539B4B}']
    function GetSuccessCallback: TSlackWebhookOnSuccessCallback;
    function GetSlackWebhook: ISlackWebhook;
    procedure SetSuccessCallback(const Value: TSlackWebhookOnSuccessCallback);
    procedure SetSlackWebhook(const Value: ISlackWebhook);
    function GetErrorCallback: TSlackWebhookOnErrorCallback;
    procedure SetErrorCallback(const Value: TSlackWebhookOnErrorCallback);
    property SuccessCallback: TSlackWebhookOnSuccessCallback read GetSuccessCallback write SetSuccessCallback;
    property ErrorCallback: TSlackWebhookOnErrorCallback read GetErrorCallback write SetErrorCallback;
    property SlackWebhook: ISlackWebhook read GetSlackWebhook write SetSlackWebhook;
  end;

  ISlackMessageAttachmentField = interface(ISlackBase)
    ['{EE24DDC3-E671-4C77-87EF-1E1F32DA145E}']
    function GetShort: Boolean;
    function GetTitle: String;
    function GetValue: String;
    procedure SetShort(const Value: Boolean);
    procedure SetTitle(const Value: String);
    procedure SetValue(const Value: String);
    property Title: String read GetTitle write SetTitle;
    property Value: String read GetValue write SetValue;
    property Short: Boolean read GetShort write SetShort;
  end;

  ISlackMessageAttachmentAction = interface(ISlackBase)
    ['{B829F0E4-6D93-43A1-A02A-FF5341FC50C3}']
    function GetActionType: TSlackActionType;
    function GetStyle: TSlackActionStyle;
    function GetText: String;
    function GetURL: String;
    procedure SetActionType(const Value: TSlackActionType);
    procedure SetStyle(const Value: TSlackActionStyle);
    procedure SetText(const Value: String);
    procedure SetURL(const Value: String);
    property ActionType: TSlackActionType read GetActionType write SetActionType;
    property Text: String read GetText write SetText;
    property URL: String read GetURL write SetURL;
    property Style: TSlackActionStyle read GetStyle write SetStyle;
  end;

  ISlackMessageAttachment = interface(ISlackBase)
    ['{C0B3401F-B90F-4904-B3B1-31A015A3EA7F}']
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

    function AddField(const Title: String; const Value: String; const Short: Boolean = True): ISlackMessageAttachmentField;
    function AddAction(const Text: String; const URL: String; const Style: TSlackActionStyle = TSlackActionStyle.sasPrimary; const ActionType: TSlackActionType = TSlackActionType.satButton): ISlackMessageAttachmentAction;
  end;

  ISlackMessage = interface(ISlackWebhook)
    ['{35ADB880-467A-42FD-B677-1B51EFB7BB2D}']
    function GetAttachments: IList<ISlackMessageAttachment>;
    function GetText: String;
    procedure SetText(const Value: String);
    function GetMarkdown: Boolean;
    procedure SetMarkdown(const Value: Boolean);
    property Text: String read GetText write SetText;
    property Markdown: Boolean read GetMarkdown write SetMarkdown;
    property Attachments: IList<ISlackMessageAttachment> read GetAttachments;
    function AddAttachment: ISlackMessageAttachment;
  end;

implementation

end.
