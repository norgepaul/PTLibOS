unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, PTLib.View.Vcl.AwesomeEdit,
  PTLib.View.Vcl.SearchEdit, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters,
  Vcl.Menus, Vcl.StdCtrls, cxButtons, JvExControls, JvLinkLabel,
  Vcl.ExtCtrls;

type
  TForm1 = class(TForm)
    PTLibSearchEdit2: TPTLibSearchEdit;
    Panel2: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    pnlHeaderButtonsTop: TPanel;
    btnExportToExcel: TcxButton;
    cxButton1: TcxButton;
    btnRefresh: TcxButton;
    edtSearch: TPTLibSearchEdit;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

end.
