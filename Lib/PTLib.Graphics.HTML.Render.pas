unit PTLib.Graphics.HTML.Render;

interface

uses
  Variants, Windows, Messages, ShellAPI, Registry, Types, System.UITypes,
  SysUtils, Math, Forms, Graphics, Controls, StdCtrls, ExtCtrls, Menus,
  Dialogs, ComCtrls, ImgList, Grids, MultiMon,
  Classes;

type
  TScriptPosition = (spNormal, spSuperscript, spSubscript);
  TJvHTMLCalcType = (htmlShow, htmlCalcWidth, htmlCalcHeight, htmlHyperLink);

const
  cBR = '<BR>';
  cHR = '<HR>';
  cTagBegin = '<';
  cTagEnd = '>';
  cLT = '<';
  cGT = '>';
  cQuote = '"';
  cCENTER = 'CENTER';
  cRIGHT = 'RIGHT';
  cHREF = 'HREF';
  cIND = 'IND';
  cCOLOR = 'COLOR';
  cBGCOLOR = 'BGCOLOR';

procedure HTMLDrawText(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; var Width, Height: Integer;
  CalcType: TJvHTMLCalcType;  MouseX, MouseY: Integer; var MouseOnLink: Boolean;
  var LinkName: string; SuperSubScriptRatio: Double; Scale: Integer);

implementation

function HTMLPrepareText(const Text: string): string;
type
  THtmlCode = record
    Html: string;
    Text: UTF8String;
  end;
const
  Conversions: array [0..6] of THtmlCode = (
    (Html: '&amp;'; Text: '&'),
    (Html: '&quot;'; Text: '"'),
    (Html: '&reg;'; Text: #$C2#$AE),
    (Html: '&copy;'; Text: #$C2#$A9),
    (Html: '&trade;'; Text: #$E2#$84#$A2),
    (Html: '&euro;'; Text: #$E2#$82#$AC),
    (Html: '&nbsp;'; Text: ' ')
  );
var
  I: Integer;
begin
  Result := Text;
  for I := Low(Conversions) to High(Conversions) do
    Result := StringReplace(Result, Conversions[I].Html, Utf8ToAnsi(Conversions[I].Text), [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, sLineBreak, '', [rfReplaceAll, rfIgnoreCase]); // only <BR> can be new line
  Result := StringReplace(Result, cBR, sLineBreak, [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, cHR, cHR + sLineBreak, [rfReplaceAll, rfIgnoreCase]); // fixed <HR><BR>
end;

function CanvasMaxTextHeight(Canvas: TCanvas): Integer;
var
  tt: TTextMetric;
begin
  GetTextMetrics(Canvas.Handle, tt);
  Result := tt.tmHeight;
end;

procedure HTMLDrawTextEx(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; var Width: Integer;
  CalcType: TJvHTMLCalcType;  MouseX, MouseY: Integer; var MouseOnLink: Boolean;
  var LinkName: string; SuperSubScriptRatio: Double; Scale: Integer);
var
  H: Integer;
begin
  HTMLDrawText(Canvas, Rect, State, Text, Width, H, CalcType, MouseX, MouseY, MouseOnLink,
    LinkName, SuperSubScriptRatio, Scale);
  if CalcType = htmlCalcHeight then
    Width := H;
end;

function HTMLTextWidth(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; SuperSubScriptRatio: Double; Scale: Integer = 100): Integer;
var
  S: Boolean;
  St: string;
begin
  HTMLDrawTextEx(Canvas, Rect, State, Text, Result, htmlCalcWidth, 0, 0, S, St, SuperSubScriptRatio, Scale);
end;

function HTMLBeforeTag(var Str: string; DeleteToTag: Boolean = False): string;
begin
  if Pos(cTagBegin, Str) > 0 then
  begin
    Result := Copy(Str, 1, Pos(cTagBegin, Str) - 1);
    if DeleteToTag then
      Delete(Str, 1, Pos(cTagBegin, Str) - 1);
  end
  else
  begin
    Result := Str;
    if DeleteToTag then
      Str := '';
  end;
end;

function GetChar(const Str: string; Pos: Word; Up: Boolean = False): Char;
begin
  if Length(Str) >= Pos then
    Result := Str[Pos]
  else
    Result := ' ';
  if Up then
    Result := UpCase(Result);
end;

function HTMLDeleteTag(const Str: string): string;
begin
  Result := Str;
  if (GetChar(Result, 1) = cTagBegin) and (Pos(cTagEnd, Result) > 1) then
    Delete(Result, 1, Pos(cTagEnd, Result));
end;

procedure HTMLDrawText(Canvas: TCanvas; Rect: TRect;
  const State: TOwnerDrawState; const Text: string; var Width, Height: Integer;
  CalcType: TJvHTMLCalcType;  MouseX, MouseY: Integer; var MouseOnLink: Boolean;
  var LinkName: string; SuperSubScriptRatio: Double; Scale: Integer);
const
  DefaultLeft = 0; // (ahuser) was 2
var
  vText, vM, TagPrp, Prp, TempLink: string;
  vCount: Integer;
  vStr: TStringList;
  Selected: Boolean;
  Alignment: TAlignment;
  Trans, IsLink: Boolean;
  CurLeft: Integer;
  CurrTop: Integer;
  // for begin and end
  OldFontStyles: TFontStyles;
  OldFontColor: TColor;
  OldBrushColor: TColor;
  OldBrushStyle: TBrushStyle;
  OldAlignment: TAlignment;
  OldFont: TFont;
  OldWidth: Integer;
  // for font style
  RemFontColor,
  RemBrushColor: TColor;
  RemFontSize: Integer;
  ScriptPosition: TScriptPosition;

  function ExtractPropertyValue(const Tag: string; PropName: string): string;
  var
    I: Integer;
  begin
    Result := '';
    PropName := UpperCase(PropName);
    if Pos(PropName, UpperCase(Tag)) > 0 then
    begin
      Result := Copy(Tag, Pos(PropName, UpperCase(Tag)) + Length(PropName), Length(Tag));
     if Pos('"', Result) <> 0 then
     begin
       Result := Copy(Result, Pos('"', Result) + 1, Length(Result));
       Result := Copy(Result, 1, Pos('"', Result) - 1);
     end
     else
     if Pos('''', Result) <> 0 then
     begin
       Result := Copy(Result, Pos('''', Result) + 1, Length(Result));
       Result := Copy(Result, 1, Pos('''', Result) - 1);
     end
     else
     begin
       Result := Trim(Result);
       Delete(Result, 1, 1);
       Result := Trim(Result);
       I := 1;
       while (I < Length(Result)) and (Result[I+1] <> ' ') do
         Inc(I);
       Result := Copy(Result, 1, I);
     end;
    end;
  end;

  procedure Style(const Style: TFontStyle; const Include: Boolean);
  begin
    if Assigned(Canvas) then
      if Include then
        Canvas.Font.Style := Canvas.Font.Style + [Style]
      else
        Canvas.Font.Style := Canvas.Font.Style - [Style];
  end;

  function CalcPos(const Str: string): Integer;
  begin
    Result := 0;

    case Alignment of
      taRightJustify:
        Result := (Rect.Right - Rect.Left) - HTMLTextWidth(Canvas, Rect, State, Str, Scale);
      taCenter: ;
       // Result := DefaultLeft + ((Rect.Right - Rect.Left) - HTMLTextWidth(Canvas, Rect, State, Str, SuperSubScriptRatio)) div 2;
    else
      Result := DefaultLeft;
    end;

    if Result <= 0 then
      Result := DefaultLeft;
  end;

  procedure NewLine(Always: Boolean = False);
  begin
    if Assigned(Canvas) then
      if Always or (vCount < vStr.Count - 1) then
      begin
        Width := Max(Width, CurLeft);
        CurLeft := Rect.Left;
        CurrTop := CurrTop + CanvasMaxTextHeight(Canvas);
      end;
  end;

  procedure Draw(const M: string);
  var
    Width, Height: Integer;
    R: TRect;
    OriginalFontSize: Integer;
    OutputStrings: TStringList;
    i: Integer;
  begin
    R := Rect;
    Inc(R.Left, CurLeft);
    if Assigned(Canvas) then
    begin
      OutputStrings := TStringList.Create;
      try
        OutputStrings.Delimiter := ' ';
        OutPutStrings.StrictDelimiter := True;
        OutputStrings.DelimitedText := M;

        OriginalFontSize := Canvas.Font.Size;
        try
          if ScriptPosition <> spNormal then
            Canvas.Font.Size := Round(Canvas.Font.Size * SuperSubScriptRatio);

          for i := 0 to pred(OutputStrings.Count) do
          begin
            Width  := Canvas.TextWidth(OutputStrings[i]);
            Height := CanvasMaxTextHeight(Canvas);

            if (CurLeft + Width > Rect.Right) and (i > 0) then
              NewLine(True);

            //if ScriptPosition = spSubscript then
            //  R.Top := R.Bottom - Height - 1;

            if IsLink and not MouseOnLink then
              if (MouseY >= R.Top) and (MouseY <= R.Top + Height) and
                 (MouseX >= R.Left) and (MouseX <= R.Left + Width) and
                 ((MouseY > 0) or (MouseX > 0)) then
              begin
                MouseOnLink := True;
                Canvas.Font.Color := clRed; // hover link
                LinkName := TempLink;
              end;

            if CalcType = htmlShow then
            begin
              if Trans then
                Canvas.Brush.Style := bsClear; // for transparent

              Canvas.TextOut(CurLeft, CurrTop, OutputStrings[i]);
            end;

            CurLeft := CurLeft + Width + Canvas.TextWidth(' ');
          end;
        finally
          Canvas.Font.Size := OriginalFontSize;
        end;
      finally
        FreeAndNil(OutputStrings);
      end;
    end;
  end;

begin
  // (p3) remove warnings
  OldFontColor := 0;
  OldBrushColor := 0;
  OldBrushStyle := bsClear;
  RemFontSize := 0;
  RemFontColor := 0;
  RemBrushColor := 0;
  OldAlignment := taLeftJustify;
  OldFont := TFont.Create;

  if Canvas <> nil then
  begin
    OldFontStyles := Canvas.Font.Style;
    OldFontColor  := Canvas.Font.Color;
    OldBrushColor := Canvas.Brush.Color;
    OldBrushStyle := Canvas.Brush.Style;
    OldAlignment  := Alignment;
    RemFontColor  := Canvas.Font.Color;
    RemBrushColor := Canvas.Brush.Color;
    RemFontSize   := Canvas.Font.size;
  end;
  try
    Alignment := taLeftJustify;
    IsLink := False;
    MouseOnLink := False;
    vText := Text;
    vStr  := TStringList.Create;
    vStr.Text := HTMLPrepareText(vText);
    LinkName := '';
    TempLink := '';
    ScriptPosition := spNormal;

    Selected := (odSelected in State) or (odDisabled in State);
    Trans := (Canvas.Brush.Style = bsClear) and not selected;

    Width := DefaultLeft;
    CurLeft := Rect.Left;
    CurrTop := Rect.Top;

    vM := '';
    for vCount := 0 to vStr.Count - 1 do
    begin
      vText := vStr[vCount];

      while vText <> '' do
      begin
        vM := HTMLBeforeTag(vText, True);
        vM := StringReplace(vM, '&lt;', cLT, [rfReplaceAll, rfIgnoreCase]); // <--+ this must be here
        vM := StringReplace(vM, '&gt;', cGT, [rfReplaceAll, rfIgnoreCase]); // <--/
        if GetChar(vText, 1) = cTagBegin then
        begin
          if vM <> '' then
            Draw(vM);

          if Pos(cTagEnd, vText) = 0 then
            Insert(cTagEnd, vText, 2);
          if GetChar(vText, 2) = '/' then
          begin
            case GetChar(vText, 3, True) of
              'A':
                begin
                  IsLink := False;
                  Canvas.Font.Assign(OldFont);
                end;
              'B':
                Style(fsBold, False);
              'I':
                Style(fsItalic, False);
              'U':
                Style(fsUnderline, False);
              'S':
                begin
                  ScriptPosition := spNormal;
                  Style(fsStrikeOut, False);
                end;
              'F':
                begin
                  if not Selected then // restore old colors
                  begin
                    Canvas.Font.Color := RemFontColor;
                    Canvas.Brush.Color := RemBrushColor;
                    Canvas.Font.Size := RemFontSize;
                    Trans := True;
                  end;
                end;
            end
          end
          else
          begin
            case GetChar(vText, 2, True) of
              'A':
                begin
                  if GetChar(vText, 3, True) = 'L' then // ALIGN
                  begin
                    TagPrp := UpperCase(Copy(vText, 2, Pos(cTagEnd, vText) - 2));
                    if Pos(cCENTER, TagPrp) > 0 then
                      Alignment := taCenter
                    else
                    if Pos(cRIGHT, TagPrp) > 0 then
                      Alignment := taRightJustify
                    else
                      Alignment := taLeftJustify;
                    CurLeft := DefaultLeft;
                    if CalcType in [htmlShow, htmlHyperLink] then
                      CurLeft := CalcPos(vText);
                  end
                  else
                  begin   // A HREF
                    TagPrp := Copy(vText, 2, Pos(cTagEnd, vText) - 2);
                    if Pos(cHREF, UpperCase(TagPrp)) > 0 then
                    begin
                      IsLink := True;
                      OldFont.Assign(Canvas.Font);
                      if not Selected then
                        Canvas.Font.Color := clBlue;
                      TempLink := ExtractPropertyValue(TagPrp, cHREF);
                    end;
                  end;
                end;
              'B':
                Style(fsBold, True);
              'I':
                if GetChar(vText, 3, True) = 'N' then //IND="%d"
                begin
                  TagPrp := Copy(vText, 2, Pos(cTagEnd, vText) - 2);
                  CurLeft := StrToInt(ExtractPropertyValue(TagPrp, cIND)); // ex IND="10"
                  if odReserved1 in State then
                    CurLeft := Round((CurLeft * Scale) div 100);
                end
                else
                  Style(fsItalic, True); // ITALIC
              'U':
                Style(fsUnderline, True);
              'S':
                begin
                  if GetChar(vText, 4, True) = 'P' then
                  begin
                    ScriptPosition := spSuperscript;
                  end
                  else if GetChar(vText, 4, True) = 'B' then
                  begin
                    ScriptPosition := spSubscript;
                  end
                  else
                  begin
                    ScriptPosition := spNormal;
                    Style(fsStrikeOut, True);
                  end;
                end;
              'H':
                if (GetChar(vText, 3, True) = 'R') and Assigned(Canvas) then // HR
                begin
                  if odDisabled in State then // only when disabled
                    Canvas.Pen.Color := Canvas.Font.Color;
                  OldWidth := Canvas.Pen.Width;
                  TagPrp := UpperCase(Copy(vText, 2, Pos(cTagEnd, vText)-2));
                  Canvas.Pen.Width := StrToIntDef(ExtractPropertyValue(TagPrp, 'SIZE'), 1); // ex HR="10"
                  if odReserved1 in State then
                    Canvas.Pen.Width := Round((Canvas.Pen.Width * Scale) div 100);
                  if CalcType = htmlShow then
                  begin
                    Canvas.MoveTo(Rect.Left, Rect.Top + CanvasMaxTextHeight(Canvas));
                    Canvas.LineTo(Rect.Right, Rect.Top + CanvasMaxTextHeight(Canvas));
                  end;
                  Rect.Top := Rect.Top + 1 + Canvas.Pen.Width;
                  Canvas.Pen.Width := OldWidth;
                  NewLine(HTMLDeleteTag(vText) <> '');
                end;
              'F':
                if (Pos(cTagEnd, vText) > 0) and (not Selected) and Assigned(Canvas) {and (CalcType in [htmlShow, htmlHyperLink])} then // F from FONT
                begin
                  TagPrp := UpperCase(Copy(vText, 2, Pos(cTagEnd, vText) - 2));
                  RemFontColor := Canvas.Font.Color;
                  RemBrushColor := Canvas.Brush.Color;

                  if Pos(cCOLOR, TagPrp) > 0 then
                  begin
                    Prp := ExtractPropertyValue(TagPrp, cCOLOR);
                    if Prp[1] = '#' then
                      Prp[1] := '$';
                    Canvas.Font.Color := StringToColor(Prp);
                  end;
                  if Pos(cBGCOLOR, TagPrp) > 0 then
                  begin
                    Prp := ExtractPropertyValue(TagPrp, cBGCOLOR);
                    if Prp[1] = '#' then
                      Prp[1] := '$';
                    if UpperCase(Prp) = 'CLNONE' then
                      Trans := True
                    else
                    begin
                      Canvas.Brush.Color := StringToColor(Prp);
                      Trans := False;
                    end;
                  end;
                  if Pos('SIZE', TagPrp) > 0 then
                  begin
                    Prp := ExtractPropertyValue(TagPrp, 'SIZE');
                    Canvas.Font.Size := StrToIntDef(Prp,2) * Canvas.Font.Size div 2;
                  end;
                end;
            end;
          end;
          vText := HTMLDeleteTag(vText);
          vM := '';
        end;
      end;
      if vM <> '' then
        Draw(vM);
      NewLine;
      vM := '';
    end;
  finally
    if Canvas <> nil then
    begin
      Canvas.Font.Style := OldFontStyles;
      Canvas.Font.Color := OldFontColor;
      Canvas.Brush.Color := OldBrushColor;
      Canvas.Brush.Style := OldBrushStyle;
      Alignment := OldAlignment;
  {    Canvas.Font.Color := RemFontColor;
      Canvas.Brush.Color:= RemBrushColor;}
    end;
    FreeAndNil(vStr);
    FreeAndNil(OldFont);
  end;
  Width := Max(Width, CurLeft - DefaultLeft);
  Height := CurrTop + CanvasMaxTextHeight(Canvas);
end;


end.

