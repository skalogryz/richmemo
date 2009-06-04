unit RichMemoTypes; 

{$mode objfpc}{$H+}

interface

uses
  Graphics;

type
  TTextStyleMask = set of (tsm_Color, tsm_Name, tsm_Size, tsm_Styles);
  
  TFontParams = record
    Name    : String;
    Size    : Integer;
    Color   : TColor;
    Style   : TFontStyles;  
  end;

implementation

end.

