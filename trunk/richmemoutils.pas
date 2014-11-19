{
 richmemoutils.pas

 Author: Dmitry 'skalogryz' Boyarintsev

 *****************************************************************************
 *                                                                           *
 *  This file is part of the Lazarus Component Library (LCL)                 *
 *                                                                           *
 *  See the file COPYING.modifiedLGPL.txt, included in this distribution,    *
 *  for details about the copyright.                                         *
 *                                                                           *
 *  This program is distributed in the hope that it will be useful,          *
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of           *
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     *
 *                                                                           *
 *****************************************************************************
}
unit RichMemoUtils;

interface

{$mode objfpc}{$h+}

uses
  Types, RichMemo;

const
  NoResize : TSize = ( cx: 0; cy : 0 );

type
  TInsertOptions = set of (ioBelowBaseLine);
var
  { Disclaimer: the function would insert an image file into RichMemo
    (if implemented by the widgetset) But in a very inefficient way.
    The image would be read again and the memory would be re-allocated for
    the image every time. So please, don't use it for smileys in
    your chat instant messaging.  A better API (with data caching) is considered.
    (That's why this method is not part of TCustomRichMemo class)

    APos - position in the text
    AImgSize - size to be inserted (in POINTS, not pixels!).
        if both width and height are 0, the image would not be resized at all.
    }
  InsertImageFromFile : function (const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string;
     const Options : TInsertOptions;
     const AImgSize: TSize
  ): Boolean = nil;

function InsertImageFromFileNoResize (const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string;
     const Options : TInsertOptions): Boolean;

implementation

function InsertImageFileDummy(const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string;
     const Options: TInsertOptions;
     const AImgSize: TSize): Boolean;
begin
  Result:=false;
end;

function InsertImageFromFileNoResize (const ARichMemo: TCustomRichMemo; APos: Integer;
     const FileNameUTF8: string;
     const Options : TInsertOptions): Boolean;
begin
  Result:=InsertImageFromFile(ARichMemo, APos, FileNameUTF8, Options, NoResize);
end;

initialization
  if not Assigned(InsertImageFromFile)  then
    InsertImageFromFile := @InsertImageFileDummy;

end.
