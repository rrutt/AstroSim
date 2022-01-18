unit AstroSimMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  AstroSimSpace;

type

  { TAstroSimMainForm }

  TAstroSimMainForm = class(TForm)
    procedure FormCreate(Sender: TObject);
  end;

  var
    MainForm: TAstroSimMainForm;

implementation

{$R *.lfm}

  var
    Space: TAstroSimSpace;
    //ResourceDirectory: UTF8String {$IFNDEF MACOSX} = '../res/' {$ENDIF};

  procedure TAstroSimMainForm.FormCreate(Sender: TObject);
  begin
    Space := TAstroSimSpace.Create(Self);
    Space.Width := Self.Width;
    Space.Height := Self.Height;
    Space.Top := 0;
    Space.Left := 0;
    Space.Parent := Self;
    Space.Initialize;
    Space.DoubleBuffered := True;

    Space.Paint;
  end;
end.

