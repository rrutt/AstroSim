unit AstroSimMainForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  AstroSimSpace;

type

  { TAstroSimMainForm }

  TAstroSimMainForm = class(TForm)
    ButtonStep: TButton;
    ButtonResume: TButton;
    ButtonPause: TButton;
    ButtonStart: TButton;
    ButtonRandomize: TButton;
    procedure ButtonRandomizeClick(Sender: TObject);
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
  const
    BORDER_SIZE = 10;
  begin
    Space := TAstroSimSpace.Create(Self);
    Space.Top := BORDER_SIZE;
    Space.Left := BORDER_SIZE;
    Space.Width := Self.Width - (2 * BORDER_SIZE);
    Space.Height := ButtonRandomize.Top - (2 * BORDER_SIZE);
    Space.Parent := Self;
    Space.Initialize;
    Space.DoubleBuffered := True;

    Space.Paint;
  end;

procedure TAstroSimMainForm.ButtonRandomizeClick(Sender: TObject);
begin
  ButtonRandomize.Enabled := false;

  Space.Randomize;
  Space.Paint;

  ButtonStart.Enabled := true;
  ButtonStep.Enabled := true;
end;

end.

