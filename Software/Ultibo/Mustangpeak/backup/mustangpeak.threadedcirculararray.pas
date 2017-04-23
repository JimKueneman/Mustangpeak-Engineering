unit mustangpeak.threadedcirculararray;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ..\Lcc\lcc_compilers.inc}

uses
  Classes,
  {$IFDEF FPC}
  mustangpeak.classes,
  {$ELSE}
  System.SyncObjs,
  {$ENDIF}
  SysUtils;

type

  TDynamicByteArrayByte = array of Byte;
  TDynamicArrayObject = array of TObject;
  TDynamicArrayInterface = array of IUnknown;

  TObjectClass = class of TObject;

  { TThreadedListBase }

  TThreadedListBase = class
  private
    FLock: TCriticalSection;
    FOnAdd: TNotifyEvent;
    FOnRemove: TNotifyEvent;
  protected
    property Lock: TCriticalSection read FLock write FLock;
    procedure DoAdd; virtual;
    procedure DoRemove; virtual;
  public
    property OnAdd: TNotifyEvent read FOnAdd write FOnAdd;
    property OnRemove: TNotifyEvent read FOnRemove write FOnRemove;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure LockArray;
    procedure UnLockArray;
  end;

  { TThreadedCirularArrayByte }

  TThreadedCirularArrayByte = class(TThreadedListBase)
  private
    FCircularArray: TDynamicByteArrayByte;
    FCount: Word;
    FHead: Word;
    FSize: Integer;
    FTail: Word;
    procedure SetSize(AValue: Integer);
  protected
    property CircularArray: TDynamicByteArrayByte read FCircularArray write FCircularArray;
    property Head: Word read FHead write FHead;
    property Tail: Word read FTail write FTail;
  public
    property Count: Word read FCount;
    property Size: Integer read FSize write SetSize default 128;
    constructor Create; override;
    procedure AddChunk(AChunk: TDynamicByteArrayByte);
    procedure RemoveChunk(var AChunk: TDynamicByteArrayByte);
  end;

  { TThreadedCirularArrayObject }

  TThreadedCirularArrayObject = class(TThreadedListBase)
  private
    FCircularArray: TDynamicArrayObject;
    FCount: Word;
    FEnumeratorIndex: Integer;
    FHead: Word;
    FOwnsObjects: Boolean;
    FSize: Integer;
    FTail: Word;
    procedure SetSize(AValue: Integer);
  protected
    property CircularArray: TDynamicArrayObject read FCircularArray write FCircularArray;
    property EnumeratorIndex: Integer read FEnumeratorIndex write FEnumeratorIndex;
    property Head: Word read FHead write FHead;
    property Tail: Word read FTail write FTail;
  public
    property Count: Word read FCount;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
    property Size: Integer read FSize write SetSize default 128;

    constructor Create; override;
    destructor Destroy; override;
    procedure Add(AnItem: TObject);
    procedure AddChunk(AChunk: TDynamicArrayObject);
    procedure Clear;
    function FirstObject: TObject;
    function NextObject: TObject;
    procedure Remove(var AnItem: TObject);
    procedure RemoveChunk(var AChunk: TDynamicArrayObject);
  end;

   { TThreadedCirularArrayInterface }

   TThreadedCirularArrayInterface = class(TThreadedListBase)
  private
    FCircularArray: TDynamicArrayInterface;
    FCount: Word;
    FHead: Word;
    FSize: Integer;
    FTail: Word;
    procedure SetSize(AValue: Integer);
  protected
    property CircularArray: TDynamicArrayInterface read FCircularArray write FCircularArray;
    property Head: Word read FHead write FHead;
    property Tail: Word read FTail write FTail;
  public
    property Count: Word read FCount;
    property Size: Integer read FSize write SetSize default 128;
    constructor Create; override;
    destructor Destroy; override;
    procedure Add(AnItem: IUnknown);
    procedure AddChunk(AChunk: TDynamicArrayInterface);
    procedure Clear;
    procedure Remove(var AnItem: IUnknown);
    procedure RemoveChunk(var AChunk: TDynamicArrayInterface);
  end;

   procedure FreeDynamicArrayObjects(var DynamicArray: TDynamicArrayObject);

implementation

procedure FreeDynamicArrayObjects(var DynamicArray: TDynamicArrayObject);
var
  i: Integer;
begin
  for i := 0 to Length(DynamicArray) - 1 do
    FreeAndNil( DynamicArray[i]);
  DynamicArray := nil;
end;

{ TThreadedCirularArrayInterface }

constructor TThreadedCirularArrayInterface.Create;
begin
  inherited Create;
  Size := 1;
end;

procedure TThreadedCirularArrayInterface.Add(AnItem: IUnknown);
begin
  if Assigned(AnItem) then
  begin
    LockArray;
    try
       // Do we have room?
       if Size - Count > 0 then
       begin
         FCircularArray[Tail] := AnItem;
         Inc(FTail);
         Inc(FCount);
         if Tail >= Length(FCircularArray) then
           Tail := 0;
       end;
    finally
      UnLockArray;
      DoAdd;
    end;
  end;
end;

procedure TThreadedCirularArrayInterface.AddChunk(AChunk: TDynamicArrayInterface);
var
  i: Integer;
begin
  if Assigned(AChunk) then
  begin
    LockArray;
    try
       // Do we have room?
       if Size - Count > Length(AChunk) - 1 then
       begin
         for i := 0 to Length(AChunk) - 1 do
         begin
           FCircularArray[Tail] := AChunk[i];
           Inc(FTail);
           Inc(FCount);
           if Tail >= Length(FCircularArray) then
             Tail := 0;
         end
       end;
      finally
      UnLockArray;
      DoAdd;
    end;
  end;
end;

procedure TThreadedCirularArrayInterface.Clear;
var
  i: Integer;
begin
  LockArray;
  try
    for i := 0 to Size - 1 do
      CircularArray[i] := nil;
    FCount := 0;
  finally
    UnLockArray;
    DoRemove;
  end;
end;

destructor TThreadedCirularArrayInterface.Destroy;
begin
  inherited Destroy;
end;

procedure TThreadedCirularArrayInterface.Remove(var AnItem: IUnknown);
var
  i: Integer;
begin
  AnItem := nil;
  LockArray;
  try
     if Count > 0 then
     begin
       AnItem := CircularArray[Head];
       Inc(FHead);
       Dec(FCount);
       if Head >= Length(FCircularArray) then
         Head := 0;
     end;
  finally
    UnLockArray;
    DoRemove;
  end;
end;

procedure TThreadedCirularArrayInterface.RemoveChunk(var AChunk: TDynamicArrayInterface);
var
  i: Integer;
  LocalCount: Word;
begin
  AChunk := nil;
  LockArray;
  try
     if Count > 0 then
     begin
       LocalCount := Count;
       SetLength(AChunk, Count);
       for i := 0 to LocalCount - 1 do
       begin
         AChunk[i] := CircularArray[Head];
         CircularArray[Head] := nil;   // Release the interface
         Inc(FHead);
         Dec(FCount);
         if Head >= Length(FCircularArray) then
           Head := 0;
       end
     end;
  finally
    UnLockArray;
    DoRemove;
  end;
end;

procedure TThreadedCirularArrayInterface.SetSize(AValue: Integer);
begin
  if FSize = AValue then Exit;
  try
    LockArray;
    SetLength(FCircularArray, 0);  // Frees all the objects
    FSize := AValue;
    SetLength(FCircularArray, Size);
  finally
    UnLockArray
  end;
end;

{ TThreadedCirularArrayObject }

constructor TThreadedCirularArrayObject.Create;
begin
  inherited Create;
  Size := 128;  // Default
end;

procedure TThreadedCirularArrayObject.Add(AnItem: TObject);
begin
  if Assigned(AnItem) then
  begin
    LockArray;
    try
       // Do we have room?
       if Size - Count > 0 then
       begin
         FCircularArray[Tail] := AnItem;
         Inc(FTail);
         Inc(FCount);
         if Tail >= Length(FCircularArray) then
           Tail := 0;
       end;
    finally
      DoAdd;
      UnLockArray;
    end;
  end;
end;

procedure TThreadedCirularArrayObject.AddChunk(AChunk: TDynamicArrayObject);
var
  i: Integer;
begin
  if Assigned(AChunk) then
  begin
    LockArray;
    try
       // Do we have room?
       if Size - Count > Length(AChunk) - 1 then
       begin
         for i := 0 to Length(AChunk) - 1 do
         begin
           FCircularArray[Tail] := AChunk[i];
           Inc(FTail);
           Inc(FCount);
           if Tail >= Length(FCircularArray) then
             Tail := 0;
         end;
       end;
    finally
      UnLockArray;
      DoAdd;
    end;
  end;
end;

procedure TThreadedCirularArrayObject.Clear;
var
  i: Integer;
begin
  if OwnsObjects then
  begin
    LockArray;
    try
      for i := 0 to Count - 1 do
        FreeAndNil(FCircularArray[i]);
      FHead := 0;
      FTail := 0;;
    finally
      UnLockArray;
      DoRemove;
    end;
  end;
end;

destructor TThreadedCirularArrayObject.Destroy;
begin
  Clear;
  inherited Destroy;
end;

function TThreadedCirularArrayObject.FirstObject: TObject;
begin
  Result := nil;
  EnumeratorIndex := Head;
  if Count > 0 then
    Result := CircularArray[EnumeratorIndex];
end;

function TThreadedCirularArrayObject.NextObject: TObject;
begin
  Result := nil;
  Inc(FEnumeratorIndex);
  if EnumeratorIndex > Size then
    EnumeratorIndex := 0;
  if EnumeratorIndex < Tail then
    Result := CircularArray[EnumeratorIndex];
end;

procedure TThreadedCirularArrayObject.Remove(var AnItem: TObject);
begin
  AnItem := nil;
  LockArray;
  try
     if Count > 0 then
     begin
       AnItem := CircularArray[Head];
       Inc(FHead);
       Dec(FCount);
       if Head >= Length(FCircularArray) then
         Head := 0;
     end
    finally
    UnLockArray;
  end;
end;

procedure TThreadedCirularArrayObject.RemoveChunk(var AChunk: TDynamicArrayObject);
var
  i: Integer;
  LocalCount: Word;
begin
  AChunk := nil;
  LockArray;
  try
     if Count > 0 then
     begin
       LocalCount := Count;
       SetLength(AChunk, Count);
       for i := 0 to LocalCount - 1 do
       begin
         AChunk[i] := CircularArray[Head];
         Inc(FHead);
         Dec(FCount);
         if Head >= Length(FCircularArray) then
           Head := 0;
       end
     end;
    finally
    UnLockArray;
  end;
end;

procedure TThreadedCirularArrayObject.SetSize(AValue: Integer);
var
  i: Integer;
begin
  if FSize = AValue then Exit;
  LockArray;
  try
    if OwnsObjects then
    begin
      if AValue < Size then
      begin
        for i := AValue to Size - 1 do
          CircularArray[i].Free;
      end;
    end;
    FSize := AValue;
    SetLength(FCircularArray, Size);
  finally
    UnLockArray
  end;
end;

{ TThreadedListBase }

constructor TThreadedListBase.Create;
begin
  inherited Create;
  FLock := TCriticalSection.Create;
end;

destructor TThreadedListBase.Destroy;
begin
  FreeAndNil(FLock);
  inherited Destroy;
end;

procedure TThreadedListBase.DoAdd;
begin
  if Assigned(OnAdd) then
    OnAdd(Self);
end;

procedure TThreadedListBase.DoRemove;
begin
  if Assigned(OnRemove) then
    OnRemove(Self);
end;

procedure TThreadedListBase.LockArray;
begin
  FLock.Enter;
end;

procedure TThreadedListBase.UnLockArray;
begin
  FLock.Leave
end;

{ TThreadedCirularArrayByte }

constructor TThreadedCirularArrayByte.Create;
begin
  inherited Create;
  Size := 128
end;

procedure TThreadedCirularArrayByte.AddChunk(AChunk: TDynamicByteArrayByte);
var
  i: Integer;
begin
  LockArray;
  try
     // Do we have room?
     if Size - Count > Length(AChunk) - 1 then
     begin
       for i := 0 to Length(AChunk) - 1 do
       begin
         FCircularArray[Tail] := AChunk[i];
         Inc(FTail);
         Inc(FCount);
         if Tail >= Length(FCircularArray) then
           Tail := 0;
       end;
     end;
    finally
    UnLockArray;
    DoAdd;
  end;
end;

procedure TThreadedCirularArrayByte.RemoveChunk(var AChunk: TDynamicByteArrayByte);
var
  i: Integer;
  LocalCount: Word;
begin
  LockArray;
  try
     if Count > 0 then
     begin
       LocalCount := Count;
       SetLength(AChunk, Count);
       for i := 0 to LocalCount - 1 do
       begin
         AChunk[i] := CircularArray[Head];
         Inc(FHead);
         Dec(FCount);
         if Head >= Length(FCircularArray) then
           Head := 0;
       end;
     end;
    finally
    UnLockArray;
    DoRemove;
  end;
end;

procedure TThreadedCirularArrayByte.SetSize(AValue: Integer);
var
  i: Integer;
begin
  if FSize = AValue then Exit;
  LockArray;
  try
    FSize := AValue;
    SetLength(FCircularArray, Size);
  finally
    UnLockArray
  end;
end;

end.

