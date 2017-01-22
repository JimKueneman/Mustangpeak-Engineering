unit lcc.transfer.nodeidmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, lcc.types, mustangpeak.threadedcirculararray,
  lcc.message;

type

  { TLccNodeIdMapping }

  TLccNodeIdMapping = class
  private
    FAliasID: Word;
    FNodeID: TNodeID;
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property AliasID: Word read FAliasID write FAliasID;
  end;

  TLccNodeIdMap = class
  private
    FMappingList: TThreadedCirularArrayObject;
    FSendLaterQueue: TThreadedCirularArrayObject;
  protected
    property MappingList: TThreadedCirularArrayObject read FMappingList write FMappingList;
    property SendLaterQueue: TThreadedCirularArrayObject read FSendLaterQueue write FSendLaterQueue;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function FindByAlias(TestAlias: Word): TLccNodeIdMapping;
    function FindByNodeID(TestNodeID: TNodeID): TLccNodeIdMapping;
    function ExtractAliasAndUpdateMessage(LccMessage: TLccMessage): Boolean;
    procedure Add(Mapping: TLccNodeIdMapping);
    procedure Clear;
    procedure Remove(Mapping: TLccNodeIdMapping);

    function MapAliasToMessage(LccMessage: TLccMessage): Boolean;
    procedure AddToSendOnceMapped(LccMessage: TLccMessage);
  end;

implementation

{ TLccNodeIdMap }

constructor TLccNodeIdMap.Create;
begin
  inherited Create;
  FMappingList := TThreadedCirularArrayObject.Create;
  SendLaterQueue := TThreadedCirularArrayObject.Create;
  MappingList.OwnsObjects := True;
end;

procedure TLccNodeIdMap.Add(Mapping: TLccNodeIdMapping);
begin
  MappingList.LockArray;
  try
    MappingList.Add(Mapping);
  finally
    MappingList.UnLockArray;
  end;
end;

procedure TLccNodeIdMap.AddToSendOnceMapped(LccMessage: TLccMessage);
begin

end;

procedure TLccNodeIdMap.Clear;
begin
  MappingList.LockArray;
  try
    MappingList.Clear;
  finally
    MappingList.UnLockArray;
  end;
end;

destructor TLccNodeIdMap.Destroy;
begin
  FreeAndNil(FMappingList);
  FreeAndNil(FSendLaterQueue);
  inherited Destroy;
end;

function TLccNodeIdMap.ExtractAliasAndUpdateMessage(LccMessage: TLccMessage): Boolean;
begin
  Result := False;
end;

function TLccNodeIdMap.FindByAlias(TestAlias: Word): TLccNodeIdMapping;
var
  Mapping: TLccNodeIdMapping;
begin
  Result := nil;
  MappingList.LockArray;
  try
    Mapping := MappingList.FirstObject as TLccNodeIdMapping;
    while Assigned(Mapping) do
    begin
      if TestAlias = Mapping.AliasID then
      begin
        Result := Mapping;
        Break
      end;
      Mapping := MappingList.NextObject as TLccNodeIdMapping;
    end;
  finally
    MappingList.UnLockArray;
  end;
end;

function TLccNodeIdMap.FindByNodeID(TestNodeID: TNodeID): TLccNodeIdMapping;
var
  Mapping: TLccNodeIdMapping;
begin
  Result := nil;
  MappingList.LockArray;
  try
    Mapping := MappingList.FirstObject as TLccNodeIdMapping;
    while Assigned(Mapping) do
    begin
      if (TestNodeID[0] = Mapping.NodeID[0]) and (TestNodeID[1] = Mapping.NodeID[1]) then
      begin
        Result := Mapping;
        Break
      end;
      Mapping := MappingList.NextObject as TLccNodeIdMapping;
    end;
  finally
    MappingList.UnLockArray;
  end;
end;

function TLccNodeIdMap.MapAliasToMessage(LccMessage: TLccMessage): Boolean;
var
  LccMapping: TLccNodeIdMapping;
begin
  Result := False;
  LccMapping := FindByNodeID(LccMessage.DestID);
  if Assigned(LccMapping) then
  begin
    LccMessage.CAN.DestAlias := LccMapping.AliasID;
    LccMapping := FindByNodeID(LccMessage.SourceID);
    if Assigned(LccMapping) then
    begin
      LccMessage.CAN.DestAlias := LccMapping.AliasID;
      LccMessage.CAN.MTI := LccMessage.MTI;
      LccMessage.CAN.Active := True;
      Result := True;
    end;
  end;
end;

procedure TLccNodeIdMap.Remove(Mapping: TLccNodeIdMapping);
begin
  MappingList.LockArray;
  try
    MappingList.Remove(Mapping);
  finally
    MappingList.UnLockArray;
  end;
end;

end.

