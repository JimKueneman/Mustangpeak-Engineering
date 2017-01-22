unit lcc.transfer.nodeidmap;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, contnrs, lcc.types, mustangpeak.threadedcirculararray;

type

  { TNodeIdMapping }

  TNodeIdMapping = class
  private
    FAliasID: Word;
    FNodeID: TNodeID;
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property AliasID: Word read FAliasID write FAliasID;
  end;

  TNodeIdMap = class
  private
    FMappingList: TThreadedCirularArrayObject;
  protected
    property MappingList: TThreadedCirularArrayObject read FMappingList write FMappingList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function FindByAlias(TestAlias: Word): TNodeIdMapping;
    function FindByNodeID(TestNodeID: TNodeID): TNodeIdMapping;
    procedure Add(Mapping: TNodeIdMapping);
    procedure Clear;
    procedure Remove(Mapping: TNodeIDMapping);
  end;

implementation

{ TNodeIdMap }

constructor TNodeIdMap.Create;
begin
  inherited Create;
  FMappingList := TThreadedCirularArrayObject.Create;
  MappingList.OwnsObjects := True;
end;

procedure TNodeIdMap.Add(Mapping: TNodeIdMapping);
begin
  MappingList.LockArray;
  try
    MappingList.Add(Mapping);
  finally
    MappingList.UnLockArray;
  end;
end;

procedure TNodeIdMap.Clear;
begin
  MappingList.LockArray;
  try
    MappingList.Clear;
  finally
    MappingList.UnLockArray;
  end;
end;

destructor TNodeIdMap.Destroy;
begin
  FreeAndNil(FMappingList);
  inherited Destroy;
end;

function TNodeIdMap.FindByAlias(TestAlias: Word): TNodeIdMapping;
var
  Mapping: TNodeIdMapping;
begin
  Result := nil;
  MappingList.LockArray;
  try
    Mapping := MappingList.FirstObject as TNodeIdMapping;
    while Assigned(Mapping) do
    begin
      if TestAlias = Mapping.AliasID then
      begin
        Result := Mapping;
        Break
      end;
      Mapping := MappingList.NextObject as TNodeIdMapping;
    end;
  finally
    MappingList.UnLockArray;
  end;
end;

function TNodeIdMap.FindByNodeID(TestNodeID: TNodeID): TNodeIdMapping;
var
  Mapping: TNodeIdMapping;
begin
  Result := nil;
  MappingList.LockArray;
  try
    Mapping := MappingList.FirstObject as TNodeIdMapping;
    while Assigned(Mapping) do
    begin
      if (TestNodeID[0] = Mapping.NodeID[0]) and (TestNodeID[1] = Mapping.NodeID[1]) then
      begin
        Result := Mapping;
        Break
      end;
      Mapping := MappingList.NextObject as TNodeIdMapping;
    end;
  finally
    MappingList.UnLockArray;
  end;
end;

procedure TNodeIdMap.Remove(Mapping: TNodeIDMapping);
begin
  MappingList.LockArray;
  try
    MappingList.Remove(Mapping);
  finally
    MappingList.UnLockArray;
  end;
end;

end.

