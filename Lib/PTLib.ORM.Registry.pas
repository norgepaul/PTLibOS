unit PTLib.ORM.Registry;

interface

uses
  System.SysUtils, System.Classes, System.Rtti, System.Variants, System.TypInfo,

  PTLib.Common.Strings,

  PTLib.ORM.Attributes,
  PTLib.ORM.Classes,
  PTLib.ORM.Interfaces;

type
  TORMClassRegistry = class
  strict private
    class var FTableMappings: TORMTableMappings;
  public
    class constructor Create;
    class destructor Destroy;

    class procedure ResisterORMClass(const AClass: TClass);
    class procedure UnResisterORMClass(const AClass: TClass); overload;

    class function GetTableFieldMappings(const ORMObject: TObject; const RaiseException: Boolean = True): IORMTableMapping; overload;
    class function GetTableFieldMappings(const ORMObjectClassName: String; const RaiseException: Boolean = True): IORMTableMapping; overload;
    class function GetTableFieldMappingsByTableName(const TableName: String; const RaiseException: Boolean = True): IORMTableMapping;

    class property TableMappings: TORMTableMappings read FTableMappings;
  end;

implementation

{ TORMClassRegistry }

class constructor TORMClassRegistry.Create;
begin
  FTableMappings := TORMTableMappings.Create;
end;

class destructor TORMClassRegistry.Destroy;
begin
  FreeAndNil(FTableMappings);
end;

class function TORMClassRegistry.GetTableFieldMappings(
  const ORMObjectClassName: String;
  const RaiseException: Boolean): IORMTableMapping;
begin
  Result := FTableMappings.GetTableFieldMappings(
    ORMObjectClassName,
    RaiseException);
end;

class function TORMClassRegistry.GetTableFieldMappings(const ORMObject: TObject;
  const RaiseException: Boolean): IORMTableMapping;
begin
  Result := FTableMappings.GetTableFieldMappings(
    ORMObject,
    RaiseException);
end;

class function TORMClassRegistry.GetTableFieldMappingsByTableName(
  const TableName: String; const RaiseException: Boolean): IORMTableMapping;
begin
  Result := FTableMappings.GetTableFieldMappingsByTableName(
    TableName,
    RaiseException);
end;

class procedure TORMClassRegistry.ResisterORMClass(const AClass: TClass);

var
  ORMTableMapping: IORMTableMapping;

  procedure RegisterORMClassRec(const RecClass: TClass);
  var
    RttiContext: TRttiContext;
    RttiType: TRttiType;
    RttiProperty: TRttiProperty;
    Attribute: TCustomAttribute;
    FieldMapping: IORMFieldMapping;
  begin
    if RecClass <> nil then
    begin
      RttiContext := TRttiContext.Create;
      RttiType := RttiContext.GetType(RecClass);

      ORMTableMapping := TORMTableMapping.Create;
      ORMTableMapping.ObjectClass := AClass;

      // Get the table name
      for Attribute in RttiType.GetAttributes do
      begin
        // Add the table name
        if Attribute is TableNameAttribute then
        begin
          ORMTableMapping.TableName := TableNameAttribute(Attribute).AsString;
        end;
      end;

      // Get the table attributes
      for Attribute in RttiType.GetAttributes do
      begin
        // Add the table attributes
        if not (Attribute is TableNameAttribute) then
        begin
          if (Attribute is PTLibORMTableAttribute) or
             ((Attribute is PTLibORMTableValueAttribute) and
              (PTLibORMTableValueAttribute(Attribute).AsVariant = Null)) then
          begin
            ORMTableMapping.Attributes.SetAttributeValue(
              PTLibORMFieldAttribute(Attribute).GetAttributePropertyName,
              Null);
          end else

          if Attribute is PTLibORMFieldValueAttribute then
          begin
            ORMTableMapping.Attributes.SetAttributeValue(
                PTLibORMFieldValueAttribute(Attribute).GetAttributePropertyName,
                PTLibORMFieldValueAttribute(Attribute).AsString);
          end;
        end;
      end;

      // Step through the object properties
      for RttiProperty in RttiType.GetProperties do
      begin
        for Attribute in RttiProperty.GetAttributes do
        begin
          // Add the field name
          if Attribute is FieldNameAttribute then
          begin
            FieldMapping := ORMTableMapping.FieldMappings.AddFieldMapping(
              RttiProperty.Name,
              PTLibORMAttribute(Attribute).AsString,
              RttiProperty.PropertyType.Handle);
          end;
        end;

        if FieldMapping = nil then
        begin
          // No field name exception
        end;

        for Attribute in RttiProperty.GetAttributes do
        begin
          // Add the field name
          if Attribute is FieldPrimaryKeyAttribute then
          begin
            ORMTableMapping.PrimaryKeyFieldMappings.AddFieldMapping(FieldMapping);
          end;
        end;

        for Attribute in RttiProperty.GetAttributes do
        begin
          if not (Attribute is FieldNameAttribute) then
          begin
            if (Attribute is PTLibORMFieldAttribute) or
               ((Attribute is PTLibORMFieldValueAttribute) and
                (PTLibORMFieldValueAttribute(Attribute).AsVariant = Null)) then
            begin
              FieldMapping.Attributes.SetAttributeValue(
                PTLibORMFieldAttribute(Attribute).GetAttributePropertyName,
                Null);
            end else
            if Attribute is PTLibORMFieldValueAttribute then
            begin
              FieldMapping.Attributes.SetAttributeValue(
                PTLibORMFieldValueAttribute(Attribute).GetAttributePropertyName,
                PTLibORMFieldValueAttribute(Attribute).AsString);
            end;
          end;
        end;
      end;

      //RegisterORMClassRec(RecClass.ClassParent);
    end;
  end;

begin
  ORMTableMapping := FTableMappings.GetTableFieldMappings(AClass.ClassName, False);

  RegisterORMClassRec(AClass);

  if ORMTableMapping = nil then
  begin
    // Exception
  end
  else
  begin
    FTableMappings.AddTableFieldMappings(ORMTableMapping);
  end;
end;

class procedure TORMClassRegistry.UnResisterORMClass(const AClass: TClass);
begin

end;

end.
