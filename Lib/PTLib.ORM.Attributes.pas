unit PTLib.ORM.Attributes;

interface

Uses
  System.Classes, System.SysUtils, System.RTTI, System.Generics.Collections, System.Variants;

type
  PTLibORMBaseAttribute = class(TCustomAttribute)
  public
    function GetAttributePropertyName: String; virtual;
  end;

  PTLibORMAttribute = class(PTLibORMBaseAttribute)
  strict private
    FValue: String;
    FIsNull: Boolean;
  private
    function GetAsInt64: Int64;
    function GetAsInteger: Integer;
    function GetAsString: String;
    function GetAsVariant: Variant;
  public
    constructor Create; overload;
    constructor Create(const IsNull: Boolean); overload;
    constructor Create(const Value: String); overload;

    property AsVariant: Variant read GetAsVariant;
    property AsString: String read GetAsString;
    property AsInteger: Integer read GetAsInteger;
    property AsInt64: Int64 read GetAsInt64;
  end;

  PTLibORMTableAttribute = class(PTLibORMAttribute)
  public
    function GetAttributePropertyName: String; override;
  end;

  PTLibORMTableValueAttribute = class(PTLibORMAttribute)
  public
    function GetAttributePropertyName: String; override;
  end;

  PTLibORMFieldAttribute = class(PTLibORMAttribute)
  public
    function GetAttributePropertyName: String; override;
  end;

  PTLibORMFieldValueAttribute = class(PTLibORMAttribute)
  public
    function GetAttributePropertyName: String; override;
  end;

  // ---------------------------------
  // Table Attributes
  // ---------------------------------

  // Set the table name associated with the class
  TableNameAttribute = class(PTLibORMTableValueAttribute);

  // Records from tables with the SoftDelete attribute will not be removed from
  // the database when DeleteObject<T>(..) is called. Instead, the ORM attributes
  // will be scanned for OnDelete and the appropriate records values will be
  // set. e.g. [OnDelete=CurrentTimestampUTC] will set the field value to the
  // current UTC datetime when a soft delete is performed,
  TableSoftDeleteAttribute = class(PTLibORMTableAttribute);

  // Sets a specific cache expiry time in ms for the table objects
  TableRecordCacheExpireTicksAttribute = class(PTLibORMTableAttribute);

  // Specifies that the object should not be cached
  TableDoNotCacheAttribute = class(PTLibORMTableAttribute);

  // Specifies that the object should always be cached
  TableAlwaystCacheAttribute = class(PTLibORMTableAttribute);

  // A list of class types that will be invalidated when an object of this type
  // is updated in the database e.g.
  // [InvalidateCacheClasses=TParentClass,TAnotherClass]
  // If the listed class has a link to this class, only the instances that are
  // linked will be invalidate, otherwise all the classes of that type
  // will be invalidated
  TableInvaidateCacheClassesAttribute = class(PTLibORMTableValueAttribute);

  // A table that only implements a partial number of table fields
  TablePartialTableAttribute = class(PTLibORMTableAttribute);


  // ---------------------------------
  // Field Attributes
  // ---------------------------------

  // Sets the field name associated with the property
  FieldNameAttribute = class(PTLibORMFieldValueAttribute);

  // Primary key field
  FieldPrimaryKeyAttribute = class(PTLibORMFieldAttribute);

  // Set field value on insert e.g. [OnInsert=0] or [OnInsert=CurrentTimestamp]
  FieldOnInsertAttribute = class(PTLibORMFieldValueAttribute);

  // Set field value on update e.g. [OnUpdate=Updated] or [OnUpdate=CurrentTimestamp]
  FieldOnUpdateAttribute = class(PTLibORMFieldValueAttribute);

  // Set field value on soft delete e.g. [OnSoftDelete=Deleted] or
  // [OnSoftDelete=CurrentTimestamp]. Only applicable if the table has
  // the SoftDelete attribute. The value will be set to Null when a
  // record is UnDeleted
  FieldOnSoftDeleteAttribute = class(PTLibORMFieldValueAttribute);

  // The field will never be loaded from the database
  FieldNoLoadAttribute = class(PTLibORMFieldAttribute);

  // The field value will never be stored to the database
  FieldNoStoreAttribute = class(PTLibORMFieldAttribute);

  // Convert a particular property value to Null during load/save operations.
  // e.g. [Null=0] will read a zero value as null and write a 0 value as null.
  FieldNullAttribute = class(PTLibORMFieldValueAttribute);

  // Convert a particular property value Non Null during load/save operations.
  // e.g. [NotNull=10] will read a value of 10 if the real value is not null
  FieldNotNullAttribute = class(PTLibORMFieldValueAttribute);

  // Using the GridNode attribute will stop a field being selected if
  // ProcessGridAttributes appears in the options
  FieldGridNoneAttribute = class(PTLibORMFieldAttribute);

  // Indicates that the field is automatically generated as a
  // sequence. If a generator is required, enter the name, otherwise,
  // leave it empty e.g. [Sequence=GENERATOR_NAME]
  FieldSequenceAttribute = class(PTLibORMFieldValueAttribute);

  // Indicates that a field has a one to one relationship with the object.
  // e.g. [OneToOne=FIELD_NAME_ID]
  FieldOneToOneAttribute = class(PTLibORMFieldValueAttribute);

  // Indicates that a field has a one to many relationship with the object.
  // e.g. [OneToMany=FIELD_NAME_ID]
  FieldOneToManyAttribute = class(PTLibORMFieldValueAttribute);

  // Used with the OneToMany to indicate the type of class contained in the
  // list property
  FieldArrayClassTypeAttribute = class(PTLibORMFieldValueAttribute);

  // When present this attribute will add an additional field with the
  // localised version of a UTC timestamp e.g.
  // [GridLocalTimestampField=CREATED]
  FieldGridLocalTimestampAttribute = class(PTLibORMFieldValueAttribute);


implementation

{ PTLibORMAttribute }

constructor PTLibORMAttribute.Create(const Value: String);
begin
  inherited Create;

  FValue := Value;
end;

constructor PTLibORMAttribute.Create;
begin
  inherited Create;
end;

constructor PTLibORMAttribute.Create(const IsNull: Boolean);
begin
  inherited Create;

  FIsNull := True;
end;

function PTLibORMAttribute.GetAsInt64: Int64;
begin
  Result := AsString.ToInt64;
end;

function PTLibORMAttribute.GetAsInteger: Integer;
begin
  Result := AsString.ToInteger;
end;

function PTLibORMAttribute.GetAsString: String;
begin
  Result := FValue;
end;

function PTLibORMAttribute.GetAsVariant: Variant;
begin
  if FIsNull then
  begin
    Result := Null;
  end
  else
  begin
    Result := FValue;
  end;
end;

{ PTLibORMAttribute }

function PTLibORMBaseAttribute.GetAttributePropertyName: String;
begin
  Result := copy(ClassName, 1, length(ClassName) - length('attribute'));
end;

{ PTLibORMTableAttribute }

function PTLibORMTableAttribute.GetAttributePropertyName: String;
begin
  Result := inherited;

  Result := copy(Result, 6, MaxInt);
end;

{ PTLibORMTableValueAttribute }

function PTLibORMTableValueAttribute.GetAttributePropertyName: String;
begin
  Result := inherited;

  if not (Self is TableNameAttribute) then
  begin
      Result := copy(Result, 6, MaxInt);
  end;
end;

{ PTLibORMFieldValueAttribute }

function PTLibORMFieldValueAttribute.GetAttributePropertyName: String;
begin
  Result := inherited;

  if not (Self is FieldNameAttribute) then
  begin
      Result := copy(Result, 6, MaxInt);
  end;
end;

{ PTLibORMFieldAttribute }

function PTLibORMFieldAttribute.GetAttributePropertyName: String;
begin
  Result := inherited;

    Result := copy(Result, 6, MaxInt);
end;

end.
