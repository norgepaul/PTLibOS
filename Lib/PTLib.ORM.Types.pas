{********************************************************************}
{                                                                    }
{           PTLib Library                                            }
{           PTLib.ORM.Types                                          }
{                                                                    }
{           Copyright (c) 1998-2016 Easy-IP AS.                      }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF EASY-IP AS. THE REGISTERED DEVELOPER                  }
{   LICENSED TO DISTRIBUTE THE CODE HEREIN AND ANY ACCOMPANYING      }
{   VCL OR FMX CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.       }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM EASY-IP AS.                                  }
{                                                                    }
{********************************************************************}

unit PTLib.ORM.Types;

interface

const
  // ---------------------------------
  // Table Attributes
  // ---------------------------------

  // Records from tables with the SoftDelete attribute will not be removed from
  // the database when DeleteObject<T>(..) is called. Instead, the ORM attributes
  // will be scanned for OnDelete and the appropriate records values will be
  // set. e.g. [OnDelete=CurrentTimestampUTC] will set the field value to the
  // current UTC datetime when a soft delete is performed,
  AttributeSoftDelete = 'SoftDelete';

  // Sets a specific cache expiry time in ms for the table objects
  AttributeCacheExpireTicks = 'CacheExpire';

  // Specifies that the object should not be cached
  AttributeDoNotCache = 'DoNotCache';

  // Specifies that the object should always be cached
  AttributeAlwaystCache = 'AlwaysCache';

  // A list of class types that will be in invalidate when an object of this type
  // is updated in the database e.g.
  // [InvalidateCacheClasses=TParentClass,TAnotherClass]
  // If the listed class has a link to this class, only the instances that are
  // linked will be invalidate, otherwise all the classes of that type
  // will be invalidated
  AttributeInvaidateCacheClasses = 'InvalidateCacheClasses';

  // A table that only implements a partial number of table fields
  AttributePartialTable = 'PartialTable';


  // ---------------------------------
  // Field Attributes
  // ---------------------------------

  // Primary key field
  AttributePrimaryKey = 'PrimaryKey';

  // Set field value on insert e.g. [OnInsert=0] or [OnInsert=CurrentTimestamp]
  AttributeOnInsert = 'OnInsert';

  // Set field value on update e.g. [OnUpdate=Updated] or [OnUpdate=CurrentTimestamp]
  AttributeOnUpdate = 'OnUpdate';

  // Set field value on soft delete e.g. [OnSoftDelete=Deleted] or
  // [OnSoftDelete=CurrentTimestamp]. Only applicable if the table has
  // the SoftDelete attribute. The value will be set to Null when a
  // record is UnDeleted
  AttributeOnSoftDelete = 'OnSoftDelete';

  // The field will never be loaded from the database
  AttributeNoLoad = 'NoLoad';

  // The field value will never be stored to the database
  AttributeNoStore = 'NoStore';

  // Convert a particular property value to Null during load/save operations.
  // e.g. [Null=0] will read a zero value as null and write a 0 value as null.
  AttributeNull = 'Null';

  // Convert a particular property value Non Null during load/save operations.
  // e.g. [NotNull=10] will read a value of 10 if the real value is not null
  AttributeNotNull = 'NotNull';

  // Using the GridNode attribute will stop a field being selected if
  // ProcessGridAttributes appears in the options
  AttributeGridNone = 'GridNone';

  // Identified the fact that the field is automatically generated as a
  // sequence. e.g. [Sequence=GENERATOR_NAME]
  AttributeSequence = 'Sequence';

  // Indicates that a field has a one to one relationship with the object.
  // e.g. [OneToOne=FIELD_NAME_ID]
  AttributeOneToOne = 'OneToOne';

  // Indicates that a field has a one to many relationship with the object.
  // e.g. [OneToMany=FIELD_NAME_ID]
  AttributeOneToMany = 'OneToMany';

  // Used with the OneToMany to indicate the type of class contained in the
  // list property
  AttributeArrayClassType = 'ArrayClassType';

  // When present this attribute will add an additional field with the
  // localised version of a UTC timestamp e.g.
  // [GridLocalTimestampField=CREATED]
  AttributeGridLocalTimestampField = 'GridLocalTimestampField';


  // ---------------------------------
  // Macros
  // ---------------------------------

  // Replaces an attribute value with the current date/time e.g.
  // [OnInsert=CurrentTimestamp]
  MacroCurrentTimestamp = 'CurrentTimestamp';

  // Replaces an attribute value with the current UTC date/time e.g.
  // [OnInsert=CurrentTimestampUTC]
  MacroCurrentTimestampUTC = 'CurrentTimestampUTC';


  // ---------------------------------
  // Internal use
  // ---------------------------------
  PropertyTypeBoolean = 'boolean';
  StrNull = 'Null';
  JSONItems = 'Items';

type
  TCommandExecutType = (
    ceNone,
    ceExecute,
    ceOpen
  );

  TDatasetStreamType = (
    dssAuto,
    dssXML,
    dssBinary,
    dssJSON
  );

  TDatasetStoreItem = (
    diMeta,
    diData,
    diDelta,
    diVisible
  );

  TDatasetStoreItems = set of TDatasetStoreItem;

  TPrimaryKeyArray = Array of Variant;

  TSQLMacroDateUnit = (
    duMilliSecond,
    duSecond,
    duMinute,
    duHour,
    duDay,
    duMonth,
    duYear
  );

  TSQLTransactionIsolation = (
    tiUnspecified,
    tiDirtyRead,
    tiReadCommitted,
    tiRepeatableRead,
    tiSnapshot,
    tiSerializible
  );

  TSQLValueType = (
    vtDefault,
    vtParameter,
    vtValue,
    vtFieldReference,
    vtSQLStatement
  );

  TGeneratedFieldListType = (
    gftFieldNames,
    gftInsertParams,
    gftUpdateFieldsAndParams
  );

  TIncludeDeleted = (
    idDefault,
    idYes,
    idNo
  );

  TORMSessionOption = (
    ManualTransactions,
    NoCache
  );
  TORMSessionOptions = set of TORMSessionOption;

  TORMOption = (
    NoCacheLoad,
    NoCacheSave,
    NoException,
    NoProcessChildClasses,
    NoProcessParentClasses,
    NoDeleted,
    ProcessGridAttributes,
    FirstOnly
  );
  TORMOptions = set of TORMOption;

  TORMSessionEndBehaviour = (
    Rollback,
    Commit
  );

  TSQLRelationalOperator = (
    EqualTo,
    NotEqualTo,
    LessThan,
    GreaterThan,
    LessThanOrEqualTo,
    GreaterThanOrEqualTo,
    Like,
    Containing,
    &In,
    &Is,
    IsNot,
    NotIn,
    SimilarTo
  );

  TSQLOperatorExpression = (
    None,
    &And,
    &Or
  );

  TSQLOrderByDirection = (
    Ascending,
    Descending
  );

  TSQLTableJoin = (
    InnerJoin,
    LeftJoin,
    RightJoin,
    FullJoin
  );

  TPagingType = (
    poNone,
    poPaged,
    poPagedReturnTotalCount
  );

implementation

end.
