unit PTLib.Common.APIs.MapQuest;

interface

uses
  SysUtils, Classes, JSON, System.Net.HttpClient,

  PTLib.Common.Interfaces,
  PTLib.Common.Classes,
  PTLib.Common.Utils,
  PTLib.Common.APIs,
  PTLib.Common.JSON;

type
  TMapQuestAPI = class(TPTLibAPI, IMapAPI)
  private
    function GeoCode(const Address: String; out Latitude, Longitude: Double): Integer;
    function ReverseGeoCode(const Latitude, Longitude: Double; out Address: String): Integer;
    function Elevation(const Latitude, Longitude: Double; out Elevation: Integer): Integer;
  end;

function NewMapQuestAPI(const APIKey: String): IMapAPI;

implementation

function NewMapQuestAPI(const APIKey: String): IMapAPI;
begin
  Result := TMapQuestAPI.Create(APIKey);
end;

function TMapQuestAPI.Elevation(const Latitude, Longitude: Double; out Elevation: Integer): Integer;
const
  MapQuestURL = 'http://open.mapquestapi.com/elevation/v1/profile?key=%s&outFormat=json&unit=m&latLngCollection=%s,%s';
var
  JSON: TJSONObject;
begin
  JSON := NewHTTPJSONObject(format(MapQuestURL, [APIKey, FloatToStrUseDot(Latitude), FloatToStrUseDot(Longitude)]));
  try
    Result := FindJSONValue(JSON, 'info.statuscode').Value.ToInteger;

    if Result = 0 then
    begin
      Elevation := FindJSONValue(JSON, 'elevationProfile[0].height').Value.ToInteger;
    end;
  finally
    FreeAndNil(JSON);
  end;
end;

function TMapQuestAPI.GeoCode(const Address: String; out Latitude, Longitude: Double): Integer;
const
  MapQuestURL = 'http://open.mapquestapi.com/geocoding/v1/address?outFormat=json&key=%s&location=%s';
var
  JSON: TJSONObject;
begin
  JSON := NewHTTPJSONObject(format(MapQuestURL, [APIKey, Address]));
  try
    Result := FindJSONValue(JSON, 'info.statuscode').Value.ToInteger;

    if Result = 0 then
    begin
      Latitude := StrToFloatDefUseDot(FindJSONValue(JSON, 'results[0].locations[0].latLng.lat').Value, 0);
      Longitude := StrToFloatDefUseDot(FindJSONValue(JSON, 'results[0].locations[0].latLng.lng').Value, 0);
    end;
  finally
    FreeAndNil(JSON);
  end;
end;

function TMapQuestAPI.ReverseGeoCode(const Latitude, Longitude: Double; out Address: String): Integer;
const
  MapQuestURL = 'http://open.mapquestapi.com/geocoding/v1/reverse?outFormat=json&key=%s&location=%s,%s';
var
  JSON: TJSONObject;
begin
  JSON := NewHTTPJSONObject(format(MapQuestURL, [APIKey, FloatToStrUseDot(Latitude), FloatToStrUseDot(Longitude)]));
  try
    Result := FindJSONValue(JSON, 'info.statuscode').Value.ToInteger;

    if Result = 0 then
    begin
      Address :=
        FindJSONValue(JSON, 'results[0].locations[0].street').Value +
        ', ' +
        FindJSONValue(JSON, 'results[0].locations[0].adminArea5').Value;
    end;
  finally
    FreeAndNil(JSON);
  end;
end;

end.
