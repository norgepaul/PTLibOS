unit PTLib.Common.Internationalisation;

interface

uses
  SysUtils;

function GetCountryFromCountryCode(const CountryCode: String; const UseCoutnryID: Boolean = False): String;
function GetPhoneNumberLengthFromCountryCode(const CountryCode: String): Integer;
function IsValidCountryCode(const CountryCode: String): Boolean;

implementation

const
  MaxNumberLength = '15';

  CountryDialingCodes: Array[0..211, 0..3] of String = (
    ('Afghanistan', '93', MaxNumberLength, 'CountryAfghanistan'),
    ('Albania', '355', MaxNumberLength, 'CountryAlbania'),
    ('Algeria', '213', MaxNumberLength, 'CountryAlgeria'),
    ('Andorra', '376', MaxNumberLength, 'CountryAndorra'),
    ('Angola', '244', MaxNumberLength, 'CountryAngola'),
    //('Anguilla' , '1', MaxNumberLength),
    ('Antarctica', '672', MaxNumberLength, 'CountryAntarctica'),
    ('Argentina', '54', MaxNumberLength, 'CountryArgentina'),
    ('Armenia', '374', MaxNumberLength, 'CountryArmenia'),
    ('Aruba', '297', MaxNumberLength, 'CountryAruba'),
    ('Australia', '61', MaxNumberLength, 'CountryAustralia'),
    ('Austria', '43', MaxNumberLength, 'CountryAustria'),
    ('Azerbaijan', '994', MaxNumberLength, 'CountryAzerbaijan'),
    ('Bahrain', '973', MaxNumberLength, 'CountryBahrain'),
    ('Bangladesh', '880', MaxNumberLength, 'CountryBangladesh'),
    ('Belarus', '375', MaxNumberLength, 'CountryBelarus'),
    ('Belgium', '32', MaxNumberLength, 'CountryBelgium'),
    ('Belize', '501', MaxNumberLength, 'CountryBelize'),
    ('Benin', '229', MaxNumberLength, 'CountryBenin'),
//    ('Bermuda', '1', MaxNumberLength, 'CountryBermuda'),
    ('Bhutan', '975', MaxNumberLength, 'CountryBhutan'),
    ('Bolivia', '591', MaxNumberLength, 'CountryBolivia'),
    ('Bosnia and Herzegovina', '387', MaxNumberLength, 'CountryBosniaAndHerzegovina'),
    ('Botswana', '267', MaxNumberLength, 'CountryBotswana'),
    ('Brazil', '55', MaxNumberLength, 'CountryBrazil'),
    ('British Indian Ocean Territory', '246', MaxNumberLength, 'CountryBritishIndianOceanTerritory'),
    ('Brunei', '673', MaxNumberLength, 'CountryBrunei'),
    ('Bulgaria', '359', MaxNumberLength, 'CountryBulgaria'),
    ('Burkina Faso', '226', MaxNumberLength, 'CountryBurkinaFaso'),
    ('Burundi', '257', MaxNumberLength, 'CountryBurundi'),
    ('Cambodia', '855', MaxNumberLength, 'CountryCambodia'),
    ('Cameroon', '237', MaxNumberLength, 'CountryCameroon'),
//    ('Canada' , '1', MaxNumberLength, 'CountryCanada''),
    ('Cape Verde', '238', MaxNumberLength, 'CountryCapeVerde'),
    ('Central African Republic', '236', MaxNumberLength, 'CountryCentralAfricanRepublic'),
    ('Chad', '235', MaxNumberLength, 'CountryChad'),
    ('Chile', '56', MaxNumberLength, 'CountryChile'),
    ('China', '86', MaxNumberLength, 'CountryChina'),
    ('Christmas Island', '61', MaxNumberLength, 'CountryChristmasIsland'),
    //('Cocos Islands', '61', MaxNumberLength),
    ('Colombia', '57', MaxNumberLength, 'CountryColombia'),
    ('Comoros', '269', MaxNumberLength, 'CountryComoros'),
    ('Cook Islands', '682', MaxNumberLength, 'CountryCookIslands'),
    ('Costa Rica', '506', MaxNumberLength, 'CountryCostaRica'),
    ('Croatia', '385', MaxNumberLength, 'CountryCroatia'),
    ('Cuba', '53', MaxNumberLength, 'CountryCuba'),
    ('Curacao', '599', MaxNumberLength, 'CountryCuracao'),
    ('Cyprus', '357', MaxNumberLength, 'CountryCyprus'),
    ('Czech Republic', '420', MaxNumberLength, 'CountryCzechRepublic'),
    ('Democratic Republic of the Congo', '243', MaxNumberLength, 'CountryDemocraticRepublicOfTheCongo'),
    ('Denmark', '45', MaxNumberLength, 'CountryDenmark'),
    ('Djibouti', '253', MaxNumberLength, 'CountryDjibouti'),
//    ('Dominica' , '1', MaxNumberLength, 'CountryDominica''),
//    ('Dominican Republic', '1', MaxNumberLength),
    ('East Timor', '670', MaxNumberLength, 'CountryEastTimor'),
    ('Ecuador', '593', MaxNumberLength, 'CountryEcuador'),
    ('Egypt', '20', MaxNumberLength, 'CountryEgypt'),
    ('El Salvador', '503', MaxNumberLength, 'CountryElSalvador'),
    ('Equatorial Guinea', '240', MaxNumberLength, 'CountryEquatorialGuinea'),
    ('Eritrea', '291', MaxNumberLength, 'CountryEritrea'),
    ('Estonia', '372', MaxNumberLength, 'CountryEstonia'),
    ('Ethiopia', '251', MaxNumberLength, 'CountryEthiopia'),
    ('Falkland Islands', '500', MaxNumberLength, 'CountryFalklandIslands'),
    ('Faroe Islands', '298', MaxNumberLength, 'CountryFaroeIslands'),
    ('Fiji', '679', MaxNumberLength, 'CountryFiji'),
    ('Finland', '358', MaxNumberLength, 'CountryFinland'),
    ('France', '33', MaxNumberLength, 'CountryFrance'),
    ('French Polynesia', '689', MaxNumberLength, 'CountryFrenchPolynesia'),
    ('Gabon', '241', MaxNumberLength, 'CountryGabon'),
    ('Gambia', '220', MaxNumberLength, 'CountryGambia'),
    ('Georgia', '995', MaxNumberLength, 'CountryGeorgia'),
    ('Germany', '49', MaxNumberLength, 'CountryGermany'),
    ('Ghana', '233', MaxNumberLength, 'CountryGhana'),
    ('Gibraltar', '350', MaxNumberLength, 'CountryGibraltar'),
    ('Greece', '30', MaxNumberLength, 'CountryGreece'),
    ('Greenland', '299', MaxNumberLength, 'CountryGreenland'),
//    ('Grenada', '1', MaxNumberLength, 'CountryGrenada'),
//    ('Guam' , '1', MaxNumberLength, 'CountryGuam''),
    ('Guatemala', '502', MaxNumberLength, 'CountryGuatemala'),
//    ('Guernsey' , '44', MaxNumberLength, 'CountryGuernsey''),
    ('Guinea', '224', MaxNumberLength, 'CountryGuinea'),
    ('Guinea-Bissau', '245', MaxNumberLength, 'CountryGuineaBissau'),
    ('Guyana', '592', MaxNumberLength, 'CountryGuyana'),
    ('Haiti', '509', MaxNumberLength, 'CountryHaiti'),
    ('Honduras', '504', MaxNumberLength, 'CountryHonduras'),
    ('Hong Kong', '852', MaxNumberLength, 'CountryHongKong'),
    ('Hungary', '36', MaxNumberLength, 'CountryHungary'),
    ('Iceland', '354', MaxNumberLength, 'CountryIceland'),
    ('India', '91', MaxNumberLength, 'CountryIndia'),
    ('Indonesia', '62', MaxNumberLength, 'CountryIndonesia'),
    ('Iran', '98', MaxNumberLength, 'CountryIran'),
    ('Iraq', '964', MaxNumberLength, 'CountryIraq'),
    ('Ireland', '353', MaxNumberLength, 'CountryIreland'),
    ('Israel', '972', MaxNumberLength, 'CountryIsrael'),
    ('Italy', '39', '10', 'CountryItaly'),
    ('Ivory Coast', '225', MaxNumberLength, 'CountryIvoryCoast'),
    ('Japan', '81', MaxNumberLength, 'CountryJapan'),
//    ('Jersey' , '44', MaxNumberLength, 'CountryJersey''),
    ('Jordan', '962', MaxNumberLength, 'CountryJordan'),
    ('Kazakhstan', '7', MaxNumberLength, 'CountryKazakhstan'),
    ('Kenya', '254', MaxNumberLength, 'CountryKenya'),
    ('Kiribati', '686', MaxNumberLength, 'CountryKiribati'),
    ('Kosovo', '383', MaxNumberLength, 'CountryKosovo'),
    ('Kuwait', '965', MaxNumberLength, 'CountryKuwait'),
    ('Kyrgyzstan', '996', MaxNumberLength, 'CountryKyrgyzstan'),
    ('Laos', '856', MaxNumberLength, 'CountryLaos'),
    ('Latvia', '371', MaxNumberLength, 'CountryLatvia'),
    ('Lebanon', '961', MaxNumberLength, 'CountryLebanon'),
    ('Lesotho', '266', MaxNumberLength, 'CountryLesotho'),
    ('Liberia', '231', MaxNumberLength, 'CountryLiberia'),
    ('Libya', '218', MaxNumberLength, 'CountryLibya'),
    ('Liechtenstein', '423', MaxNumberLength, 'CountryLiechtenstein'),
    ('Lithuania', '370', MaxNumberLength, 'CountryLithuania'),
    ('Luxembourg', '352', MaxNumberLength, 'CountryLuxembourg'),
    ('Macau', '853', MaxNumberLength, 'CountryMacau'),
    ('Macedonia', '389', MaxNumberLength, 'CountryMacedonia'),
    ('Madagascar', '261', MaxNumberLength, 'CountryMadagascar'),
    ('Malawi', '265', MaxNumberLength, 'CountryMalawi'),
    ('Malaysia', '60', MaxNumberLength, 'CountryMalaysia'),
    ('Maldives', '960', MaxNumberLength, 'CountryMaldives'),
    ('Mali', '223', MaxNumberLength, 'CountryMali'),
    ('Malta', '356', MaxNumberLength, 'CountryMalta'),
    ('Marshall Islands', '692', MaxNumberLength, 'CountryMarshallIslands'),
    ('Mauritania', '222', MaxNumberLength, 'CountryMauritania'),
    ('Mauritius', '230', MaxNumberLength, 'CountryMauritius'),
    ('Mayotte', '262', MaxNumberLength, 'CountryMayotte'),
    ('Mexico', '52', MaxNumberLength, 'CountryMexico'),
    ('Micronesia', '691', MaxNumberLength, 'CountryMicronesia'),
    ('Moldova', '373', MaxNumberLength, 'CountryMoldova'),
    ('Monaco', '377', MaxNumberLength, 'CountryMonaco'),
    ('Mongolia', '976', MaxNumberLength, 'CountryMongolia'),
    ('Montenegro', '382', MaxNumberLength, 'CountryMontenegro'),
    ('Morocco', '212', MaxNumberLength, 'CountryMorocco'),
    ('Mozambique', '258', MaxNumberLength, 'CountryMozambique'),
    ('Myanmar', '95', MaxNumberLength, 'CountryMyanmar'),
    ('Namibia', '264', MaxNumberLength, 'CountryNamibia'),
    ('Nauru', '674', MaxNumberLength, 'CountryNauru'),
    ('Nepal', '977', MaxNumberLength, 'CountryNepal'),
    ('Netherlands', '31', MaxNumberLength, 'CountryNetherlands'),
    ('Netherlands Antilles', '599', MaxNumberLength, 'CountryNetherlandsAntilles'),
    ('New Caledonia', '687', MaxNumberLength, 'CountryNewCaledonia'),
    ('New Zealand', '64', MaxNumberLength, 'CountryNewZealand'),
    ('Nicaragua', '505', MaxNumberLength, 'CountryNicaragua'),
    ('Niger', '227', MaxNumberLength, 'CountryNiger'),
    ('Nigeria', '234', MaxNumberLength, 'CountryNigeria'),
    ('Niue', '683', MaxNumberLength, 'CountryNiue'),
    ('North Korea', '850', MaxNumberLength, 'CountryNorthKorea'),
    ('Norway', '47', '8', 'CountryNorway'),
    ('Oman', '968', MaxNumberLength, 'CountryOman'),
    ('Pakistan', '92', MaxNumberLength, 'CountryPakistan'),
    ('Palau', '680', MaxNumberLength, 'CountryPalau'),
    ('Palestine', '970', MaxNumberLength, 'CountryPalestine'),
    ('Panama', '507', MaxNumberLength, 'CountryPanama'),
    ('Papua New Guinea', '675', MaxNumberLength, 'CountryPapuaNewGuinea'),
    ('Paraguay', '595', MaxNumberLength, 'CountryParaguay'),
    ('Peru', '51', MaxNumberLength, 'CountryPeru'),
    ('Philippines', '63', MaxNumberLength, 'CountryPhilippines'),
    ('Pitcairn', '64', MaxNumberLength, 'CountryPitcairn'),
    ('Poland', '48', MaxNumberLength, 'CountryPoland'),
    ('Portugal', '351', MaxNumberLength, 'CountryPortugal'),
    ('Qatar', '974', MaxNumberLength, 'CountryQatar'),
    ('Republic of the Congo', '242', MaxNumberLength, 'CountryRepublicOfTheCongo'),
    ('Reunion', '262', MaxNumberLength, 'CountryReunion'),
    ('Romania', '40', MaxNumberLength, 'CountryRomania'),
    ('Russia', '7', MaxNumberLength, 'CountryRussia'),
    ('Rwanda', '250', MaxNumberLength, 'CountryRwanda'),
    ('Saint Barthelemy', '590', MaxNumberLength, 'CountrySaintBarthelemy'),
    ('Saint Helena', '290', MaxNumberLength, 'CountrySaintHelena'),
    ('Saint Martin', '590', MaxNumberLength, 'CountrySaintMartin'),
    ('Saint Pierre and Miquelon', '508', MaxNumberLength, 'CountrySaintPierreAndMiquelon'),
    ('Samoa', '685', MaxNumberLength, 'CountrySamoa'),
    ('San Marino', '378', MaxNumberLength, 'CountrySanMarino'),
    ('Sao Tome and Principe', '239', MaxNumberLength, 'CountrySaoTomeAndPrincipe'),
    ('Saudi Arabia', '966', MaxNumberLength, 'CountrySaudiArabia'),
    ('Senegal', '221', MaxNumberLength, 'CountrySenegal'),
    ('Serbia', '381', MaxNumberLength, 'CountrySerbia'),
    ('Seychelles', '248', MaxNumberLength, 'CountrySeychelles'),
    ('Sierra Leone', '232', MaxNumberLength, 'CountrySie'),
    ('Singapore', '65', MaxNumberLength, 'CountrySingapore'),
    ('Slovakia', '421', MaxNumberLength, 'CountrySlovakia'),
    ('Slovenia', '386', MaxNumberLength, 'CountrySlovenia'),
    ('Solomon Islands', '677', MaxNumberLength, 'CountrySolomonIslands'),
    ('Somalia', '252', MaxNumberLength, 'CountrySomalia'),
    ('South Africa', '27', MaxNumberLength, 'CountrySouthAfrica'),
    ('South Korea', '82', MaxNumberLength, 'CountrySouthKorea'),
    ('South Sudan', '211', MaxNumberLength, 'CountrySouthSudan'),
    ('Spain', '34', '9', 'CountrySpain'),
    ('Sri Lanka', '94', MaxNumberLength, 'CountrySriLanka'),
    ('Sudan', '249', MaxNumberLength, 'CountrySudan'),
    ('Suriname', '597', MaxNumberLength, 'CountrySuriname'),
    ('Svalbard and Jan Mayen', '47', MaxNumberLength, 'CountrySvalbardAndJanMayen'),
    ('Swaziland', '268', MaxNumberLength, 'CountrySwaziland'),
    ('Sweden', '46', MaxNumberLength, 'CountrySweden'),
    ('Switzerland', '41', MaxNumberLength, 'CountrySwitzerland'),
    ('Syria', '963', MaxNumberLength, 'CountrySyria'),
    ('Taiwan', '886', MaxNumberLength, 'CountryTaiwan'),
    ('Tajikistan', '992', MaxNumberLength, 'CountryTajikistan'),
    ('Tanzania', '255', MaxNumberLength, 'CountryTanzania'),
    ('Thailand', '66', MaxNumberLength, 'CountryThailand'),
    ('Togo', '228', MaxNumberLength, 'CountryTogo'),
    ('Tokelau', '690', MaxNumberLength, 'CountryTokelau'),
    ('Tonga', '676', MaxNumberLength, 'CountryTonga'),
    ('Tunisia', '216', MaxNumberLength, 'CountryTunisia'),
    ('Turkey', '90', MaxNumberLength, 'CountryTurkey'),
    ('Turkmenistan', '993', MaxNumberLength, 'CountryTurkmenistan'),
    ('Tuvalu', '688', MaxNumberLength, 'CountryTuvalu'),
    ('Uganda', '256', MaxNumberLength, 'CountryUganda'),
    ('Ukraine', '380', MaxNumberLength, 'CountryUkraine'),
    ('United Arab Emirates', '971', MaxNumberLength, 'CountryUnitedArabEmirates'),
    ('United Kingdom', '44', MaxNumberLength, 'CountryUnitedKingdom'),
    ('United States', '1', MaxNumberLength, 'CountryUnitedStates'),
    ('Uruguay', '598', MaxNumberLength, 'CountryUruguay'),
    ('Uzbekistan', '998', MaxNumberLength, 'CountryUzbekistan'),
    ('Vanuatu', '678', MaxNumberLength, 'CountryVanuatu'),
    ('Vatican', '379', MaxNumberLength, 'CountryVatican'),
    ('Venezuela', '58', MaxNumberLength, 'CountryVenezuela'),
    ('Vietnam', '84', MaxNumberLength, 'CountryVietnam'),
    ('Wallis and Futuna', '681', MaxNumberLength, 'CountryWallisAndFutuna'),
    ('Western Sahara', '212', MaxNumberLength, 'CountryWesternSahara'),
    ('Yemen', '967', MaxNumberLength, 'CountryYemen'),
    ('Zambia', '260', MaxNumberLength, 'CountryZambia'),
    ('Zimbabwe', '263', MaxNumberLength, 'CountryZimbabwe')
);

function RemoveCountryCodePrefix(const CountryCode: String): String;
begin
  Result := Trim(CountryCode);

  if (length(Result) >= 1) and
     (Result[low(CountryCode)] = '+') then
  begin
    Result := copy(Result, low(CountryCode) + 1, MaxInt);
  end else

  if (length(Result) >= 2) and
     (copy(CountryCode, low(CountryCode), 2) = '00') then
  begin
    Result := copy(Result, low(CountryCode) + 2, MaxInt);
  end;
end;

(*procedure StoreToClipboard;
var
  Text: String;
  i: Integer;
begin
  Text := '';

  for i := Low(CountryDialingCodes) to High(CountryDialingCodes) do
  begin
    AddLine(Text, CountryDialingCodes[i,3] + '=' + CountryDialingCodes[i,0]);
  end;

  Clipboard.AsText := Text;
end; *)

function GetCountryIndexCountryCode(const CountryCode: String): Integer;
var
  Code: String;
  i: Integer;
begin
  Code := RemoveCountryCodePrefix(CountryCode);

  Result := -1;

  for i := Low(CountryDialingCodes) to High(CountryDialingCodes) do
  begin
    if Code = CountryDialingCodes[i, 1] then
    begin
      Result := i;

      Break;
    end;
  end;
end;

function GetCountryFromCountryCode(const CountryCode: String; const UseCoutnryID: Boolean): String;
var
  idx: Integer;
begin
  idx := GetCountryIndexCountryCode(RemoveCountryCodePrefix(CountryCode));

  if idx = -1 then
  begin
    Result := '';
  end
  else
  begin
    if UseCoutnryID then
    begin
      Result := CountryDialingCodes[idx, 3];
    end
    else
    begin
      Result := CountryDialingCodes[idx, 0];
    end;
  end;
end;

function GetPhoneNumberLengthFromCountryCode(const CountryCode: String): Integer;
var
  idx: Integer;
begin
  idx := GetCountryIndexCountryCode(RemoveCountryCodePrefix(CountryCode));

  if idx = -1 then
  begin
    Result := 15;
  end
  else
  begin
    Result := StrToIntDef(CountryDialingCodes[idx, 2], 15);
  end;
end;

function IsValidCountryCode(const CountryCode: String): Boolean;
begin
  Result := GetCountryFromCountryCode(CountryCode) <> '';
end;

end.
