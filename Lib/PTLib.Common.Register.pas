unit PTLib.Common.Register;

interface

uses
  SysUtils, DesignIntf, DesignEditors,
  Classes,

  PTLib.Common.Statistics,
  PTLib.Common.Thread.Manager,
  PTLib.Common.Timers,
  PTLib.Common.TimedEvents,
  PTLib.Common.Log,
  PTLib.Common.Log.LocalFile,
  PTLib.Common.Scheduler,
  PTLib.Common.Configuration.XML,
  PTLib.Common.Configuration,
  PTLib.Common.Log.Transmitter,
  PTLib.Common.Log.Receiver,
  PTLib.Common.Localisation,
  PTLib.Common.Network.Rest.Server,
  PTLib.Common.Network.HttpFileDownload,
  PTLib.Common.PriorityQueue,
  PTLib.Common.Network.PriorityHTTPQueue,
  PTLib.Database.Services.BackupRestore.Firebird;

procedure Register;

implementation

const
  PTLibCommon = 'PTLib';

procedure Register;
begin
  RegisterComponents(PTLibCommon,
    [TUsageTimeStatistics,
     TThreadManager,
     TPTLibLog,
     TPTLibFileLog,
     TScheduler,
     TPTLibFirebirdBackup,
     TPTLibFirebirdRestore,
     TPTLibLogTransmitter,
     TPTLibLogReceiver,
     TPTLibXMLConfiguration,
     TPTLibLocaliser,
     TPTLibIdRestServer,
     TPTLibHTTPFileDownload,
     TPTLibMultiPriorityStringDataQueues,
     TPTLibPriorityHTTPQueue,
     TPTLibTimedEvents]);
end;

end.
