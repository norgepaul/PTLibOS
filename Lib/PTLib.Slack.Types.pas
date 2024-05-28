unit PTLib.Slack.Types;

interface

type
  TSlackActionType = (
    satButton
  );

  TSlackActionStyle = (
    sasPrimary,
    sasDanger
  );

  TBookingRequestType = (
    brtUnknown,
    brtBooking,
    brtCancellation
  );

const
  SlackActionTypeValues: Array[TSlackActionType] of String = (
    'button'
  );

  SlackActionStyleValues: Array[TSlackActionStyle] of String = (
    'primary',
    'danger'
  );

  BookingRequestTypeDescription: Array[TBookingRequestType] of String = (
    'Unklnown',
    'Booking',
    'Cancellation'
  );

implementation

end.
