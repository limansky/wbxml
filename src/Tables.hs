----------------------------------------------------------------------
-- |
-- Module : WBXML.Tables
-- Copyright : Mike Limansky, 2011
--
-- Definition of WBXML Tables
--
----------------------------------------------------------------------

module Tables where

import Data.Word --(Word8)

type WbxmlTable = [(String, Word8, Word8)]

airSyncTable :: WbxmlTable
airSyncTable =
    [ ( "Sync",                   0x00, 0x05 ) -- since r1.0
    , ( "Responses",              0x00, 0x06) -- since r1.0
    , ( "Add",                    0x00, 0x07) -- since r1.0
    , ( "Change",                 0x00, 0x08) -- since r1.0
    , ( "Delete",                 0x00, 0x09) -- since r1.0
    , ( "Fetch",                  0x00, 0x0a) -- since r1.0
    , ( "SyncKey",                0x00, 0x0b) -- since r1.0
    , ( "ClientId",               0x00, 0x0c) -- since r1.0
    , ( "ServerId",               0x00, 0x0d) -- since r1.0
    , ( "Status",                 0x00, 0x0e) -- since r1.0
    , ( "Collection",             0x00, 0x0f) -- since r1.0
    , ( "Class",                  0x00, 0x10) -- since r1.0
    , ( "Version",                0x00, 0x11) -- not defined in r8.0 but in r1.0
    , ( "CollectionId",           0x00, 0x12) -- since r1.0
    , ( "GetChanges",             0x00, 0x13) -- since r1.0
    , ( "MoreAvailable",          0x00, 0x14) -- since r1.0
    , ( "WindowSize",             0x00, 0x15) -- since r1.0
    , ( "Commands",               0x00, 0x16) -- since r1.0
    , ( "Options",                0x00, 0x17) -- since r1.0
    , ( "FilterType",             0x00, 0x18) -- since r1.0
    , ( "Truncation",             0x00, 0x19) -- not defined in r8.0 but in r1.0
    , ( "RTFTruncation",          0x00, 0x1a) -- corrected in libwbxml 0.11.0, not defined in r8.0 but in r1.0
    , ( "Conflict",               0x00, 0x1b) -- since r1.0
    , ( "Collections",            0x00, 0x1c) -- since r1.0
    , ( "ApplicationData",        0x00, 0x1d) -- since r1.0
    , ( "DeletesAsMoves",         0x00, 0x1e) -- since r1.0
    , ( "NotifyGUID",             0x00, 0x1f) -- not defined in r8.0 but in r1.0
    , ( "Supported",              0x00, 0x20) -- since r1.0
    , ( "SoftDelete",             0x00, 0x21) -- since r1.0
    , ( "MIMESupport",            0x00, 0x22) -- since r1.0
    , ( "MIMETruncation",         0x00, 0x23) -- since r1.0
    , ( "Wait",                   0x00, 0x24) -- since r1.0
    , ( "Limit",                  0x00, 0x25) -- since r1.0
    , ( "Partial",                0x00, 0x26) -- since r1.0
    , ( "ConversationMode",       0x00, 0x27) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "MaxItems",               0x00, 0x28) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "HeartbeatInterval",      0x00, 0x29) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1

    -- Code Page: Contacts (since v2.5 and r1.0)
    , ( "Anniversary",            0x01, 0x05) -- since r1.0
    , ( "AssistantName",          0x01, 0x06) -- since r1.0
    , ( "AssistantTelephoneNumber", 0x01, 0x07) -- corrected in libwbxml 0.11.0
    , ( "Birthday",               0x01, 0x08) -- since r1.0
    , ( "Body",                   0x01, 0x09) -- not defined in r8.0 but in r1.0
    , ( "BodySize",               0x01, 0x0a) -- not defined in r8.0 but in r1.0
    , ( "BodyTruncated",          0x01, 0x0b) -- not defined in r8.0 but in r1.0
    , ( "Business2PhoneNumber",   0x01, 0x0c) -- changed in r8.0, r1.0: Business2TelephoneNumber
    , ( "BusinessCity",           0x01, 0x0d) -- since r1.0
    , ( "BusinessCountry",        0x01, 0x0e) -- since r1.0
    , ( "BusinessPostalCode",     0x01, 0x0f) -- since r1.0
    , ( "BusinessState",          0x01, 0x10) -- since r1.0
    , ( "BusinessStreet",         0x01, 0x11) -- since r1.0
    , ( "BusinessFaxNumber",      0x01, 0x12) -- since r1.0
    , ( "BusinessPhoneNumber",    0x01, 0x13) -- changed in r8.0, r1.0: BusinessTelephoneNumber
    , ( "CarPhoneNumber",         0x01, 0x14) -- since r1.0
    , ( "Categories",             0x01, 0x15) -- since r1.0
    , ( "Category",               0x01, 0x16) -- since r1.0
    , ( "Children",               0x01, 0x17) -- since r1.0
    , ( "Child",                  0x01, 0x18) -- since r1.0
    , ( "CompanyName",            0x01, 0x19) -- since r1.0
    , ( "Department",             0x01, 0x1a) -- since r1.0
    , ( "Email1Address",          0x01, 0x1b) -- since r1.0
    , ( "Email2Address",          0x01, 0x1c) -- since r1.0
    , ( "Email3Address",          0x01, 0x1d) -- since r1.0
    , ( "FileAs",                 0x01, 0x1e) -- since r1.0
    , ( "FirstName",              0x01, 0x1f) -- since r1.0
    , ( "Home2PhoneNumber",       0x01, 0x20) -- changed in r8.0, r1.0: BusinessTelephoneNumber
    , ( "HomeCity",               0x01, 0x21) -- since r1.0
    , ( "HomeCountry",            0x01, 0x22) -- since r1.0
    , ( "HomePostalCode",         0x01, 0x23) -- since r1.0
    , ( "HomeState",              0x01, 0x24) -- since r1.0
    , ( "HomeStreet",             0x01, 0x25) -- since r1.0
    , ( "HomeFaxNumber",          0x01, 0x26) -- since r1.0
    , ( "HomePhoneNumber",        0x01, 0x27) -- changed in r8.0, r1.0: BusinessTelephoneNumber
    , ( "JobTitle",               0x01, 0x28) -- since r1.0
    , ( "LastName",               0x01, 0x29) -- since r1.0
    , ( "MiddleName",             0x01, 0x2a) -- since r1.0
    , ( "MobilePhoneNumber",      0x01, 0x2b) -- changed in r8.0, r1.0: BusinessTelephoneNumber
    , ( "OfficeLocation",         0x01, 0x2c) -- since r1.0
    , ( "OtherCity",              0x01, 0x2d) -- since r1.0
    , ( "OtherCountry",           0x01, 0x2e) -- since r1.0
    , ( "OtherPostalCode",        0x01, 0x2f) -- since r1.0
    , ( "OtherState",             0x01, 0x30) -- since r1.0
    , ( "OtherStreet",            0x01, 0x31) -- since r1.0
    , ( "PagerNumber",            0x01, 0x32) -- since r1.0
    , ( "RadioPhoneNumber",       0x01, 0x33) -- changed in r8.0, r1.0: BusinessTelephoneNumber
    , ( "Spouse",                 0x01, 0x34) -- since r1.0
    , ( "Suffix",                 0x01, 0x35) -- since r1.0
    , ( "Title",                  0x01, 0x36) -- since r1.0
    , ( "WebPage",                0x01, 0x37) -- since r1.0
    , ( "YomiCompanyName",        0x01, 0x38) -- since r1.0
    , ( "YomiFirstName",          0x01, 0x39) -- since r1.0
    , ( "YomiLastName",           0x01, 0x3a) -- since r1.0
    , ( "CompressedRTF",          0x01, 0x3b) -- corrected in libwbxml 0.11.0, not defined in r8.0 but in r1.0
    , ( "Picture",                0x01, 0x3c) -- since r1.0
    , ( "Alias",                  0x01, 0x3d) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "WeightedRank",           0x01, 0x3e) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1

    -- Code Page: Email (since v2.5 and r1.0)
    , ( "Attachment",             0x02, 0x05) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "Attachments",            0x02, 0x06) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "AttName",                0x02, 0x07) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "AttSize",                0x02, 0x08) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "AttOId",                 0x02, 0x09) -- corrected in libwbxml 0.11.0, not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "AttMethod",              0x02, 0x0a) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "AttRemoved",             0x02, 0x0b) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "Body",                   0x02, 0x0c) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "BodySize",               0x02, 0x0d) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "BodyTruncated",          0x02, 0x0e) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "DateReceived",           0x02, 0x0f) -- supported since v2.5
    , ( "DisplayName",            0x02, 0x10) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "DisplayTo",              0x02, 0x11) -- supported since v2.5
    , ( "Importance",             0x02, 0x12) -- supported since v2.5
    , ( "MessageClass",           0x02, 0x13) -- supported since v2.5
    , ( "Subject",                0x02, 0x14) -- supported since v2.5
    , ( "Read",                   0x02, 0x15) -- supported since v2.5
    , ( "To",                     0x02, 0x16) -- supported since v2.5
    , ( "Cc",                     0x02, 0x17) -- supported since v2.5
    , ( "From",                   0x02, 0x18) -- supported since v2.5
    , ( "Reply-To",               0x02, 0x19) -- supported since v2.5
    , ( "AllDayEvent",            0x02, 0x1a) -- supported since v2.5
    , ( "Categories",             0x02, 0x1b) -- r1.0: supported by v2.5, v12.0 and 12.1; BUT r8.0: not supported by 12.1
    , ( "Category",               0x02, 0x1c) -- r1.0: supported by v2.5, v12.0 and 12.1; BUT r8.0: not supported by 12.1
    , ( "DTStamp",                0x02, 0x1d) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "EndTime",                0x02, 0x1e) -- supported since v2.5
    , ( "InstanceType",           0x02, 0x1f) -- supported since v2.5
    , ( "BusyStatus",             0x02, 0x20) -- supported since v2.5
    , ( "Location",               0x02, 0x21) -- supported since v2.5
    , ( "MeetingRequest",         0x02, 0x22) -- supported since v2.5
    , ( "Organizer",              0x02, 0x23) -- supported since v2.5
    , ( "RecurrenceId",           0x02, 0x24) -- supported since v2.5
    , ( "Reminder",               0x02, 0x25) -- supported since v2.5
    , ( "ResponseRequested",      0x02, 0x26) -- supported since v2.5
    , ( "Recurrences",            0x02, 0x27) -- supported since v2.5
    , ( "Recurrence",             0x02, 0x28) -- supported since v2.5
    , ( "Recurrence_Type",        0x02, 0x29) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_Until",       0x02, 0x2a) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_Occurrences", 0x02, 0x2b) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_Interval",    0x02, 0x2c) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_DayOfWeek",   0x02, 0x2d) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_DayOfMonth",  0x02, 0x2e) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_WeekOfMonth", 0x02, 0x2f) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_MonthOfYear", 0x02, 0x30) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "StartTime",              0x02, 0x31) -- supported since v2.5
    , ( "Sensitivity",            0x02, 0x32) -- supported since v2.5
    , ( "TimeZone",               0x02, 0x33) -- supported since v2.5
    , ( "GlobalObjId",            0x02, 0x34) -- supported since v2.5
    , ( "ThreadTopic",            0x02, 0x35) -- supported since v2.5
    , ( "MIMEData",               0x02, 0x36) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "MIMETruncated",          0x02, 0x37) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "MIMESize",               0x02, 0x38) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "InternetCPID",           0x02, 0x39) -- supported since v2.5
    , ( "Flag",                   0x02, 0x3a) -- supported since v12.0
    , ( "FlagStatus",             0x02, 0x3b) -- supported since v12.0
    , ( "ContentClass",           0x02, 0x3c) -- supported since v12.0
    , ( "FlagType",               0x02, 0x3d) -- supported since v12.0
    , ( "CompleteTime",           0x02, 0x3e) -- supported since v12.0
    , ( "DisallowNewTimeProposal",0x02, 0x3f) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1

    -- Code Page: AirNotify

    {- There are conflicting version informations.
     *
     * r1.0: supported by v2.5, v12.0 and v12.1
     * r8.0: This code page is no longer in use.
     * r8.0: Tokens 05 to 17 have been defined.
     *-}

    , ( "Notify",                 0x03, 0x05) -- not defined in r8.0 but in r1.0, only supported by v2.0 and v2.5
    , ( "Notification",           0x03, 0x06) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "Version",                0x03, 0x07) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "LifeTime",               0x03, 0x08) -- corrected in libwbxml 0.11.0, not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "DeviceInfo",             0x03, 0x09) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "Enable",                 0x03, 0x0a) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "Folder",                 0x03, 0x0b) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "ServerId",               0x03, 0x0c) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "DeviceAddress",          0x03, 0x0d) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "ValidCarrierProfiles",   0x03, 0x0e) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "CarrierProfile",         0x03, 0x0f) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "Status",                 0x03, 0x10) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "Responses",              0x03, 0x11) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "Devices",                0x03, 0x12) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "Device",                 0x03, 0x13) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "Id",                     0x03, 0x14) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "Expiry",                 0x03, 0x15) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "NotifyGUID",             0x03, 0x16) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "DeviceFriendlyName",     0x03, 0x17) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1

    -- Code Page: Calendar (since v2.5 and r1.0)
    , ( "TimeZone",               0x04, 0x05) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "AllDayEvent",            0x04, 0x06) -- supported since v2.5
    , ( "Attendees",              0x04, 0x07) -- supported since v2.5
    , ( "Attendee",               0x04, 0x08) -- supported since v2.5
    , ( "Attendee_Email",         0x04, 0x09) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Attendee_Name",          0x04, 0x0a) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Body",                   0x04, 0x0b) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "BodyTruncated",          0x04, 0x0c) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "BusyStatus",             0x04, 0x0d) -- supported since v2.5
    , ( "Categories",             0x04, 0x0e) -- supported since v2.5
    , ( "Category",               0x04, 0x0f) -- supported since v2.5
    , ( "Compressed_RTF",         0x04, 0x10) -- corrected in libwbxml 0.11.0, not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "DTStamp",                0x04, 0x11) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "EndTime",                0x04, 0x12) -- supported since v2.5
    , ( "Exception",              0x04, 0x13) -- supported since v2.5
    , ( "Exceptions",             0x04, 0x14) -- supported since v2.5
    , ( "Exception_Deleted",      0x04, 0x15) -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: Exception_IsDeleted
    , ( "Exception_StartTime",    0x04, 0x16) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Location",               0x04, 0x17) -- supported since v2.5
    , ( "MeetingStatus",          0x04, 0x18) -- supported since v2.5
    , ( "Organizer_Email",        0x04, 0x19) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Organizer_Name",         0x04, 0x1a) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence",             0x04, 0x1b) -- supported since v2.5
    , ( "Recurrence_Type",        0x04, 0x1c) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_Until",       0x04, 0x1d) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_Occurrences", 0x04, 0x1e) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_Interval",    0x04, 0x1f) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_DayOfWeek",   0x04, 0x20) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_DayOfMonth",  0x04, 0x21) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_WeekOfMonth", 0x04, 0x22) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Recurrence_MonthOfYear", 0x04, 0x23) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Reminder",               0x04, 0x24) -- supported since v2.5
    , ( "Sensitivity",            0x04, 0x25) -- supported since v2.5
    , ( "Subject",                0x04, 0x26) -- supported since v2.5
    , ( "StartTime",              0x04, 0x27) -- supported since v2.5
    , ( "UID",                    0x04, 0x28) -- supported since v2.5
    , ( "Attendee_Status",        0x04, 0x29) -- corrected in libwbxml 0.11.0, supported since v12.0
    , ( "Attendee_Type",          0x04, 0x2a) -- corrected in libwbxml 0.11.0, supported since v12.0
    , ( "DisallowNewTimeProposal",0x04, 0x33) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "ResponseRequested",      0x04, 0x34) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "AppointmentReplyTime",   0x04, 0x35) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "ResponseType",           0x04, 0x36) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "CalendarType",           0x04, 0x37) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "IsLeapMonth",            0x04, 0x38) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "FirstDayOfWeek",         0x04, 0x39) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "OnlineMeetingConfLink",  0x04, 0x3a) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "OnlineMeetingExternalLink",0x04, 0x3b) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1

    -- Code Page: Move (since v2.5 and r1.0)
    , ( "MoveItems",              0x05, 0x05) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Move",                   0x05, 0x06) -- since r1.0
    , ( "SrcMsgId",               0x05, 0x07) -- since r1.0
    , ( "SrcFldId",               0x05, 0x08) -- since r1.0
    , ( "DstFldId",               0x05, 0x09) -- since r1.0
    , ( "Response",               0x05, 0x0a) -- since r1.0
    , ( "Status",                 0x05, 0x0b) -- since r1.0
    , ( "DstMsgId",               0x05, 0x0c) -- since r1.0

    -- Code Page: ItemEstimate (since v2.5 and r1.0)
    , ( "GetItemEstimate",        0x06, 0x05) -- since r1.0
    , ( "Version",                0x06, 0x06) -- r8.0: only supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "Collections",            0x06, 0x07) -- since r1.0
    , ( "Collection",             0x06, 0x08) -- since r1.0
    , ( "Class",                  0x06, 0x09) -- r8.0: only supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "CollectionId",           0x06, 0x0a) -- since r1.0
    , ( "DateTime",               0x06, 0x0b) -- r8.0: only supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "Estimate",               0x06, 0x0c) -- since r1.0
    , ( "Response",               0x06, 0x0d) -- since r1.0
    , ( "Status",                 0x06, 0x0e) -- since r1.0

    -- Code Page: FolderHierarchy (since v2.5 and r1.0)
    , ( "Folders",                0x07, 0x05) -- not defined in r8.0 but in r1.0
    , ( "Folder",                 0x07, 0x06) -- not defined in r8.0 but in r1.0
    , ( "DisplayName",            0x07, 0x07) -- since r1.0
    , ( "ServerId",               0x07, 0x08) -- since r1.0
    , ( "ParentId",               0x07, 0x09) -- since r1.0
    , ( "Type",                   0x07, 0x0a) -- since r1.0
    , ( "Response",               0x07, 0x0b) -- not defined in r8.0 but in r1.0
    , ( "Status",                 0x07, 0x0c) -- since r1.0
    , ( "ContentClass",           0x07, 0x0d) -- not defined in r8.0 but in r1.0
    , ( "Changes",                0x07, 0x0e) -- since r1.0
    , ( "Add",                    0x07, 0x0f) -- since r1.0
    , ( "Delete",                 0x07, 0x10) -- since r1.0
    , ( "Update",                 0x07, 0x11) -- since r1.0
    , ( "SyncKey",                0x07, 0x12) -- since r1.0
    , ( "FolderCreate",           0x07, 0x13) -- since r1.0
    , ( "FolderDelete",           0x07, 0x14) -- since r1.0
    , ( "FolderUpdate",           0x07, 0x15) -- since r1.0
    , ( "FolderSync",             0x07, 0x16) -- since r1.0
    , ( "Count",                  0x07, 0x17) -- since r1.0
    , ( "Version",                0x07, 0x18) -- not defined in r8.0 but in r1.0

    -- Code Page: MeetingResponse (since v2.5 and r1.0)
    , ( "CalendarId",             0x08, 0x05) -- changed in r8.0, r1.0: CallID
    , ( "CollectionId",           0x08, 0x06) -- since r1.0
    , ( "MeetingResponse",        0x08, 0x07) -- since r1.0
    , ( "RequestId",              0x08, 0x08) -- changed in r8.0, r1.0: ReqId
    , ( "Request",                0x08, 0x09) -- since r1.0
    , ( "Result",                 0x08, 0x0a) -- since r1.0
    , ( "Status",                 0x08, 0x0b) -- since r1.0
    , ( "UserResponse",           0x08, 0x0c) -- since r1.0
    , ( "Version",                0x08, 0x0d) -- not defined in r8.0 but in r1.0
    , ( "InstanceId",             0x08, 0x0e) -- since r8.0?

    -- Code Page: Tasks (since v2.5 and r1.0)
    , ( "Body",                   0x09, 0x05) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "BodySize",               0x09, 0x06) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "BodyTruncated",          0x09, 0x07) -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "Categories",             0x09, 0x08) -- supported since v2.5
    , ( "Category",               0x09, 0x09) -- supported since v2.5
    , ( "Complete",               0x09, 0x0a) -- supported since v2.5
    , ( "DateCompleted",          0x09, 0x0b) -- supported since v2.5
    , ( "DueDate",                0x09, 0x0c) -- supported since v2.5
    , ( "UTCDueDate",             0x09, 0x0d) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Importance",             0x09, 0x0e) -- supported since v2.5
    , ( "Recurrence",             0x09, 0x0f) -- supported since v2.5
    , ( "Recurrence_Type",        0x09, 0x10) -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceType
    , ( "Recurrence_Start",       0x09, 0x11) -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceStart
    , ( "Recurrence_Until",       0x09, 0x12) -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceUntil
    , ( "Recurrence_Occurrences", 0x09, 0x13) -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceOccurrences
    , ( "Recurrence_Interval",    0x09, 0x14) -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceInterval
    , ( "Recurrence_DayOfMonth",  0x09, 0x15) -- supported since v2.5, changed in r8.0, r1.0: RecurrenceDayOfMonth
    , ( "Recurrence_DayOfWeek",   0x09, 0x16) -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceDayOfWeek
    , ( "Recurrence_DayOfMonth",  0x09, 0x15) -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceDayOfMonth
    , ( "Recurrence_WeekOfMonth", 0x09, 0x17) -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceWeekOfMonth
    , ( "Recurrence_MonthOfYear", 0x09, 0x18) -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceMonthOfYear
    , ( "Recurrence_Regenerate",  0x09, 0x19) -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceRegenerate
    , ( "Recurrence_DeadOccur",   0x09, 0x1a) -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceDeadOccour
    , ( "ReminderSet",            0x09, 0x1b) -- supported since v2.5
    , ( "ReminderTime",           0x09, 0x1c) -- supported since v2.5
    , ( "Sensitivity",            0x09, 0x1d) -- supported since v2.5
    , ( "StartDate",              0x09, 0x1e) -- supported since v2.5
    , ( "UTCStartDate",           0x09, 0x1f) -- corrected in libwbxml 0.11.0, supported since v2.5
    , ( "Subject",                0x09, 0x20) -- supported since v2.5
    , ( "CompressedRTF",          0x09, 0x21) -- corrected in libwbxml 0.11.0, not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    , ( "OrdinalDate",            0x09, 0x22) -- supported since v12.0
    , ( "SubOrdinalDate",         0x09, 0x23) -- supported since v12.0
    , ( "CalendarType",           0x09, 0x24) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "IsLeapMonth",            0x09, 0x25) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "FirstDayOfWeek",         0x09, 0x26) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1

    -- Code Page: ResolveRecipients (since v2.5 and r1.0)
    , ( "ResolveRecipients",      0x0a, 0x05) -- since r1.0
    , ( "Response",               0x0a, 0x06) -- since r1.0
    , ( "Status",                 0x0a, 0x07) -- since r1.0
    , ( "Type",                   0x0a, 0x08) -- since r1.0
    , ( "Recipient",              0x0a, 0x09) -- since r1.0
    , ( "DisplayName",            0x0a, 0x0a) -- since r1.0
    , ( "EmailAddress",           0x0a, 0x0b) -- since r1.0
    , ( "Certificates",           0x0a, 0x0c) -- since r1.0
    , ( "Certificate",            0x0a, 0x0d) -- since r1.0
    , ( "MiniCertificate",        0x0a, 0x0e) -- since r1.0
    , ( "Options",                0x0a, 0x0f) -- since r1.0
    , ( "To",                     0x0a, 0x10) -- since r1.0
    , ( "CertificateRetrieval",   0x0a, 0x11) -- since r1.0
    , ( "RecipientCount",         0x0a, 0x12) -- since r1.0
    , ( "MaxCertificates",        0x0a, 0x13) -- since r1.0
    , ( "MaxAmbiguousRecipients", 0x0a, 0x14) -- since r1.0
    , ( "CertificateCount",       0x0a, 0x15) -- since r1.0
    , ( "Availability",           0x0a, 0x16) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "StartTime",              0x0a, 0x17) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "EndTime",                0x0a, 0x18) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "MergedFreeBusy",         0x0a, 0x19) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "Picture",                0x0a, 0x1a) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "MaxSize",                0x0a, 0x1b) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "Data",                   0x0a, 0x1c) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "MaxPictures",            0x0a, 0x1d) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1

    -- Code Page: ValidateCert (since v2.5 and r1.0)
    , ( "ValidateCert",           0x0b, 0x05) -- since r1.0
    , ( "Certificates",           0x0b, 0x06) -- since r1.0
    , ( "Certificate",            0x0b, 0x07) -- since r1.0
    , ( "CertificateChain",       0x0b, 0x08) -- since r1.0
    , ( "CheckCRL",               0x0b, 0x09) -- since r1.0
    , ( "Status",                 0x0b, 0x0a) -- since r1.0

    -- Code Page: Contacts2 (since v2.5 and r1.0)
    , ( "CustomerId",             0x0c, 0x05) -- since r1.0
    , ( "GovernmentId",           0x0c, 0x06) -- since r1.0
    , ( "IMAddress",              0x0c, 0x07) -- since r1.0
    , ( "IMAddress2",             0x0c, 0x08) -- since r1.0
    , ( "IMAddress3",             0x0c, 0x09) -- since r1.0
    , ( "ManagerName",            0x0c, 0x0a) -- since r1.0
    , ( "CompanyMainPhone",       0x0c, 0x0b) -- since r1.0
    , ( "AccountName",            0x0c, 0x0c) -- since r1.0
    , ( "NickName",               0x0c, 0x0d) -- since r1.0
    , ( "MMS",                    0x0c, 0x0e) -- since r1.0

    -- Code Page: Ping (since v2.5 and r1.0)
    , ( "Ping",                   0x0d, 0x05) -- since r1.0
    , ( "AutdState",              0x0d, 0x06) -- not used by protocol
    , ( "Status",                 0x0d, 0x07) -- since r1.0
    , ( "HeartbeatInterval",      0x0d, 0x08) -- since r1.0
    , ( "Folders",                0x0d, 0x09) -- since r1.0
    , ( "Folder",                 0x0d, 0x0a) -- since r1.0
    , ( "Id",                     0x0d, 0x0b) -- since r1.0
    , ( "Class",                  0x0d, 0x0c) -- since r1.0
    , ( "MaxFolders",             0x0d, 0x0d) -- since r1.0

    -- Code Page: Provision (since v2.5 and r1.0)
    , ( "Provision",                                0x0e, 0x05) -- supported since v2.5
    , ( "Policies",                                 0x0e, 0x06) -- supported since v2.5
    , ( "Policy",                                   0x0e, 0x07) -- supported since v2.5
    , ( "PolicyType",                               0x0e, 0x08) -- supported since v2.5
    , ( "PolicyKey",                                0x0e, 0x09) -- supported since v2.5
    , ( "Data",                                     0x0e, 0x0a) -- supported since v2.5
    , ( "Status",                                   0x0e, 0x0b) -- supported since v2.5
    , ( "RemoteWipe",                               0x0e, 0x0c) -- supported since v2.5
    , ( "EASProvisionDoc",                          0x0e, 0x0d) -- supported since v12.0
    , ( "DevicePasswordEnabled",                    0x0e, 0x0e) -- supported since v12.0
    , ( "AlphanumericDevicePasswordRequired",       0x0e, 0x0f) -- supported since v12.0
    , ( "DeviceEncryptionEnabled",                  0x0e, 0x10) -- r1.0: supported since v12.0
    , ( "RequireStorageCardEncryption",             0x0e, 0x10) -- r1.0: supported by v2.0 and v2.5
    , ( "PasswordRecoveryEnabled",                  0x0e, 0x11) -- supported since v12.0
    , ( "DocumentBrowseEnabled",                    0x0e, 0x12) -- supported since v12.0, not defined in r8.0 but in r1.0
    , ( "AttachmentsEnabled",                       0x0e, 0x13) -- supported since v12.0
    , ( "MinDevicePasswordLength",                  0x0e, 0x14) -- supported since v12.0
    , ( "MaxInactivityTimeDeviceLock",              0x0e, 0x15) -- supported since v12.0
    , ( "MaxDevicePasswordFailedAttempts",          0x0e, 0x16) -- supported since v12.0
    , ( "MaxAttachmentSize",                        0x0e, 0x17) -- supported since v12.0
    , ( "AllowSimpleDevicePassword",                0x0e, 0x18) -- supported since v12.0
    , ( "DevicePasswordExpiration",                 0x0e, 0x19) -- supported since v12.0
    , ( "DevicePasswordHistory",                    0x0e, 0x1a) -- supported since v12.0
    , ( "AllowStorageCard",                         0x0e, 0x1b) -- supported since v12.1
    , ( "AllowCamera",                              0x0e, 0x1c) -- supported by v2.0 and v2.5
    , ( "RequireDeviceEncryption",                  0x0e, 0x1d) -- supported by v2.0 and v2.5
    , ( "AllowUnsignedApplications",                0x0e, 0x1e) -- supported by v2.0 and v2.5
    , ( "AllowUnsignedInstallationPackages",        0x0e, 0x1f) -- supported by v2.0 and v2.5
    , ( "MinDevicePasswordComplexCharacters",       0x0e, 0x20) -- supported by v2.0 and v2.5
    , ( "AllowWiFi",                                0x0e, 0x21) -- supported by v2.0 and v2.5
    , ( "AllowTextMessaging",                       0x0e, 0x22) -- supported by v2.0 and v2.5
    , ( "AllowPOPIMAPEmail",                        0x0e, 0x23) -- supported by v2.0 and v2.5
    , ( "AllowBluetooth",                           0x0e, 0x24) -- supported by v2.0 and v2.5
    , ( "AllowIrDA",                                0x0e, 0x25) -- supported by v2.0 and v2.5
    , ( "RequireManualSyncWhenRoaming",             0x0e, 0x26) -- supported by v2.0 and v2.5
    , ( "AllowDesktopSync",                         0x0e, 0x27) -- supported by v2.0 and v2.5
    , ( "MaxCalendarAgeFilter",                     0x0e, 0x28) -- supported by v2.0 and v2.5
    , ( "AllowHTMLEmail",                           0x0e, 0x29) -- supported by v2.0 and v2.5
    , ( "MaxEmailAgeFilter",                        0x0e, 0x2a) -- supported by v2.0 and v2.5
    , ( "MaxEmailBodyTruncationSize",               0x0e, 0x2b) -- supported by v2.0 and v2.5
    , ( "MaxEmailHTMLBodyTruncationSize",           0x0e, 0x2c) -- supported by v2.0 and v2.5
    , ( "RequireSignedSMIMEMessages",               0x0e, 0x2d) -- supported by v2.0 and v2.5
    , ( "RequireEncryptedSMIMEMessages",            0x0e, 0x2e) -- supported by v2.0 and v2.5
    , ( "RequireSignedSMIMEAlgorithm",              0x0e, 0x2f) -- supported by v2.0 and v2.5
    , ( "RequireEncryptionSMIMEAlgorithm",          0x0e, 0x30) -- supported by v2.0 and v2.5
    , ( "AllowSMIMEEncryptionAlgorithmNegotiation", 0x0e, 0x31) -- supported by v2.0 and v2.5
    , ( "AllowSMIMESoftCerts",                      0x0e, 0x32) -- supported by v2.0 and v2.5
    , ( "AllowBrowser",                             0x0e, 0x33) -- supported by v2.0 and v2.5
    , ( "AllowConsumerEmail",                       0x0e, 0x34) -- supported by v2.0 and v2.5
    , ( "AllowRemoteDesktop",                       0x0e, 0x35) -- supported by v2.0 and v2.5
    , ( "AllowInternetSharing",                     0x0e, 0x36) -- supported by v2.0 and v2.5
    , ( "UnapprovedInROMApplicationList",           0x0e, 0x37) -- supported by v2.0 and v2.5
    , ( "ApplicationName",                          0x0e, 0x38) -- supported by v2.0 and v2.5
    , ( "ApprovedApplicationList",                  0x0e, 0x39) -- supported by v2.0 and v2.5
    , ( "Hash",                                     0x0e, 0x3a) -- supported by v2.0 and v2.5

    -- Code Page: Search (since v2.5 and r1.0)
    -- Token 0x06 and 0x16 are not supported.
    , ( "Search",                 0x0f, 0x05) -- supported since v2.5
    , ( "Store",                  0x0f, 0x07) -- supported since v2.5
    , ( "Name",                   0x0f, 0x08) -- supported since v2.5
    , ( "Query",                  0x0f, 0x09) -- supported since v2.5
    , ( "Options",                0x0f, 0x0a) -- supported since v2.5
    , ( "Range",                  0x0f, 0x0b) -- supported since v2.5
    , ( "Status",                 0x0f, 0x0c) -- supported since v2.5
    , ( "Response",               0x0f, 0x0d) -- supported since v2.5
    , ( "Result",                 0x0f, 0x0e) -- supported since v2.5
    , ( "Properties",             0x0f, 0x0f) -- supported since v2.5
    , ( "Total",                  0x0f, 0x10) -- supported since v2.5
    , ( "EqualTo",                0x0f, 0x11) -- supported since v12.0
    , ( "Value",                  0x0f, 0x12) -- supported since v12.0
    , ( "And",                    0x0f, 0x13) -- supported since v12.0
    , ( "Or",                     0x0f, 0x14) -- supported since v12.0, r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "FreeText",               0x0f, 0x15) -- supported since v12.0
    , ( "DeepTraversal",          0x0f, 0x17) -- supported since v12.0
    , ( "LongId",                 0x0f, 0x18) -- supported since v12.0
    , ( "RebuildResults",         0x0f, 0x19) -- supported since v12.0
    , ( "LessThan",               0x0f, 0x1a) -- supported since v12.0
    , ( "GreaterThan",            0x0f, 0x1b) -- supported since v12.0
    , ( "Schema",                 0x0f, 0x1c) -- supported since v12.0, r8.0: not defined in r8.0 but in r1.0
    , ( "Supported",              0x0f, 0x1d) -- supported since v12.0, r8.0: not defined in r8.0 but in r1.0
    , ( "UserName",               0x0f, 0x1e) -- since 8.0?
    , ( "Password",               0x0f, 0x1f) -- since 8.0?
--    , ( "ConversationId",         0x0f, 0x20, WBXML_TAG_OPTION_BINARY) -- since 8.0?
    , ( "Picture",                0x0f, 0x21) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "MaxSize",                0x0f, 0x22) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "MaxPictures",            0x0f, 0x23) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1

    -- Code Page: GAL (since v2.5 and r1.0)
    , ( "DisplayName",            0x10, 0x05) -- since r1.0
    , ( "Phone",                  0x10, 0x06) -- since r1.0
    , ( "Office",                 0x10, 0x07) -- since r1.0
    , ( "Title",                  0x10, 0x08) -- since r1.0
    , ( "Company",                0x10, 0x09) -- since r1.0
    , ( "Alias",                  0x10, 0x0a) -- since r1.0
    , ( "FirstName",              0x10, 0x0b) -- since r1.0
    , ( "LastName",               0x10, 0x0c) -- since r1.0
    , ( "HomePhone",              0x10, 0x0d) -- since r1.0
    , ( "MobilePhone",            0x10, 0x0e) -- since r1.0
    , ( "EmailAddress",           0x10, 0x0f) -- since r1.0
    , ( "Picture",                0x10, 0x10) -- not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "Status",                 0x10, 0x11) -- not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "Data",                   0x10, 0x12) -- not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1

    -- Code Page: AirSyncBase (since v12.0 and r1.0)
    , ( "BodyPreference",         0x11, 0x05) -- since r1.0
    , ( "Type",                   0x11, 0x06) -- since r1.0
    , ( "TruncationSize",         0x11, 0x07) -- since r1.0
    , ( "AllOrNone",              0x11, 0x08) -- since r1.0
    , ( "Body",                   0x11, 0x0a) -- since r1.0
    , ( "Data",                   0x11, 0x0b) -- since r1.0
    , ( "EstimatedDataSize",      0x11, 0x0c) -- since r1.0
    , ( "Truncated",              0x11, 0x0d) -- since r1.0
    , ( "Attachments",            0x11, 0x0e) -- since r1.0
    , ( "Attachment",             0x11, 0x0f) -- since r1.0
    , ( "DisplayName",            0x11, 0x10) -- since r1.0
    , ( "FileReference",          0x11, 0x11) -- since r1.0
    , ( "Method",                 0x11, 0x12) -- since r1.0
    , ( "ContentId",              0x11, 0x13) -- since r1.0
    , ( "ContentLocation",        0x11, 0x14) -- r8.0: not used
    , ( "IsInline",               0x11, 0x15) -- since r1.0
    , ( "NativeBodyType",         0x11, 0x16) -- since r1.0
    , ( "ContentType",            0x11, 0x17) -- since r1.0
    , ( "Preview",                0x11, 0x18) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "BodyPartPreference",     0x11, 0x19) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1 or 14
    , ( "BodyPart",               0x11, 0x1a) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1 or 14
    , ( "Status",                 0x11, 0x1b) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1 or 14

    -- Code Page: Settings (since v12.1 and r1.0)
    , ( "Settings",                   0x12, 0x05) -- since r1.0
    , ( "Status",                     0x12, 0x06) -- since r1.0
    , ( "Get",                        0x12, 0x07) -- since r1.0
    , ( "Set",                        0x12, 0x08) -- since r1.0
    , ( "Oof",                        0x12, 0x09) -- since r1.0
    , ( "OofState",                   0x12, 0x0a) -- since r1.0
    , ( "StartTime",                  0x12, 0x0b) -- since r1.0
    , ( "EndTime",                    0x12, 0x0c) -- since r1.0
    , ( "OofMessage",                 0x12, 0x0d) -- since r1.0
    , ( "AppliesToInternal",          0x12, 0x0e) -- since r1.0
    , ( "AppliesToExternalKnown",     0x12, 0x0f) -- since r1.0
    , ( "AppliesToExternalUnknown",   0x12, 0x10) -- since r1.0
    , ( "Enabled",                    0x12, 0x11) -- since r1.0
    , ( "ReplyMessage",               0x12, 0x12) -- since r1.0
    , ( "BodyType",                   0x12, 0x13) -- since r1.0
    , ( "DevicePassword",             0x12, 0x14) -- since r1.0
    , ( "Password",                   0x12, 0x15) -- since r1.0
    , ( "DeviceInformation",          0x12, 0x16) -- since r1.0
    , ( "Model",                      0x12, 0x17) -- since r1.0
    , ( "IMEI",                       0x12, 0x18) -- since r1.0
    , ( "FriendlyName",               0x12, 0x19) -- since r1.0
    , ( "OS",                         0x12, 0x1a) -- since r1.0
    , ( "OSLanguage",                 0x12, 0x1b) -- since r1.0
    , ( "PhoneNumber",                0x12, 0x1c) -- since r1.0
    , ( "UserInformation",            0x12, 0x1d) -- since r1.0
    , ( "EmailAddresses",             0x12, 0x1e) -- since r1.0
    , ( "SmtpAddress",                0x12, 0x1f) -- since r1.0
    , ( "UserAgent",                  0x12, 0x20) -- since r8.0?
    , ( "EnableOutboundSMS",          0x12, 0x21) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "MobileOperator",             0x12, 0x22) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "PrimarySmtpAddress",         0x12, 0x23) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "Accounts",                   0x12, 0x24) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "Account",                    0x12, 0x25) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "AccountId",                  0x12, 0x26) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "AccountName",                0x12, 0x27) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "UserDisplayName",            0x12, 0x28) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "SendDisabled",               0x12, 0x29) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "ihsManagementInformation",   0x12, 0x2b) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1

    -- Code Page: DocumentLibrary (since v12.1 and r1.0)
    , ( "LinkId",             0x13, 0x05) -- since r1.0
    , ( "DisplayName",        0x13, 0x06) -- since r1.0
    , ( "IsFolder",           0x13, 0x07) -- since r1.0
    , ( "CreationDate",       0x13, 0x08) -- since r1.0
    , ( "LastModifiedDate",   0x13, 0x09) -- since r1.0
    , ( "IsHidden",           0x13, 0x0a) -- since r1.0
    , ( "ContentLength",      0x13, 0x0b) -- since r1.0
    , ( "ContentType",        0x13, 0x0c) -- since r1.0

    -- Code Page: ItemOperations (since v12.1 and r1.0)
    , ( "ItemOperations",      0x14, 0x05) -- since r1.0
    , ( "Fetch",               0x14, 0x06) -- since r1.0
    , ( "Store",               0x14, 0x07) -- since r1.0
    , ( "Options",             0x14, 0x08) -- since r1.0
    , ( "Range",               0x14, 0x09) -- since r1.0
    , ( "Total",               0x14, 0x0a) -- since r1.0
    , ( "Properties",          0x14, 0x0b) -- since r1.0
    , ( "Data",                0x14, 0x0c) -- since r1.0
    , ( "Status",              0x14, 0x0d) -- since r1.0
    , ( "Response",            0x14, 0x0e) -- since r1.0
    , ( "Version",             0x14, 0x0f) -- since r1.0
    , ( "Schema",              0x14, 0x10) -- since r1.0
    , ( "Part",                0x14, 0x11) -- since r1.0
    , ( "EmptyFolderContents", 0x14, 0x12) -- since r1.0
    , ( "DeleteSubFolders",    0x14, 0x13) -- since r1.0
    , ( "UserName",            0x14, 0x14) -- since r8.0?
    , ( "Password",            0x14, 0x15) -- since r8.0?
    , ( "Move",                0x14, 0x16) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "DstFldId",            0x14, 0x17) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
--    , ( "ConversationId",      0x14, 0x18, WBXML_TAG_OPTION_BINARY) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "MoveAlways",          0x14, 0x19) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1

    -- Code Page: ComposeMail (since v14.0 and r8.0?)
    -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "SendMail",               0x15, 0x05) -- since r8.0?
    , ( "SmartForward",           0x15, 0x06) -- since r8.0?
    , ( "SmartReply",             0x15, 0x07) -- since r8.0?
    , ( "SaveInSentItems",        0x15, 0x08) -- since r8.0?
    , ( "ReplaceMime",            0x15, 0x09) -- since r8.0?
    , ( "Source",                 0x15, 0x0b) -- since r8.0?
    , ( "FolderId",               0x15, 0x0c) -- since r8.0?
    , ( "ItemId",                 0x15, 0x0d) -- since r8.0?
    , ( "LongId",                 0x15, 0x0e) -- since r8.0?
    , ( "InstanceId",             0x15, 0x0f) -- since r8.0?
--    , ( "MIME",                   0x15, 0x10, WBXML_TAG_OPTION_BINARY) -- since r8.0?
    , ( "ClientId",               0x15, 0x11) -- since r8.0?
    , ( "Status",                 0x15, 0x12) -- since r8.0?
    , ( "AccountId",              0x15, 0x13) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1

    -- Code Page: Email2 (since v14.0 and r8.0?)
    -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "UmCallerID",             0x16, 0x05) -- since r8.0?
    , ( "UmUserNotes",            0x16, 0x06) -- since r8.0?
    , ( "UmAttDuration",          0x16, 0x07) -- since r8.0?
    , ( "UmAttOrder",             0x16, 0x08) -- since r8.0?
--    , ( "ConversationId",         0x16, 0x09, WBXML_TAG_OPTION_BINARY) -- since r8.0?
--    , ( "ConversationIndex",      0x16, 0x0a, WBXML_TAG_OPTION_BINARY) -- since r8.0?
    , ( "LastVerbExecuted",       0x16, 0x0b) -- since r8.0?
    , ( "LastVerbExecutionTime",  0x16, 0x0c) -- since r8.0?
    , ( "ReceivedAsBcc",          0x16, 0x0d) -- since r8.0?
    , ( "Sender",                 0x16, 0x0e) -- since r8.0?
    , ( "CalendarType",           0x16, 0x0f) -- since r8.0?
    , ( "IsLeapMonth",            0x16, 0x10) -- since r8.0?
    , ( "AccountId",              0x16, 0x11) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "FirstDayOfWeek",         0x16, 0x12) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "MeetingMessageType",     0x16, 0x13) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1

    -- Code Page: Notes (since v14.0 and r8.0?)
    -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , ( "Subject",                0x17, 0x05) -- since r8.0?
    , ( "MessageClass",           0x17, 0x06) -- since r8.0?
    , ( "LastModifiedDate",       0x17, 0x07) -- since r8.0?
    , ( "Categories",             0x17, 0x08) -- since r8.0?
    , ( "Category",               0x17, 0x09) -- since r8.0?

    -- Code Page: RightsManagement (since r8.0?)
    -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , ( "RightsManagementSupport",0x18, 0x05) -- since r8.0?
    , ( "RightsManagementTemplates",0x18, 0x06) -- since r8.0?
    , ( "RightsManagementTemplate",0x18, 0x07) -- since r8.0?
    , ( "RightsManagementLicense",0x18, 0x08) -- since r8.0?
    , ( "EditAllowed",            0x18, 0x09) -- since r8.0?
    , ( "ReplyAllowed",           0x18, 0x0a) -- since r8.0?
    , ( "ReplyAllAllowed",        0x18, 0x0b) -- since r8.0?
    , ( "ForwardAllowed",         0x18, 0x0c) -- since r8.0?
    , ( "ModifyRecipientsAllowed",0x18, 0x0d) -- since r8.0?
    , ( "ExtractAllowed",         0x18, 0x0e) -- since r8.0?
    , ( "PrintAllowed",           0x18, 0x0f) -- since r8.0?
    , ( "ExportAllowed",          0x18, 0x10) -- since r8.0?
    , ( "ProgrammaticAccessAllowed",0x18, 0x11) -- since r8.0?
    , ( "RMOwner",                0x18, 0x12) -- since r8.0?
    , ( "ContentExpiryDate",      0x18, 0x13) -- since r8.0?
    , ( "TemplateID",             0x18, 0x14) -- since r8.0?
    , ( "TemplateName",           0x18, 0x15) -- since r8.0?
    , ( "TemplateDescription",    0x18, 0x16) -- since r8.0?
    , ( "ContentOwner",           0x18, 0x17) -- since r8.0?
    , ( "RemoveRightsManagementDistribution",0x18, 0x18 ) -- since r8.0?

    ]
