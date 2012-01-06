----------------------------------------------------------------------
-- |
-- Module : WBXML.Tables
-- Copyright : Mike Limansky, 2011
--
-- Definition of WBXML Tables
--
----------------------------------------------------------------------

module Wbxml.Tables where

import Data.Word --(Word8)
import Data.List (find)

type WbxmlTagTable = [(Word8, [(Word8, String)])]
type WbxmlAttrTable = [(Word8, [(Word8, String, String)])]
type WbxmlAttrValueTable = [(Word8, [(Word8, String)])]

-- |Searches for the tag in provided table by code page and tag code
findTag :: WbxmlTagTable    -- ^ table to search the tag
        -> Word8            -- ^ code page
        -> Word8            -- ^ tag code
        -> Maybe String     -- ^ the return value
findTag t p c = lookup p t >>= lookup c

-- |Searches for the tag page and code in proivded tag by tagname
findCode [] _ = Nothing
findCode (p:ps) tag = case find (\x -> snd x == tag) (snd p) of
    Just (c, _) -> Just (fst p, c)
    Nothing     -> findCode ps tag

-------------------------------------------------------
--    WML 1.3 (WAP 1.2.1: "WAP-191-WML-20000219-a.pdf")
-------------------------------------------------------
wml13TagTable :: WbxmlTagTable
wml13TagTable =
    [ (0x00, [ (0x1c, "a")
             , (0x22, "anchor") -- WML 1.1
             , (0x23, "access")
             , (0x24, "b")
             , (0x25, "big")
             , (0x26, "br")
             , (0x27, "card")
             , (0x28, "do")
             , (0x29, "em")
             , (0x2a, "fieldset")
             , (0x2b, "go")
             , (0x2c, "head")
             , (0x2d, "i")
             , (0x2e, "img")
             , (0x2f, "input")
             , (0x30, "meta")
             , (0x31, "noop")
             , (0x20, "p") -- WML 1.1
             , (0x21, "postfield") -- WML 1.1
             , (0x1b, "pre")
             , (0x32, "prev")
             , (0x33, "onevent")
             , (0x34, "optgroup")
             , (0x35, "option")
             , (0x36, "refresh")
             , (0x37, "select")
             , (0x3e, "setvar") -- WML 1.1
             , (0x38, "small")
             , (0x39, "strong")
             , (0x1f, "table") -- WML 1.1
             , (0x1d, "td") -- WML 1.1
             , (0x3b, "template")
             , (0x3c, "timer")
             , (0x1e, "tr") -- WML 1.1
             , (0x3d, "u")
             , (0x3f, "wml")
             ]
    )]

wml13AttrTable :: WbxmlAttrTable
wml13AttrTable =
    [ (0x00, [ (0x05, "accept-charset", "")
             , (0x06, "align", "bottom")
             , (0x07, "align", "center")
             , (0x08, "align", "left")
             , (0x09, "align", "middle")
             , (0x0a, "align", "right")
             , (0x0b, "align", "top")
             , (0x0c, "alt", "")
             , (0x0d, "content", "") 
             , (0x0f, "domain", "")
             , (0x10, "emptyok", "false")
             , (0x11, "emptyok", "true")
             , (0x12, "format", "")
             , (0x13, "height", "")
             , (0x14, "hspace", "")
             , (0x15, "ivalue", "") -- WML 1.1
             , (0x16, "iname", "") -- WML 1.1
             , (0x18, "label", "")
             , (0x19, "localsrc", "")
             , (0x1a, "maxlength", "")
             , (0x1b, "method", "get")
             , (0x1c, "method", "post")
             , (0x1d, "mode", "nowrap")
             , (0x1e, "mode", "wrap")
             , (0x1f, "multiple", "false")
             , (0x20, "multiple", "true")
             , (0x21, "name", "")
             , (0x22, "newcontext", "false")
             , (0x23, "newcontext", "true")
             , (0x24, "onpick", "") -- WML 1.1
             , (0x25, "onenterbackward", "")
             , (0x26, "onenterforward", "")
             , (0x27, "ontimer", "")
             , (0x28, "optional", "false")
             , (0x29, "optional", "true")
             , (0x2a, "path", "")
             , (0x2e, "scheme", "")
             , (0x2f, "sendreferer", "false")
             , (0x30, "sendreferer", "true")
             , (0x31, "size", "")
             , (0x32, "src", "")
             , (0x33, "ordered", "true") -- WML 1.1
             , (0x34, "ordered", "false") -- WML 1.1
             , (0x35, "tabindex", "")
             , (0x36, "title", "")
             , (0x37, "type", "")
             , (0x38, "type", "accept")
             , (0x39, "type", "delete")
             , (0x3a, "type", "help")
             , (0x3b, "type", "password")
             , (0x3c, "type", "onpick")
             , (0x3d, "type", "onenterbackward")
             , (0x3e, "type", "onenterforward")
             , (0x3f, "type", "ontimer")
             , (0x45, "type", "options")
             , (0x46, "type", "prev")
             , (0x47, "type", "reset")
             , (0x48, "type", "text")
             , (0x49, "type", "vnd.")
             , (0x4a, "href", "") -- WML 1.1
             , (0x4b, "href", "http://") -- WML 1.1
             , (0x4c, "href", "https://") -- WML 1.1
             , (0x4d, "value", "")
             , (0x4e, "vspace", "")
             , (0x4f, "width", "")
             , (0x50, "xml:lang", "")
             , (0x52, "align", "") -- WML 1.1
             , (0x53, "columns", "") -- WML 1.1
             , (0x54, "class", "") -- WML 1.1
             , (0x55, "id", "") -- WML 1.1
             , (0x56, "forua", "false") -- WML 1.1
             , (0x57, "forua", "true") -- WML 1.1
             , (0x58, "src", "http://") -- WML 1.1
             , (0x59, "src", "https://") -- WML 1.1
             , (0x5a, "http-equiv", "") -- WML 1.1
             , (0x5b, "http-equiv", "Content-Type") -- WML 1.1
             , (0x5c, "content", "application/vnd.wap.wmlc;charset=") -- WML 1.1
             , (0x5d, "http-equiv", "Expires") -- WML 1.1
             , (0x5e, "accesskey", "") -- WML 1.2
             , (0x5f, "enctype", "") -- WML 1.2
             , (0x60, "enctype", "application/x-www-form-urlencoded") -- WML 1.2
             , (0x61, "enctype", "multipart/form-data") -- WML 1.2
             , (0x62, "xml:space", "preserve") -- WML 1.3
             , (0x63, "xml:space", "default") -- WML 1.3
             , (0x64, "cache-control", "no-cache") -- WML 1.3
             ]
    )]

wml13AttrValueTable =
    [ (0x00, [ (0x85, ".com/")
             , (0x86, ".edu/")
             , (0x87, ".net/")
             , (0x88, ".org/")
             , (0x89, "accept")
             , (0x8a, "bottom")
             , (0x8b, "clear")
             , (0x8c, "delete")
             , (0x8d, "help")
             , (0x8f, "http://www.")
             , (0x8e, "http://")
             , (0x91, "https://www.")
             , (0x90, "https://")    
             , (0x93, "middle")
             , (0x94, "nowrap")
             , (0x96, "onenterbackward")
             , (0x97, "onenterforward")
             , (0x95, "onpick") -- WML 1.1
             , (0x98, "ontimer")
             , (0x99, "options")
             , (0x9a, "password")
             , (0x9b, "reset")
             , (0x9d, "text")
             , (0x9e, "top")
             , (0x9f, "unknown")
             , (0xa0, "wrap")
             , (0xa1, "www.")
             ]
    )]

--------------------------------------------
--    WTA 1.0 (WAP 1.0: "wta-30-apr-98.pdf")
--------------------------------------------
wta10TagTable :: WbxmlTagTable
wta10TagTable =
    [ (0x00, [ (0x05, "EVENT")
             , (0x06, "EVENTTABLE")
             , (0x07, "TYPE")
             , (0x08, "URL")
             , (0x09, "WTAI")
             ]
    )]

wta10AttrTable =
    [ (0x00, [ (0x05, "NAME", "")
             , (0x06, "VALUE", "")
             ]
    )]

------------------------------------------------
--    WTA WML 1.2 ("WAP-266-WTA-20010908-a.pdf")
------------------------------------------------
wtaWml12TagTable :: WbxmlTagTable
wtaWml12TagTable =
    -- Code Page 0 (WML 1.2)
    [ (0x00, [ (0x1c, "a")
             , (0x22, "anchor")
             , (0x23, "access")
             , (0x24, "b")
             , (0x25, "big")
             , (0x26, "br")
             , (0x27, "card")
             , (0x28, "do")
             , (0x29, "em")
             , (0x2a, "fieldset")
             , (0x2b, "go")
             , (0x2c, "head")
             , (0x2d, "i")
             , (0x2e, "img")
             , (0x2f, "input")
             , (0x30, "meta")
             , (0x31, "noop")
             , (0x20, "p")
             , (0x21, "postfield")
             , (0x1b, "pre")
             , (0x32, "prev")
             , (0x33, "onevent")
             , (0x34, "optgroup")
             , (0x35, "option")
             , (0x36, "refresh")
             , (0x37, "select")
             , (0x3e, "setvar")
             , (0x38, "small")
             , (0x39, "strong")
             , (0x1f, "table")
             , (0x1d, "td")
             , (0x3b, "template")
             , (0x3c, "timer")
             , (0x1e, "tr")
             , (0x3d, "u")
             , (0x3f, "wml")
             ])


    -- Code Page 1 (WTA)
    , (0x01, [ (0x3f, "wta-wml") ]
    )]

wtawml12AttrTable :: WbxmlAttrTable
wtawml12AttrTable =
    -- Code Page 0 (WML 1.2)
    [ (0x00, [ (0x05, "accept-charset", "")
             , (0x06, "align", "bottom")
             , (0x07, "align", "center")
             , (0x08, "align", "left")
             , (0x09, "align", "middle")
             , (0x0a, "align", "right")
             , (0x0b, "align", "top")
             , (0x0c, "alt", "")
             , (0x0d, "content", "")
             , (0x0f, "domain", "")
             , (0x10, "emptyok", "false")
             , (0x11, "emptyok", "true")
             , (0x12, "format", "")
             , (0x13, "height", "")
             , (0x14, "hspace", "")
             , (0x15, "ivalue", "")
             , (0x16, "iname", "")
             , (0x18, "label", "")
             , (0x19, "localsrc", "")
             , (0x1a, "maxlength", "")
             , (0x1b, "method", "get")
             , (0x1c, "method", "post")
             , (0x1d, "mode", "nowrap")
             , (0x1e, "mode", "wrap")
             , (0x1f, "multiple", "false")
             , (0x20, "multiple", "true")
             , (0x21, "name", "")
             , (0x22, "newcontext", "false")
             , (0x23, "newcontext", "true")
             , (0x24, "onpick", "")
             , (0x25, "onenterbackward", "")
             , (0x26, "onenterforward", "")
             , (0x27, "ontimer", "")
             , (0x28, "optional", "false")
             , (0x29, "optional", "true")
             , (0x2a, "path", "")
             , (0x2e, "scheme", "")
             , (0x2f, "sendreferer", "false")
             , (0x30, "sendreferer", "true")
             , (0x31, "size", "")
             , (0x32, "src", "")
             , (0x33, "ordered", "true")
             , (0x34, "ordered", "false")
             , (0x35, "tabindex", "")
             , (0x36, "title", "")
             , (0x37, "type", "")
             , (0x38, "type", "accept")
             , (0x39, "type", "delete")
             , (0x3a, "type", "help")
             , (0x3b, "type", "password")
             , (0x3c, "type", "onpick")
             , (0x3d, "type", "onenterbackward")
             , (0x3e, "type", "onenterforward")
             , (0x3f, "type", "ontimer")
             , (0x45, "type", "options")
             , (0x46, "type", "prev")
             , (0x47, "type", "reset")
             , (0x48, "type", "text")
             , (0x49, "type", "vnd.")
             , (0x4a, "href", "")
             , (0x4b, "href", "http://")
             , (0x4c, "href", "https://")
             , (0x4d, "value", "")
             , (0x4e, "vspace", "")
             , (0x4f, "width", "")
             , (0x50, "xml:lang", "")
             , (0x52, "align", "")
             , (0x53, "columns", "")
             , (0x54, "class", "")
             , (0x55, "id", "")
             , (0x56, "forua", "false")
             , (0x57, "forua", "true")
             , (0x58, "src", "http://")
             , (0x59, "src", "https://")
             , (0x5a, "http-equiv", "")
             , (0x5b, "http-equiv", "Content-Type")
             , (0x5c, "content", "application/vnd.wap.wmlc;charset=")
             , (0x5d, "http-equiv", "Expires")
             , (0x5e, "accesskey", "")
             , (0x5f, "enctype", "")
             , (0x60, "enctype", "application/x-www-form-urlencoded")    
             , (0x61, "enctype", "multipart/form-data")
             ])

    -- Code Page 1 (WTA)
    , (0x01, [ (0x06, "href", "wtai://wp/mc;")
             , (0x07, "href", "wtai://wp/sd;")
             , (0x08, "href", "wtai://wp/ap;")
             , (0x09, "href", "wtai://ms/ec;")
             , (0x05, "href", "wtai://")        
             , (0x12, "type", "wtaev-cc/ic")
             , (0x13, "type", "wtaev-cc/cl")
             , (0x14, "type", "wtaev-cc/co")
             , (0x15, "type", "wtaev-cc/oc")
             , (0x16, "type", "wtaev-cc/cc")
             , (0x17, "type", "wtaev-cc/dtmf")
             , (0x21, "type", "wtaev-nt/it")
             , (0x22, "type", "wtaev-nt/st")
             , (0x20, "type", "wtaev-nt/")
             , (0x30, "type", "wtaev-pb/")
             , (0x38, "type", "wtaev-lg/")
             , (0x51, "type", "wtaev-ms/ns")
             , (0x50, "type", "wtaev-ms/")
             , (0x59, "type", "wtaev-gsm/ru")
             , (0x5a, "type", "wtaev-gsm/ch")
             , (0x5b, "type", "wtaev-gsm/ca")
             , (0x58, "type", "wtaev-gsm/")
             , (0x60, "type", "wtaev-pdc")
             , (0x69, "type", "wtaev-ansi136/ia")
             , (0x6a, "type", "wtaev-ansi136/if")
             , (0x68, "type", "wtaev-ansi136")
             , (0x70, "type", "wtaev-cdma/")
             , (0x11, "type", "wtaev-cc")
             , (0x10, "type", "wtaev-")
             ]
    )]

wtawml12AttrValueTable :: WbxmlAttrValueTable
wtawml12AttrValueTable =
    -- Code Page 0 (WML 1.2)
    [ (0x00, [ (0x85, ".com/")
             , (0x86, ".edu/")
             , (0x87, ".net/")
             , (0x88, ".org/")
             , (0x89, "accept")
             , (0x8a, "bottom")
             , (0x8b, "clear")
             , (0x8c, "delete")
             , (0x8d, "help")
             , (0x8f, "http://www.")
             , (0x8e, "http://")    
             , (0x91, "https://www.")
             , (0x90, "https://")    
             , (0x93, "middle")
             , (0x94, "nowrap")
             , (0x96, "onenterbackward")
             , (0x97, "onenterforward")
             , (0x95, "onpick")
             , (0x98, "ontimer")
             , (0x99, "options")
             , (0x9a, "password")
             , (0x9b, "reset")
             , (0x9d, "text")
             , (0x9e, "top")
             , (0x9f, "unknown")
             , (0xa0, "wrap")
             , (0xa1, "www.")
             ]
    )]

----------------------------------------------------
--    CHANNEL 1.1 (WAP 1.1: "SPEC-WTA-19990716.pdf")
----------------------------------------------------

channel11TagTable :: WbxmlTagTable
channel11TagTable =
    [ (0x00, [ (0x05, "channel")
             , (0x06, "title")
             , (0x07, "abstract")
             , (0x08, "resource")
             ]
    )]

channel11AttrTable :: WbxmlAttrTable
channel11AttrTable =
    [ (0x00, [ (0x05, "maxspace", "")
             , (0x06, "base", "")
             , (0x07, "href", "")
             , (0x08, "href", "http://")
             , (0x09, "href", "https://")
             , (0x0a, "lastmod", "")
             , (0x0b, "etag", "")
             , (0x0c, "md5", "")
             , (0x0d, "success", "")
             , (0x0e, "success", "http://")
             , (0x0f, "success", "https://")
             , (0x10, "failure", "")
             , (0x11, "failure", "http://")
             , (0x12, "failure", "https://")
             , (0x13, "EventId", "")
             ]
    )]

------------------------------------------------
--    CHANNEL 1.2 ("WAP-266-WTA-20010908-a.pdf")
------------------------------------------------

channel12TagTable :: WbxmlTagTable
channel12TagTable =
    [ (0x00, [ (0x05, "channel")
             , (0x06, "title")
             , (0x07, "abstract")
             , (0x08, "resource")
             ]
    )]

channel12AttrTable :: WbxmlAttrTable
channel12AttrTable =
    [ (0x00, [ (0x05, "maxspace", "")
             , (0x06, "base", "")
             , (0x07, "href", "")
             , (0x08, "href", "http://")
             , (0x09, "href", "https://")
             , (0x0a, "lastmod", "")
             , (0x0b, "etag", "")
             , (0x0c, "md5", "")
             , (0x0d, "success", "")
             , (0x0e, "success", "http://")
             , (0x0f, "success", "https://")
             , (0x10, "failure", "")
             , (0x11, "failure", "http://")
             , (0x12, "failure", "https://")
             , (0x13, "eventid", "")
             , (0x14, "eventid", "wtaev-")
             , (0x15, "channelid", "")
             , (0x16, "useraccessible", "")
             ]
    )]

airSyncTagTable :: WbxmlTagTable
airSyncTagTable =
    [ (0x00, [ ( 0x05, "Sync") -- since r1.0
             , ( 0x06, "Responses") -- since r1.0
             , ( 0x07, "Add") -- since r1.0
             , ( 0x08, "Change") -- since r1.0
             , ( 0x09, "Delete") -- since r1.0
             , ( 0x0a, "Fetch") -- since r1.0
             , ( 0x0b, "SyncKey") -- since r1.0
             , ( 0x0c, "ClientId") -- since r1.0
             , ( 0x0d, "ServerId") -- since r1.0
             , ( 0x0e, "Status") -- since r1.0
             , ( 0x0f, "Collection") -- since r1.0
             , ( 0x10, "Class") -- since r1.0
             , ( 0x11, "Version") -- not defined in r8.0 but in r1.0
             , ( 0x12, "CollectionId") -- since r1.0
             , ( 0x13, "GetChanges") -- since r1.0
             , ( 0x14, "MoreAvailable") -- since r1.0
             , ( 0x15, "WindowSize") -- since r1.0
             , ( 0x16, "Commands") -- since r1.0
             , ( 0x17, "Options") -- since r1.0
             , ( 0x18, "FilterType") -- since r1.0
             , ( 0x19, "Truncation") -- not defined in r8.0 but in r1.0
             , ( 0x1a, "RTFTruncation") -- corrected in libwbxml 0.11.0, not defined in r8.0 but in r1.0
             , ( 0x1b, "Conflict") -- since r1.0
             , ( 0x1c, "Collections") -- since r1.0
             , ( 0x1d, "ApplicationData") -- since r1.0
             , ( 0x1e, "DeletesAsMoves") -- since r1.0
             , ( 0x1f, "NotifyGUID") -- not defined in r8.0 but in r1.0
             , ( 0x20, "Supported") -- since r1.0
             , ( 0x21, "SoftDelete") -- since r1.0
             , ( 0x22, "MIMESupport") -- since r1.0
             , ( 0x23, "MIMETruncation") -- since r1.0
             , ( 0x24, "Wait") -- since r1.0
             , ( 0x25, "Limit") -- since r1.0
             , ( 0x26, "Partial") -- since r1.0
             , ( 0x27, "ConversationMode") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x28, "MaxItems") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x29, "HeartbeatInterval") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    ])
    -- Code Page: Contacts (since v2.5 and r1.0)
    , (0x01, [ ( 0x05, "Anniversary") -- since r1.0
             , ( 0x06, "AssistantName") -- since r1.0
             , ( 0x07, "AssistantTelephoneNumber") -- corrected in libwbxml 0.11.0
             , ( 0x08, "Birthday") -- since r1.0
             , ( 0x09, "Body") -- not defined in r8.0 but in r1.0
             , ( 0x0a, "BodySize") -- not defined in r8.0 but in r1.0
             , ( 0x0b, "BodyTruncated") -- not defined in r8.0 but in r1.0
             , ( 0x0c, "Business2PhoneNumber") -- changed in r8.0, r1.0: Business2TelephoneNumber
             , ( 0x0d, "BusinessCity") -- since r1.0
             , ( 0x0e, "BusinessCountry") -- since r1.0
             , ( 0x0f, "BusinessPostalCode") -- since r1.0
             , ( 0x10, "BusinessState") -- since r1.0
             , ( 0x11, "BusinessStreet") -- since r1.0
             , ( 0x12, "BusinessFaxNumber") -- since r1.0
             , ( 0x13, "BusinessPhoneNumber") -- changed in r8.0, r1.0: BusinessTelephoneNumber
             , ( 0x14, "CarPhoneNumber") -- since r1.0
             , ( 0x15, "Categories") -- since r1.0
             , ( 0x16, "Category") -- since r1.0
             , ( 0x17, "Children") -- since r1.0
             , ( 0x18, "Child") -- since r1.0
             , ( 0x19, "CompanyName") -- since r1.0
             , ( 0x1a, "Department") -- since r1.0
             , ( 0x1b, "Email1Address") -- since r1.0
             , ( 0x1c, "Email2Address") -- since r1.0
             , ( 0x1d, "Email3Address") -- since r1.0
             , ( 0x1e, "FileAs") -- since r1.0
             , ( 0x1f, "FirstName") -- since r1.0
             , ( 0x20, "Home2PhoneNumber") -- changed in r8.0, r1.0: BusinessTelephoneNumber
             , ( 0x21, "HomeCity") -- since r1.0
             , ( 0x22, "HomeCountry") -- since r1.0
             , ( 0x23, "HomePostalCode") -- since r1.0
             , ( 0x24, "HomeState") -- since r1.0
             , ( 0x25, "HomeStreet") -- since r1.0
             , ( 0x26, "HomeFaxNumber") -- since r1.0
             , ( 0x27, "HomePhoneNumber") -- changed in r8.0, r1.0: BusinessTelephoneNumber
             , ( 0x28, "JobTitle") -- since r1.0
             , ( 0x29, "LastName") -- since r1.0
             , ( 0x2a, "MiddleName") -- since r1.0
             , ( 0x2b, "MobilePhoneNumber") -- changed in r8.0, r1.0: BusinessTelephoneNumber
             , ( 0x2c, "OfficeLocation") -- since r1.0
             , ( 0x2d, "OtherCity") -- since r1.0
             , ( 0x2e, "OtherCountry") -- since r1.0
             , ( 0x2f, "OtherPostalCode") -- since r1.0
             , ( 0x30, "OtherState") -- since r1.0
             , ( 0x31, "OtherStreet") -- since r1.0
             , ( 0x32, "PagerNumber") -- since r1.0
             , ( 0x33, "RadioPhoneNumber") -- changed in r8.0, r1.0: BusinessTelephoneNumber
             , ( 0x34, "Spouse") -- since r1.0
             , ( 0x35, "Suffix") -- since r1.0
             , ( 0x36, "Title") -- since r1.0
             , ( 0x37, "WebPage") -- since r1.0
             , ( 0x38, "YomiCompanyName") -- since r1.0
             , ( 0x39, "YomiFirstName") -- since r1.0
             , ( 0x3a, "YomiLastName") -- since r1.0
             , ( 0x3b, "CompressedRTF") -- corrected in libwbxml 0.11.0, not defined in r8.0 but in r1.0
             , ( 0x3c, "Picture") -- since r1.0
             , ( 0x3d, "Alias") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x3e, "WeightedRank") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1

    ])
    -- Code Page: Email (since v2.5 and r1.0)
    , (0x02, [ ( 0x05, "Attachment") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x06, "Attachments") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x07, "AttName") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x08, "AttSize") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x09, "AttOId") -- corrected in libwbxml 0.11.0, not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0a, "AttMethod") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0b, "AttRemoved") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0c, "Body") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0d, "BodySize") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0e, "BodyTruncated") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0f, "DateReceived") -- supported since v2.5
             , ( 0x10, "DisplayName") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x11, "DisplayTo") -- supported since v2.5
             , ( 0x12, "Importance") -- supported since v2.5
             , ( 0x13, "MessageClass") -- supported since v2.5
             , ( 0x14, "Subject") -- supported since v2.5
             , ( 0x15, "Read") -- supported since v2.5
             , ( 0x16, "To") -- supported since v2.5
             , ( 0x17, "Cc") -- supported since v2.5
             , ( 0x18, "From") -- supported since v2.5
             , ( 0x19, "Reply-To") -- supported since v2.5
             , ( 0x1a, "AllDayEvent") -- supported since v2.5
             , ( 0x1b, "Categories") -- r1.0: supported by v2.5, v12.0 and 12.1; BUT r8.0: not supported by 12.1
             , ( 0x1c, "Category") -- r1.0: supported by v2.5, v12.0 and 12.1; BUT r8.0: not supported by 12.1
             , ( 0x1d, "DTStamp") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x1e, "EndTime") -- supported since v2.5
             , ( 0x1f, "InstanceType") -- supported since v2.5
             , ( 0x20, "BusyStatus") -- supported since v2.5
             , ( 0x21, "Location") -- supported since v2.5
             , ( 0x22, "MeetingRequest") -- supported since v2.5
             , ( 0x23, "Organizer") -- supported since v2.5
             , ( 0x24, "RecurrenceId") -- supported since v2.5
             , ( 0x25, "Reminder") -- supported since v2.5
             , ( 0x26, "ResponseRequested") -- supported since v2.5
             , ( 0x27, "Recurrences") -- supported since v2.5
             , ( 0x28, "Recurrence") -- supported since v2.5
             , ( 0x29, "Recurrence_Type") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x2a, "Recurrence_Until") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x2b, "Recurrence_Occurrences") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x2c, "Recurrence_Interval") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x2d, "Recurrence_DayOfWeek") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x2e, "Recurrence_DayOfMonth") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x2f, "Recurrence_WeekOfMonth") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x30, "Recurrence_MonthOfYear") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x31, "StartTime") -- supported since v2.5
             , ( 0x32, "Sensitivity") -- supported since v2.5
             , ( 0x33, "TimeZone") -- supported since v2.5
             , ( 0x34, "GlobalObjId") -- supported since v2.5
             , ( 0x35, "ThreadTopic") -- supported since v2.5
             , ( 0x36, "MIMEData") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x37, "MIMETruncated") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x38, "MIMESize") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x39, "InternetCPID") -- supported since v2.5
             , ( 0x3a, "Flag") -- supported since v12.0
             , ( 0x3b, "FlagStatus") -- supported since v12.0
             , ( 0x3c, "ContentClass") -- supported since v12.0
             , ( 0x3d, "FlagType") -- supported since v12.0
             , ( 0x3e, "CompleteTime") -- supported since v12.0
             , ( 0x3f, "DisallowNewTimeProposal") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    ])
             -- Code Page: AirNotify

    {- There are conflicting version informations.
     *
     * r1.0: supported by v2.5, v12.0 and v12.1
     * r8.0: This code page is no longer in use.
     * r8.0: Tokens 05 to 17 have been defined.
     *-}
    , (0x03, [ ( 0x05, "Notify") -- not defined in r8.0 but in r1.0, only supported by v2.0 and v2.5
             , ( 0x06, "Notification") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x07, "Version") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x08, "LifeTime") -- corrected in libwbxml 0.11.0, not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x09, "DeviceInfo") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0a, "Enable") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0b, "Folder") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0c, "ServerId") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0d, "DeviceAddress") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0e, "ValidCarrierProfiles") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0f, "CarrierProfile") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x10, "Status") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x11, "Responses") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x12, "Devices") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x13, "Device") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x14, "Id") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x15, "Expiry") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x16, "NotifyGUID") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x17, "DeviceFriendlyName") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
    ])

    -- Code Page: Calendar (since v2.5 and r1.0)
    , (0x04, [ ( 0x05, "TimeZone") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x06, "AllDayEvent") -- supported since v2.5
             , ( 0x07, "Attendees") -- supported since v2.5
             , ( 0x08, "Attendee") -- supported since v2.5
             , ( 0x09, "Attendee_Email") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x0a, "Attendee_Name") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x0b, "Body") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0c, "BodyTruncated") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x0d, "BusyStatus") -- supported since v2.5
             , ( 0x0e, "Categories") -- supported since v2.5
             , ( 0x0f, "Category") -- supported since v2.5
             , ( 0x10, "Compressed_RTF") -- corrected in libwbxml 0.11.0, not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x11, "DTStamp") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x12, "EndTime") -- supported since v2.5
             , ( 0x13, "Exception") -- supported since v2.5
             , ( 0x14, "Exceptions") -- supported since v2.5
             , ( 0x15, "Exception_Deleted") -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: Exception_IsDeleted
             , ( 0x16, "Exception_StartTime") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x17, "Location") -- supported since v2.5
             , ( 0x18, "MeetingStatus") -- supported since v2.5
             , ( 0x19, "Organizer_Email") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x1a, "Organizer_Name") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x1b, "Recurrence") -- supported since v2.5
             , ( 0x1c, "Recurrence_Type") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x1d, "Recurrence_Until") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x1e, "Recurrence_Occurrences") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x1f, "Recurrence_Interval") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x20, "Recurrence_DayOfWeek") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x21, "Recurrence_DayOfMonth") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x22, "Recurrence_WeekOfMonth") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x23, "Recurrence_MonthOfYear") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x24, "Reminder") -- supported since v2.5
             , ( 0x25, "Sensitivity") -- supported since v2.5
             , ( 0x26, "Subject") -- supported since v2.5
             , ( 0x27, "StartTime") -- supported since v2.5
             , ( 0x28, "UID") -- supported since v2.5
             , ( 0x29, "Attendee_Status") -- corrected in libwbxml 0.11.0, supported since v12.0
             , ( 0x2a, "Attendee_Type") -- corrected in libwbxml 0.11.0, supported since v12.0
             , ( 0x33, "DisallowNewTimeProposal") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x34, "ResponseRequested") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x35, "AppointmentReplyTime") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x36, "ResponseType") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x37, "CalendarType") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x38, "IsLeapMonth") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x39, "FirstDayOfWeek") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x3a, "OnlineMeetingConfLink") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x3b, "OnlineMeetingExternalLink") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    ])
    -- Code Page: Move (since v2.5 and r1.0)
    , (0x05, [ ( 0x05, "MoveItems") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x06, "Move") -- since r1.0
             , ( 0x07, "SrcMsgId") -- since r1.0
             , ( 0x08, "SrcFldId") -- since r1.0
             , ( 0x09, "DstFldId") -- since r1.0
             , ( 0x0a, "Response") -- since r1.0
             , ( 0x0b, "Status") -- since r1.0
             , ( 0x0c, "DstMsgId") -- since r1.0
    ])

    -- Code Page: ItemEstimate (since v2.5 and r1.0)
    , (0x06, [ ( 0x05, "GetItemEstimate") -- since r1.0
             , ( 0x06, "Version") -- r8.0: only supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x07, "Collections") -- since r1.0
             , ( 0x08, "Collection") -- since r1.0
             , ( 0x09, "Class") -- r8.0: only supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x0a, "CollectionId") -- since r1.0
             , ( 0x0b, "DateTime") -- r8.0: only supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x0c, "Estimate") -- since r1.0
             , ( 0x0d, "Response") -- since r1.0
             , ( 0x0e, "Status") -- since r1.0
    ])

             -- Code Page: FolderHierarchy (since v2.5 and r1.0)
    , (0x07, [ ( 0x05, "Folders") -- not defined in r8.0 but in r1.0
             , ( 0x06, "Folder") -- not defined in r8.0 but in r1.0
             , ( 0x07, "DisplayName") -- since r1.0
             , ( 0x08, "ServerId") -- since r1.0
             , ( 0x09, "ParentId") -- since r1.0
             , ( 0x0a, "Type") -- since r1.0
             , ( 0x0b, "Response") -- not defined in r8.0 but in r1.0
             , ( 0x0c, "Status") -- since r1.0
             , ( 0x0d, "ContentClass") -- not defined in r8.0 but in r1.0
             , ( 0x0e, "Changes") -- since r1.0
             , ( 0x0f, "Add") -- since r1.0
             , ( 0x10, "Delete") -- since r1.0
             , ( 0x11, "Update") -- since r1.0
             , ( 0x12, "SyncKey") -- since r1.0
             , ( 0x13, "FolderCreate") -- since r1.0
             , ( 0x14, "FolderDelete") -- since r1.0
             , ( 0x15, "FolderUpdate") -- since r1.0
             , ( 0x16, "FolderSync") -- since r1.0
             , ( 0x17, "Count") -- since r1.0
             , ( 0x18, "Version") -- not defined in r8.0 but in r1.0
    ])

    -- Code Page: MeetingResponse (since v2.5 and r1.0)
    , (0x08, [ ( 0x05, "CalendarId") -- changed in r8.0, r1.0: CallID
             , ( 0x06, "CollectionId") -- since r1.0
             , ( 0x07, "MeetingResponse") -- since r1.0
             , ( 0x08, "RequestId") -- changed in r8.0, r1.0: ReqId
             , ( 0x09, "Request") -- since r1.0
             , ( 0x0a, "Result") -- since r1.0
             , ( 0x0b, "Status") -- since r1.0
             , ( 0x0c, "UserResponse") -- since r1.0
             , ( 0x0d, "Version") -- not defined in r8.0 but in r1.0
             , ( 0x0e, "InstanceId") -- since r8.0?
    ])

    -- Code Page: Tasks (since v2.5 and r1.0)
    , (0x09, [ ( 0x05, "Body") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x06, "BodySize") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x07, "BodyTruncated") -- not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x08, "Categories") -- supported since v2.5
             , ( 0x09, "Category") -- supported since v2.5
             , ( 0x0a, "Complete") -- supported since v2.5
             , ( 0x0b, "DateCompleted") -- supported since v2.5
             , ( 0x0c, "DueDate") -- supported since v2.5
             , ( 0x0d, "UTCDueDate") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x0e, "Importance") -- supported since v2.5
             , ( 0x0f, "Recurrence") -- supported since v2.5
             , ( 0x10, "Recurrence_Type") -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceType
             , ( 0x11, "Recurrence_Start") -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceStart
             , ( 0x12, "Recurrence_Until") -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceUntil
             , ( 0x13, "Recurrence_Occurrences") -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceOccurrences
             , ( 0x14, "Recurrence_Interval") -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceInterval
             , ( 0x15, "Recurrence_DayOfMonth") -- supported since v2.5, changed in r8.0, r1.0: RecurrenceDayOfMonth
             , ( 0x16, "Recurrence_DayOfWeek") -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceDayOfWeek
             , ( 0x15, "Recurrence_DayOfMonth") -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceDayOfMonth
             , ( 0x17, "Recurrence_WeekOfMonth") -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceWeekOfMonth
             , ( 0x18, "Recurrence_MonthOfYear") -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceMonthOfYear
             , ( 0x19, "Recurrence_Regenerate") -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceRegenerate
             , ( 0x1a, "Recurrence_DeadOccur") -- corrected in libwbxml 0.11.0, supported since v2.5, changed in r8.0, r1.0: RecurrenceDeadOccour
             , ( 0x1b, "ReminderSet") -- supported since v2.5
             , ( 0x1c, "ReminderTime") -- supported since v2.5
             , ( 0x1d, "Sensitivity") -- supported since v2.5
             , ( 0x1e, "StartDate") -- supported since v2.5
             , ( 0x1f, "UTCStartDate") -- corrected in libwbxml 0.11.0, supported since v2.5
             , ( 0x20, "Subject") -- supported since v2.5
             , ( 0x21, "CompressedRTF") -- corrected in libwbxml 0.11.0, not defined in r8.0 but in r1.0, supported by v2.5, v12.0 and v12.1
             , ( 0x22, "OrdinalDate") -- supported since v12.0
             , ( 0x23, "SubOrdinalDate") -- supported since v12.0
             , ( 0x24, "CalendarType") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x25, "IsLeapMonth") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x26, "FirstDayOfWeek") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    ])

    -- Code Page: ResolveRecipients (since v2.5 and r1.0)
    , (0x0a, [ ( 0x05, "ResolveRecipients") -- since r1.0
             , ( 0x06, "Response") -- since r1.0
             , ( 0x07, "Status") -- since r1.0
             , ( 0x08, "Type") -- since r1.0
             , ( 0x09, "Recipient") -- since r1.0
             , ( 0x0a, "DisplayName") -- since r1.0
             , ( 0x0b, "EmailAddress") -- since r1.0
             , ( 0x0c, "Certificates") -- since r1.0
             , ( 0x0d, "Certificate") -- since r1.0
             , ( 0x0e, "MiniCertificate") -- since r1.0
             , ( 0x0f, "Options") -- since r1.0
             , ( 0x10, "To") -- since r1.0
             , ( 0x11, "CertificateRetrieval") -- since r1.0
             , ( 0x12, "RecipientCount") -- since r1.0
             , ( 0x13, "MaxCertificates") -- since r1.0
             , ( 0x14, "MaxAmbiguousRecipients") -- since r1.0
             , ( 0x15, "CertificateCount") -- since r1.0
             , ( 0x16, "Availability") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x17, "StartTime") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x18, "EndTime") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x19, "MergedFreeBusy") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x1a, "Picture") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x1b, "MaxSize") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x1c, "Data") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x1d, "MaxPictures") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    ])

    -- Code Page: ValidateCert (since v2.5 and r1.0)
    , (0x0b, [ ( 0x05, "ValidateCert") -- since r1.0
             , ( 0x06, "Certificates") -- since r1.0
             , ( 0x07, "Certificate") -- since r1.0
             , ( 0x08, "CertificateChain") -- since r1.0
             , ( 0x09, "CheckCRL") -- since r1.0
             , ( 0x0a, "Status") -- since r1.0
    ])

    -- Code Page: Contacts2 (since v2.5 and r1.0)
    , (0x0c, [ ( 0x05, "CustomerId") -- since r1.0
             , ( 0x06, "GovernmentId") -- since r1.0
             , ( 0x07, "IMAddress") -- since r1.0
             , ( 0x08, "IMAddress2") -- since r1.0
             , ( 0x09, "IMAddress3") -- since r1.0
             , ( 0x0a, "ManagerName") -- since r1.0
             , ( 0x0b, "CompanyMainPhone") -- since r1.0
             , ( 0x0c, "AccountName") -- since r1.0
             , ( 0x0d, "NickName") -- since r1.0
             , ( 0x0e, "MMS") -- since r1.0
    ])

    -- Code Page: Ping (since v2.5 and r1.0)
    , (0x0d, [ ( 0x05, "Ping") -- since r1.0
             , ( 0x06, "AutdState") -- not used by protocol
             , ( 0x07, "Status") -- since r1.0
             , ( 0x08, "HeartbeatInterval") -- since r1.0
             , ( 0x09, "Folders") -- since r1.0
             , ( 0x0a, "Folder") -- since r1.0
             , ( 0x0b, "Id") -- since r1.0
             , ( 0x0c, "Class") -- since r1.0
             , ( 0x0d, "MaxFolders") -- since r1.0
    ])

    -- Code Page: Provision (since v2.5 and r1.0)
    , (0x0e, [ ( 0x05, "Provision") -- supported since v2.5
             , ( 0x06, "Policies") -- supported since v2.5
             , ( 0x07, "Policy") -- supported since v2.5
             , ( 0x08, "PolicyType") -- supported since v2.5
             , ( 0x09, "PolicyKey") -- supported since v2.5
             , ( 0x0a, "Data") -- supported since v2.5
             , ( 0x0b, "Status") -- supported since v2.5
             , ( 0x0c, "RemoteWipe") -- supported since v2.5
             , ( 0x0d, "EASProvisionDoc") -- supported since v12.0
             , ( 0x0e, "DevicePasswordEnabled") -- supported since v12.0
             , ( 0x0f, "AlphanumericDevicePasswordRequired") -- supported since v12.0
             , ( 0x10, "DeviceEncryptionEnabled") -- r1.0: supported since v12.0
             , ( 0x10, "RequireStorageCardEncryption") -- r1.0: supported by v2.0 and v2.5
             , ( 0x11, "PasswordRecoveryEnabled") -- supported since v12.0
             , ( 0x12, "DocumentBrowseEnabled") -- supported since v12.0, not defined in r8.0 but in r1.0
             , ( 0x13, "AttachmentsEnabled") -- supported since v12.0
             , ( 0x14, "MinDevicePasswordLength") -- supported since v12.0
             , ( 0x15, "MaxInactivityTimeDeviceLock") -- supported since v12.0
             , ( 0x16, "MaxDevicePasswordFailedAttempts") -- supported since v12.0
             , ( 0x17, "MaxAttachmentSize") -- supported since v12.0
             , ( 0x18, "AllowSimpleDevicePassword") -- supported since v12.0
             , ( 0x19, "DevicePasswordExpiration") -- supported since v12.0
             , ( 0x1a, "DevicePasswordHistory") -- supported since v12.0
             , ( 0x1b, "AllowStorageCard") -- supported since v12.1
             , ( 0x1c, "AllowCamera") -- supported by v2.0 and v2.5
             , ( 0x1d, "RequireDeviceEncryption") -- supported by v2.0 and v2.5
             , ( 0x1e, "AllowUnsignedApplications") -- supported by v2.0 and v2.5
             , ( 0x1f, "AllowUnsignedInstallationPackages") -- supported by v2.0 and v2.5
             , ( 0x20, "MinDevicePasswordComplexCharacters") -- supported by v2.0 and v2.5
             , ( 0x21, "AllowWiFi") -- supported by v2.0 and v2.5
             , ( 0x22, "AllowTextMessaging") -- supported by v2.0 and v2.5
             , ( 0x23, "AllowPOPIMAPEmail") -- supported by v2.0 and v2.5
             , ( 0x24, "AllowBluetooth") -- supported by v2.0 and v2.5
             , ( 0x25, "AllowIrDA") -- supported by v2.0 and v2.5
             , ( 0x26, "RequireManualSyncWhenRoaming") -- supported by v2.0 and v2.5
             , ( 0x27, "AllowDesktopSync") -- supported by v2.0 and v2.5
             , ( 0x28, "MaxCalendarAgeFilter") -- supported by v2.0 and v2.5
             , ( 0x29, "AllowHTMLEmail") -- supported by v2.0 and v2.5
             , ( 0x2a, "MaxEmailAgeFilter") -- supported by v2.0 and v2.5
             , ( 0x2b, "MaxEmailBodyTruncationSize") -- supported by v2.0 and v2.5
             , ( 0x2c, "MaxEmailHTMLBodyTruncationSize") -- supported by v2.0 and v2.5
             , ( 0x2d, "RequireSignedSMIMEMessages") -- supported by v2.0 and v2.5
             , ( 0x2e, "RequireEncryptedSMIMEMessages") -- supported by v2.0 and v2.5
             , ( 0x2f, "RequireSignedSMIMEAlgorithm") -- supported by v2.0 and v2.5
             , ( 0x30, "RequireEncryptionSMIMEAlgorithm") -- supported by v2.0 and v2.5
             , ( 0x31, "AllowSMIMEEncryptionAlgorithmNegotiation") -- supported by v2.0 and v2.5
             , ( 0x32, "AllowSMIMESoftCerts") -- supported by v2.0 and v2.5
             , ( 0x33, "AllowBrowser") -- supported by v2.0 and v2.5
             , ( 0x34, "AllowConsumerEmail") -- supported by v2.0 and v2.5
             , ( 0x35, "AllowRemoteDesktop") -- supported by v2.0 and v2.5
             , ( 0x36, "AllowInternetSharing") -- supported by v2.0 and v2.5
             , ( 0x37, "UnapprovedInROMApplicationList") -- supported by v2.0 and v2.5
             , ( 0x38, "ApplicationName") -- supported by v2.0 and v2.5
             , ( 0x39, "ApprovedApplicationList") -- supported by v2.0 and v2.5
             , ( 0x3a, "Hash") -- supported by v2.0 and v2.5
    ])

    -- Code Page: Search (since v2.5 and r1.0)
    -- Token 0x06 and 0x16 are not supported.
    , (0x0f, [ ( 0x05, "Search") -- supported since v2.5
             , ( 0x07, "Store") -- supported since v2.5
             , ( 0x08, "Name") -- supported since v2.5
             , ( 0x09, "Query") -- supported since v2.5
             , ( 0x0a, "Options") -- supported since v2.5
             , ( 0x0b, "Range") -- supported since v2.5
             , ( 0x0c, "Status") -- supported since v2.5
             , ( 0x0d, "Response") -- supported since v2.5
             , ( 0x0e, "Result") -- supported since v2.5
             , ( 0x0f, "Properties") -- supported since v2.5
             , ( 0x10, "Total") -- supported since v2.5
             , ( 0x11, "EqualTo") -- supported since v12.0
             , ( 0x12, "Value") -- supported since v12.0
             , ( 0x13, "And") -- supported since v12.0
             , ( 0x14, "Or") -- supported since v12.0, r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x15, "FreeText") -- supported since v12.0
             , ( 0x17, "DeepTraversal") -- supported since v12.0
             , ( 0x18, "LongId") -- supported since v12.0
             , ( 0x19, "RebuildResults") -- supported since v12.0
             , ( 0x1a, "LessThan") -- supported since v12.0
             , ( 0x1b, "GreaterThan") -- supported since v12.0
             , ( 0x1c, "Schema") -- supported since v12.0, r8.0: not defined in r8.0 but in r1.0
             , ( 0x1d, "Supported") -- supported since v12.0, r8.0: not defined in r8.0 but in r1.0
             , ( 0x1e, "UserName") -- since 8.0?
             , ( 0x1f, "Password") -- since 8.0?
--    , ( 0x20, "ConversationId", WBXML_TAG_OPTION_BINARY) -- since 8.0?
             , ( 0x21, "Picture") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x22, "MaxSize") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x23, "MaxPictures") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    ])

    -- Code Page: GAL (since v2.5 and r1.0)
    , (0x10, [ ( 0x05, "DisplayName") -- since r1.0
             , ( 0x06, "Phone") -- since r1.0
             , ( 0x07, "Office") -- since r1.0
             , ( 0x08, "Title") -- since r1.0
             , ( 0x09, "Company") -- since r1.0
             , ( 0x0a, "Alias") -- since r1.0
             , ( 0x0b, "FirstName") -- since r1.0
             , ( 0x0c, "LastName") -- since r1.0
             , ( 0x0d, "HomePhone") -- since r1.0
             , ( 0x0e, "MobilePhone") -- since r1.0
             , ( 0x0f, "EmailAddress") -- since r1.0
             , ( 0x10, "Picture") -- not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x11, "Status") -- not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x12, "Data") -- not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    ])

    -- Code Page: AirSyncBase (since v12.0 and r1.0)
    , (0x11, [ ( 0x05, "BodyPreference") -- since r1.0
             , ( 0x06, "Type") -- since r1.0
             , ( 0x07, "TruncationSize") -- since r1.0
             , ( 0x08, "AllOrNone") -- since r1.0
             , ( 0x0a, "Body") -- since r1.0
             , ( 0x0b, "Data") -- since r1.0
             , ( 0x0c, "EstimatedDataSize") -- since r1.0
             , ( 0x0d, "Truncated") -- since r1.0
             , ( 0x0e, "Attachments") -- since r1.0
             , ( 0x0f, "Attachment") -- since r1.0
             , ( 0x10, "DisplayName") -- since r1.0
             , ( 0x11, "FileReference") -- since r1.0
             , ( 0x12, "Method") -- since r1.0
             , ( 0x13, "ContentId") -- since r1.0
             , ( 0x14, "ContentLocation") -- r8.0: not used
             , ( 0x15, "IsInline") -- since r1.0
             , ( 0x16, "NativeBodyType") -- since r1.0
             , ( 0x17, "ContentType") -- since r1.0
             , ( 0x18, "Preview") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x19, "BodyPartPreference") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1 or 14
             , ( 0x1a, "BodyPart") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1 or 14
             , ( 0x1b, "Status") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1 or 14
    ])

    -- Code Page: Settings (since v12.1 and r1.0)
    , (0x12, [ ( 0x05, "Settings") -- since r1.0
             , ( 0x06, "Status") -- since r1.0
             , ( 0x07, "Get") -- since r1.0
             , ( 0x08, "Set") -- since r1.0
             , ( 0x09, "Oof") -- since r1.0
             , ( 0x0a, "OofState") -- since r1.0
             , ( 0x0b, "StartTime") -- since r1.0
             , ( 0x0c, "EndTime") -- since r1.0
             , ( 0x0d, "OofMessage") -- since r1.0
             , ( 0x0e, "AppliesToInternal") -- since r1.0
             , ( 0x0f, "AppliesToExternalKnown") -- since r1.0
             , ( 0x10, "AppliesToExternalUnknown") -- since r1.0
             , ( 0x11, "Enabled") -- since r1.0
             , ( 0x12, "ReplyMessage") -- since r1.0
             , ( 0x13, "BodyType") -- since r1.0
             , ( 0x14, "DevicePassword") -- since r1.0
             , ( 0x15, "Password") -- since r1.0
             , ( 0x16, "DeviceInformation") -- since r1.0
             , ( 0x17, "Model") -- since r1.0
             , ( 0x18, "IMEI") -- since r1.0
             , ( 0x19, "FriendlyName") -- since r1.0
             , ( 0x1a, "OS") -- since r1.0
             , ( 0x1b, "OSLanguage") -- since r1.0
             , ( 0x1c, "PhoneNumber") -- since r1.0
             , ( 0x1d, "UserInformation") -- since r1.0
             , ( 0x1e, "EmailAddresses") -- since r1.0
             , ( 0x1f, "SmtpAddress") -- since r1.0
             , ( 0x20, "UserAgent") -- since r8.0?
             , ( 0x21, "EnableOutboundSMS") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x22, "MobileOperator") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x23, "PrimarySmtpAddress") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x24, "Accounts") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x25, "Account") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x26, "AccountId") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x27, "AccountName") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x28, "UserDisplayName") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x29, "SendDisabled") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x2b, "ihsManagementInformation") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    ])

    -- Code Page: DocumentLibrary (since v12.1 and r1.0)
    , (0x13, [ ( 0x05, "LinkId") -- since r1.0
             , ( 0x06, "DisplayName") -- since r1.0
             , ( 0x07, "IsFolder") -- since r1.0
             , ( 0x08, "CreationDate") -- since r1.0
             , ( 0x09, "LastModifiedDate") -- since r1.0
             , ( 0x0a, "IsHidden") -- since r1.0
             , ( 0x0b, "ContentLength") -- since r1.0
             , ( 0x0c, "ContentType") -- since r1.0
    ])

    -- Code Page: ItemOperations (since v12.1 and r1.0)
    , (0x14, [ ( 0x05, "ItemOperations") -- since r1.0
             , ( 0x06, "Fetch") -- since r1.0
             , ( 0x07, "Store") -- since r1.0
             , ( 0x08, "Options") -- since r1.0
             , ( 0x09, "Range") -- since r1.0
             , ( 0x0a, "Total") -- since r1.0
             , ( 0x0b, "Properties") -- since r1.0
             , ( 0x0c, "Data") -- since r1.0
             , ( 0x0d, "Status") -- since r1.0
             , ( 0x0e, "Response") -- since r1.0
             , ( 0x0f, "Version") -- since r1.0
             , ( 0x10, "Schema") -- since r1.0
             , ( 0x11, "Part") -- since r1.0
             , ( 0x12, "EmptyFolderContents") -- since r1.0
             , ( 0x13, "DeleteSubFolders") -- since r1.0
             , ( 0x14, "UserName") -- since r8.0?
             , ( 0x15, "Password") -- since r8.0?
             , ( 0x16, "Move") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x17, "DstFldId") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
--    , ( 0x18, "ConversationId", WBXML_TAG_OPTION_BINARY) -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
             , ( 0x19, "MoveAlways") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    ])

    -- Code Page: ComposeMail (since v14.0 and r8.0?)
    -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , (0x15, [ ( 0x05, "SendMail") -- since r8.0?
             , ( 0x06, "SmartForward") -- since r8.0?
             , ( 0x07, "SmartReply") -- since r8.0?
             , ( 0x08, "SaveInSentItems") -- since r8.0?
             , ( 0x09, "ReplaceMime") -- since r8.0?
             , ( 0x0b, "Source") -- since r8.0?
             , ( 0x0c, "FolderId") -- since r8.0?
             , ( 0x0d, "ItemId") -- since r8.0?
             , ( 0x0e, "LongId") -- since r8.0?
             , ( 0x0f, "InstanceId") -- since r8.0?
--    , ( 0x10, "MIME", WBXML_TAG_OPTION_BINARY) -- since r8.0?
             , ( 0x11, "ClientId") -- since r8.0?
             , ( 0x12, "Status") -- since r8.0?
             , ( 0x13, "AccountId") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    ])

    -- Code Page: Email2 (since v14.0 and r8.0?)
    -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , (0x16, [ ( 0x05, "UmCallerID") -- since r8.0?
             , ( 0x06, "UmUserNotes") -- since r8.0?
             , ( 0x07, "UmAttDuration") -- since r8.0?
             , ( 0x08, "UmAttOrder") -- since r8.0?
--    , ( 0x09, "ConversationId", WBXML_TAG_OPTION_BINARY) -- since r8.0?
--    , ( 0x0a, "ConversationIndex", WBXML_TAG_OPTION_BINARY) -- since r8.0?
             , ( 0x0b, "LastVerbExecuted") -- since r8.0?
             , ( 0x0c, "LastVerbExecutionTime") -- since r8.0?
             , ( 0x0d, "ReceivedAsBcc") -- since r8.0?
             , ( 0x0e, "Sender") -- since r8.0?
             , ( 0x0f, "CalendarType") -- since r8.0?
             , ( 0x10, "IsLeapMonth") -- since r8.0?
             , ( 0x11, "AccountId") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x12, "FirstDayOfWeek") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
             , ( 0x13, "MeetingMessageType") -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    ])

    -- Code Page: Notes (since v14.0 and r8.0?)
    -- r8.0: not supported when the MS-ASProtocolVersion header is set to 12.1
    , (0x17, [ ( 0x05, "Subject") -- since r8.0?
             , ( 0x06, "MessageClass") -- since r8.0?
             , ( 0x07, "LastModifiedDate") -- since r8.0?
             , ( 0x08, "Categories") -- since r8.0?
             , ( 0x09, "Category") -- since r8.0?
    ])

    -- Code Page: RightsManagement (since r8.0?)
    -- r8.0: not supported when the MS-ASProtocolVersion header is set to 14.0 or 12.1
    , (0x18, [ ( 0x05, "RightsManagementSupport") -- since r8.0?
             , ( 0x06, "RightsManagementTemplates") -- since r8.0?
             , ( 0x07, "RightsManagementTemplate") -- since r8.0?
             , ( 0x08, "RightsManagementLicense") -- since r8.0?
             , ( 0x09, "EditAllowed") -- since r8.0?
             , ( 0x0a, "ReplyAllowed") -- since r8.0?
             , ( 0x0b, "ReplyAllAllowed") -- since r8.0?
             , ( 0x0c, "ForwardAllowed") -- since r8.0?
             , ( 0x0d, "ModifyRecipientsAllowed") -- since r8.0?
             , ( 0x0e, "ExtractAllowed") -- since r8.0?
             , ( 0x0f, "PrintAllowed") -- since r8.0?
             , ( 0x10, "ExportAllowed") -- since r8.0?
             , ( 0x11, "ProgrammaticAccessAllowed") -- since r8.0?
             , ( 0x12, "RMOwner") -- since r8.0?
             , ( 0x13, "ContentExpiryDate") -- since r8.0?
             , ( 0x14, "TemplateID") -- since r8.0?
             , ( 0x15, "TemplateName") -- since r8.0?
             , ( 0x16, "TemplateDescription") -- since r8.0?
             , ( 0x17, "ContentOwner") -- since r8.0?
             , ( 0x18, "RemoveRightsManagementDistribution" ) -- since r8.0?
    ])
    ]
