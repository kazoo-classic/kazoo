# Accounts

## About Accounts

#### Schema

Accounts represent tenants or customers on the system. Each account represents an individual dataset or sandbox that only one tenant can access. The data set is architecturally independent from other tenants.



Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`blacklists.[]` |   | `string()` |   | `false` |  
`blacklists` | A list blacklist ids that apply to the account | `array(string())` |   | `false` |  
`call_recording.account` | endpoint recording settings | [#/definitions/call_recording](#call_recording) |   | `false` |  
`call_recording.endpoint` | endpoint recording settings | [#/definitions/call_recording](#call_recording) |   | `false` |  
`call_recording` | call recording configuration | `object()` |   | `false` |  
`call_restriction` | Account level call restrictions for each available number classification | `object()` | `{}` | `false` |  
`call_waiting` | Parameters for server-side call waiting | [#/definitions/call_waiting](#call_waiting) |   | `false` |  
`caller_id` | The account default caller ID parameters | [#/definitions/caller_id](#caller_id) |   | `false` |  
`caller_id_options.outbound_privacy` | Determines what appears as caller id for offnet outbound calls. Values: full - hides name and number; name - hides only name; number - hides only number; none - hides nothing | `string('full' | 'name' | 'number' | 'none')` |   | `false` |  
`caller_id_options.show_rate` | Whether to show the rate | `boolean()` |   | `false` |  
`caller_id_options` | custom properties for configuring caller_id | `object()` |   | `false` |  
`dial_plan` | A list of default rules used to modify dialed numbers | [#/definitions/dialplans](#dialplans) |   | `false` |  
`do_not_disturb.enabled` | The default value for do-not-disturb | `boolean()` |   | `false` |  
`do_not_disturb` |   | `object()` |   | `false` |  
`enabled` | Determines if the account is currently enabled | `boolean()` | `true` | `false` | `supported`
`flags.[]` |   | `string()` |   | `false` | `supported`
`flags` | Flags set by external applications | `array(string())` |   | `false` | `supported`
`formatters` | Schema for request formatters | [#/definitions/formatters](#formatters) |   | `false` |  
`language` | The language for this account | `string()` |   | `false` | `supported`
`metaflows` | Actions applied to a call outside of the normal callflow, initiated by the caller(s) | [#/definitions/metaflows](#metaflows) |   | `false` |  
`music_on_hold.media_id` | The ID of a media object that should be used as the default music on hold | `string(0..2048)` |   | `false` |  
`music_on_hold` | The default music on hold parameters | `object()` | `{}` | `false` |  
`name` | A friendly name for the account | `string(1..128)` |   | `true` | `supported`
`notifications.first_occurrence.sent_initial_call` | has the account made their first call | `boolean()` | `false` | `false` |  
`notifications.first_occurrence.sent_initial_registration` | has the account registered their first device | `boolean()` | `false` | `false` |  
`notifications.first_occurrence` | send emails on these account-firsts | `object()` |   | `false` |  
`notifications.low_balance.enabled` | should the account be checked for this alert | `boolean()` |   | `false` |  
`notifications.low_balance.last_notification` | Timestamp, in Gregorian seconds, of when the last low_balance alert was sent | `integer()` |   | `false` |  
`notifications.low_balance.sent_low_balance` | has the alert been sent (avoids duplication/spamming) | `boolean()` |   | `false` |  
`notifications.low_balance.threshold` | account balance to send alert on | `number()` |   | `false` |  
`notifications.low_balance` | Low balance settings | `object()` |   | `false` |  
`notifications` | account notification settings | `object()` |   | `false` |  
`org` | Full legal name of the organization | `string()` |   | `false` |  
`preflow.always` | The ID of a callflow to always execute prior to processing the callflow with numbers/patterns matching the request | `string()` |   | `false` |  
`preflow` | Each property provides functionality that can be applied to calls using the callflow application | `object()` | `{}` | `false` |  
`realm` | The realm of the account, ie: 'account1.2600hz.com' | `string(4..253)` |   | `false` | `supported`
`ringtones.external` | The alert info SIP header added when the call is from internal sources | `string(0..256)` |   | `false` |  
`ringtones.internal` | The alert info SIP header added when the call is from external sources | `string(0..256)` |   | `false` |  
`ringtones` | Ringtone Parameters | `object()` | `{}` | `false` |  
`timezone` | The default timezone | `string(5..32)` |   | `false` | `supported`
`topup.threshold` | The account balance when topup occurs | `number()` |   | `false` |  
`topup` | Topup settings for the account | `object()` |   | `false` |  
`voicemail.notify.callback` | Schema for a callback options | [#/definitions/notify.callback](#notifycallback) |   | `false` |  
`voicemail.notify` |   | `object()` |   | `false` |  
`voicemail` |   | `object()` |   | `false` |  
`zones` | A priority ordered mapping of zones for the account | `object()` |   | `false` |  

### call_recording

endpoint recording settings


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`any` | settings for any calls to/from the endpoint | [#/definitions/call_recording.source](#call_recordingsource) |   | `false` |  
`inbound` | settings for inbound calls to the endpoint | [#/definitions/call_recording.source](#call_recordingsource) |   | `false` |  
`outbound` | settings for outbound calls from the endpoint | [#/definitions/call_recording.source](#call_recordingsource) |   | `false` |  

### call_recording.parameters


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`enabled` | is recording enabled | `boolean()` |   | `false` |  
`format` | What format to store the recording on disk | `string('mp3' | 'wav')` |   | `false` |  
`record_min_sec` | The minimum length, in seconds, the recording must be to be considered successful. Otherwise it is deleted | `integer()` |   | `false` |  
`record_on_answer` | Recording should start on answer | `boolean()` |   | `false` |  
`record_on_bridge` | Recording should start on bridge | `boolean()` |   | `false` |  
`record_sample_rate` | What sampling rate to use on the recording | `integer()` |   | `false` |  
`time_limit` | Time limit, in seconds, for the recording | `integer()` |   | `false` |  
`url` | The URL to use when sending the recording for storage | `string()` |   | `false` |  

### call_recording.source


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`any` | settings for calls from any network | [#/definitions/call_recording.parameters](#call_recordingparameters) |   | `false` |  
`offnet` | settings for calls from offnet networks | [#/definitions/call_recording.parameters](#call_recordingparameters) |   | `false` |  
`onnet` | settings for calls from onnet networks | [#/definitions/call_recording.parameters](#call_recordingparameters) |   | `false` |  

### call_waiting

Parameters for server-side call waiting


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`enabled` | Determines if server side call waiting is enabled/disabled | `boolean()` |   | `false` |  

### caller_id

Defines caller ID settings based on the type of call being made


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`asserted.name` | The asserted identity name for the object type | `string(0..35)` |   | `false` |  
`asserted.number` | The asserted identity number for the object type | `string(0..35)` |   | `false` |  
`asserted.realm` | The asserted identity realm for the object type | `string()` |   | `false` |  
`asserted` | Used to convey the proven identity of the originator of a request within a trusted network. | `object()` |   | `false` |  
`emergency.name` | The caller id name for the object type | `string(0..35)` |   | `false` |  
`emergency.number` | The caller id number for the object type | `string(0..35)` |   | `false` |  
`emergency` | The caller ID used when a resource is flagged as 'emergency' | `object()` |   | `false` |  
`external.name` | The caller id name for the object type | `string(0..35)` |   | `false` |  
`external.number` | The caller id number for the object type | `string(0..35)` |   | `false` |  
`external` | The default caller ID used when dialing external numbers | `object()` |   | `false` |  
`internal.name` | The caller id name for the object type | `string(0..35)` |   | `false` |  
`internal.number` | The caller id number for the object type | `string(0..35)` |   | `false` |  
`internal` | The default caller ID used when dialing internal extensions | `object()` |   | `false` |  

### dialplans

Permit local dialing by converting the dialed number to a routable form


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`system.[]` |   | `string()` |   | `false` |  
`system` | List of system dial plans | `array(string())` |   | `false` |  

### formatters

Schema for request formatters


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`^[[:alnum:]_]+$` | Key to match in the route request JSON | `array([#/definitions/formatters.format_options](#formattersformat_options)) | [#/definitions/formatters.format_options](#formattersformat_options)` |   | `false` |  

### formatters.format_options

Schema for formatter options


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`direction` | Only apply the formatter on the relevant request direction | `string('inbound' | 'outbound' | 'both')` |   | `false` |  
`match_invite_format` | Applicable on fields with SIP URIs. Will format the username portion to match the invite format of the outbound request. | `boolean()` |   | `false` |  
`prefix` | Prepends value against the result of a successful regex match | `string()` |   | `false` |  
`regex` | Matches against the value, with optional capture group | `string()` |   | `false` |  
`strip` | If set to true, the field will be stripped from the payload | `boolean()` |   | `false` |  
`suffix` | Appends value against the result of a successful regex match | `string()` |   | `false` |  
`value` | Replaces the current value with the static value defined | `string()` |   | `false` |  

### metaflow

A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`children./.+/` | A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to | [#/definitions/metaflow](#metaflow) |   | `false` |  
`children` | Children metaflows | `object()` |   | `false` |  
`data` | The data/arguments of the metaflow module | `object()` | `{}` | `false` |  
`module` | The name of the metaflow module to execute at this node | `string(1..64)` |   | `true` |  

### metaflows

Actions applied to a call outside of the normal callflow, initiated by the caller(s)


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`binding_digit` | What DTMF will trigger the collection and analysis of the subsequent DTMF sequence | `string('1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' | '0' | '*' | '#')` | `*` | `false` |  
`digit_timeout` | How long to wait between DTMF presses before processing the collected sequence (milliseconds) | `integer()` |   | `false` |  
`listen_on` | Which leg(s) of the call to listen for DTMF | `string('both' | 'self' | 'peer')` |   | `false` |  
`numbers./^[0-9]+$/` | A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to | [#/definitions/metaflow](#metaflow) |   | `false` |  
`numbers` | A list of static numbers with their flows | `object()` |   | `false` |  
`patterns./.+/` | A metaflow node defines a module to execute, data to provide to that module, and one or more children to branch to | [#/definitions/metaflow](#metaflow) |   | `false` |  
`patterns` | A list of patterns with their flows | `object()` |   | `false` |  

### notify.callback

Schema for a callback options


Key | Description | Type | Default | Required | Support Level
--- | ----------- | ---- | ------- | -------- | -------------
`attempts` | How many attempts without answer will system do | `integer()` |   | `false` |  
`disabled` | Determines if the system will call to callback number | `boolean()` |   | `false` |  
`interval_s` | How long will system wait between call back notification attempts | `integer()` |   | `false` |  
`number` | Number for callback notifications about new messages | `string()` |   | `false` |  
`schedule` | Schedules interval between callbacks | `array(integer())` |   | `false` |  
`timeout_s` | How long will system wait for answer to callback | `integer()` |   | `false` |  



## Create

> PUT /v2/accounts

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

## Patch

> PATCH /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X PATCH \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/parents

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/parents
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/tree

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/tree
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/api_key

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/api_key
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/api_key

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/api_key
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/siblings

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/siblings
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/descendants

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/descendants
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/children

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/children
```

## Create

> PUT /v2/accounts/{ACCOUNT_ID}/reseller

```shell
curl -v -X PUT \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/reseller
```

## Remove

> DELETE /v2/accounts/{ACCOUNT_ID}/reseller

```shell
curl -v -X DELETE \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/reseller
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/move

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/move
```

