# Call Channels

## About Call Channels

The Channels API allows queries to find active channels for an account, a user, or a device. Given a call-id for a channel, a limited set of commands are allowed to be executed against that channel (such as hangup, transfer, or play media).

NOTE: Konami is an outdated and unsupported 2600Hz module. If you need support on this module, please ensure you are signed up for Konami Pro.

## Fetch active channels system wide.

!!! note
    For super duper admin only. Be sure to set `system_config`->`crossbar.channels`->`system_wide_channels_list` flag to `true`

> GET /v2/channels

```shell
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/channels
```

## Fetch active channels for an account

> GET /v2/accounts/{ACCOUNT_ID}/channels

```shell
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels
```

```json
{
    "auth_token": "{AUTH_TOKEN}",
    "data": [
        {
            "answered": true,
            "authorizing_id": "63fbb9ac78e11f3ccb387928a423798a",
            "authorizing_type": "device",
            "destination": "user_zu0bf7",
            "direction": "outbound",
            "other_leg": "d220c187-e18edc42-bab2459d@10.26.0.91",
            "owner_id": "72855158432d790dfb22d03ff64c033e",
            "presence_id": "user_zu0bf7@account.realm.com",
            "timestamp": 63573977746,
            "username": "user_zu0bf7",
            "uuid": "dab25c76-7479-4ed2-ba92-6b725d68e351"
        }
    ],
    "request_id": "{REQUEST_ID}",
    "revision": "{REVISION}",
    "status": "success"
}
```

## Fetch channels for a user or device

> GET /v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/channels

For user with `{USER_ID}`:

```shell
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/users/{USER_ID}/channels
```

For device with `{DEVICE_ID}`:

> GET /v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/channels

```shell
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/devices/{DEVICE_ID}/channels
```

## Fetch a channel's details

> GET /v2/accounts/{ACCOUNT_ID}/channels/{UUID}

```shell
curl -v -X GET \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```

## Execute an application against a Channel

!!! note
    This API requires Konami to be running and metaflows to be enabled on the call

> POST /v2/accounts/{ACCOUNT_ID}/channels/{UUID}

```shell
curl -v -X POST \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"action": "transfer", "target": "2600", "takeback_dtmf": "*1", "moh": "media_id" }}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```

Available `action` values are `hold`, `unhold`, `transfer`, `hangup`, `break`, `callflow`, `move` and `intercept`.

### Move

```shell
curl -v -X POST \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data": {"action": "move", "owner_id": "2e04e3205b36b6291f854995e80985b0", "device_id": "c27b0a86c5e7b0f2a5999967fd8bbf09", "auto_answer": true, "can_call_self": true, "dial_strategy": "simultaneous"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`auto_answer` | Whether to auto-answer the new leg | `boolean()` | `false` | `false`
`can_call_self` | Can intercept devices of the same targeted user | `boolean()` | `true` | `false`
`device_id` | Move the call to a specific device | `string()` |   | `false`
`dial_strategy` | How to ring the endpoints, if multiple | `string()` | `simultaneous` | `false`
`owner_id` | User ID to use for finding endpoints | `string()` |   | `false`

### Hold

```shell
curl -v -X POST \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"action":"hold"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```

Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`action` | Action to perform | `string('hold' | 'unhold')` | | `true`
`moh` | Media ID or indicator selection for the b-leg | `string('{media_id}' | 'silence' | 'pulse' | 'beep' | 'beep{frequency}' | 'tone_stream://{tones pattern}')` | `shout://{mp3 stream URL}` | `local_stream://{local stream URL}` | `{user or account MoH}` | `false`
`moh_aleg` | Indicator selection for the a-leg | `string('silence' | 'pulse' | 'beep' | 'beep{frequency}' | 'tone_stream://{tones pattern}')` | `beep` | `false`
`moh_bleg` | Media ID or indicator selection for the b-leg (included for compatibility) | `string('{media_id}' | 'silence' | 'pulse' | 'beep' | 'beep{frequency}' | 'tone_stream://{tones pattern}')` | `shout://{mp3 stream URL}` | `local_stream://{local stream URL}` | `{user or account MoH}` | `false`
`unhold_key` | DTMF key[s] that when pressed will unhold the call (Note: if unhold_key is used, no unhold event will be generated)| `string()` |   | `false`
`update_channel` | Update the channel property `is_onhold` | `boolean()` | `true` | `false`

### Transfer

When using the channels API to perform a transfer, the UUID specified is the transferor, and the other leg is the transferee. In other words, if Bob wants to transfer Alice to somewhere, use the UUID of Bob's leg in the request to the API.

```shell
curl -v -X POST \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"data":{"action":"transfer","target":"2600","transfer_type":"attended"}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```

Key | Description | Type | Default
--- | ----------- | ---- | -------
`moh` | MoH Media ID or indicator selection to be played to the transferee | `string('{media_id}' | 'silence' | 'pulse' | 'beep' | 'beep{frequency}' | 'tone_stream://{tones pattern}' | 'shout://{mp3 stream}' | 'local_stream://{local stream}')` | `{user or account MoH}` | `false`
`target` | Extension/DID to transfer to | `string()` |
`transfer_type` | What type of transfer to perform | `string('attended' | 'blind')` | `blind`


## Put a feature (metaflow) on a channel

!!! note
    This API requires Konami Pro to be running and metaflows to be enabled on the call

> PUT /v2/accounts/{ACCOUNT_ID}/channels/{UUID}

```shell
curl -v -X PUT \
    -H "Content-Type: application/json" \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    -d '{"action":"metaflow", "data": {"data": { "module": "hangup" }}}' \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/channels/{UUID}
```

The Metaflow feature is a `metaflow` object which validates with its corresponding JSON schema.

### Reasoning

The `POST` action requires that every Metaflow action would have to be coded into the module.

### Benefits

The Metaflow feature allows adding new types of Metaflows without changing the code.
It also allows full Metaflows and not only single actions, i.e., the `children` node is also processed.
