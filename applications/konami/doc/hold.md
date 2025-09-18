## Hold

### About Hold

Some phones do not support putting the other line on hold or making it easy to set custom music to play while on hold. The Konami *hold* module permits both.

#### Schema

Put the leg on hold, or remove hold



Key | Description | Type | Default | Required
--- | ----------- | ---- | ------- | --------
`moh` | Media ID or indicator selection for the b-leg | `string('{media_id}' | 'silence' | 'pulse' | 'beep' | 'beep{frequency}' | 'tone_stream://{tones pattern}')` | `shout://{mp3 stream URL}` | `local_stream://{local stream URL}` | `{user or account MoH}` | `false`
`moh_aleg` | Indicator selection for the a-leg | `string('silence' | 'pulse' | 'beep' | 'beep{frequency}' | 'tone_stream://{tones pattern}')` | `beep` | `false`
`moh_bleg` | Media ID or indicator selection for the b-leg (included for compatibility) | `string('{media_id}' | 'silence' | 'pulse' | 'beep' | 'beep{frequency}' | 'tone_stream://{tones pattern}')` | `shout://{mp3 stream URL}` | `local_stream://{local stream URL}` | `{user or account MoH}` | `false`
`unhold` | If true, an unhold action is performed (using the FS 'break' command), rather than hold" | `string()` |   | `false`
`unhold_key` | DTMF key[s] that when pressed will unhold the call | `string()` |   | `false`
`update_channel` | Update the channel property `is_onhold` (not compatible with unhold_key) | `boolean()` | `true` | `false`



### How It Works

Alice is talking to Bob and would like to put him on hold while she performs a request Bob has made of her. She presses her *hold* metaflow number (say `*5`). Bob will be placed on hold, and Alice is free to talk without her audio being transmitted to Bob.

Once Alice is ready to talk with Bob again, she can press unhold key (1 by default) to be reconnected with Bob.

### Configure the metaflow

The *hold* module should be placed under the "numbers" key in the "metaflows" object. It can take a custom media file to play to the on-hold party; otherwise the system default music is played.

```json
    "metaflows":{
        "numbers":{
            "5":{
                "module":"hold",
                "data":{
                    "moh_aleg":"pulse",
                    "moh_bleg":"some_useful_media_id",
                    "unhold_key":"5"
                }
            }
        },
        "patterns":{...},
        "binding_key":"*"
    }
```
