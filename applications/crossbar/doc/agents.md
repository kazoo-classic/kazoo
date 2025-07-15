### API for managing agent status
This API provides convenient way for agent status management without need to dial feature codes. It's useful for various call center agent/supervisor interfaces.

##### Log In/Log Out agent to/from some queue

/agents/AID/queue_status (GET, POST):

```json
{
  "data":{
    "action":{{action}},
    "queue_id":{{queue_id}}
  }
}
```

where

{{action}} - "login" | "logout"
and {{queue_id}} is an ID of the queue

##### Set agent status:

/agents/AID/status  (GET, POST):

```json
{
  "data":{
    "status":{{status}},
    "timeout":{{timeout}},
    "presence_id":{{id}},
    "presence_state":{{state}}
  }
}
```

where
{{status}} - "login" | "logout" | "pause" | "resume"
{{timeout}} - timeout for "pause" status
`presence_id` и `presence_state` - optional fields for presence information

If the agent is on call in time of request, then "pause",  "resume"  and "logout" commands will be executed right after the agent is back from the call.

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/stats_summary

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/stats_summary
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/stats

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/stats
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/status
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/restart

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/restart
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/queue_status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/queue_status
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/queue_status

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/queue_status
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/stats_summary

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/stats_summary
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/stats_summary/{USER_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/stats_summary/{USER_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/stats

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/stats
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/stats/{USER_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/stats/{USER_ID}
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/status

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/status
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/status

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/{USER_ID}/status
```

## Fetch

> GET /v2/accounts/{ACCOUNT_ID}/agents/status/{USER_ID}

```shell
curl -v -X GET \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/status/{USER_ID}
```

## Change

> POST /v2/accounts/{ACCOUNT_ID}/agents/status/{USER_ID}

```shell
curl -v -X POST \
    -H "X-Auth-Token: {AUTH_TOKEN}" \
    http://{SERVER}:8000/v2/accounts/{ACCOUNT_ID}/agents/status/{USER_ID}
```

