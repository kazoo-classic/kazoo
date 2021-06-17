# ACDC Agent Status

## ETS Tables (In memory):

| Function | Table Name  | Archived | Search Archive |Description |
| -------- | --------- | ----------- | -------- | ------ |
| `status_table_id()` | `'acdc_stats_status'` | `true` | `false` | new record is created on every Agent status change |  
| `agent_cur_status_table_id()` | `'acdc_stats_agent_cur_status'` | `false` | `false` | Agent record is updated on every agent status change |  
  
#  
archive process runs every /system_config/acdc/archive_period_ms, default = 60000 (1min)  
  
archive_window = system_config/acdc/archive_window_s default 3600 (1hr)  
records older than archive will be archived to MODB
   
cleanup proccess runs every /system_config/acdc/cleanup_period_ms, default = 360000 (6mins)  
  
cleanup_window = system_config/acdc/cleanup_window_s default 86400 (24hours)  
records older than cleanup window will be removed
#
    
## Both acdc_stats_status and acdc_stats_agent_cur_status store #status_stat{} records.

```
#status_stat{id,
             agent_id,
             account_id,
             status = logged_in | logged_out | ready | connecting | connected | wrapup | paused | outbound | inbound
             timestamp,
             wait_time,
             pause_time,
             pause_alias,
             callid,
             caller_id_name,
             caller_id_number,
             is_archived = false}
```

## API Endpoint:
### /agents/status -> acdc_stats_agent_cur_status -> agent_cur_status_table_id()  
### /agents/{{agent_id}}/status -> acdc_stats_agent_cur_status -> agent_cur_status_table_id()  
   
### /agents/status?recent=true -> status_table_id -> acdc_stats_status()  
### /agents/{{agent_id}}/status?recent=true -> status_table_id -> acdc_stats_status()  
  
   
/agents/status : current status  
/agents/status?recent=true : All status events for the last 24 hours  (CLEANUP_WINDODW)

/agents/status?status=STATUS : All Agents whose current status is STATUS  
/agents/status?recent=true&status=STATUS : All status events of type STATUS for the last 24 hours  (CLEANUP_WINDODW)

For example: /agents/{{agent_id}}/status?recent=true&status=logged_in will return a list of logged in events from agent_id
