# ACDC Call Stats

## ETS Tables (In memory):

| Function | Table Name  | Archived | Search Archive |Description |
| -------- | --------- | ----------- | -------- | ------ |
| `call_table_id()` | `'acdc_stats_call'` | `true` | **false** | record is updated during an ongoing call, search MODB via `/acountd_id/acdc_call_stats` endpoint |  
| `call_summary_table_id()` | `'acdc_stats_call_summary'` | `true` | `true` | record is created after a call completes, previously only this stat was archived to the couch MODB. |  
| `agent_call_table_id()` | `'acdc_stats_agent_call'` | **false** | **false** | record is created after a call completes |  
  
#  
archive process runs every /system_config/acdc/archive_period_ms, default = 60000 (1min)  
  
archive_window = system_config/acdc/archive_window_s default 3600 (1hr)  
records older than archive will be archived to MODB
   
cleanup proccess runs every /system_config/acdc/cleanup_period_ms, default = 360000 (6mins)  
  
cleanup_window = system_config/acdc/cleanup_window_s default 86400 (24hours)  
records older than cleanup window will be removed
#
    
## acdc_stats_call

```
#call_stat{id,
           call_id,
           account_id,
           queue_id,
           agent_id,
           entered_timestamp,
           abandoned_timestamp,
           handled_timestamp,
           processed_timestamp,
           hung_up_by,
           entered_position,
           exited_position,
           abandoned_reason,
           is_callback,
           misses = [{agent_id, reason, timestamp},...],
           status = waiting|handled|abandoned|processed
           caller_id_name,
           caller_id_number,
           caller_priority,
           required_skills = [],
           is_archived}
```
## acdc_stats_call_summary

```
#call_summary_stat{id,
                   account_id,
                   queue_id,
                   call_id,
                   status = abandoned|processed,
                   entered_position,
                   wait_time,
                   talk_time,
                   timestamp,
                   is_archived}
```
## acdc_stats_agent_call

new record is created on every agent status change  

```
#agent_call_stat{id,
                 account_id,
                 queue_id,
                 agent_id,
                 call_id,
                 status = handled|missed,
                 talk_time,
                 timestamp}
```

## API Endpoint:
### /agents/stats -> current_calls_req -> call_table_id() 
### /agents/{{agent_id}}/stats -> current_calls_req -> call_table_id() 


/agents/stats : Last 24 hours  
/agents/stats?recent=true : Last 24 hours  
/agents/stats?start_range={{START_TIMESTAMP}} : Stats from START_TIMESTAMP to now()  
/agents/starts/?start_range={{START_TIMESTAMP}}&end_range={{END_TIMESTAMP}} :stats from START_TIMESTAMP to END_TIMESTAMP  


**NOTES**
**Max range is limited to now() - {{ACDC_CLEANUP_WINDOW}} which defaults to 24 hrs**  
**To get call stats outside this range then use the acdc_call_stats endpoint instead.**   
** REALLY BIG NOTE **
** stats endpoint now return the raw call records and not the summary data, use stats_summary for this **

```
	agent_id
    	total_calls   : count of records with agent_id + count of misses with agent_id  
        answered_calls: count of records with agent_id & status = handled | processed
        talk_time     : sum of (processed_timestamp - handled_timestamp) with agent_id & status = processed
        missed_calls  : count of misses with agent_id

    agent_id/queue_id
		total_calls    : count of records with (agent_id & queue_id) + count of misses with (agent_id & queuue_id)
        answered_calls : count of records with agent_id & queue_id & status = handled | processed
        talk_time      : sum of (processed_timestamp - handled_timestamp) with agent_id & queue_id & status = processed
        missed_calls   : count of misses with (agent_id & queue_id)
```
### /agents/stats_summary -> agent_calls_req -> agent_call_table_id()

```
    agent_id/queue_id
        missed_calls    :   count of records with (agent_id & queue_id)  & status = missed
        answered_calls  :   count of records with (agent_id & queue_id)  & status = handled
        total_calls     :   count of records with (agent_id & queue_id) + count of misses with (agent_id & queuue_id)
        talk_time       :   sum of (processed_timestamp - handled_timestamp) with agent_id & queue_id 
```        

### /queues/stats_summary -> call_summary_req -> call_summary_table_id()
### /queues/{{queue_id}}/stats_summary -> call_summary_req -> call_summary_table_id()


```
Summarized/queue_id
    max_entered_position    : max value of entered_position with queue_id 
    average_talk_time       : sum of talk_time with queue_id / (total_calls - abandoned_calls)
    average_wait_time       : sum of wait_time with queue_id / total_calls
    abandoned_calls         : count of records with queue_id & status == abandoned
    total_calls             : count of records with queue_id
```

### /queues/stats -> current_calls_req -> call_table_id()
### /queues/{{queue_id}}/stats -> current_calls_req -> call_table_id()


```
data/stats
        [#call_stat{},#call_stat{},,,,]
```

## CouchDB MODB Databases:

### Views
- call_stats/crossbar_listing   : view of `acdc_stats_call` archive records with key = entered_timestamp    
- call_stats/call_log           : view of `acdc_stats_call` archive records key = [queue_id, entered_timestamp]
- call_stats/call_summary       : view of `acdc_stats_call_summary` archive records with map/reduce function to optimize summarized data

## API Endpoint:

### /accounts/{{acountd_id}}/acdc_call_stats

This API gives access to the *call_stats/crossbar_listing* MODB database view  
returns the payload in either JSON or CSV
