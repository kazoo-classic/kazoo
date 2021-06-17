## SUP-able functions

| Function | Arguments | Description |
| -------- | --------- | ----------- |
| `agent_detail/2` | `(AccountId,AgentId)` | Display name, username and email info |
| `agent_login/2` | `(AccountId,AgentId)` | Login AgentId to all queues |
| `agent_logout/2` | `(AccountId,AgentId)` | Logout  AgentId from all queues |
| `agent_pause/2` | `(AccountId,AgentId)` | Pause AgentId for default time |
| `agent_pause/3` | `(AccountId,AgentId,Time)` | Pause AgentId for Time (secs) |
| `agent_presence_id/2` | `(AccountId,AgentId)` | Use presence_id as BLF id to monitor Agent status, Green - logged_in: Red - logged_out : Amber : paused |
| `agent_queue_login/3` | `(AccountId,AgentId,QueueId)` | Login AgentId to QueueId |
| `agent_queue_logout/3` | `(AccountId,AgentId,QueueId)` | Logout AgentId from QueueId |
| `agent_resume/2` | `(AccountId,AgentId)` | Resume AgentId from pause |
| `agent_summary/2` | `(AccountId,AgentId)` | |
| `agents_detail/0` |  | Display all Agents details for all accounts |
| `agents_detail/1` | `(AccountId)` | Display all Agents details from AccountId |
| `agents_summary/0` |  | Display summary for all agents |
| `agents_summary/1` | `(AccountId)` | Display summary for all Agents from AccountId |
| `current_agents/1` | `(AccountId)` | Display current agents info for AccountId, including priority and skills (if skbrr) |
| `current_calls/1` | `(AccountId)` | Display list of waiting calls in the Queue and currently currently being handled by agents |
| `current_calls/2` | `(AccountId,QueueId)` | Display list of waiting calls in the Queue and currently currently being handled by agents |
| `current_queues/1` | `(AccountId)` | |
| `current_statuses/1` | `(AccountId)` | |
| `flush_call_stat/1` | `(CallId)` | |
| `logout_agent/2` | `(AccountId,AgentId)` | |
| `logout_agents/1` | `(AccountId)` | |
| `migrate/0` |  | |
| `migrate_to_acdc_db/0` |  | |
| `queue_detail/2` | `(AccountId,QueueId)` | |
| `queue_restart/2` | `(AccountId,QueueId)` | |
| `queue_summary/2` | `(AccountId,QueueId)` | |
| `queues_detail/0` |  | |
| `queues_detail/1` | `(AccountId)` | |
| `queues_restart/1` | `(AccountId)` | |
| `queues_summary/0` |  | |
| `queues_summary/1` | `(AccountId)` | |
| `refresh/0` |  | |
| `refresh_account/1` | `(Account)` | |
| `register_views/0` |  | |
