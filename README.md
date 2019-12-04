# INSTALLATION NOTES

## on aio:
###   Install Erlang OTP
```
    curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl  
    chmod +x kerl
    mv kerl /usr/bin
    kerl list releases
    kerl build 19.2 19.2 # this takes a while
    kerl install 19.2 /usr/local/otp-19.2
    . /usr/local/otp-19.2/activate
```

### Clone github
```
    git clone https://github.com/itlevel3/kazoo.git kazoo.itlevel3
    cd kazoo.itlevel3
    git checkout 4.3.58-acdc
```
###   Build the release 
```
	cd kazoo.itlevel3
    . /usr/local/otp-19.2/activate
	make
	make build-release
	make sup_completion
	mkdir _rel/kazoo/log
	cd _rel
	mv kazoo kazoo.itlevel3-4.3.58
	tar cvzf kazoo-itlevel3-4.3.58.tgz kazoo.itlevel3-4.3.58
```
###   copy release tarball and sup completion to production servers 
```
    scp kazoo-itlevel3-4.3.58.tgz root@aio.sipengines.com:
    scp kazoo-itlevel3-4.3.58.tgz root@aio2.sipengines.com:
    scp kazoo-itlevel3-4.3.58.tgz root@aio3.sipengines.com:
	scp sup.bash aio:/etc/bash_completion.d/
	scp sup.bash aio2:/etc/bash_completion.d/
   	scp sup.bash aio3:/etc/bash_completion.d/
```

### On production server:
####  Pre-flight:
*	yum info kazoo-freeswitch
	make sure installed version of freeswitch is  kazoo-freeswitch-4.3-4.el7.centos (NOTE 4.3-6.el7.centos doesn't work with 4.3.58)
*	Installation makes changes to the bigcouch DB so backup the DB before you start
*	yum info kazoo-kamailio
		make sure installed version of kamialio is kazoo-kamailio-4.3-25.el7.centos

####  Flight:
```
	systemctl stop kazoo-applications
	cd /opt
	mv kazoo kazoo-4.3.18
	tar xvzf ~/kazoo-itlevel3-4.3.58.tgz
	rm ~/kazoo-itlevel3-4.3.58.tgz
	chown -R kazoo:daemon kazoo.itlevel3-4.3.58
	ln -s kazoo.itlevel3-4.3.58 kazoo
	systemctl start kazoo-applications
	
	sup crossbar_maintenance start_module cb_queues 
	sup crossbar_maintenance start_module cb_agents 
	sup crossbar_maintenance start_module cb_acdc_call_stats 
	
	sup acdc_maintenance migrate
	sup acdc_maintenance refresh

	sup kapps_controller start_app acdc  <-- how do we make it permanent

	sup blackhole_maintenance start_module bh_acdc_agent
	sup blackhole_maintenance start_module bh_acdc_queue 
```
####  Rollback:
```
	systemctl stop kazoo-applications
	cd /opt
	rm kazoo
	ln -s kazoo-4.3.18 kazoo
	systemctl start kazoo-applications
```
If still experiencing problems then restore DB from backup
	

# SANITY TESTING VOXTER ACDC FEATURES 
in commit https://github.com/voxter/kazoo/commit/db5e7c83f5b26f35428721f4dae08f05067c8625

## announcements:
------------
add this object to the queue
```
        "announcements": {
            "wait_time_announcements_enabled": true,
            "position_announcements_enabled": true,
            "interval": 30
        },
```
audio file played to caller position in the queue and estimated wait time.

tested and working needs queue.announcements see here:  https://github.com/voxter/kazoo/blob/db5e7c83f5b26f35428721f4dae08f05067c8625/applications/crossbar/doc/queues.md


## skill based routing how does it works
-------------------------------------
Best guess at the moment:

1. Add acdc_required_skills to start of callflow with 
```
   "add": [
     "skill1",
     ...
   ],
   "remove": [
     "skill2",
     ...
   ]
 }
```
To add or remove required skills for the call

2. Agent has a list of skills in "acdc_skills"
```
"acdc_skills" : [
			"skill1", "skill2", "skill3"
                    ] 
```
Use PATCH {{base_url}}/accounts/{{account_id}}/users/{{user_id}} to add skills to user doc

3. Set  queue.strategy to "skills_based_round_robin"

Then only agents which have the skills will be taken from the queue


## call priority how does it work
------------------------------

1) callflow can set "priority" in data to an integer value, higher the number the higher the priority
2) and/or the icoming call can have a custom variable <<"Call-Priority">> again an integer
3) Callflow data (1) has prescedence over (2)
4) default call-priority is 0
5) Call is then placed in the queue just behind all the calls with the same Call-Priority

e.g. call queue has 3 calls all 0 : q: 0,0,0
new call comes in with priority 5 and jumps to head of q:  5,0,0,0
new call come in with prioirty 4 goes behind 5 but ahead of 0 q: 5,4,0,0,0


## callbacks - leave a callback number 
-----------------------------------

If callflow has data: "enter_as_callback" : true then call is a callback
plays a prompt either: <<"breakout-prompt">> 
                   or:breakout.media

==================================================================================================
entered/exit - 
    Amit to Test
==================================================================================================
## pause alias : reason for pause
-----------------------------
```
POST {{base_url}}/accounts/{{account_id}}/agents/{{user_id}}/status
{
	"data":{
		"status": "pause",
		"timeout": 30,
		"alias": "this is an alias"
	}
}
```

GET {{base_url}}/accounts/{{account_id}}/agents/{{user_id}}/status
```
    "data": {
        "5ad503fa47dde31cb88ca5ccc1bce062": {
            "agent_id": "5ad503fa47dde31cb88ca5ccc1bce062",
            "timestamp": 63740390173,
            "status": "paused",
            "pause_time": 30,
            "pause_alias": "this is an alias",
            "id": "5ad503fa47dde31cb88ca5ccc1bce062::63740390173"
        }
    }
```

## agent restart API?
------------------
here: {{base_url}}/accounts/{{account_id}}/agents/{{agent_id}}/restart

## Early end wrapup time
---------------------

PATCH {{base_url}}/accounts/{{account_id}}/queues/{{queue_id}}
```
{
	"data": {
		"agent_wrapup_time": 60
	}
}
```

Verify that default wrapup time is now 60 secs
Now during the wrapup period

POST {{base_url}}/accounts/{{account_id}}/agents/{{user_id}}/status
```
{
	"data":{
		"status": "end_wrapup"
	}
}
```
Verify agent is now ready before wrapup time expires


## agent availabity check  callflow
--------------------------------
Add acdc_agent_availability to call flow to first check if there are agents available
```
{
	"data":{
    "flow": {
      "module": "acdc_agent_availability",
      "data": {"id": "9a218da18b8104c888f0d47d946ffac0"},
      "children": {
       	 "available": {
             "data": {
                        "id": "9a218da18b8104c888f0d47d946ffac0"
                    },
                    "module": "acdc_member",
                    "children": {}
                },                
          "unavailable": {
              "data": {
			"id": "/system_media/en-us%2Fqueue-no_agents_available"
		      },
   			"module": "play",
              		"children": {}
               }
            }
        }
	}
}
```

## average wait time check
-----------------------

Branch call flow based on Agent Available & Average wait time > branch_key:
```
{
"data":{
    "flow": {

        "module": "acdc_agent_availability",
        "data": {"id": "9a218da18b8104c888f0d47d946ffac0"},
        "children": {
             "available": {
                  "module": "acdc_wait_time",
                  "data": {"id": "9a218da18b8104c888f0d47d946ffac0"},
                  "children": {
                        "_": {
                               "data": {"id": "9a218da18b8104c888f0d47d946ffac0"},
                               "module": "acdc_member",
                               "children": {}
                             },
                        "60": {
                               "data": {"id": "/system_media/en-us%2Fqueue-no_agents_available"},
                               "module": "play",
                               "children": {}
                              }
                  }
             },
             "unavailable": {
                  "data": {"id": "/system_media/en-us%2Fqueue-no_agents_available"},
                  "module": "play",
                  "children": {}
             }
        }
    }
}
}
```

## Agent Websock API
---------------
```
            send({
                action: 'subscribe',
                auth_token: {AUTH_TOKEN},
                request_id: {REQUEST_ID},
                data: {
                    account_id: {ACCOUNT_ID},
                    binding: 'acdc_stats.status.{ACCOUNT_ID}.*'
                }
            });
            send({
                action: 'subscribe',
                auth_token: {AUTH_TOKEN},
                request_id: {REQUEST_ID},
                data: {
                    account_id: {ACCOUNT_ID},
                    binding: 'acdc.agent.action.*'
                }
            });
            send({
                action: 'subscribe',
                auth_token: {AUTH_TOKEN},
                request_id: {REQUEST_ID},
                data: {
                    account_id: {ACCOUNT_ID},
                    binding: 'acdc.agent_change.*'
                }
            });
            send({
                action: 'subscribe',
                auth_token: {AUTH_TOKEN},
                request_id: {REQUEST_ID},
                data: {
                    account_id: {ACCOUNT_ID},
                    binding: 'acdc_queue.doc_created'
                }
            });
            send({
                action: 'subscribe',
                auth_token: {AUTH_TOKEN},
                request_id: {REQUEST_ID},
                data: {
                    account_id: {ACCOUNT_ID},
                    binding: 'acdc_queue.doc_edited'
                }
            });
            send({
                action: 'subscribe',
                auth_token: {AUTH_TOKEN},
                request_id: {REQUEST_ID},
                data: {
                    account_id: {ACCOUNT_ID},
                    binding: 'acdc_queue.doc_deleted'
                }
            });

            send({
                action: 'subscribe',
                auth_token: {AUTH_TOKEN},
                request_id: {REQUEST_ID},
                data: {
                    account_id: {ACCOUNT_ID},
                    binding: 'acdc.agent.action.*'
                }
            });
        }
            send({
                action: 'subscribe',
                auth_token: {AUTH_TOKEN},
                request_id: {REQUEST_ID},
                data: {
                    account_id: {ACCOUNT_ID},
                    binding: 'acdc_stats.status.{ACCOUNT_ID}.*'
                }
            });
```
================================================================================
2019-12-02 16:09:21:055: Message published

Node:         kazoo-rabbitmq@aio
Connection:   138.197.64.230:60104 -> 157.245.112.58:5672
Virtual host: /
User:         guest
Channel:      60
Exchange:     kapps
Routing keys: [<<"acdc_stats.status.f1f5976fde3c06d4630d659daec2a097.5ad503fa47dde31cb88ca5ccc1bce062">>]
Routed queues: [<<"kazoo_apps@aio.sipengines.com-<0.3080.0>-c9e54fa4">>,
                <<"kazoo_apps@aio.sipengines.com-<0.3131.0>-9494e6ee">>,
                <<"kazoo_apps@aio.sipengines.com-<0.3238.0>-9dd8d4ba">>,
                <<"kazoo_apps@aio.sipengines.com-acdc_stats-<0.2979.0>-5151c948">>,
                <<"kazoo_apps@aio.sipengines.com-blackhole_listener-<0.22570.58>-ea8d7e1d">>,
                <<"kazoo_apps@aio3.sipengines.com-<0.2828.0>-148c1660">>,
                <<"kazoo_apps@aio3.sipengines.com-<0.2899.0>-cefee8f9">>,
                <<"kazoo_apps@aio3.sipengines.com-<0.3078.0>-b2531adc">>,
                <<"kazoo_apps@aio3.sipengines.com-acdc_stats-<0.2720.0>-968bc4da">>]
Properties:   [{<<"timestamp">>,signedint,63742522161038542},
               {<<"content_type">>,longstr,<<"application/json">>}]
Payload: 
{"Timestamp":63742522161,"Agent-ID":"5ad503fa47dde31cb88ca5ccc1bce062","Account-ID":"f1f5976fde3c06d4630d659daec2a097","Node":"kazoo_apps@aio3.sipengines.com","Msg-ID":"4512d865c49da380","Event-Name":"logged_out","Event-Category":"acdc_status_stat","App-Version":"4.0.0","App-Name":"acdc"}

================================================================================

2019-12-02 16:09:21:065: Message published

Node:         kazoo-rabbitmq@aio
Connection:   157.245.112.58:59746 -> 157.245.112.58:5672
Virtual host: /
User:         guest
Channel:      48
Exchange:     kapps
Routing keys: [<<"acdc_stats.status.f1f5976fde3c06d4630d659daec2a097.5ad503fa47dde31cb88ca5ccc1bce062">>]
Routed queues: [<<"kazoo_apps@aio.sipengines.com-<0.3080.0>-c9e54fa4">>,
                <<"kazoo_apps@aio.sipengines.com-<0.3131.0>-9494e6ee">>,
                <<"kazoo_apps@aio.sipengines.com-<0.3238.0>-9dd8d4ba">>,
                <<"kazoo_apps@aio.sipengines.com-acdc_stats-<0.2979.0>-5151c948">>,
                <<"kazoo_apps@aio.sipengines.com-blackhole_listener-<0.22570.58>-ea8d7e1d">>,
                <<"kazoo_apps@aio3.sipengines.com-<0.2828.0>-148c1660">>,
                <<"kazoo_apps@aio3.sipengines.com-<0.2899.0>-cefee8f9">>,
                <<"kazoo_apps@aio3.sipengines.com-<0.3078.0>-b2531adc">>,
                <<"kazoo_apps@aio3.sipengines.com-acdc_stats-<0.2720.0>-968bc4da">>]
Properties:   [{<<"timestamp">>,signedint,63742522161041456},
               {<<"content_type">>,longstr,<<"application/json">>}]
Payload: 
{"Timestamp":63742522161,"Agent-ID":"5ad503fa47dde31cb88ca5ccc1bce062","Account-ID":"f1f5976fde3c06d4630d659daec2a097","Node":"kazoo_apps@aio.sipengines.com","Msg-ID":"231e98b8d8f32f1f","Event-Name":"logged_out","Event-Category":"acdc_status_stat","App-Version":"4.0.0","App-Name":"acdc"}

================================================================================

