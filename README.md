# INSTALLATION NOTES ALMA LINUX 8

Kazoo-Applications needs to be built from source using erlang OTP 19.3

Special thanks to;

- [ruhnet](https://github.com/ruhnet/kazoo)
- [itlevel3](https://github.com/itlevel3/kazoo)
- [voxter](https://github.com/voxter/kazoo)


## Install Erlang OTP

```bash
​##kerl install
## This helps us install the erlan otp version we want
curl -O https://raw.githubusercontent.com/kerl/kerl/master/kerl
chmod +x kerl
mv kerl /usr/bin

#Deps for build/making
dnf groupinstall -y "Development Tools"
dnf install -y ncurses-devel gcc gcc-c++ make glibc-devel git \
               java-1.8.0-openjdk-devel unixODBC-devel wxGTK3-devel \
               wget ncurses-devel python2 python2-yaml

#link python 2.7 to command python, required for make command later
ln -s /usr/bin/python2.7 /usr/sbin/python

#openssl 1.0 requirement for erl 19.3
curl https://www.openssl.org/source/openssl-1.0.2r.tar.gz | tar xfz - && cd openssl-1.0.2r && ./config --prefix=/usr/local/openssl-1.0.2r -fpic && make && sudo make install && cd .. && rm -rf openssl-1.0.2r creates=/usr/local/openssl-1.0.2r

###FOP, used to generate docs.
mkdir /opt/fop
cd /opt/fop
wget https://dlcdn.apache.org/xmlgraphics/fop/binaries/fop-2.9-bin.tar.gz
tar -zxvf fop-2.9-bin.tar.gz
ln -s  /opt/fop/fop-2.9/fop/fop /usr/bin/fop
cd /opt

### Build ERL OTP 19.3
export KERL_CONFIGURE_OPTIONS="--with-ssl=/usr/local/openssl-1.0.2r"
kerl build 19.3 19.3
kerl install 19.3 /usr/local/otp-19.3
. /usr/local/otp-19.3/activate
```

## Build Kazoo-Applications

```bash
cd /opt
git clone https://github.com/kazoo-classic/kazoo/

### Dependencies for this fork, pqueue for acdc and elixir
#### ELIXIR
cd /tmp
wget https://github.com/elixir-lang/elixir/archive/v1.5.3.tar.gz
tar -xzvf v1.5.3.tar.gz
cd elixir-1.5.3
make clean
mkdir -p /usr/local/otp-19.3/elixir
make install PREFIX=/usr/local/otp-19.3/elixir
export PATH=/usr/local/otp-193/elixir/bin:$PATH
source ~/.bashrc

#### PQUEUE
cd /opt/kazoo/deps
git clone https://github.com/okeuday/pqueue.git
cd /opt/kazoo/deps/pqueue
git checkout v1.7.0
wget https://github.com/rebar/rebar/archive/refs/tags/2.6.4.tar.gz
tar -zxvf 2.6.4.tar.gz
cd rebar-2.6.4/
make
cd ..
./rebar-2.6.4/rebar --version
./rebar-2.6.4/rebar get-deps
./rebar-2.6.4/rebar compile
​
### Build Kazoo
cd /opt/kazoo
make
make install
```

## Get Configs

```bash
useradd -G daemon kazoo
cd /opt/kazoo
#​## Use 4.3 from 2600hz or the kazoo-classic repo
git https://github.com/kazoo-classic/kazoo-configs-core.git
mkdir /etc/kazoo -p
\cp -R /opt/kazoo/kazoo-configs-core/core /etc/kazoo/core
chown kazoo:daemon -R /etc/kazoo/core
# move files to appropriate locations.
\cp /opt/kazoo/kazoo-configs-core/system/systemd/* /usr/lib/systemd/system/
\cp /opt/kazoo/kazoo-configs-core/system/init.d/* /etc/init.d/
\cp /opt/kazoo/kazoo-configs-core/system/logrotate.d/* /etc/logrotate.d/
\cp /opt/kazoo/kazoo-configs-core/system/security/limits.d/* /etc/security/limits.d/
\cp /opt/kazoo/kazoo-configs-core/system/sbin/* /usr/sbin/
systemctl daemon-reload
systemctl enable kazoo-applications
### optionally run ecallmgr if you're not running it inside of kazoo-applications itself
#systemctl enable kazoo-ecallmgr

# link the sup command
ln -s /opt/kazoo/bin/sup /usr/sbin/sup
```

## Edit configuration

Go set your elang cookie in /opt/kazoo/.erlang.cookie

Go configure /etc/kazoo/core/config.ini

## RsyslogD

```bash
cat <<EOF>/etc/rsyslog.d/90-kazoo-core.conf
local0.*                                                /var/log/kazoo/kazoo.log
& ~
EOF

systemctl restart rsyslogd
```
##  Flight:
```
	systemctl start kazoo-applications
	systemctl enable kazoo-applications
	
	sup crossbar_maintenance start_module cb_queues 
	sup crossbar_maintenance start_module cb_agents 
	sup crossbar_maintenance start_module cb_acdc_call_stats 
	
	sup acdc_maintenance migrate
	sup acdc_maintenance refresh

	sup kapps_controller start_app acdc

	sup blackhole_maintenance start_module bh_acdc_agent
	sup blackhole_maintenance start_module bh_acdc_queue 
```

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

## entered/exit - 
Amit to Test

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

