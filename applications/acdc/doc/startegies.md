### Queue Strategies

#### Round Robin strategy

In a multi node/zone cluster round robin doesn't work as an enduser might expect.

There are queue worker tasks which take a call off the shared call queue and try to find an agent. In a round-robin strategy there is 1 queue worker task per agent to ensure that under a heavy load all agents can ring at the same time. However this approach can lead to some anomolies.

Consider this scenario:

Lets say we have a round robin queue Q1 with 4 agents (A1, A2, A3, A4) all with the same priority and there are 2 nodes in a cluster N1 and N2.

When we start up Q1 will have 4 queue workes on N1 and 4 queue workers on N2. Assuming the incomoing calls are spread evenly between N1 and N2 then the 1st 8 calls will be taken off the queue and tried to match with the 4 agents. Thus it's possible for the 8th caller to get answered first and the 1st caller to be answered last.

When an agent completes a call the agentId is marked 'ready' and put to the back of the agent queue. Federation ensures that the Agent is put to the back on all nodes even if there are multiple zones.

#### Ring All strategy

In a 'ring-all' it doesn't make any sense to have 1 queue worker per agent. Only 1 worker task takes a call off the queue and rings all agents. Thus in the same 2 node cluster only the 1st 2 calls are taken from the queue and tried to match with an agent.  

