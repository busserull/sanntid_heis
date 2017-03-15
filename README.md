# sanntid_heis
Elevator project in Sanntidsystemer

Project descrption: https://github.com/TTK4145/Project

## Modules

#### beastly_elevator_app
OTP module for starting and stopping the application, nothing  fancy.

#### beastly_elevator_sup
Supervisor starts and stops all processes, monitoring them and restarts them in correct order if someone dies. OTP behaviour: supervisor.

#### peer_finder
Locates other erlang nodes on the network and tries to connect them to the node cluster. OTP behaviour: gen_server.

#### order_backlog
Store all the orders on every node, shows the elevator_controller where to go. OTP behaviour: gen_server.

#### elevator_controller
Simple FSM which controls one local elevator, gets information from the order_backlog about which direction the next order is at, and when it reached it. OTP behaviour: gen_statem.

#### elevator_driver
interfaces the c program which controls the hardware on the elevator. OTP behaviour: gen_server.

#### cost 
contains functions used to calculate the best order for one elevator to choose,
does not spawn any processes simply used by order_backlog.

