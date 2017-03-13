{application, beastly_elevator,
 [{description, "An OTP application"},
  {vsn, "0.1.0"},
  {registered, [beastly_elevator]},
  {mod, { beastly_elevator_app, []}},
  {applications,
   [kernel,
    stdlib
   ]},
  {env,
  [{door_open_period, 4000},
   {number_of_floors, 4},
   {number_of_elevators, 1},
   {traveling_timeout, 3000}
  ]},
  {modules, 
  [environment_controller, 
   beastly_elevator_sup, 
   beastly_elevator_app,
   test_backlog
  ]},
  {maintainers, []},
  {licenses, []},
  {links, []}
 ]}.
