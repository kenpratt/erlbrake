{application, erlhoptoad,
 [
  {description, ""},
  {vsn, "1"},
  {modules, [
             erlhoptoad_app,
             erlhoptoad_sup,
             hoptoad
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { erlhoptoad_app, []}},
  {env, []}
 ]}.
