{application, erlhoptoad,
 [
  {description, "erlhoptoad"},
  {vsn, "0.1"},
  {modules, [
             erlhoptoad,
             erlhoptoad_app,
             erlhoptoad_sup,
             erlhoptoad_error_logger,
	     erlhoptoad,
             hoptoad
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib
                 ]},
  {mod, { erlhoptoad_app, []}},
  {env, [
   {apikey, "ENTER_API_KEY"},
   {environment, "development"}
  ]}
 ]}.
