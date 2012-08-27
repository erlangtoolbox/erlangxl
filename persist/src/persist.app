{application, persist, [
	{description, "tiny inmemory DB with async fsync"},
	{modules, []},
	{applications, [
		kernel,
		stdlib,
		xl_stdlib,
		xl_io
	]},
	{env, []}
]}.
