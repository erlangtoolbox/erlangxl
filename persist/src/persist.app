{application, persist, [
	{description, "tiny inmemory DB with async fsync"},
	{modules, []},
	{applications, [
		kernel,
		stdlib,
		strikead_stdlib,
		strikead_io
	]},
	{env, []}
]}.
