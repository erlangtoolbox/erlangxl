#include <erl_nif.h>
ERL_NIF_TERM xl_ref_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
ERL_NIF_TERM xl_ref_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

void xl_ref_dtor(ErlNifEnv* env, void* arg);

int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

static ErlNifFunc nif_funcs[] = {
    {"new", 1, xl_ref_new},
    {"value", 1, xl_ref_value}
};

ERL_NIF_INIT(xl_ref, nif_funcs, &on_load, NULL, NULL, NULL)

typedef struct { ErlNifEnv* env; ERL_NIF_TERM value; } ref_t;
static ErlNifResourceType* XL_REF_RESOURCE;

ERL_NIF_TERM xl_ref_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ERL_NIF_TERM object = argv[0];

    ref_t* ref = (ref_t*)enif_alloc_resource(XL_REF_RESOURCE, sizeof(ref_t));
    ref->env = enif_alloc_env();
    ref->value = enif_make_copy(ref->env, object);
    ERL_NIF_TERM ref_term = enif_make_resource(env, ref);
    enif_release_resource(ref);
    return ref_term;
}

ERL_NIF_TERM xl_ref_value(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    ref_t* ref;
    if (enif_get_resource(env, argv[0], XL_REF_RESOURCE, (void**)&ref))
        return ref->value;
    else
        return enif_make_badarg(env);
}

void xl_ref_dtor(ErlNifEnv* env, void* arg) {
  ref_t* ref = (ref_t*) arg;
  enif_free_env(ref->env);
}

int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  XL_REF_RESOURCE = enif_open_resource_type(env, NULL, "xl_ref_resource", &xl_ref_dtor, flags, 0);
  return 0;
}
