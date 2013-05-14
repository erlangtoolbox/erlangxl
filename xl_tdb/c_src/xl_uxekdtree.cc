#include <erl_nif.h>
#include <vector>
#include <utility>
#include <sys/time.h>
#include "xl_uxekdtree.h"

extern "C" {
    ERL_NIF_TERM xl_uxekdtree_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM xl_uxekdtree_dump(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM xl_uxekdtree_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);
    ERL_NIF_TERM xl_uxekdtree_depth(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]);

    void xl_uxekdtree_dtor(ErlNifEnv* env, void* arg);

    int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info);

    static ErlNifFunc nif_funcs[] = {
        {"new", 1, xl_uxekdtree_new},
        {"dump", 1, xl_uxekdtree_dump},
        {"size", 1, xl_uxekdtree_size},
        {"depth", 1, xl_uxekdtree_depth}
    };

    ERL_NIF_INIT(xl_uxekdtree, nif_funcs, &on_load, NULL, NULL, NULL)
}

long current_time() {
    timeval time;
    gettimeofday(&time, NULL);
    return (time.tv_sec * 1000) + (time.tv_usec / 1000);
}

typedef struct { uxekdtree::Tree* tree; ErlNifEnv* env; } tree_ref_t;
static ErlNifResourceType* XL_UXEKDTREE_RESOURCE;

ERL_NIF_TERM xl_uxekdtree_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    unsigned int points_length;
    ERL_NIF_TERM points = argv[0];

    if (!enif_get_list_length(env, points, &points_length)) return enif_make_badarg(env);

    uxekdtree::Points pts;
    ERL_NIF_TERM head, tail;
    int current_tuple_size, tuple_size = -1;
    const ERL_NIF_TERM* tuple;

    long copy_start = current_time();

    ErlNifEnv* tree_env = enif_alloc_env();
    while (enif_get_list_cell(env, points, &head, &tail)) {
        if (enif_get_tuple(env, head, &current_tuple_size, &tuple)
            && (tuple_size == -1 || tuple_size == current_tuple_size)
            && current_tuple_size > 1
        ) {
            std::vector<ERL_NIF_TERM> point;
            for(int i = 0; i < current_tuple_size - 1; i++)
                point.push_back(tuple[i]);

            pts.push_back(new std::pair<std::vector<ERL_NIF_TERM>, ERL_NIF_TERM>(point, tuple[current_tuple_size -1 ]));
            points = tail;
            tuple_size = current_tuple_size;
        } else {

            enif_free_env(tree_env);
            return enif_make_badarg(env);
        }
    }

    long tree_start = current_time();

    tree_ref_t* ref = (tree_ref_t*)enif_alloc_resource(XL_UXEKDTREE_RESOURCE, sizeof(tree_ref_t));
    ref->env = tree_env;
    ref->tree = new uxekdtree::Tree(tree_env, pts.begin(), pts.end());
    ERL_NIF_TERM tree_res = enif_make_resource(env, ref);
    enif_release_resource(ref);
    std::cout << "PERFORMANCE: copy: " << tree_start - copy_start << " tree: " << current_time() - tree_start << std::endl;
    return tree_res;
}

ERL_NIF_TERM xl_uxekdtree_dump(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    tree_ref_t* ref;
    if (enif_get_resource(env, argv[0], XL_UXEKDTREE_RESOURCE, (void**)&ref))
        return enif_make_tuple2(env, enif_make_atom(env, "xl_uxekdtree"), ref->tree->dump(env));
    else
        return enif_make_badarg(env);
}

ERL_NIF_TERM xl_uxekdtree_size(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    tree_ref_t* ref;
    if (enif_get_resource(env, argv[0], XL_UXEKDTREE_RESOURCE, (void**)&ref))
        return enif_make_int(env, ref->tree->size());
    else
        return enif_make_badarg(env);
}

ERL_NIF_TERM xl_uxekdtree_depth(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) {
    tree_ref_t* ref;
    if (enif_get_resource(env, argv[0], XL_UXEKDTREE_RESOURCE, (void**)&ref))
        return enif_make_int(env, ref->tree->depth());
    else
        return enif_make_badarg(env);
}

void xl_uxekdtree_dtor(ErlNifEnv* env, void* arg) {
  tree_ref_t* ref = (tree_ref_t*) arg;
  enif_free_env(ref->env);
  delete ref->tree;
}

int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info) {
  ErlNifResourceFlags flags = (ErlNifResourceFlags)(ERL_NIF_RT_CREATE | ERL_NIF_RT_TAKEOVER);
  XL_UXEKDTREE_RESOURCE = enif_open_resource_type(env, NULL, "xl_uxekdtree_resource", &xl_uxekdtree_dtor, flags, 0);
  return 0;
}
