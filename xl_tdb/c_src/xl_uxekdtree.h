#ifndef _XL_UXEKDTREE_H_
#define _XL_UXEKDTREE_H_

#include <algorithm>
#include <iostream>
#include <math.h>
#include <sys/param.h>

namespace uxekdtree {

typedef std::pair<std::vector<ERL_NIF_TERM>, ERL_NIF_TERM> Point;
typedef std::vector<Point*> Points;
typedef std::vector<ERL_NIF_TERM> Values;

class TreeNode {
    public:
        virtual ERL_NIF_TERM dump(ErlNifEnv* env) = 0;
        virtual int size() = 0;
        virtual int depth() = 0;
};

class Node: public TreeNode {
    ERL_NIF_TERM value;
    int plane;
    TreeNode * undefined;
    TreeNode * less;
    TreeNode * equal;
    TreeNode * greater;
    TreeNode * excluded;

    public:
        Node(ERL_NIF_TERM _value, int _plane, TreeNode * _undefined, TreeNode * _less, TreeNode * _equal, TreeNode * _greater, TreeNode * _excluded):
             value(_value), plane(_plane), undefined(_undefined), less(_less), equal(_equal),
             greater(_greater), excluded(_excluded) {};

        ~Node() {
            delete undefined;
            delete less;
            delete equal;
            delete greater;
            delete excluded;
        }

        virtual ERL_NIF_TERM dump(ErlNifEnv* env)  {
            return enif_make_tuple7(env, value, enif_make_uint(env, plane + 1),
                undefined->dump(env),
                less->dump(env),
                equal->dump(env),
                greater->dump(env),
                excluded->dump(env)
            );
        }

        virtual int size() {
            return 1 + undefined->size() + less->size() + equal->size() + greater->size() + excluded->size();
        }

        virtual int depth() {
            int udepth = undefined->depth();
            int ldepth = less->depth();
            int edepth = equal->depth();
            int gdepth = greater->depth();
            int xdepth = excluded->depth();
            return 1 + MAX(MAX(MAX(MAX(udepth, ldepth), edepth), gdepth), xdepth);
        }
};


class Leaf: public TreeNode {
    Values values;

    public:
        Leaf(Points::iterator begin, Points::iterator end) {
            std::reverse_iterator<Points::iterator> it = std::reverse_iterator<Points::iterator>(end);
            std::reverse_iterator<Points::iterator> rend = std::reverse_iterator<Points::iterator>(begin);
            for(; it != rend; ++it) values.push_back((*it)->second);
        }

        ~Leaf() {
            values.clear();
        }

        virtual ERL_NIF_TERM dump(ErlNifEnv* env)  {
            ERL_NIF_TERM list = enif_make_list(env, 0);
            for(Values::iterator it = values.begin(); it != values.end(); ++it)
                list = enif_make_list_cell(env, *it, list);
            return list;
        }

        virtual int size() {
            return 0;
        }

        virtual int depth() {
            return 0;
        }
};

ERL_NIF_TERM scalar_value(ErlNifEnv* env, ERL_NIF_TERM v) {
    int tuple_size;
    const ERL_NIF_TERM* tuple;

// todo add check for {x, ...}
    if (enif_is_tuple(env, v)
        && enif_get_tuple(env, v, &tuple_size, &tuple)
        && tuple_size == 2)
        return tuple[1];
    else
        return v;
}

struct is_exclude {
    ErlNifEnv* env;
    int plane;
    is_exclude(ErlNifEnv* _env, int _plane):env(_env), plane(_plane){};
    bool operator() (Point* p) {
        int tuple_size;
        const ERL_NIF_TERM* tuple;
        ERL_NIF_TERM v = p->first[plane];
        return (enif_is_tuple(env, v) && enif_get_tuple(env, v, &tuple_size, &tuple) && tuple_size == 2);
    }
};

struct compare {
    ERL_NIF_TERM undefined;
    compare(ERL_NIF_TERM _undefined): undefined(_undefined){}
    int operator() (ERL_NIF_TERM l, ERL_NIF_TERM r) {
        if (enif_compare(l, undefined) == 0 && enif_compare(r, undefined) == 0) return 0;
        else if (enif_compare(l, undefined) == 0) return -1;
        else if (enif_compare(r, undefined) == 0) return 1;
        else {
            int res = enif_compare(l, r);
            if (res < 0) return -1;
            if (res > 0) return 1;
            return 0;
        }
    }
};

struct partitioner {
    ErlNifEnv* env;
    compare cmp;
    ERL_NIF_TERM value;
    int type;
    int plane;
    partitioner(ErlNifEnv* _env, ERL_NIF_TERM _value, int _type, ERL_NIF_TERM _undefined, int _plane):
        env(_env), cmp(_undefined), value(_value), type(_type), plane(_plane){}
    bool operator() (Point* p) {
        return cmp(scalar_value(env, p->first[plane]), value) == type;
    }
};

struct sorter {
    int plane;
    compare cmp;
    sorter(int _plane, ERL_NIF_TERM _undefined): plane(_plane), cmp(_undefined){}
    bool operator() (Point* l, Point* r) {
        return cmp(l->first[plane], r->first[plane]) < 0;
    }
};


TreeNode* new_tree(ErlNifEnv* env, ERL_NIF_TERM undefined, Points::iterator begin, Points::iterator end, unsigned int plane_pos, std::vector<int> planes) {

    if (begin == end) return new Leaf(begin, end);

    if (planes.empty()) {
        return new Leaf(begin, end);
    }

    if(plane_pos >= planes.size()) return new_tree(env, undefined, begin, end, 0, planes);

    int plane = planes[plane_pos];

    Points::iterator undef_end = std::partition(begin, end, partitioner(env, undefined, 0, undefined, plane));

    std::vector<int> planes_wo_one(planes.begin(), planes.end());
    planes_wo_one.erase(planes_wo_one.begin() + plane_pos);

    if (undef_end == end) {
        return new Node(
            undefined,
            plane,
            new_tree(env, undefined, begin, undef_end, plane_pos, planes_wo_one),
            new Leaf(end, end),
            new Leaf(end, end),
            new Leaf(end, end),
            new Leaf(end, end)
        );
    }
    std::sort(undef_end, end, sorter(plane, undefined));
    Point* median = undef_end[(int)round((end-undef_end) / 2.0) - 1];
    ERL_NIF_TERM median_value = scalar_value(env, median->first[plane]);
    Points::iterator less_end = std::partition(undef_end, end, partitioner(env, median_value, -1, undefined, plane));
    Points::iterator greater_end = std::partition(less_end, end, partitioner(env, median_value, 1, undefined, plane));
    Points::iterator ex_end = std::partition(greater_end, end, is_exclude(env, plane));
    return new Node(median_value, plane,
        new_tree(env, undefined, begin, undef_end, plane_pos, planes_wo_one),
        new_tree(env, undefined, undef_end, less_end, plane_pos + 1, planes),
        new_tree(env, undefined, ex_end, end, plane_pos, planes_wo_one),
        new_tree(env, undefined, less_end, greater_end, plane_pos + 1, planes),
        new_tree(env, undefined, greater_end, ex_end, plane_pos, planes_wo_one)
    );
}

std::vector<int> planes(Point* point) {
    std::vector<int> planes;
    for(unsigned int i = 0; i < point->first.size(); i++) planes.push_back(i);
    return planes;
}

class Tree {
    TreeNode * root;
    public:
        Tree(ErlNifEnv* env, Points::iterator begin, Points::iterator end) {
            ERL_NIF_TERM undefined = enif_make_atom(env, "undefined");
            if (begin != end)
                root = new_tree(env, undefined, begin, end, 0, planes(*begin));
            else
                root = new Leaf(begin, end);
        }

        ~Tree() {
            delete root;
        }

        ERL_NIF_TERM dump(ErlNifEnv* env) {
            return root->dump(env);
        }

        int size() {
            return root->size();
        }

        int depth() {
            return root->depth();
        }
};

}
#endif
