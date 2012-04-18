%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 2001-2011. All Rights Reserved.
%% 
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%% 
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%% 
%% %CopyrightEnd%
%%
%% =====================================================================
%% Ordered Sets implemented as General Balanced Trees
%%
%% Copyright (C) 1999-2001 Richard Carlsson
%%
%% An implementation of ordered sets using Prof. Arne Andersson's
%% General Balanced Trees. This can be much more efficient than using
%% ordered lists, for larger sets, but depends on the application. See
%% notes below for details.
%% ---------------------------------------------------------------------
%% Notes:
%%
%% The complexity on set operations is bounded by either O(|S|) or O(|T|
%% * log(|S|)), where S is the largest given set, depending on which is
%% fastest for any particular function call. For operating on sets of
%% almost equal size, this implementation is about 3 times slower than
%% using ordered-list sets directly. For sets of very different sizes,
%% however, this solution can be arbitrarily much faster; in practical
%% cases, often between 10 and 100 times. This implementation is
%% particularly suited for ackumulating elements a few at a time,
%% building up a large set (more than 100-200 elements), and repeatedly
%% testing for membership in the current set.
%%
%% As with normal tree structures, lookup (membership testing),
%% insertion and deletion have logarithmic complexity.
%%
%% Operations:
%%
%% - empty(): returns empty set.
%%
%%   Alias: new(), for compatibility with `sets'.
%%
%% - is_empty(S): returns 'true' if S is an empty set, and 'false'
%%   otherwise.
%%
%% - size(S): returns the number of nodes in the set as an integer.
%%   Returns 0 (zero) if the set is empty.
%%
%% - singleton(X): returns a set containing only the element X.
%%
%% - is_member(X, S): returns `true' if element X is a member of set S,
%%   and `false' otherwise.
%%
%%   Alias: is_element(), for compatibility with `sets'.
%%
%% - insert(X, S): inserts element X into set S; returns the new set.
%%   *Assumes that the element is not present in S.*
%%
%% - add(X, S): adds element X to set S; returns the new set. If X is
%%   already an element in S, nothing is changed.
%%
%%   Alias: add_element(), for compatibility with `sets'.
%%
%% - delete(X, S): removes element X from set S; returns new set.
%%   Assumes that the element exists in the set.
%%
%% - delete_any(X, S): removes key X from set S if the key is present
%%   in the set, otherwise does nothing; returns new set.
%%
%%   Alias: del_element(), for compatibility with `sets'.
%%
%% - balance(S): rebalances the tree representation of S. Note that this
%%   is rarely necessary, but may be motivated when a large number of
%%   elements have been deleted from the tree without further
%%   insertions. Rebalancing could then be forced in order to minimise
%%   lookup times, since deletion only does not rebalance the tree.
%%
%% - union(S1, S2): returns a new set that contains each element that is
%%   in either S1 or S2 or both, and no other elements.
%%
%% - union(Ss): returns a new set that contains each element that is in
%%   at least one of the sets in the list Ss, and no other elements.
%%
%% - intersection(S1, S2): returns a new set that contains each element
%%   that is in both S1 and S2, and no other elements.
%%
%% - intersection(Ss): returns a new set that contains each element that
%%   is in all of the sets in the list Ss, and no other elements.
%%
%% - is_disjoint(S1, S2): returns `true' if none of the elements in S1
%%   occurs in S2.
%%
%% - difference(S1, S2): returns a new set that contains each element in
%%   S1 that is not also in S2, and no other elements.
%%
%%   Alias: subtract(), for compatibility with `sets'.
%%
%% - is_subset(S1, S2): returns `true' if each element in S1 is also a
%%   member of S2, and `false' otherwise.
%%
%% - to_list(S): returns an ordered list of all elements in set S. The
%%   list never contains duplicates.
%%
%% - from_list(List): creates a set containing all elements in List,
%%   where List may be unordered and contain duplicates.
%%
%% - from_ordset(L): turns an ordered-set list L into a set. The list
%%   must not contain duplicates.
%%
%% - smallest(S): returns the smallest element in set S. Assumes that
%%   the set S is nonempty.
%%
%% - largest(S): returns the largest element in set S. Assumes that the
%%   set S is nonempty.
%%
%% - take_smallest(S): returns {X, S1}, where X is the smallest element
%%   in set S, and S1 is the set S with element X deleted. Assumes that
%%   the set S is nonempty.
%%
%% - take_largest(S): returns {X, S1}, where X is the largest element in
%%   set S, and S1 is the set S with element X deleted. Assumes that the
%%   set S is nonempty.
%%
%% - iterator(S): returns an iterator that can be used for traversing
%%   the entries of set S; see `next'. The implementation of this is
%%   very efficient; traversing the whole set using `next' is only
%%   slightly slower than getting the list of all elements using
%%   `to_list' and traversing that. The main advantage of the iterator
%%   approach is that it does not require the complete list of all
%%   elements to be built in memory at one time.
%%
%% - next(T): returns {X, T1} where X is the smallest element referred
%%   to by the iterator T, and T1 is the new iterator to be used for
%%   traversing the remaining elements, or the atom `none' if no
%%   elements remain.
%%
%% - filter(P, S): Filters set S using predicate function P. Included
%%   for compatibility with `sets'.
%%
%% - fold(F, A, S): Folds function F over set S with A as the initial
%%   ackumulator. Included for compatibility with `sets'.
%%
%% - is_set(S): returns 'true' if S appears to be a set, and 'false'
%%   otherwise. Not recommended; included for compatibility with `sets'.

-module(gb_sets).

-export([empty/0, is_empty/1, size/1, singleton/1, is_member/2,
	 insert/2, add/2, delete/2, delete_any/2, balance/1, union/2,
	 union/1, intersection/2, intersection/1, is_disjoint/2, difference/2,
	 is_subset/2, to_list/1, from_list/1, from_ordset/1, smallest/1,
	 largest/1, take_smallest/1, take_largest/1, iterator/1, next/1,
	 filter/2, fold/3, is_set/1]).

%% `sets' compatibility aliases:

-export([new/0, is_element/2, add_element/2, del_element/2,
	 subtract/2]).

%% GB-trees adapted from Sven-Olof Nyström's implementation for
%% representation of sets.
%%
%% Data structures:
%% - {Size, Tree}, where `Tree' is composed of nodes of the form:
%% - {Key, Smaller, Bigger}, and the "empty tree" node:
%% - nil.
%%
%% No attempt is made to balance trees after deletions. Since deletions
%% don't increase the height of a tree, this should be OK.
%%
%% Original balance condition h(T) <= ceil(c * log(|T|)) has been
%% changed to the similar (but not quite equivalent) condition 2 ^ h(T)
%% <= |T| ^ c. This should also be OK.
%%
%% Behaviour is logarithmic (as it should be).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some macros. 

-define(c, 1.46).    % 1 / ln 2; this appears to be best

-define(lb(X), math:log(X) * 1.4426950408889634).

%% ceil(c * log(|T| + d(T)))
%% We add 1 because the cardinality is defined as number of leaves, which is 
%% the number of elements + 1.
-define(height_bound(S, Dels), trunc(?c * ?lb(S + 1 + Dels)) + 1).

-define(b, 10).

-define(pow2(X), 1 bsl X).

%% 2^(b/c) * |T|
-define(deletions_bound(S), (?pow2(?b / ?c) - 1) * S).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Some types.

-type gb_set_node() :: 'nil' | {term(), _, _}.
-opaque iter() :: [gb_set_node()].

%% A declaration equivalent to the following is currently hard-coded
%% in erl_types.erl
%%
%% -opaque gb_set() :: {non_neg_integer(), gb_set_node()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec empty() -> Set when
      Set :: gb_set().

empty() ->
    {0, 0, nil}.

-spec new() -> Set when
      Set :: gb_set().

new() -> empty().

-spec is_empty(Set) -> boolean() when
      Set :: gb_set().

is_empty({0, _, nil}) ->
    true;
is_empty(_) ->
    false.

-spec size(Set) -> non_neg_integer() when
      Set :: gb_set().

size({Size, _, _}) ->
    Size.

-spec singleton(Element) -> gb_set() when
      Element :: term().

singleton(Key) ->
    insert(Key, empty()).

-spec is_element(Element, Set) -> boolean() when
      Element :: term(),
      Set :: gb_set().

is_element(Key, S) ->
    is_member(Key, S).

-spec is_member(Element, Set) -> boolean() when
      Element :: term(),
      Set :: gb_set().

is_member(Key, {_, _, T}) ->
    is_member_1(Key, T).

is_member_1(Key, {Key1, Smaller, _}) when Key < Key1 ->
    is_member_1(Key, Smaller);
is_member_1(Key, {Key1, _, Bigger}) when Key > Key1 ->
    is_member_1(Key, Bigger);
is_member_1(_, {_, _, _}) ->
    true;
is_member_1(_, nil) ->
    false.

-spec insert(Element, Set1) -> Set2 when
      Element :: term(),
      Set1 :: gb_set(),
      Set2 :: gb_set().

insert(Key, {S, Dels, T}) ->
    S1 = S + 1,
    {S1, Dels,
     case insert_1(Key, T, 0, S1, Dels) of
         {ok, T1}            -> T1;
         {exceeds, T1, _, _} -> balance(T1, S1)
     end}.

insert_1(Key, nil, H, TotalS, Dels) ->
    T  = {Key, nil, nil},
    ST = 1,
    HT = 1,
    case H > ?height_bound(TotalS, Dels) of
        true  -> {exceeds, T, ST, HT};
        false -> {ok, T}
    end;
insert_1(Key, {Key1, Smaller, Bigger}, Height, TotalS, Dels) when Key < Key1 ->
    case insert_1(Key, Smaller, Height + 1, TotalS, Dels) of
        {ok, T} ->
            {ok, {Key1, T, Bigger}};
        {exceeds, T, ST, HT} ->
            {SBigger, HBigger} = count(Bigger),
            S = ST + SBigger,
            H = max(HT, HBigger) + 1,
            case H > ?height_bound(S, Dels) of
                true  -> {exceeds, {Key1, T, Bigger}, S, H};
                false -> T1 = balance(T, ST),
                         {ok, {Key1, T1, Bigger}}
            end
    end;
insert_1(Key, {Key1, Smaller, Bigger}, Height, TotalS, Dels) when Key > Key1 ->
    case insert_1(Key, Bigger, Height + 1, TotalS, Dels) of
        {ok, T} ->
            {ok, {Key1, Smaller, T}};
        {exceeds, T, ST, HT} ->
            {SSmaller, HSmaller} = count(Smaller),
            S = ST + SSmaller,
            H = max(HT, HSmaller) + 1,
            case H > ?height_bound(S, Dels) of
                true  -> {exceeds, {Key1, Smaller, T}, S, H};
                false -> T1 = balance(T, ST),
                         {ok, {Key1, Smaller, T1}}
            end
    end;
insert_1(Key, _, _, _, _) ->
    erlang:error({key_exists, Key}).

%% Returns {Size, Height}
count(nil) ->
    {0, 0};
count({_, Smaller, Bigger}) ->
    {WSmaller, HSmaller} = count(Smaller),
    {WBigger,  HBigger}  = count(Bigger),
    {WSmaller + WBigger + 1, max(HSmaller, HBigger) + 1}.

-spec balance(Set1) -> Set2 when
      Set1 :: gb_set(),
      Set2 :: gb_set().

balance({S, _, T}) ->
    {S, 0, balance(T, S)}.

balance(T, S) ->
    balance_list(to_list_1(T), S).

balance_list(L, S) ->
    {T, _} = balance_list_1(L, S),
    T.

balance_list_1(L, S) when S > 1 ->
    Sm = S - 1,
    S2 = Sm div 2,
    S1 = Sm - S2,
    {T1, [K | L1]} = balance_list_1(L, S1),
    {T2, L2} = balance_list_1(L1, S2),
    T = {K, T1, T2},
    {T, L2};
balance_list_1([Key | L], 1) ->
    {{Key, nil, nil}, L};
balance_list_1(L, 0) ->
    {nil, L}.

-spec add_element(Element, Set1) -> Set2 when
      Element :: term(),
      Set1 :: gb_set(),
      Set2 :: gb_set().

add_element(X, S) ->
    add(X, S).

-spec add(Element, Set1) -> Set2 when
      Element :: term(),
      Set1 :: gb_set(),
      Set2 :: gb_set().

add(X, S) ->
    Foo = case is_member(X, S) of
	true ->
	    S;    % we don't have to do anything here
	false ->
	    insert(X, S)
    end,
    Foo.

-spec from_list(List) -> Set when
      List :: [term()],
      Set :: gb_set().

from_list(L) ->
    from_ordset(ordsets:from_list(L)).

-spec from_ordset(List) -> Set when
      List :: [term()],
      Set :: gb_set().

from_ordset(L) ->
    S = length(L),
    {S, 0, balance_list(L, S)}.

-spec del_element(Element, Set1) -> Set2 when
      Element :: term(),
      Set1 :: gb_set(),
      Set2 :: gb_set().

del_element(Key, S) ->
    delete_any(Key, S).

-spec delete_any(Element, Set1) -> Set2 when
      Element :: term(),
      Set1 :: gb_set(),
      Set2 :: gb_set().

delete_any(Key, S) ->
    case is_member(Key, S) of
 	true ->
 	    delete(Key, S);
 	false ->
 	    S
    end.

check_deletions({S, Dels, T}) when Dels >= ?deletions_bound(S) ->
    {S, 0, balance(T, S)};
check_deletions(S) ->
    S.

-spec delete(Element, Set1) -> Set2 when
      Element :: term(),
      Set1 :: gb_set(),
      Set2 :: gb_set().

delete(Key, {S, Dels, T}) ->
    T1 = delete_1(Key, T),
    check_deletions({S + 1, Dels + 1, T1}).

delete_1(Key, {Key1, Smaller, Larger}) when Key < Key1 ->
    Smaller1 = delete_1(Key, Smaller),
    {Key1, Smaller1, Larger};
delete_1(Key, {Key1, Smaller, Bigger}) when Key > Key1 ->
    Bigger1 = delete_1(Key, Bigger),
    {Key1, Smaller, Bigger1};
delete_1(_, {_, Smaller, Larger}) ->
    merge(Smaller, Larger).

merge(Smaller, nil) ->
    Smaller;
merge(nil, Larger) ->
    Larger;
merge(Smaller, Larger) ->
    {Key, Larger1} = take_smallest1(Larger),
    {Key, Smaller, Larger1}.

-spec take_smallest(Set1) -> {Element, Set2} when
      Set1 :: gb_set(),
      Set2 :: gb_set(),
      Element :: term().

take_smallest({S, Dels, T}) ->
    {Key, Larger} = take_smallest1(T),
    {Key, check_deletions({S - 1, Dels + 1, Larger})}.

take_smallest1({Key, nil, Larger}) ->
    {Key, Larger};
take_smallest1({Key, Smaller, Larger}) ->
    {Key1, Smaller1} = take_smallest1(Smaller),
    {Key1, {Key, Smaller1, Larger}}.

-spec smallest(Set) -> term() when
      Set :: gb_set().

smallest({_, _, T}) ->
    smallest_1(T).

smallest_1({Key, nil, _Larger}) ->
    Key;
smallest_1({_Key, Smaller, _Larger}) ->
    smallest_1(Smaller).

-spec take_largest(Set1) -> {Element, Set2} when
      Set1 :: gb_set(),
      Set2 :: gb_set(),
      Element :: term().

take_largest({S, Dels, T}) ->
    {Key, Smaller} = take_largest1(T),
    {Key, check_deletions({S - 1, Dels + 1, Smaller})}.

take_largest1({Key, Smaller, nil}) ->
    {Key, Smaller};
take_largest1({Key, Smaller, Larger}) ->
    {Key1, Larger1} = take_largest1(Larger),
    {Key1, {Key, Smaller, Larger1}}.

-spec largest(Set) -> term() when
      Set :: gb_set().

largest({_, _, T}) ->
    largest_1(T).

largest_1({Key, _Smaller, nil}) ->
    Key;
largest_1({_Key, _Smaller, Larger}) ->
    largest_1(Larger).

-spec to_list(Set) -> List when
      Set :: gb_set(),
      List :: [term()].

to_list({_, _, T}) ->
    to_list(T, []).

to_list_1(T) -> to_list(T, []).

to_list({Key, Small, Big}, L) ->
    to_list(Small, [Key | to_list(Big, L)]);
to_list(nil, L) -> L.

-spec iterator(Set) -> Iter when
      Set :: gb_set(),
      Iter :: iter().

iterator({_, _, T}) ->
    iterator(T, []).

%% The iterator structure is really just a list corresponding to the
%% call stack of an in-order traversal. This is quite fast.

iterator({_, nil, _} = T, As) ->
    [T | As];
iterator({_, L, _} = T, As) ->
    iterator(L, [T | As]);
iterator(nil, As) ->
    As.

-spec next(Iter1) -> {Element, Iter2} | 'none' when
      Iter1 :: iter(),
      Iter2 :: iter(),
      Element :: term().

next([{X, _, T} | As]) ->
    {X, iterator(T, As)};
next([]) ->
    none.

%% Set operations:

-spec union(Set1, Set2) -> Set3 when
      Set1 :: gb_set(),
      Set2 :: gb_set(),
      Set3 :: gb_set().

union(S1, S2) ->
    {L, R} = order(S1, S2),
    Foo = fold(add/2, L, R),
    Foo.

-spec union(SetList) -> Set when
      SetList :: [gb_set(),...],
      Set :: gb_set().

union(Ss) -> lists:foldl(union/2, empty(), Ss).

order(S1 = {Size1, _, _}, S2 = {Size2, _, _}) when Size1 > Size2 ->
    {S1, S2};
order(S1, S2) ->
    {S2, S1}.

-spec intersection(Set1, Set2) -> Set3 when
      Set1 :: gb_set(),
      Set2 :: gb_set(),
      Set3 :: gb_set().

intersection(S1, S2) ->
    {L, R} = order(S1, S2),
    fold(fun (X, S) ->
                 case is_element(X, L) of
                     true  -> insert(X, S);
                     false -> S
                 end
         end, empty(), R).

-spec intersection(SetList) -> Set when
      SetList :: [gb_set(),...],
      Set :: gb_set().

intersection([S | Ss]) -> lists:foldl(insersection/2, S, Ss).

-spec is_disjoint(Set1, Set2) -> boolean() when
      Set1 :: gb_set(),
      Set2 :: gb_set().

is_disjoint(S1, S2) ->
    {L, R} = order(S1, S2),
    lists:foldl(fun (X, B) -> B andalso (not is_member(X, L)) end, true, R).

-spec subtract(Set1, Set2) -> Set3 when
      Set1 :: gb_set(),
      Set2 :: gb_set(),
      Set3 :: gb_set().

subtract(S1, S2) ->
    difference(S1, S2).

-spec difference(Set1, Set2) -> Set3 when
      Set1 :: gb_set(),
      Set2 :: gb_set(),
      Set3 :: gb_set().

difference(S1, S2) ->
    fold(fun delete_any/2, S1, S2).


-spec is_subset(Set1, Set2) -> boolean() when
      Set1 :: gb_set(),
      Set2 :: gb_set().

is_subset({S1, _, _}, {S2, _, _}) when S1 > S2 ->
    false;
is_subset(S1, S2) ->
    fold(fun (X, B) -> B andalso is_member(X, S2) end, true, S1).


%% For compatibility with `sets':

-spec is_set(Term) -> boolean() when
      Term :: term().

is_set({0, D, nil})       when is_integer(D)          -> true;
is_set({S, D, {_, _, _}}) when is_integer(S), S >= 0,
                               is_integer(D), D >= 0  -> true;
is_set(_)                                             -> false.

-spec filter(Pred, Set1) -> Set2 when
      Pred :: fun((E :: term()) -> boolean()),
      Set1 :: gb_set(),
      Set2 :: gb_set().

filter(F, S) ->
    from_ordset([X || X <- to_list(S), F(X)]).

-spec fold(Function, Acc0, Set) -> Acc1 when
      Function :: fun((E :: term(), AccIn) -> AccOut),
      Acc0 :: term(),
      Acc1 :: term(),
      AccIn :: term(),
      AccOut :: term(),
      Set :: gb_set().

fold(F, A, {_, _, T}) when is_function(F, 2) ->
    fold_1(F, A, T).

fold_1(F, Acc0, {Key, Small, Big}) ->
    Acc1 = fold_1(F, Acc0, Small),
    Acc = F(Key, Acc1),
    fold_1(F, Acc, Big);
fold_1(_, Acc, _) ->
    Acc.
