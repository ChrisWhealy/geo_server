%% -----------------------------------------------------------------------------
%% Make HTML elements from the supplied data
as_binary(V) when is_binary(V) -> V;
as_binary(V) when is_list(V)   -> list_to_binary(V);
as_binary(V)                   -> list_to_binary(io_lib:format("~p",[V])).

start_el(Tag) -> list_to_binary(["<",Tag,">"]).
end_el(Tag)   -> list_to_binary(["</",Tag,">"]).

%% The Attrs parameter must be a list of tagged 2-tuples where the tag is the
%% attribute name and the second element is attribute value
start_el(Tag, Attrs) -> start_el(Tag, Attrs, []).

start_el(Tag, [],                     Acc) -> list_to_binary(["<",Tag,Acc,">"]);
start_el(Tag, [{Attr, Value} | Rest], Acc) -> start_el(Tag, Rest, list_to_binary([Acc," ", as_binary(Attr), "=", as_binary(Value)]));
start_el(Tag, [SomeVal | Rest],       Acc) -> start_el(Tag, Rest, list_to_binary([Acc," ", as_binary(SomeVal)]));
start_el(Tag, {Attr, Value},          Acc) -> start_el(Tag, [],   list_to_binary([Acc," ", as_binary(Attr), "=", as_binary(Value)])).

make_el(Tag, undefined)   -> list_to_binary([start_el(Tag), end_el(Tag)]);
make_el(Tag, "Undefined") -> list_to_binary([start_el(Tag), end_el(Tag)]);
make_el(Tag, Content)     -> list_to_binary([start_el(Tag), as_binary(Content), end_el(Tag)]).

make_el(Tag, [],    Content) -> make_el(Tag, Content);
make_el(Tag, Attrs, Content) -> list_to_binary([start_el(Tag, Attrs), as_binary(Content), end_el(Tag)]).

make_empty_el(Tag)        -> start_el(Tag).
make_empty_el(Tag, Attrs) -> list_to_binary([start_el(Tag, Attrs)]).

make_p(Content)  -> make_el("p",  Content).
make_th(Content) -> make_el("th", Content).
make_td(Content) -> make_el("td", Content).
make_tr(Content) -> make_el("tr", Content).

make_td(Attrs, Content) -> make_el("td", Attrs, Content).

make_table(Attrs, Content) -> make_el("table", Attrs, Content).
make_form(Attrs, Content)  -> make_el("form", Attrs, Content).

make_input(Attrs) -> make_empty_el("input",Attrs).

make_cb(Name, true) -> make_empty_el("input", [{name, Name}, {type, checkbox}, {value, true}, checked]);
make_cb(Name, _)    -> make_empty_el("input", [{name, Name}, {type, checkbox}, {value, true}]).

make_br() -> make_empty_el("br").





delimit(Vals) -> list_to_binary(["'", Vals, "'"]).