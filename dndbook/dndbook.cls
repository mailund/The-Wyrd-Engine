\RequirePackage {expl3}
\ProvidesExplClass {dndbook/dndbook} {2020/04/21} {0.8.0} { Template for DnD 5e material }

\bool_new:N \c__dnd_isclass_bool
\bool_set_true:N \c__dnd_isclass_bool

\input {dndbook/dndoptions.clo}

\LoadClass {book}

\input {dndbook/dndcore.def}
