edocset
====
Erlang doclet module for generating a Dash docset from Edoc.

# Usage
If you use rebar, you edit your `rebar.config` like:

```
{edoc_opts, [
  {doclet, edocset_doclet},
  {xml_export, xmerl_xml}
]}.
{deps, [
  {edocset, ".*", {git, "git://github.com/mtgto/edocset.git", {branch, "master"}}}
]}.
```

Then, you execute `rebar doc` and `XXXX.docset` is generated.
