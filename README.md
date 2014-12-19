edocset
====
Erlang doclet module for generating a [Dash](http://kapeli.com/dash) docset from Edoc.

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

# Demo
Here is an example of viewing [cowboy](https://github.com/ninenines/cowboy) API doc.

Watch demo video in YouTube by clicking below image.

[![Demo video](https://cloud.githubusercontent.com/assets/1213991/5506157/bc48b214-87da-11e4-97eb-dbb4c02f62bb.gif)](http://youtu.be/kceOouNxsJs)
