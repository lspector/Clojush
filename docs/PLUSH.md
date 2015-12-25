# Plush Genomes / Epigentics

> In a change from previous versions of PushGP, the most recent version of Clojush
does not evolve Push programs directly, but instead uses a separate linear genome representation
that we translate into Push programs prior to execution. The new Plush
genomes are linear sequences of instructions that may have one or more epigenetic
markers attached to each instruction. The epigenetic markers affect the translation of
the Plush genomes into Push programs. For example, the silent marker is a boolean
that tells whether a particular instruction will appear in the translated program.

[- Tom Helmuth](https://web.cs.umass.edu/publication/docs/2015/UM-CS-PhD-2015-005.pdf) 

A Plush genome looks like this:

```clojure
({:close 1, :silent false, :instruction code_frominteger} {:close 0, :silent false, :instruction genome_yank} {:close 0, :silent false, :instruction float_eq} {:close 0, :silent false, :instruction code_size} {:close 0, :silent false, :instruction float_max})
```

Look at the [`translate.clj` tests](./test/clojush/midje/translate.clj) for
some examples of how they are translated. Also check out the docs
for the
[`clojush.translate/var-translate-plush-genome-to-push-program`](https://lspector.github.io/Clojush/clojush.translate.html#var-translate-plush-genome-to-push-program)
function to understand how they are represented in the codebase.
