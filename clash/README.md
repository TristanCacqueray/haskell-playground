# retroclash playground

This project contains clash sample from the [retroclash](https://unsafePerform.IO/retroclash/) book.

## Intro

```ShellSession
$ nix-shell ~/src/github.com/TristanCacqueray/easy-retroclash-nix/shell.nix
[nix-shell]$ clashi src/Button.hs
*Button> -- Sampling demo
*Button> :t topEntity
topEntity ::
     "BTN" ::: ("1" ::: Signal System Bit, "2" ::: Signal System Bit)
  -> "LED" ::: ("1" ::: Signal System Bit, "2" ::: Signal System Bit)

*Button> import qualified Data.List as L
*Button L> let input = fromList ([low, low, high] <> L.repeat low)
*Button L> sampleN 10 $ fst $ topEntity (input, input)
[0,0,1,0,0,0,0,0,0,0]

*Button L> -- Synthesis demo
*Button L> :verilog
GHC: Parsing and optimising modules took: 0.383s
GHC: Loading external modules from interface files took: 0.000s
GHC: Parsing annotations took: 0.000s
Clash: Parsing and compiling primitives took 0.159s
GHC+Clash: Loading modules cumulatively took 0.967s
Clash: Compiling Button.topEntity
Clash: Normalization took 0.001s
Clash: Netlist generation took 0.000s
Clash: Total compilation took 0.987s
```

Verilog file can be visualized at http://digitaljs.tilk.eu/

## Blink

```ShellSession
$ clashi -isrc/ src/BlinkSimulation.hs
*BlinkSimulation> main
```
