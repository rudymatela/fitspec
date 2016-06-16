Important modules
-----------------

* [FitSpec](FitSpec.hs):
  the entry point, import this to use FitSpec;

* [FitSpec.Engine](FitSpec/Engine.hs):
  main engine that tests mutants against properties;

* [FitSpec.Report](FitSpec/Report.hs):
  gather results from the engine and build textual reports;

* [FitSpec.Mutable](FitSpec/Mutable.hs):
  list mutations of a given function without repetitions;

* [FitSpec.ShowMutable](FitSpec/ShowMutable.hs):
  show mutations;

* [example benchmarks](bench):
  example use cases for FitSpec,
  some are customizable using command line arguments
  (sorting, booleans, lists, pretty-printing, etc).
