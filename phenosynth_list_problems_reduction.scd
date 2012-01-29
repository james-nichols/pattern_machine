//Do this until an error is thrown:
(
~island = PSIsland.new;
~island.play;
)
//Now, debug it
~island.population;
~wIdx=~island.population.detectIndex({|i, idx| i.class!=PSPhenotype;});
~w=~island.population[~wIdx];
~w.class;
~w.asCompileString;
//perhaps if it's an intractable open function, whci ios pretty common, you can do this:
~w.def.context

)