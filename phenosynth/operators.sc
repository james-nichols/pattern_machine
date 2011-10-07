/*
genetic operators - mutators, selectors, crossovers....
*/
PSCrossovers {
	*uniformCrossover {|params, chromosomes|
		//an awful crossover, useful for only the most boneheaded of problems
		^(chromosomes[0].size).collect({|i| chromosomes.slice(nil, i).choose;});
	}
}
PSMutators {
	*floatPointMutation{|params, chromosome|
		var rate;
		var amp = params.mutationSize;
		rate = params.mutationProb * (chromosome.size.reciprocal);
		chromosome.do({|val, index|
			(rate.coin).if ({
				//exponentially distributed mutations to mimic flipping bits in 
				//32 bit binary floats. lazy, inefficient, effective.
				chromosome[index] = (val + 
					(2.0 ** (32.0.rand.neg)) *
					(2.rand*2-1)
				).wrap(0, 1);
			});
		});
		^chromosome;
	}
}
PSDeathSelectors {
	//death selector protocol:
	//return an array of things to kill
	*byRoulettePerRate {|params, population|
		//choose enough doomed to meet the death rate on average, by fitness-
		// weighted roulette
		var hitList, localFitnesses, maxFitness, negFitnesses, meanFitness, rate;
		rate = params.deathRate;
		localFitnesses = population.collect({|i| i.fitness;});
		maxFitness = localFitnesses.maxItem;
		negFitnesses = maxFitness - localFitnesses;
		meanFitness = negFitnesses.mean;
		hitList = population.select(
			{|i| ((((maxFitness - i.fitness)/meanFitness)*rate).coin)}
		);
		^hitList;
	}
	*byRoulettePerRateAdultsOnly {|params, population|
		//choose enough doomed to meet the death rate on average, by fitness-
		// weighted roulette
		var hitList, localFitnesses, maxFitness, negFitnesses, meanFitness, localPopulation, rate;
		rate = params.deathRate;
		localPopulation = population.select(_.logicalAge>1);
		localFitnesses = localPopulation.collect(_.fitness);
		maxFitness = localFitnesses.maxItem;
		negFitnesses = maxFitness - localFitnesses;
		meanFitness = negFitnesses.mean;
		hitList = localPopulation.select(
			((((maxFitness - _.fitness)/meanFitness)*rate).coin)
		);
		^hitList;
	}
}
PSBirthSelectors {
	//birth selector protocol:
	//return an array of arrays of parents
	*byRoulettePerTotal {|params, population|
		// choose enough proud parents to keep the population constant, by
		// fitness-weighted roulette
		var parentList, localFitnesses, meanFitness, targetBirths;
		targetBirths = (params.population) - population.size;
		localFitnesses = population.collect({|i| i.fitness;});
		meanFitness = localFitnesses.mean;
		localFitnesses = localFitnesses / (localFitnesses.sum);
		parentList = targetBirths.collect(
			params.numParents.collect(
				population.wchoose(localFitnesses)
			)
		);
		^parentList;
	}

}