PSIsland {
	/*Islands manage populations of individuals, which can be anything
	responding to \chromosome and \fitness.*/

	/*
	where we define the default operators. These are, in general, vanilla
	functions or paths to Library functions that will be cast to Library
	functions at run time, in the initOperators method.
	
	Since we can't define naked functions in a classvar, these *particular* ones
	are all Library Paths.
	*/
	
	classvar <defaultInitialChromosomeFactory = #[phenosynth, chromosome_fact, basic];
	classvar <defaultIndividualFactory = #[phenosynth, individual_fact, basic];
	classvar <defaultFitnessEvaluator = #[phenosynth, fitness_evals, chromosomemean];
	classvar <defaultTerminationCondition = #[phenosynth, termination_conds, basic];
	classvar <defaultDeathSelector = #[phenosynth, death_selectors, byRoulettePerRate];
	classvar <defaultBirthSelector = #[phenosynth, birth_selectors, byRoulettePerTotal];
	classvar <defaultMutator = #[phenosynth, mutators, floatPointMutation];
	classvar <defaultCrossover = #[phenosynth, crossovers, uniformCrossover];
	//we keep instance settings in a mutable Environment so that
	//generic function parameters can be passed to mutators, and they may
	//be modified at run-time without defining new functions
	var <>params;
	
	//This is the main state variable
	var <population;
	
	// this is another state variable. If I got one more I'd make it
	// a state *dictionary*
	var <iterations = 0;
	
	//flag to stop iterator gracefuly.
	var playing = false;
	
	/*
	Here are the variables that hold the operator functions.
	Paths are coerced to functions at instantiation time to avoid problems with 
	not knowing when the Library gets filled.
	*/
	var <deathSelector;
	var <birthSelector;
	var <mutator;
	var <crossover;
	var <initialChromosomeFactory;
	var <individualFactory;
	var <fitnessEvaluator;
	var <terminationCondition;

	var <>log;
	
	// default values for that parameter thing
	// I wish I could do this with a literal instead of a method
	// because overriding is way awkward this way.
	*defaultParams {
		^(
			\deathRate: 0.1,
			\populationSize: 100,
			\numParents: 2,
			\chromosomeMinLength: 20,
			\crossoverProb: 1,
			\individualClass: PSPhenotype,
			\mutationProb: 0.1,
			\mutationSize: 0.1,
			\stopIterations: 1000
		);
	}
	*new {|params|
		^super.newCopyArgs(
			this.defaultParams.updatedFrom(params ? Event.new);
		).init;
	}
	deathSelector_ {|fn|
		deathSelector = this.loadFunction(fn);
	}
	birthSelector_ {|fn|
		birthSelector = this.loadFunction(fn);
	}
	mutator_ {|fn|
		mutator = this.loadFunction(fn);
	}
	crossover_ {|fn|
		crossover = this.loadFunction(fn);
	}
	initialChromosomeFactory_ {|fn|
		initialChromosomeFactory = this.loadFunction(fn);
	}
	individualFactory_ {|fn|
		individualFactory = this.loadFunction(fn);
	}
	fitnessEvaluator_ {|fn|
		fitnessEvaluator = this.loadFunction(fn);
	}
	terminationCondition_ {|fn|
		terminationCondition = this.loadFunction(fn);
	}
	init {
		this.initOperators;
		population = LinkedList.new;
		this.log = NullLogger.new;
	}
	initOperators {
		this.deathSelector = this.class.defaultDeathSelector;
		this.birthSelector = this.class.defaultBirthSelector;
		this.mutator = this.class.defaultMutator;
		this.crossover = this.class.defaultCrossover;
		this.initialChromosomeFactory = this.class.defaultInitialChromosomeFactory;
		this.individualFactory = this.class.defaultIndividualFactory;
		this.fitnessEvaluator = this.class.defaultFitnessEvaluator;
		this.terminationCondition = this.class.defaultTerminationCondition;
	}
	loadFunction {|nameOrFunction|
		/* we have a method here for two reasons:
		1. it allows us to transparently pass through actual functions, but load
			other things from the Library
		2. to force a test for missing library names, or it's hard to track what
			went wrong. (Because nil is callable(!))
		*/
		var candidate;
		nameOrFunction.isFunction.if(
			{^nameOrFunction},
			{
				candidate = Library.atList(nameOrFunction);
				candidate.isNil.if({
					("Nothing found at %".format(nameOrFunction.cs)).throw;
				});
				candidate.isFunction.not.if({
					("Non-function found at % (%)".format(
						nameOrFunction.cs, candidate.cs)
				).throw;
				});
				^candidate;
			}
		);
	}
	add {|phenotype|
		population.add(phenotype);
	}
	remove {|phenotype|
		population.remove(phenotype);
	}
	populate {
		params.populationSize.do({
			this.add(initialChromosomeFactory.value(params));
		});
	}
	evaluate {
		population.do({|phenotype|
			phenotype.fitness = fitnessEvaluator.value(params, phenotype);
			phenotype.incAge;
		});
	}
	breed {|parentLists|
		parentLists.do({|parents|
			this.breedParents(parents);
		});
	}
	breedParents {|individuals|
		//take a nested list of parents and turn them in to new population.
		var newChromosome;
		params.crossoverProb.coin.if({
			newChromosome = crossover.value(
				params,
				individuals.collect({|i| i.chromosome;})
			);
		}, {
			newChromosome = individuals.choose.chromosome.copy;
		});
		newChromosome = mutator.value(params, newChromosome);
		this.add(individualFactory.value(params, newChromosome));
	}
	cull {|individuals|
		//take a list of the damned and kill them
		individuals.do({|i| this.remove(i);});
	}
	tend {
		// walk the population, doing all the things that GAs do.
		// this is a synchronous thing per default; if you want to do it
		// incrementally, that's your bag.
		var toCull, toBreed;
		var beforeFitness, afterFitness;
		log.logFlush(nil, \preevaluate, iterations);
		this.evaluate;
		log.logFlush(nil, \postevaluate, iterations);
		toCull = deathSelector.value(params, population);
		log.logFlush(nil, \postdeath, iterations);
		//beforeFitness = population.collect(_.fitness).mean;
		this.cull(toCull);
		log.logFlush(nil, \prebreedchoice, iterations);
		//afterFitness = population.collect(_.fitness).mean;
		//[\fitness_delta, afterFitness - beforeFitness].postln;
		toBreed = birthSelector.value(params, population);
		log.logFlush(nil, \prebreed, iterations);
		this.breed(toBreed);
		log.logFlush(nil, \postbreed, iterations);
		iterations = iterations + 1;
	}
	fitnesses {
		^population.collect(_.fitness);
	}
	play {
		//The fire button. trigger this, and the simulation will run until it is bored
		var iterator;
		this.populate;
		playing = true;
		iterator = this.iterator;
		while {iterator.next } {
			//Wouldn't it be nice if I could suspend execution momentarily here?
			/*teaBreak.notNil.if({
				((iterations % teaBreak) == (teaBreak-1).if(
				);
			});*/
			//action happens in iterator
			log.logFlush(nil, \iteratedagain);
		};
	}
	free {
		playing = false;
	}
	iterator {
		/* Return a routine that does the work of triggering the work we want as
			long as things are supposed to be moving along. */
		^Routine.new({while(
			log.logFlush(nil, \loggin);
			{
				log.logFlush(nil, \iteratin, params, iterations, playing);
				log.logFlush(nil, \terminyet, terminationCondition.value(
					params, population, iterations
				));
				(terminationCondition.value(
					params, population, iterations
				).not) && 
				playing 
			},
			{
				log.logFlush(nil, \pretend, iterations);
				this.tend;
				log.logFlush(nil, \posttend, iterations);
				[\iterations, iterations, this.fitnesses.mean].postln;
				log.logFlush(nil, \yieldy, iterations);
				//((iterations % 100) == 0).if({1.wait;});
				true.yield;
			};
		);
		log.logFlush(nil, \donenow, iterations);
		false.yield;}, stackSize: 16384);//seems to overflow easily?
	}
	reset {
		this.cull(population);
		this.populate;
		iterations = 0;
	}
}
