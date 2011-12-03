PSPhenotype {
	var <chromosome;
	var <>fitness=0;
	var <logicalAge=0;
	var <birthTime;
	
	classvar <>genomeSize = 3;
	
	*new{|chromosome|
		^super.new.init(chromosome);
	}
	*newRandom {|forceGenomeSize|
		var newChromosome = {1.0.rand;}.dup(forceGenomeSize ? genomeSize);
		^this.new(newChromosome);
	}
	init {|chromosome|
		//don't just copy a ref- each phenotype gets its own copy.
		this.chromosome = Array.newFrom(chromosome);
	}
	chromosome_ {|newChromosome|
		chromosome = newChromosome;
	}
	play {|out|
		NotYetImplementedError.new.throw;
	}
	clockOn {
		birthTime = Date.gmtime.rawSeconds;
	}
	wallClockAge {
		birthTime.isNil.if({^0.0}, {
			^Date.gmtime.rawSeconds - birthTime;
		});
	}
	incAge {
		//incremenets logical (iteration count) age
		logicalAge = logicalAge +1;
	}
	printOn { arg stream;
		stream << this.class.asString <<"(" << chromosome << ")";
	}
}

PSSynthDefPhenotype : PSPhenotype {
	//A phenotype mapping a chromosome to the inputs of a Synth
	classvar <synthdef = \ps_reson_saw;
	classvar <map;
	
	var <mappedArgs;
	*initClass {
		StartUp.add {
			this.setUpSynthDefs;
		};
		StartUp.add {
			this.setUpMappingToSynthDef;
		};
	}
	*setUpSynthDefs {
		/*Nothing to do here;  I just use the generic \ps_reson_saw in the
		playsynthdefs file. Subclasses might be otherwise.*/
	}
	*setUpMappingToSynthDef {	
		map = (
			\pitch: \midfreq.asSpec,
			\ffreq: \midfreq.asSpec,
			\rq: \rq.asSpec
		);
	}
	stop {|synth|
		synth.set(\gate, 0);
	}
	chromosomeAsSynthArgs {
		/*This list comprehension is not especially clear now, is it?
		What it does is zip together the key, map spec and value 
		lists into one, then iterates over this, returning mapped values
		associated with their keys as a synth expects*/
		^(all {: [keySpecVal[0], keySpecVal[1].map(keySpecVal[2])],
			keySpecVal <- (this.class.map.asSortedArray +++ chromosome)
		}).flat;
	}
}
	
