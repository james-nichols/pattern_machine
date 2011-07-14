/* the simplest flocking model.

TODO:

* make sonification be handled by a separate class using dependecy perhaps?
* vary noise/order parameters
* use self-freeing envelope
* handle param lag time
* 100% wet delay for cheapo doppler
* per-speaker delay for phase info
* doneAction synth freeing

*/
VicsekParticle {
	/*hold particle location and heading, and enforce boundary contraints.
	updating etc is handled by the wrapping VicsekGrid class*/
	var dim, <pos, <vel, <>synth;
	*new {|pos, vel, dim=2|
		^super.newCopyArgs(dim).pos_(pos).vel_(vel);
	}
	pos_ {|posArray|
		posArray.isNil.if(
			{pos = RealVector.rand(dim)},
			{
				//["wrapping to", posArray.wrap(0,1)].postln;
				pos = RealVector.newFrom(posArray.wrap(0,1));}
		)
	}
	vel_{|velArray|
		velArray.isNil.if(
			{vel=VicsekGrid.randomVector(dim);},
			{vel=RealVector.newFrom(velArray);}
		);
		vel = vel/(vel.norm.max(0.00000001));
	}
	/*play {|server, target, bus, addAction=\addToTail|
	/*		synth = */
		}*/
	free {
		
	}
}
VicsekGrid {
	var <>population, <>noise, <>delta, <>radius, <dim, <tickTime, <clock, <ticker, <particles, myServer, myGroup, myOutBus, addAction, <myBuffer, <isPlaying=false;
	classvar normRng;
	
	*new {|population, noise, delta, radius, dim=2, tickTime=1|
		^super.newCopyArgs(population, noise, delta, radius, dim, tickTime).init; 
	}
	init {
		particles = population.collect({VicsekParticle.new;});
	}
	start {
		clock = TempoClock.new(tickTime.reciprocal, 1);
		ticker = Task.new({loop {this.tick; 1.wait;}}, clock);
		ticker.start;
	}
	stop {
		ticker.stop;
	}
	tick {
		var tempVels = population.collect({this.class.nullVector(dim);});
		//move
		particles.do({|particle, idx|
			//["shunting particle", particle.pos, "by", particle.vel*delta].postln;
			particle.pos = particle.pos+(particle.vel*delta);
			//["shunted particle", particle.pos].postln;
		});
		//aggregate
		(0..(population-2)).do({|i|
			(1..(population-1)).do({|j|
				var a, b;
				a=particles[i];
				b=particles[j];
				//["updatingneighbourhoods", i, j, tempVels[i], tempVels[j]].postln;
				((a.pos-b.pos).norm<radius).if({
					tempVels[i] = tempVels[i] + b.vel;
					tempVels[j] = tempVels[j] + a.vel;
					//["updated", tempVels[i]].postln;
				});
			});
		});
		particles.do({|particle, i|
			particle.vel_(tempVels[i]);
			//["vel now", particle.vel].postln;
		});
		//randomise?
		particles.do({|particle|
			particle.vel_(
				(particle.vel*(1-noise)) + 
				(noise*(this.class.randomVector(dim)))
			);
		});
		isPlaying.not.if({^this});
		//but if we ARE playing...
		particles.do({|particle| 
			particle.synth.set(
				\xpos, particle.pos[0],
				\ypos, particle.pos[1],
				\zpos, particle.pos[2],
				\xvel, particle.vel[0],
				\yvel, particle.vel[1],
				\zvel, particle.vel[2]
			);
		});
	}
	server {^myServer;}
	group {^myGroup;}
	bus {^myOutBus;}
	play {|server, target, bus, addAction=\addToTail|
		//this really should be wrapped in a generic other class
		Task({
			myServer = server ?? Server.default;
			VicsekSynths.loadSynthDefs(myServer);
			myGroup = Group.new((target ? myServer), addAction);
			myOutBus = bus ?? {Bus.audio(myServer, 4)};
			myBuffer = Buffer.read(myServer, "/Users/dan/Library/Application Support/Ableton/Library/Samples/tests/cariboutesque.aif");
			["did that load?"].postln;
			myBuffer.debug;
			server.sync;
			["...now?"].postln;
			myBuffer.debug;
			//final FX bus
			{ |amp = 1.0|
				var son;
				son = In.ar(myOutBus, 4) * amp * 0.7;
				ReplaceOut.ar(myOutBus, son);
			}.play(server, myOutBus, group: myOutBus, addAction:\addToTail);
			server.sync;
			particles.do({|particle| 
				particle.synth = Synth.new(\vicsek_gull4,
					[
						\i_out, myOutBus,
						\gate, 1,
						\buffer, myBuffer,
						\xpos, particle.pos[0],
						\ypos, particle.pos[1],
						\zpos, particle.pos[2],
						\xvel, particle.vel[0],
						\yvel, particle.vel[1],
						\zvel, particle.vel[2]
					],
					myGroup
				);
				particle.synth.debug;
				server.sync;
			});
			server.sync;
			isPlaying = true;
		}).play;
	}
	free {
		particles.do({|particle| 
			particle.synth.isNil.if( {
				particle.synth.set(\gate, 0);
				particle.synth = nil;
			});
			particle.free;
		});
		isPlaying = false;
	}
	*randomVector {|nDim=2|
		//un-normalised vector with angle equidistribution, mean length 1
		normRng.isNil.if({normRng = Pgauss(0.0, 1, inf).asStream});
		^RealVector.newFrom((normRng.nextN(nDim))/((2*nDim).sqrt));
	}
	*nullVector {|nDim=2|
		^RealVector.newFrom(0.dup(nDim));
	}
}
VicsekSynths {
	*loadSynthDefs {|server|
		SynthDef(\vicsek_gull4, {
			|i_out,
			 gate,
			 buffer,
			 xpos,ypos,zpos,
			 xvel,yvel,zvel,
			 tickTime=1,
			 rescale=1.407| //only look at part of the surface
			//synth vars
			var amp, alive, outMono, posX, posY, posZ, pointer, randRatio, windowSize, env;
			posX = xpos.linlin(0, 1, rescale.neg, rescale);
			posY = ypos.linlin(0, 1, rescale.neg, rescale);
			posZ = zpos.linlin(0, 1, -1.05, 1.05);
			alive = (
				((posX.abs)<1) * 
				((posY.abs)<1) * 
				((posZ.abs)<1)
			);
			posX = Lag.kr(posX, tickTime*2).clip2(1);
			posY = Lag.kr(posY, tickTime*2).clip2(1);
			posZ = Lag.kr(posZ, tickTime*2).clip2(1);
			xvel = Lag.kr(xvel, tickTime*2);
			yvel = Lag.kr(yvel, tickTime*2);
			zvel = Lag.kr(zvel, tickTime*2);
			amp = (2 - LFCub.kr(0, iphase: posX.abs.linlin(0,1, 0.75, 0.25))) *
			      (2 - LFCub.kr(0, iphase: posY.abs.linlin(0,1, 0.75, 0.25))) *
			      alive;
			pointer = xvel.linlin(-1,1,0,1);
			windowSize = yvel.linexp(-1, 1, 0.005, 0.1);
			randRatio = zvel.linlin(-1, 1, 0.0, 0.2);
			outMono = Warp1.ar(
				1,						// num channels (Class docs claim only mono works)
				buffer,				// buffer
				pointer,			// start pos
				1,						// pitch shift
				windowSize,		// window size (sec?)
				-1,						// envbufnum (-1=Hanning)
				4,						// overlap
				randRatio,		// rand ratio
				2							// interp (2=linear)
			);
			env = EnvGen.kr(
				  Env.asr(tickTime, 1, tickTime, 'linear'),
				  gate: gate,
				  doneAction: 2);
			Out.ar(i_out, Pan4.ar(outMono, posX, posY, level: amp*env));
		}, [
			\ir,
			nil,
			nil,
			nil,
			nil, nil, nil,
			nil, nil, nil,
			nil,
			\ir
		]).send(server);
	}
}
