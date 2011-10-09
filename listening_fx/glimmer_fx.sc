GlimmerFilter {
	var outbus, inbus, <glimmerTracker, server, freqBuf, ratioBuf, freqBufPointer, group;
	
	*initClass{
		StartUp.add({
			this.loadSynthDefs
		});
	}
	*loadSynthDefs {
		//This little guy voices a whole bunch of flange frequencies at a given polyphony with known rate
		//input is assumed to be stereo
		//This one tracks frequencies in the input
		SynthDef.new(\findfreqs, {|in, rate = 10, gate=1, freqBuf, freqBufPointer|
			var hasFreq, freq, index, writing=0;
			rate = rate.min(ControlRate.ir/2);//so triggers work
			//we presume freqBuf has 513 samples, and use 512. Why not?
			#freq, hasFreq = Pitch.kr(Mix.ar(In.ar(in, 2)), execFreq: rate);
			writing = hasFreq* gate;
			index = Stepper.kr(Impulse.kr(rate) * writing, max: 511);
			index = (index+(512*(1-writing))).min(512);  //this last bit moves the read head to the end when there is no freq. Maybe I should do this at demand rate instead?
			//freq.poll(10, \written);
			BufWr.kr(
				inputArray: freq,
				bufnum: freqBuf,
				phase: index
			);
			BufRd.kr(numChannels:1,
				bufnum: freqBuf,
				phase: index,
				interpolation:1
			);//.poll(10, \read);
			Out.kr(freqBufPointer, Gate.kr(index, hasFreq));
		}).add;
	}
	*new {|outbus, inbus, glimmerTracker|
		^super.newCopyArgs(outbus,inbus,glimmerTracker).init;
	}
	init {
		server = outbus.server;
		glimmerTracker.isNil.if({
			glimmerTracker = GlimmerTracker.new(inbus);
		});
	}
	play {
		/*~freqBuf = Buffer(s, 513, 1);
		~ratioBuf = Buffer(s, 512, 1);
		// alloc and set the values
		//pitches all 440Hz by default
		s.listSendMsg( ~freqBuf.allocMsg( ~freqBuf.setnMsg(0, 440!513) ).postln );
		//ratios all 1 by default.
		s.listSendMsg( ~ratioBuf.allocMsg( ~ratioBuf.setnMsg(0, 1!513) ).postln );
		//Now..
		~outbus = Bus.new(\audio, 0, 4);
		~freqBufPointer = Bus.control(s, 1);
		~inbus = Bus.audio(s, 2);
		~voxGroup = Group.new(s);
		~fxGroup = Group.after(~voxGroup);
		~outGroup = Group.after(~fxGroup);
		//read hardware in at head of ~voxGroup
		~inVox={ SoundIn.ar([0, 1]) }.play(target:~voxGroup, outbus:~inbus);
		//s.volume.gui;
		s.volume.volume= -20;
		~listener = Synth.head(~fxGroup, \findfreqs, [\in, ~inbus, \freqBuf, ~freqBuf, \freqBufPointer, ~freqBufPointer]);
		~fx = Synth.tail(~fxGroup, \glimmergrains, [\in, ~inbus, \out, ~outbus, \freqBuf, ~freqBuf, \freqBufPointer, ~freqBufPointer, \numBuf, ~ratioBuf, \trigRate, 1, \wideness, 1]);
		~freqBufPointer.get(_.postln);
		~freqBuf.loadToFloatArray(count: -1, action: {|arr| arr.postln;});*/
	}
}

