# Detect the period in data known to be periodic.

I have a bunch _(on the order of several hundred)_ of ever increasing numbers that reflect moments
in time when a blip was detected. I know that most blips should have almost the same delay in
between them. But some blips have passed under the radar _(a "miss")_, and there are false
positives as well _(a "glitch")_. How can I separate the good, periodic blips and predict when
blips would occur in the future?

**Note**: I actually compute with the usual floating point precision, but the numbers here will be
given rounded to 3 significant digits for ease of reading.

1. The average delay between blips is $0.474$ and the variance is $1.557 \cdot 10^{-3}$. he actual
   period appears to be a tiny bit shorter: when I listen to the actual record against a metronome
   set to the average delay, it does not take long for the metronome to start dragging.

1. If I plot my data as pairs in $\mathbb{N} \times \mathbb{R}$ and fit a straight line, it will
   be systematically biased: if there are more misses than glitches, perceived slope will
   consistently be more than actual, and the other way around for glitches.

   If I train on a random half of samples and validate against the other half, I get values like
   $T = 0.476, \mathrm{mse} = 6.627 \cdot 10^{-2}$. Audibly, the predictions are dragging even
   more.

1. I can take differences between blip time stamps, and detect a large, dense cluster — the
   average of that cluster will be the rhythm I need.

   I tried this as well, using K-Means clustering. The average of the largest cluster hangs around
   $3.881$ for anywhere between $2$ to $9$ clusters, and then starts to fluctuate around $0.471$
   and $0.465$. The truth is closer to $0.471$, but not precisely.


