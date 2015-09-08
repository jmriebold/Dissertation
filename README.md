This repository contains all of my dissertation data, along with the scripts I used for analysis and plotting, and the dissertation itself.

# Contents
The main directory contains a PDF of the dissertation, along with my BibTeX file.

The Data subdirectory contains the various data files used in the dissertation, in both Excel and (Unicode) plaintext. Speaker-Data contains demographic information, VOIS3D-Data is a VOIS3D session file (used for the calculation of overlap fractions), and Vowel-Data contains the formant measurements used for the meat of the paper.

The Scripts subdirectory contains several scripts I wrote to create the graphics in the dissertation. generate-trajectory-plots.r generates a set of SSANOVA plots along multiple dimensions, generate-vowel-plots.r generates a set of vowel plots along multiple dimensions, and run-stats.r builds and tests several statistical models.