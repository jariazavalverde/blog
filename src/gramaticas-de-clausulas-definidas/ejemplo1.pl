oracion -->
	sintagma_nominal,
	sintagma_verbal.

sintagma_nominal -->
	determinante,
	sustantivo.
sintagma_nominal -->
	sustantivo.

sintagma_verbal -->
	verbo,
	sintagma_nominal.

determinante --> [los].
determinante --> [un].

sustantivo --> [belmont].
sustantivo --> [vampiros].
sustantivo --> [colmillos].

verbo --> [matan].
verbo --> [tienen].