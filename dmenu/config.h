/* See LICENSE file for copyright and license details. */
/* Default settings; can be overriden by command line. */

static int topbar = 1;                      /* -b  option; if 0, dmenu appears at bottom     */
/* -fn option overrides fonts[0]; default X11 font or font set */
static const char *fonts[] = {
	"monospace:size=11"
};
static const char *prompt      = NULL;      /* -p  option; prompt to the left of input field */
static const char *colors[SchemeLast][2] = {

/* SET TEXT COLOUR */
				/*     fg         bg       */
	/* Normal */
	[SchemeNorm]		 = { "#ffffff", "#333333" },

	/* Selected */
	[SchemeSel]		 = { "#ff3030", "#333333" },

	/* Normal & Highlighted */
	[SchemeNormHighlight]    = { "#30ff30", "#333333" },

	/* Selected & Highlighted */
	[SchemeSelHighlight] 	 = { "#30ff30", "#333333" },

	/* Out */
	[SchemeOut] 		 = { "#f2ff00", "#333333" },

};
/* -l option; if nonzero, dmenu uses vertical list with given number of lines */
static unsigned int lines      = 0;
/* -h option; minimum height of a menu line */

/* SET LINE HEIGHT */
static unsigned int lineheight = 24;

static unsigned int min_lineheight = 8;
/*
 * Characters not considered part of a word while deleting words
 * for example: " /?\"&[]"
 */
static const char worddelimiters[] = " ";