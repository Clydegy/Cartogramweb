/******************************** Pre-processor directives  ********************************/

/** To define the macro EXTERN. Note that CARTO_H will be defined only in the main.c file (where the global variables are declared and hence not initialized)  **/
#ifdef CARTO_H_
  #define EXTERN
#else
  #define EXTERN extern
#endif

/******************************** Inclusions. ********************************/

#include <fftw3.h>
#include <string>

/* Rcpp attributes to use openmp library. */
// [[Rcpp::plugins(openmp)]]
// Protect against compilers without OpenMP
#ifdef _OPENMP
#include <omp.h>
#endif

/******************************** Definitions. *******************************/

#define LL (512)            /* Maximum dimension of the FFT lattice is L x L. */
#define MIN_POP_FAC  (0.2)      /* Replace area 0 by the minimum times this. */
#define PADDING (1.5)          /* Determines space between map and boundary. */
#define BLUR_WIDTH (5e0)  /* Width of Gaussian blur to smoothen the density. */
#define N_INTEGR (5)                              /* Number of integrations. */
#define MAX_STRING_LENGTH (1000)
#define PIE (3.14159265358979323846264338327950288419716939937510)

/********************************** Macros. **********************************/

#define MAX(a,b) (((a)>(b)) ? (a) : (b))
#define MIN(a,b) (((a)>(b)) ? (b) : (a))

/*********************************** Types. **********************************/

typedef struct {      /* Useful structure for coordinates in two dimensions. */
  double x;
  double y;
} POINT;

/*
This clashes with an inbuilt R enum (since R is based off C). We make use of C++ truth values. 
typedef enum {          
  FALSE,                
  TRUE
} BOOLEAN; 
*/
/***************************** Global variables. *****************************/

/* Variables for map. */

EXTERN double map_maxx, map_maxy, map_minx, map_miny, *target_area,
  *xproj, *xproj2, *yproj, *yproj2;
EXTERN int max_id, n_poly, *n_polycorn, *n_polyinreg, n_reg, *polygon_id,
  **polyinreg, *region_id, *region_id_inv;
EXTERN POINT **cartcorn, **polycorn;

/* Variables for digitizing the density. */

EXTERN double *rho_ft, *rho_init;
EXTERN fftw_plan plan_fwd;
EXTERN int lx, ly;

/**************************** Function prototypes. ***************************/

double polygon_area (int ncrns, POINT *polygon);
void fill_with_density1 (char *gen_file_name, char *area_file_name);
void fill_with_density2 (void);
void read_gen (char *gen_file);
void ps_figure (char *ps_name, POINT **corn);
double interpol (double x, double y, double *grid, char *zero);
void integrate (void);
void project (bool proj_graticule);
void output_to_ascii (void);

