/* Program for the construction of cartograms. The program needs polygon     */
/* coordinates and a value for the (relative) target area for each region as */
/* input, calculates the new coordinates, and writes these to a file.        */
/* Postscript images of the original map and the cartogram are created.      */

/* If you use output created by this program please acknowledge the use of   */
/* this code and its first publication in:                                   */
/* Michael T. Gastner, Vivien Seguy and Pratyush More, "A fast flow-based    */
/* algorithm for creating density-equalizing map  projections" (submitted    */
/* for publication).                                                         */

/* The code must be called as "./cartogram.exe input.gen target_area.dat".   */
/* The input coordinates in the first command-line argument must be in       */
/* ArcInfo "generate" format of the type:                                    */

/* 2302 Maine02              ID for region followed by optional description. */
/* 0.302204 -0.188090        Pairs of x-, y-coordinates. Orientation along   */
/* 0.302716 -0.187835        outer boundaries must be clockwise. If a        */
/* ...                       polygon has a hole, the inner boundary must be  */
/* 0.303897 -0.193159        anticlockwise.                                  */
/* 0.302204 -0.188090        Regions are not permitted to overlap. This is   */
/* END                       *not* checked by this code!                     */
/* 2301 Maine01                                                              */
/* 0.333358 -0.200693                                                        */
/* ...                                                                       */
/* 0.333358 -0.200693                                                        */
/* END                                                                       */
/* 2301 Maine01              IDs can repeat if a region constists of         */
/* 0.334699 -0.204771        multiple polygons.                              */
/* ...                                                                       */
/* 0.334699 -0.204771                                                        */
/* END                       Each polygon terminates with END.               */
/* END                       One more END signals the end of the file.       */

/* The target area in the second command-line argument must be given in the  */
/* space-delimited format                                                    */
/* region_ID target_area (optional comment)                                  */
/* For example,                                                              */

/* 1 9.0 Alabama                                                             */
/* 4 11.0 Arizona                                                            */
/* 5 6.0 Arkansas                                                            */
/* ...                                                                       */

/* Output:                                                                   */
/* (1) The output coordinates are written to cartogram.gen in ArcInfo        */
/*     "generate" format.                                                    */
/* (2) A postscript image of the original input is prepared as map.eps, an   */
/*     image of the cartogram as cartogram.eps.                              */
/* (3) The relative area errors are printed to error.dat.                    */

/**************************** Pre-processor directives ***********************/
/* Define dummy macro to disable extern linkage for global variables*/
#ifndef CARTO_H_
#define CARTO_H_
#endif
/******************************** Inclusions. ********************************/

#include <stdio.h>
#include <stdlib.h>
//#include <fftw3.h> . This is redundant, since the header file already includes it.
#include "cartogram.h"
#include <vector> // Use C++ STL to return data; Rcpp maps these to correponding R objects.
/* New inclusion for integration with Rcpp */
#include <Rcpp.h>
using namespace Rcpp;

/*********************************** Main. ***********************************/
/* Use R attribute to make main function accessible in R package. */

// [[Rcpp::export]]
DataFrame transformMap (CharacterVector argv) //std::vector< std::vector<double> > transformMap(CharacterVector argv) // paths contains the paths to the two files
{
  int i, j, k;
  
  /**************************** Parameter checks. ****************************/

  if (argv.size() != 3)
  {
    fprintf(stderr, "ERROR: Use as ./cartogram input.gen target_area.dat\n");
    exit(1);
  }

  /* The equations of motion are integrated on an lx-times-ly square grid.   */
  /* The parameter L will become max(lx, ly). Optimal values for lx and ly   */
  /* are computed in fill_with_density.c. According to the FFTW              */
  /* documentation, "transforms whose sizes are powers of 2 are especially   */
  /* fast". For reasons of efficiency, we only allow powers of 2.            */

  if ((LL <= 0) || ((LL & (~LL + 1)) != LL))
  {
    fprintf(stderr, "ERROR: L must be an integer power of 2.\n");
    exit(1);
  }

  /* If a region contains exactly zero population, it will be replaced by    */
  /* MIN_POP_FAC times the smallest positive population in any region.       */
  /* Obviously, MIN_POP_FAC should be <1. A value around 0.2 is sensible.    */
  /* Regions of zero area cause mathematical problems and are almost never   */
  /* aesthetically desirable.                                                */

  if (MIN_POP_FAC >= 1.0 || MIN_POP_FAC <= 0.0)
  {
    fprintf(stderr, "ERROR: MIN_POP_FAC must be < 1.0.\n");
    exit(1);
  }

  /* We leave some space between the edges of the lx-times-ly grid and the   */
  /* mapped regions so that the cartogram is unaffected by the particular    */
  /* choice of boundary conditions. The parameter PADDING determines how     */
  /* much space we leave. A value around 1.5 is sensible.                    */

  if (PADDING < 1.0)
  {
    fprintf(stderr, "ERROR: PADDING must be >= 1.0.\n");
    exit(1);
  }

  /***************************** Read input data. ****************************/

  /* Read the original polygon coordinates, fill the lx-times-ly grid with   */
  /* density and print a map. rho_ft[] will be filled with the Fourier       */
  /* transform of the initial density.                                       */

  fill_with_density1(argv[1], argv[2]);

  /*************** Allocate memory for the projected positions. **************/
  /* The following definitions are deprecated in C++.
  xproj = (double*) malloc(lx * ly * sizeof(double));
  yproj = (double*) malloc(lx * ly * sizeof(double)); */

  xproj = new double[lx * ly];
  yproj = new double[lx * ly];

  cartcorn = (POINT **)malloc(n_poly * sizeof(POINT *));
  for (i = 0; i < n_poly; i++)
    cartcorn[i] = (POINT *)malloc(n_polycorn[i] * sizeof(POINT));

  /* xproj[i*ly+j] and yproj[i*ly+j] will store the current position of the  */
  /* point that started at (i+0.5, j+0.5).                                   */

  for (i = 0; i < lx; i++)
  {
#pragma omp parallel for
    for (j = 0; j < ly; j++)
    {
      xproj[i * ly + j] = i + 0.5;
      yproj[i * ly + j] = j + 0.5;
    }
  }

  /************** First integration of the equations of motion. **************/

  printf("Starting integration 1 out of %i\n", N_INTEGR);
  integrate();
  project(false); /* FALSE because we do not need to project the graticule. */

  /********* Additional integrations to come closer to target areas. *********/
  /*
  xproj2 = (double*) malloc(lx * ly * sizeof(double));
  yproj2 = (double*) malloc(lx * ly * sizeof(double));*/

  xproj2 = new double[lx * ly];
  yproj2 = new double[lx * ly];

  for (k = 1; k < N_INTEGR; k++)
    fill_with_density2();

  /* Copy the current graticule before resetting. We will construct the    */
  /* final graticule by interpolating (xproj2, yproj2) on the basis of     */
  /* (xproj, yproj).                                                       */

  for (i = 0; i < lx; i++)
  {
#pragma omp parallel for
    for (j = 0; j < ly; j++)
    {
      xproj2[i * ly + j] = xproj[i * ly + j];
      yproj2[i * ly + j] = yproj[i * ly + j];
    }
  }

  for (i = 0; i < lx; i++)
  {
#pragma omp parallel for
    for (j = 0; j < ly; j++)
    {
      xproj[i * ly + j] = i + 0.5;
      yproj[i * ly + j] = j + 0.5;
    }
  }

  printf("Starting integration %i out of %i\n", k + 1, N_INTEGR);
  integrate();
  project(true); /* TRUE because we need to project the graticule too. */

  /* Overwrite xproj with xproj2. */

  for (i = 0; i < lx; i++)
  {
    for (j = 0; j < ly; j++)
    {
      xproj[i * ly + j] = xproj2[i * ly + j];
      yproj[i * ly + j] = yproj2[i * ly + j];
    }
  }

  ps_figure((char *)"cartogram.eps", cartcorn);
  output_to_ascii();

  /******************************* Return projected coordinates ******************************/

  // First, count the number of coordinates present acorss all polygons

  int num_coords = 0;
  for (i = 0; i < n_poly; i++)
  {
    for (j = 0; j < n_polycorn[i]; j++)
      num_coords++;
  }

  /* Declare Vectors to hold the x-coordinates, 
   * y-coordinates, the polygon_id, and the region _id. */
  NumericVector x_coords(num_coords);
  NumericVector y_coords(num_coords);
  IntegerVector pid(num_coords);
  IntegerVector rid(num_coords);
  
  int counter = 0;
  // iterate through every polygon
  for (i = 0; i < n_poly; i++)
  {
    //iterate through each coordinate of every polygon
    for (j = 0; j < n_polycorn[i]; j++)
    {
      /* Add values to the vector*/ 
      x_coords[counter] = cartcorn[i][j].x;
      y_coords[counter] = cartcorn[i][j].y;
      pid[counter] = polygon_id [i];
      rid[counter] = region_id [ polygon_id [i] ];
      counter++ ;   
    }
  }

  /* Construct the data frame to return */
  DataFrame all_coords = DataFrame::create( _["X_Coordinate"] = x_coords , _["Y_Coordinate"] = y_coords , _["Region_ID"] = rid , _["Polygon_ID"] = pid);  

  /******************************* Free memory. ******************************/

  fftw_destroy_plan(plan_fwd);
  fftw_free(rho_ft);
  fftw_free(rho_init);
  for (i = 0; i < n_poly; i++)
    free(cartcorn[i]);
  free(cartcorn);
  free(polygon_id);
  free(region_id);
  free(region_id_inv);
  for (i = 0; i < n_reg; i++)
    free(polyinreg[i]);
  free(polyinreg);
  free(n_polyinreg);
  free(xproj);
  free(yproj);
  free(xproj2);
  free(yproj2);
  free(target_area);
  for (i = 0; i < n_poly; i++)
    free(polycorn[i]);
  free(polycorn);
  free(n_polycorn);

  /******************************* Return DataFrame ******************************/
  return (all_coords);
}
