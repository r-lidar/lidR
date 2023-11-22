// Program for preparing a lidar CHM or DSM by removing cavities and spikes.
// Author: Benoît St-Onge, Geophoton inc. (www.geophoton.ca)
// Author's email: bstonge@protonmail.com
// See https://github.com/Geophoton-inc/cavity_fill_v_3/ for more information.
// License: GNU General Public license.
// Version: October 2, 2023

// The core algorithm was first published in:
// St-Onge, B., 2008. Methods for improving the quality of a true orthomosaic of Vexcel UltraCam images created using a
// lidar digital surface model, Proceedings of the Silvilaser 2008, Edinburg, 555-562.
// https://citeseerx.ist.psu.edu/document?repid=rep1&type=pdf&doi=81365288221f3ac34b51a82e2cfed8d58defb10e
// A related version of the algorithm was later published in:
// Ben-Arie, J. R., G. J. Hay, R. P. Powers, G. Castilla et B. St-Onge, 2009. Development and Evaluation of a Pit Filling
// Algorithm for LiDAR Canopy Height Models, IEEE Transactions on Computers and Geosciences, 35:1940-1949
// https://www.sciencedirect.com/science/article/abs/pii/S0098300409000624

// 26 October 2023 - Jean-Romain Roussel - add support for nodata
// 26 October 2023 - Jean-Romain Roussel - Fix memory leak
// 26 October 2023 - Jean-Romain Roussel - avoid using exit();

#include <Rcpp.h>
#include <stdlib.h>
#include <math.h>

float *chm_prep(const float *geom, int snlin, int sncol, int lap_size, float thr_cav, float thr_spk, int med_size, int dil_radius, float nodata);
float* prepare_filter_elements(int);
void prepare_files();
unsigned char * find_holes(int, int, int, int, int, int, int, float, float, int, float*, float*);
float * interpolate(int, int, int, int, int, int, float *, unsigned char *);
float * median_filter(int, int, int, int, int, int, int, float *, unsigned char *);
float get_median(int, float *);

float *chm_prep(const float *geom, int snlin, int sncol, int lap_size, float thr_cav, float thr_spk, int med_size, int dil_radius, float nodata)
{
  float *image, *fe, *gi, *out_scene;
  int mini, maxi, minj, maxj;
  unsigned char *hole_map2;
  long int n;
  float minz = 999999.0;

  // Set the min and max i and j.
  minj=0;
  maxj=sncol-1;
  mini=0;
  maxi=snlin-1;

  if ((image = (float *)malloc((long int)sncol*(long int)snlin*sizeof(float))) == NULL) {
    Rprintf("Insufficient memory to hold the output image.\n"); // # nocov
    return NULL;  // # nocov
  }

  for(n=0;n<snlin*sncol;n++) {
    if (*(geom+n) != nodata && *(geom+n) < minz) minz = *(geom+n);
    *(image+n)=*(geom+n);
  }

  /* Filter slope is hardcoded to 5.0 below as this value seems to work well for all situations. */
  fe = prepare_filter_elements(lap_size);

  if (fe == NULL) {
    free(image);  // # nocov
    return NULL;  // # nocov
  }

  hole_map2 = find_holes(lap_size, snlin, sncol, mini, maxi, minj, maxj, thr_cav, thr_spk, dil_radius, fe, image);

  free(fe);

  if (hole_map2 == NULL) {
    free(image);  // # nocov
    return NULL; // # nocov
  }

  gi = interpolate(snlin, sncol, mini, maxi, minj, maxj, image, hole_map2);

  free(image);

  if (gi == NULL) {
    free(hole_map2);  // # nocov
    return NULL; // # nocov
  }

  out_scene = median_filter(med_size, snlin, sncol, mini, maxi, minj, maxj, gi, hole_map2);

  // Free pointers.
  free(hole_map2);
  free(gi);

  if (out_scene == NULL) {
    return NULL;  // # nocov
  }

  //printf("  Filtering pass completed.\n");

  for(n=0;n<snlin*sncol;n++) {
    if (*(out_scene+n) < minz) {
      *(out_scene+n)=nodata;
    }
  }

  return out_scene;
}

float* prepare_filter_elements(int size) {
  int line, col, nb_fe;
  float dist, fe_sum, fe_center, *fe;


  if(!(fe=(float *)malloc(size*size*sizeof(float)))) {
    Rprintf("Out of memory.\n");  // # nocov
    return NULL;  // # nocov
  }

  nb_fe = 0; /* Initialize number of filter elements */

  fe_sum=0.0;
  for(line=0;line<size;line++)	{
    for(col=0;col<size;col++)	{
      dist = hypot(line-((int)((size-1)/2.0)),col-((int)((size-1)/2.0)));
      if(dist<=(float)(size-1)/2.0) {
        *(fe+line*size+col) = dist;
        nb_fe++;
      }
      else *(fe+line*size+col) = 0.0;
      fe_sum+=*(fe+line*size+col);
    }
  }

  fe_center = (-1)*fe_sum/(float)nb_fe; /* Calculates the value of the center element */

  fe_sum=0.0;
  for(line=0;line<size;line++)	{
    for(col=0;col<size;col++)	{
      dist = hypot(line-((int)((size-1)/2.0)),col-((int)((size-1)/2.0)));
      if(dist<=(float)(size-1)/2.0) *(fe+line*size+col) = fe_center+dist;
      else *(fe+line*size+col) = 0.0;
      fe_sum+=*(fe+line*size+col);
    }
  }

  return fe;
}


unsigned char * find_holes(int size, int snlin, int sncol, int mini, int maxi, int minj, int maxj, float thresh_cavity, float thresh_spike, int dilation_size, float *fe, float *scene)	{

  int 	i,j,nb_fe,filt_j,filt_i,
  s_f; /* Filter half size */
  long int n;
  unsigned char *hole_map, *hole_map2;
  float *hole_score;

  if ((hole_score = (float *)malloc((long int)snlin*(long int)sncol*(long int)4)) == NULL) {
    Rprintf("Insufficient memory for the output hole score scene buffer.\n");  // # nocov
    return NULL;  // # nocov
  }

  if ((hole_map = (unsigned char *)malloc((long int)snlin*(long int)sncol)) == NULL) {
    Rprintf("Insufficient memory for the output hole map scene buffer.\n");  // # nocov
    free(hole_score);  // # nocov
    return NULL;
  }

  if ((hole_map2 = (unsigned char *)malloc((long int)snlin*(long int)sncol)) == NULL) {
    Rprintf("Insufficient memory for the output hole map scene buffer.\n");  // # nocov
    free(hole_score);  // # nocov
    free(hole_map);  // # nocov
    return NULL; // # nocov
  }

  // Initialize rasters.
  for(n=0;n<snlin*sncol;n++)
  {
    *(hole_score+n)=0.0;
    *(hole_map+n)=0;
    *(hole_map2+n)=0;
  }

  s_f = (int)((float)(size-1)/2.0);
  nb_fe = size*size;

  //printf("  Finding cavities.\n");
  for(i=mini;i<maxi;i++) {
    for(j=minj;j<maxj;j++) {
      if(i>s_f && i<snlin-s_f && j>s_f && j<sncol-s_f)	{
        for(filt_i=0;filt_i<size;filt_i++) {
          for(filt_j=0;filt_j<size;filt_j++) {
            *(hole_score+i*sncol+j)+=*(fe+filt_i*size+filt_j)*(*(scene+(i-s_f+filt_i)*sncol+j-s_f+filt_j)/(float)nb_fe);
          }
        }
      }
    }
  }

  /* Map holes from scores and threshold. */

  for(i=mini;i<maxi;i++) {
    for(j=minj;j<maxj;j++) {
      if(*(hole_score+i*sncol+j) > thresh_cavity) *(hole_map+i*sncol+j) = 1; /* Detect pits*/
  if(*(hole_score+i*sncol+j) < thresh_spike) *(hole_map+i*sncol+j) = 1; /* Detect spikes*/
    }
  }

  /* Dilate holes */
  unsigned char *dil_buf;
  int dil_op_size, dil_buf_size;

  dil_op_size = dilation_size*2 + 1; /* Dilatation operator size (side length in pixels) */
  dil_buf_size = dil_op_size*dil_op_size;

  /* Create dilation operator buffer */
  if ((dil_buf = (unsigned char *)malloc(dil_buf_size)) == NULL) {
    Rprintf("Insufficient memory for the output hole map scene buffer.\n");  // # nocov
    free(hole_score);  // # nocov
    free(hole_map);  // # nocov
    return NULL; // # nocov
  }

  /* Initialize buffer */
  for(i=0;i<dil_buf_size;i++) *(dil_buf+i) = 0;

  int bi, bj;
  float buf_dist;

  for(bi=0;bi<dil_op_size;bi++) {
    for(bj=0;bj<dil_op_size;bj++) {
      buf_dist = hypot((abs(dilation_size-bi)),(dilation_size-bj));
      if(buf_dist <= (float)dilation_size) *(dil_buf+bi*dil_op_size+bj) = 1;
    }
  }

  /* Apply dilation */
  for(i=mini;i<maxi;i++) {
    for(j=minj;j<maxj;j++) {
      if(*(hole_map+i*sncol+j) == 1) *(hole_map2+i*sncol+j) = 1;
      if(i>=dilation_size && i<snlin-dilation_size && j>=dilation_size && j<sncol-dilation_size)	{
        for(bi=0;bi<dil_op_size;bi++) {
          for(bj=0;bj<dil_op_size;bj++) {
            if(*(dil_buf+bi*dil_op_size+bj)==1 && *(hole_map+(i-dilation_size+bi)*sncol+j-dilation_size+bj)==1) *(hole_map2+i*sncol+j) = 1;
          }
        }
      }
    }
  }

  return hole_map2;
}


float * interpolate(int snlin, int sncol, int mini, int maxi, int minj, int maxj, float *scene, unsigned char *hole_map2) {
  int ncol, nlig;
  float *gi;

  unsigned char search_left,left,search_right,right,search_up,up,search_down,down;
  int i,j;
  int col_pos,lig_pos,side_pos;
  int d_left = 0;
  int d_right = 0;
  int d_up = 0;
  int d_down = 0;
  int stepmax = 20; /* Largest hole size, in pixels. This is set to contain interpolation in the case of large cavities. */
  int n, step;
  float left_avg,right_avg,up_avg,down_avg,n_avg;

  if ((gi = (float *)malloc((long int)snlin*(long int)sncol*(long int)4)) == NULL) {
    Rprintf("Insufficient memory for the output lidar scene buffer.\n");  // # nocov
    return NULL;  // # nocov
  }

  for(n=0;n<snlin*sncol;n++) {
    *(gi+n)=*(scene+n);
  }

  nlig = snlin;
  ncol = sncol;

  //printf("  Interpolating across cavities.\n");

  for(i=mini;i<maxi;i++) {
    for(j=minj;j<maxj;j++) {

      if(*(hole_map2+(j+i*ncol))==1) {

        search_left = 1;
        left = 0;
        step=1;
        left_avg = 0.0;
        n = 0;

        while(search_left) { /* Moves leftward until search_left = 0 */

  col_pos=j-step;

          if(col_pos<0 || step > stepmax) {
            search_left = 0;
          }
          else {

            for(side_pos=(-step);side_pos<=step;side_pos++) { /* Explores positions for values */
  lig_pos=i+side_pos;

              if(lig_pos>=0 && lig_pos<nlig && *(hole_map2+(col_pos+lig_pos*ncol))!=1) {
                left_avg += *(scene+(col_pos+lig_pos*ncol));
                left = 1;
                search_left = 0; /* Data value was found, search is over. */
  d_left = step;
  n++;
              }
            }
          }
          step++;
          if(n) left_avg = left_avg/(float)n;

        }

        search_right = 1;
        right = 0;
        step=1;
        right_avg = 0.0;
        n = 0;

        while(search_right) { /* Moves rightward until search_right = 0 */

  col_pos=j+step;

          if(col_pos>=ncol || step > stepmax) {
            search_right = 0;
          }
          else {

            for(side_pos=(-step);side_pos<=step;side_pos++) { /* Explores positions for values */
  lig_pos=i+side_pos;

              if(lig_pos>=0 && lig_pos<nlig && *(hole_map2+(col_pos+lig_pos*ncol))!=1) {
                right_avg += *(scene+(col_pos+lig_pos*ncol));
                right = 1;
                search_right = 0; /* Data value was found, search is over. */
  d_right = step;
  n++;
              }
            }
          }
          step++;
          if(n) right_avg = right_avg/(float)n;

        }

        search_up = 1;
        up = 0;
        step=1;
        up_avg = 0.0;
        n = 0;

        while(search_up) { /* Moves leftward until search_left = 0 */

  lig_pos=i-step;

          if(lig_pos<0 || step > stepmax) {
            search_up = 0;
          }
          else {

            for(side_pos=(-step);side_pos<=step;side_pos++) { /* Explores positions for values */
  col_pos=j+side_pos;

              if(col_pos>=0 && col_pos<ncol && *(hole_map2+(col_pos+lig_pos*ncol))!=1) {
                up_avg += *(scene+(col_pos+lig_pos*ncol));
                up = 1;
                search_up = 0; /* Data value was found, search is over. */
  d_up = step;
  n++;
              }
            }
          }
          step++;
          if(n) up_avg = up_avg/(float)n;

        }


        search_down = 1;
        down = 0;
        step=1;
        down_avg = 0.0;
        n = 0;

        while(search_down) { /* Moves leftward until search_left = 0 */

  lig_pos=i+step;

          if(lig_pos>=nlig || step > stepmax) {
            search_down = 0;
          }
          else {

            for(side_pos=(-step);side_pos<=step;side_pos++) { /* Explores positions for values */
  col_pos=j+side_pos;

              if(col_pos>=0 && col_pos<ncol && *(hole_map2+(col_pos+lig_pos*ncol))!=1) {
                down_avg += *(scene+(col_pos+lig_pos*ncol));
                down = 1;
                search_down = 0; /* Data value was found, search is over. */
  d_down = step;
  n++;
              }
            }
          }
          step++;
          if(n) down_avg = down_avg/(float)n;

        }
        n_avg=0;
        if(left) {
          *(gi+(j+i*ncol))=left_avg/(float)d_left;
          n_avg+=1.0/(float)d_left;
        }

        if(right) {
          if(n_avg>0.0) {
            *(gi+(j+i*ncol))+=right_avg/(float)d_right;
            n_avg+=1.0/(float)d_right;
          }
          else {
            *(gi+(j+i*ncol))=right_avg/(float)d_right;
            n_avg+=1.0/(float)d_right;
          }
        }

        if(up) {
          if(n_avg>0.0) {
            *(gi+(j+i*ncol))+=up_avg/(float)d_up;
            n_avg+=1.0/(float)d_up;
          }
          else {
            *(gi+(j+i*ncol))=up_avg/(float)d_up;
            n_avg+=1.0/(float)d_up;
          }
        }

        if(down) {
          if(n_avg>0.0) {
            *(gi+(j+i*ncol))+=down_avg/(float)d_down;
            n_avg+=1.0/(float)d_down;
          }
          else {
            *(gi+(j+i*ncol))=down_avg/(float)d_down;
            n_avg+=1.0/(float)d_down;
          }
        }

        if(n_avg>0.0) *(gi+(j+i*ncol))=*(gi+(j+i*ncol))/(float)n_avg;
      }
    }
  }

  return gi;
}


float * median_filter(int msize, int snlin, int sncol, int mini, int maxi, int minj, int maxj, float *gi, unsigned char *hole_map2)	{
  int 	i,j,filt_j,filt_i, s_f; /* Filter half size */
  long int scursor, n;
  float *out_scene, *mfe; /* Filter element value */

  if ((out_scene = (float *)malloc((long int)snlin*(long int)sncol*(long int)4)) == NULL)
  {
    Rprintf("Insufficient memory for the output lidar scene buffer.\n");  // # nocov
    return NULL;  // # nocov
  }

  for(n=0;n<snlin*sncol;n++) {
    *(out_scene+n) = *(gi+n);
  }

  s_f = (int)((float)(msize-1)/2.0);
  //nb_fe = msize*msize;

  if(!(mfe=(float *)malloc(msize*msize*sizeof(float))))
  {
    Rprintf("Out of memory.\n");  // # nocov
    free(out_scene);  // # nocov
    return NULL; // # nocov
  }

  //print("  Median filtering.\n");
  for(i=mini;i<maxi;i++) {
    for(j=minj;j<maxj;j++) {

      scursor = (long int)i*(long int)sncol+(long int)j;

      if(i>s_f && i<snlin-s_f && j>s_f && j<sncol-s_f)	{

        if(*(hole_map2+i*sncol+j) == 1)   {
          for(filt_i=0;filt_i<msize;filt_i++) {
            for(filt_j=0;filt_j<msize;filt_j++) {

              *(mfe+filt_i*msize+filt_j)=*(gi+(i-s_f+filt_i)*sncol+j-s_f+filt_j);
            }
          }

          *(out_scene+scursor) = get_median(msize*msize, mfe);

        }
        else *(out_scene+scursor) = *(gi+scursor);

      }
    }
  }

  return out_scene;
}

float get_median(int array_size, float * mfe) {

  int i, j, median_observation;
  float current;

  median_observation = round((float)array_size/2.0)-1;

  for(i=1;i<array_size;i++)	{
    current = *(mfe+i);
    j = i;
    while ((j > 0) && (*(mfe+j-1) > current)) {
      *(mfe+j) = *(mfe+j-1);
      j =  j - 1;
    }
    *(mfe+j)= current;
  }

  return *(mfe+median_observation);

}

// [[Rcpp::export(rng = false)]]
Rcpp::NumericVector C_chm_prep(std::vector<float> data, int snlin, int sncol, int lap_size, float thr_cav, float thr_spk, int med_size, int dil_radius, float nodata)
{
  Rcpp::NumericVector out(data.size());
  float* odata = chm_prep(&data[0], snlin, sncol, lap_size, thr_cav, thr_spk, med_size, dil_radius, nodata);
  if (odata == 0) Rcpp::stop("c++ chm_prep failure");
  for (int i = 0 ; i < out.size() ; i++)  out[i] = (double)odata[i];
  free(odata);
  return out;
}
