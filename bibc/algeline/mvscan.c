/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF MVSCAN ALGELINE  DATE 02/06/2006   AUTEUR MCOURTOI M.COURTOIS */
#include <stdio.h>
#ifdef CRAY
/*
* Copyright (c) 1992 Carnegie Mellon University
*                    SCAL project: Guy Blelloch, Siddhartha Chatterjee,
*                                  Jonathan Hardwick, Jay Sipelstein,
*                                  Marco Zagha
* All Rights Reserved.
*
* Permission to use, copy, modify and distribute this software and its
* documentation is hereby granted, provided that both the copyright
* notice and this permission notice appear in all copies of the
* software, derivative works or modified versions, and any portions
* thereof, and that both notices appear in supporting documentation.
*
* CARNEGIE MELLON ALLOWS FREE USE OF THIS SOFTWARE IN ITS "AS IS"
* CONDITION.  CARNEGIE MELLON DISCLAIMS ANY LIABILITY OF ANY KIND FOR
* ANY DAMAGES WHATSOEVER RESULTING FROM THE USE OF THIS SOFTWARE.
*
* The SCAL project requests users of this software to return to
*
*  Marco Zagha and Guy Blelloch         marco.zagha@cs.cmu.edu
*  School of Computer Science           guy.blelloch@cs.cmu.edu
*  Carnegie Mellon University
*  5000 Forbes Ave.
*  Pittsburgh PA 15213-3890
*
* any improvements or extensions that they make and grant Carnegie Mellon
* the rights to redistribute these changes.
*/


/* SEGMENT DESCRIPTOR FORMAT:
 *  [SHAPE FACTORS ] [INCLUSIVE LAST-START ] [NONZEROP FLAGS] [ COMPR. FLAGS ]
 *        6                   64                  siz_fopb(m)      siz_fopb(n)
 */

#define add_ptr(v, a)   ((v) + (a))

#define shape_from_sd(sd, m)                (sd)
#define inclusive_last_start_from_sd(sd, m) add_ptr(sd, 6)
#define nonzerop_from_sd(sd, m)             add_ptr(sd, 70)
#define bools_from_sd(sd, m)                add_ptr(sd, 70 + siz_fopb(m))

#define siz_fopb(length) (((length) >> 6) + 2) /* max size of packed boolean */


#define BANKS_MASK 3   /* don't allow stride to be multiple of 4 */

/* sets vlen to approximately 64 and stride to approximately len/vlen,
 * but corrects for bad strides and edge effects.
 */


/*********************** FORTRAN entry points **************************/
/* scratch must be at least as big as sparse matrix */
  void MVSCAN(result, matrix, vector, indx, pntr, descriptor, nrows, scratch)
  double *result, *matrix, *vector;
  long *indx, *pntr, *descriptor;
  double *scratch;
  long *nrows;
#elif defined SOLARIS || IRIX || TRU64 || LINUX64 || SOLARIS64 || P_LINUX 
   void mvscan_ (double *result, double *matrix, double *vector,
                 long *indx, long *pntr, long *descriptor, long *nrows, double *scratch)
#elif defined HPUX
   void mvscan (double *result, double *matrix, double *vector,
                 long *indx, long *pntr, long *descriptor, long *nrows, double *scratch)
#elif defined PPRO_NT
   void __stdcall MVSCAN (double *result, double *matrix, double *vector,
                          long *indx, long *pntr, long *descriptor, long *nrows, double *scratch)
#endif
{
#if defined SOLARIS || PPRO_NT || HPUX || IRIX || TRU64 || LINUX64 || SOLARIS64 || P_LINUX 
   void abort(void);
   printf("\n Arret provoque dans MVSCAN \n");
   abort();
#endif
#ifdef CRAY
  void XMVMULT(), BCKPUZD();
  long vlen, stride, full;
  long vlen_m, stride_m, full_m;
  long *flags, *nonzerop, *lastind;
  long *shape;
  double *scanned_s;
  long n, m;

  m = *nrows;
  n = pntr[m] - 1;
  shape = shape_from_sd(descriptor, m);
  vlen   = shape[0]; stride   = shape[1]; full   = shape[2];
  vlen_m = shape[3]; stride_m = shape[4]; full_m = shape[5];

  flags = bools_from_sd(descriptor, m);
  nonzerop = nonzerop_from_sd(descriptor, m);
  lastind = inclusive_last_start_from_sd(descriptor, m);
  scanned_s = scratch;

  /* matrix-vector multiply, leaving result at beginning of each row */
  vector = add_ptr(vector, -1);  /* -1 because indx is one-based */
  XMVMULT(scanned_s, matrix, indx, vector, add_ptr(flags, stride-1), lastind,
            0.0, vlen, stride, full);

  /* gather the result from the beginning of each row */
  scanned_s = add_ptr(scanned_s, -1);  /* -1 because pntr is one-based */
  /* gather from scanned_s but use identity for zero length rows */
  BCKPUZD(result, scanned_s, pntr, nonzerop,
          vlen_m, stride_m, full_m);
#endif
}
