/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF MKDESC ALGELINE  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
/* ------------------------------------------------------------------ */
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



/* sets vlen to approximately 64 and stride to approximately len/vlen,
 * but corrects for bad strides and edge effects.
 */

/*********************** FORTRAN entry points **************************/

/* descriptor must be of size: 74 + rows/64 + nonzeros/64 */
/* scratch must be at least as big as sparse matrix */

   extern void compsf();
   void MKDESC (long *descriptor, long *pntr, long *nrows, long *scratch)
#elif defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX 
   void mkdesc_ (long *descriptor, long *pntr, long *nrows, long *scratch)
#elif defined HPUX
   void mkdesc (long *descriptor, long *pntr, long *nrows, long *scratch)
#elif defined PPRO_NT
   void __stdcall MKDESC (long *descriptor, long *pntr, long *nrows, long *scratch)
#endif
{
#if defined SOLARIS || PPRO_NT || HPUX || IRIX || TRU64 || SOLARIS64 || P_LINUX 
   void abort(void);
   printf("\n Arret provoque dans MKDESC \n");
   abort();
#elif defined CRAY
  long *uncompressed_flags, *compressed_flags, *inclusive_last_start, *nonzerop;
  long *shape;
  long j, n, m;
  long vlen, stride, full;
  long vlen_m, stride_m, full_m;

  m = *nrows;
  n = pntr[m] - 1;
  compsf(n, &vlen, &stride, &full);
  compsf(m, &vlen_m, &stride_m, &full_m);
  shape = shape_from_sd(descriptor, m);
  inclusive_last_start = inclusive_last_start_from_sd(descriptor, m);
  compressed_flags = bools_from_sd(descriptor, m);
  nonzerop = nonzerop_from_sd(descriptor, m);
  uncompressed_flags = scratch;
  shape[0] = vlen;   shape[1] = stride;    shape[2] = full;
  shape[3] = vlen_m; shape[4] = stride_m;  shape[5] = full_m;

  /* nonzerop[j] = (pntr[j] != pntr[j+1])     (nonzerop are packed booleans) */
  PNEQWUZ(nonzerop, pntr, add_ptr(pntr, 1), vlen_m, stride_m, full_m);

  for (j = 0; j < n; j++) uncompressed_flags[j] = 0;
#pragma ivdep
  for (j = 0; j < m; j++) uncompressed_flags[pntr[j]-1] = 1;

  /* compress the flags and also find the last segment start handled by
   * each vector register element. */
  CLASTIN(inclusive_last_start, compressed_flags, uncompressed_flags,
          vlen, stride, full);
#endif
}
