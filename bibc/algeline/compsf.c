/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF compsf ALGELINE  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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

#pragma inline compsf
#endif

void compsf(len, vlen, stride, rem)
long len, *vlen, *stride, *rem;
{
#ifdef CRAY
  long len1, s, div;
  len1 = len-1;
  s = (len1>>6)+1;
  s = s + !(s & BANKS_MASK);  /* increment stride if s is bad stride */
  *stride = s;
  div = len1/s;
  *vlen = div+1;
  *rem = len1-div * s+1;
#endif
}
