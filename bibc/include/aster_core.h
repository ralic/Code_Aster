/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF aster_core include  DATE 21/05/2012   AUTEUR COURTOIS M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2012  EDF R&D              WWW.CODE-ASTER.ORG */
/*                                                                    */
/* THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR      */
/* MODIFY IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS     */
/* PUBLISHED BY THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE */
/* LICENSE, OR (AT YOUR OPTION) ANY LATER VERSION.                    */
/* THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL,    */
/* BUT WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF     */
/* MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU   */
/* GENERAL PUBLIC LICENSE FOR MORE DETAILS.                           */
/*                                                                    */
/* YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE  */
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */
/* RESPONSABLE COURTOIS */

#ifndef ASTER_CORE_H
#define ASTER_CORE_H

#include "aster.h"
/*
 *   PUBLIC FUNCTIONS
 *
 */

INTEGER DEFS( JDCGET, jdcget, _IN char *, STRING_SIZE );
extern void DEFSP( JDCSET, jdcset, _IN char *, STRING_SIZE, _IN INTEGER * );
extern PyObject* GetJdcAttr(_IN char *);
extern double get_tpmax();
extern void DEFP(RDTMAX, rdtmax, DOUBLE *);

extern PyObject* asterc_getopt(_IN char *);
extern long asterc_getopt_long(_IN char *, _OUT int *);
extern double asterc_getopt_double(_IN char *, _OUT int *);
extern char* asterc_getopt_string(_IN char *, _OUT int *);
extern void DEFSPP(GTOPTI,gtopti, _IN char *, STRING_SIZE, _OUT INTEGER *, _OUT INTEGER *);
extern void DEFSPP(GTOPTR,gtoptr, _IN char *, STRING_SIZE, _OUT DOUBLE *, _OUT INTEGER *);
extern void DEFSSP(GTOPTK,gtoptk, _IN char *, STRING_SIZE, _OUT char *, STRING_SIZE, _OUT INTEGER *);

extern void DEFSPSPSPPPP(UTPRIN,utprin, _IN char *, _IN STRING_SIZE, _IN INTEGER *,
                         _IN char *, _IN STRING_SIZE,
                         _IN INTEGER *, _IN char *, _IN STRING_SIZE, _IN INTEGER *,
                         _IN INTEGER *, _IN INTEGER *, _IN DOUBLE *);
extern void DEFPP(CHKMSG,chkmsg, _IN INTEGER *, _OUT INTEGER *);
extern void DEFSSP(CHEKSD,cheksd,_IN char *,_IN STRING_SIZE, _IN char *, _IN STRING_SIZE,
                   _OUT INTEGER *);

extern void DEF0(PRHEAD,prhead);

extern PyObject* aster_matfpe(PyObject*, PyObject *);

/* FIN ASTER_CORE_H */
#endif
