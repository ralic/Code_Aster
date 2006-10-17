/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF iniast utilitai  DATE 17/10/2006   AUTEUR MCOURTOI M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2001  EDF R&D              WWW.CODE-ASTER.ORG */
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
#include <stdio.h>
#include <sys/types.h>
#include <string.h>
#include <stdlib.h>

#include "aster.h"

extern void STDCALL(ERRLIC, errlic)();
extern void DEFPPPSP(VERSIO, versio, INTEGER *, INTEGER *, INTEGER *, char *, int, INTEGER *);
#define CALL_VERSIO(a,b,c,d,e) CALLPPPSP(VERSIO,versio,a,b,c,d,e)
extern void DEFP(DATE, date, INTEGER *v);
#define CALL_DATE(a) CALLP(DATE,date,a)

INTEGER DEFPPP(INIAST, iniast, long *r1, long *r2, long *r3)
{
   long ier,ivers,iutil,iniv,ilog,v[6],vrand;
   int a1,a2,a3,delta;
   char vdate[9];
#ifndef _NO_EXPIR
   void STDCALL(ERRLIC, errlic)(void);
#endif
   
   vdate[8] = '\0';
   CALL_VERSIO(&ivers,&iutil,&iniv,&vdate[0],&ilog);
   sscanf(vdate,"%d/%d/%d",&a1,&a2,&a3);
   CALL_DATE(&v[0]);
/* calcul du nombre de jours à la louche */
   delta=(a3-v[0]-1900)*365+(a2+15-v[1])*30+a1-v[2];
   if (delta < 0 ) {
#ifndef _NO_EXPIR
      F_FUNC(ERRLIC, errlic)();
#endif
   }
   srand( (unsigned int) v[4]+v[5] );
   vrand = rand();
   *r1 = vrand * (ivers+a1);
   *r2 = vrand * (iutil+a2);
   *r3 = vrand * (iniv +a3);
   ier = 0;
   return vrand;
}
