/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF iniast utilitai  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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

#if defined CRAY
   long INIAST(long *r1, long *r2, long *r3)
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
#define INIAST iniast_
   long iniast_(long *r1, long *r2, long *r3)
#elif defined HPUX
#define INIAST iniast
   long iniast(long *r1, long *r2, long *r3)
#elif defined PPRO_NT
   long __stdcall INIAST(long *r1, long *r2, long *r3)
#endif
{

int status,days;
char * string;
long ier,ivers,iutil,iniv,ilog,v[6],vrand;
int a1,a2,a3,delta;
unsigned long ldate;
char vdate[9];

#if defined CRAY
   extern void ERRLIC(void);
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
#define ERRLIC errlic_
#define VERSIO versio_
#define DATE date_
   extern void errlic_(void);
   extern void versio(long *a, long *b, long *c, char *v, long *d, unsigned long *l);
   extern void date (long *v);
#elif defined HPUX
#define ERRLIC errlic
#define VERSIO versio
#define DATE date
   extern void errlic(void);
   extern void versio(long *a, long *b, long *c, char *v, long *d, unsigned long *l);
   extern void date (long *v);
#elif defined PPRO_NT
   extern void __stdcall ERRLIC(void);
   extern void __stdcall VERSIO(long *a, long *b, long *c, char *v, unsigned long *l, long *d);
   extern void __stdcall DATE (long *v);
#endif

 vdate[8] = '\0' ;
#if defined PPRO_NT
 VERSIO (&ivers,&iutil,&iniv,&vdate[0],&ldate,&ilog);
#elif defined SOLARIS || HPUX || IRIX || P_LINUX || TRU64 || SOLARIS64 
 VERSIO (&ivers,&iutil,&iniv,&vdate[0],&ilog,&ldate);
#endif
 sscanf(vdate,"%d/%d/%d",&a1,&a2,&a3);
 DATE(&v[0]);
/* calcul du nombre de jours à la louche */
 delta=(a3-v[0]-1900)*365+(a2+15-v[1])*30+a1-v[2];
 delta=100;
if (delta == 0 ) {
     printf ("\nCETTE VERSION DE CODE_ASTER EXPIRE CE SOIR A MINUIT \n");
}
else if (delta < 0 ) {
     printf ("\nCETTE VERSION DE CODE_ASTER EST EXPIREE \n");
     ERRLIC();
}
else if (delta < 60 ) {
     printf ("\nCETTE VERSION DE CODE_ASTER EXPIRE DANS : %d  JOUR(S) \n",delta);
}
 srand( (unsigned int) v[4]+v[5] );
 vrand = rand();
 *r1 = vrand * (ivers+a1);
 *r2 = vrand * (iutil+a2);
 *r3 = vrand * (iniv +a3);
 ier = 0;
 return (vrand);
}
