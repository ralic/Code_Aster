/* ------------------------------------------------------------------ */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2011  EDF R&D              WWW.CODE-ASTER.ORG */
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
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,      */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */
#include "aster.h"
#include <stdlib.h>

INTEGER DEFSS(GTENV, gtenv, char *name, STRING_SIZE lname, char *value, STRING_SIZE lvalue)
{
   void *malloc(size_t size);
   long code,lval,i,lmin;
#define lg 64
   char nom[lg],*val;

   long ln,lv;
   lv = (long) lvalue;
   ln = (long) lname;

   for (i=0;i<lg;i++) {nom[i]=' ';}
   lmin = ln;
   if (ln > lg-1) lmin = lg-1;
   for (i=0;i<lmin;i++) {nom[i]=name[i];}
   nom[lmin] = '\0';
   for (i=0;i<lv;i++) {value[i]=' ';}
   val = getenv(nom);
   if (val == NULL ) { code = 0;}
   else              {
      code = 1;
      lval = strlen(val);
      if ( lval > lv ) {lval =lv;}
      for (i=0;i<lval;i++) {value[i]=val[i];}
      for (i=lval;i<lv;i++) {value[i]=' ';}
   }
   return code;
}
