/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF GTENV UTILITAI  DATE 23/09/2002   AUTEUR MCOURTOI M.COURTOIS */
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
#include <string.h>
#ifdef CRAY
#include <fortran.h>
   long GTENV( _fcd nameF, _fcd valueF )
#elif defined SOLARIS || IRIX || P_LINUX || TRU64 || SOLARIS64 
   long gtenv_(char *name, char *value, long lname, long lvalue)
#elif defined HPUX
   long gtenv(char *name, char *value, long lname, long lvalue)
#elif defined PPRO_NT
   extern long __stdcall GTENV(char *name,unsigned long lname, char *value,unsigned long lvalue)
#endif
{
void *malloc(size_t size);
long code,lval,i,lmin;
#define lg 64
char nom[lg],*val;
extern char *getenv (const char *name);

#if defined  CRAY
    char *value,*name;
    long ln,lv;
    ln = _fcdlen(nameF);
    name=_fcdtocp(nameF);
    lv = _fcdlen(valueF);
    value = (char *) malloc(lv+1);
#else
    long ln,lv;
    lv = (long) lvalue;
    ln = (long) lname;
#endif
for (i=0;i<lg;i++) {nom[i]=' ';}
lmin = ln;
if (ln > lg-1) lmin = lg-1;
for (i=0;i<lmin;i++) {nom[i]=name[i];}
nom[lmin] = '\0';
for (i=0;i<lv;i++) {value[i]=' ';}
val = getenv(nom);
if (val == NULL ) { code = 0;}
else              { code = 1;
                    lval = strlen(val);
                    if ( lval > lv ) {lval =lv;}
                    for (i=0;i<lval;i++) {value[i]=val[i];}
                    for (i=lval;i<lv;i++) {value[i]=' ';}

#if defined CRAY
                    strncpy (_fcdtocp(valueF),value,lv);
                    free (value);
#endif
}
return ( code );
}
