/* ------------------------------------------------------------------ */
/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF astermodule supervis  DATE 21/06/2004   AUTEUR DURAND C.DURAND */
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
/* RESPONSABLE                                 D6BHHJP J.P.LEFEBVRE   */
/* ------------------------------------------------------------------ */

#include <stdio.h>
#include "Python.h"
#include <math.h>
#include <ctype.h>


#ifndef min
#define min(A,B)  ((A) < (B) ? (A) : (B))
#endif

#define VARIABLE_LEN 16




#ifndef UTILITES_H        /*{*/
#define UTILITES_H


/* pour indiquer le  statut des arguments des fonctions. */

#ifdef _IN
#error  _IN est deja definie
#endif
#define _IN

#ifdef _OUT
#error  _OUT est deja definie
#endif
#define _OUT

#ifdef _INOUT
#error  _INOUT est deja definie
#endif
#define _INOUT

#ifdef _UNUSED
#error  _UNUSED est deja definie
#endif
#define _UNUSED

/* Pour définir les appels et signatures de fonctions appelables en Fortran
 * On utilise l'operateur de concatenation ## du préprocesseur C (cpp) pour ajouter l'underscore
 * au nom en majuscule ou minuscule de la fonction à définir ou à appeler.
 * Pour les anciens compilateurs non ANSI, utiliser un commentaire vide à la place.
 * Pour appeler une subroutine Fortran de nom SUB avec un argument string et 2 arguments autres, faire:
 * #define CALL_SUB(a,b,c) CALLSPP(SUB,sub,a,b,c)
 * puis : CALL_SUB(a,b,c)
 * Pour définir une fonction C de nom SUB avec un argument string et 2 arguments autres,
 * appelable depuis le fortran, faire:
 * void DEFSPP(SUB,sub,char * nomobj,int lnom,double *d,INTEGER *i)
 * {
 * }
 * ici, lnom est l'entier qui indique la longueur de la chaine Fortran nomobj
 * Les macros définies ici ne servent qu'à former le nom de la fonction et à
 * mettre les arguments dans le bon ordre. On utilise l'ordre de Visual comme
 * base (pointeur char suivi d'un int) et on reordonne pour les autres compilateurs.
 */

/* Operateur de concatenation */
#define  _(A,B)   A##B

#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX
#define F_FUNC(UN,LN)                            _(LN,_)

#elif defined HPUX
#define F_FUNC(UN,LN)                            LN

#elif defined PPRO_NT
#define F_FUNC(UN,LN)                            UN

#endif

#if defined SOLARIS || IRIX || TRU64 || SOLARIS64 || P_LINUX || HPUX
#define STDCALL(UN,LN)                           F_FUNC(UN,LN)

#define CALLSS(UN,LN,a,b)                        F_FUNC(UN,LN)(a,b,strlen(a),strlen(b))
#define CALLSSP(UN,LN,a,b,c)                        F_FUNC(UN,LN)(a,b,c,strlen(a),strlen(b))
#define CALLSSPP(UN,LN,a,b,c,d)                        F_FUNC(UN,LN)(a,b,c,d,strlen(a),strlen(b))
#define CALLSSPPP(UN,LN,a,b,c,d,e)                        F_FUNC(UN,LN)(a,b,c,d,e,strlen(a),strlen(b))
#define CALLS(UN,LN,a)                     F_FUNC(UN,LN)(a,strlen(a))
#define CALLSP(UN,LN,a,b)                     F_FUNC(UN,LN)(a,b,strlen(a))
#define CALLSPP(UN,LN,a,b,c)                     F_FUNC(UN,LN)(a,b,c,strlen(a))
#define CALLSPPP(UN,LN,a,b,c,d)                     F_FUNC(UN,LN)(a,b,c,d,strlen(a))
#define CALLSPPPP(UN,LN,a,b,c,d,e)                     F_FUNC(UN,LN)(a,b,c,d,e,strlen(a))

#define CALLSPPPPS(UN,LN,a,b,c,d,e,f)                     F_FUNC(UN,LN)(a,b,c,d,e,f,strlen(a),strlen(f))
#define CALLPPPSP(UN,LN,a,b,c,d,e)                        F_FUNC(UN,LN)(a,b,c,d,e,strlen(d))
#define DEFPPPSP(UN,LN,a,b,c,d,ld,e)                        STDCALL(UN,LN)(a,b,c,d,e,ld)

#define DEFS(UN,LN,a,la)                      STDCALL(UN,LN)(a,la)
#define DEFSP(UN,LN,a,la,b)                      STDCALL(UN,LN)(a,b,la)
#define DEFSPP(UN,LN,a,la,b,c)                   STDCALL(UN,LN)(a,b,c,la)
#define DEFSPPP(UN,LN,a,la,b,c,d)                   STDCALL(UN,LN)(a,b,c,d,la)
#define DEFSPPPP(UN,LN,a,la,b,c,d,e)                   STDCALL(UN,LN)(a,b,c,d,e,la)

#define DEFSPPPPS(UN,LN,a,la,b,c,d,e,f,lf)          STDCALL(UN,LN)(a,b,c,d,e,f,la,lf)
#define DEFSSPPPPP(UN,LN,a,la,b,lb,c,d,e,f,g)    STDCALL(UN,LN)(a,b,c,d,e,f,g,la,lb)
#define DEFSSPPPP(UN,LN,a,la,b,lb,c,d,e,f)    STDCALL(UN,LN)(a,b,c,d,e,f,la,lb)
#define DEFSSPPP(UN,LN,a,la,b,lb,c,d,e)    STDCALL(UN,LN)(a,b,c,d,e,la,lb)
#define DEFSSPP(UN,LN,a,la,b,lb,c,d)    STDCALL(UN,LN)(a,b,c,d,la,lb)
#define DEFSSP(UN,LN,a,la,b,lb,c)    STDCALL(UN,LN)(a,b,c,la,lb)
#define DEFSS(UN,LN,a,la,b,lb)    STDCALL(UN,LN)(a,b,la,lb)

#define DEFSSSPPPPS(UN,LN,a,la,b,lb,c,lc,d,e,f,g,h,lh)    STDCALL(UN,LN)(a,b,c,d,e,f,g,h,la,lb,lc,lh)
#define CALLSSSPPPPS(UN,LN,a,b,c,d,e,f,g,h)    F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,strlen(a),strlen(b),strlen(c),strlen(h))

#define DEFSPPSSSP(UN,LN,a,la,b,c,d,ld,e,le,f,lf,g)    STDCALL(UN,LN)(a,b,c,d,e,f,g,la,ld,le,lf)
#define CALLSPPSSSP(UN,LN,a,b,c,d,e,f,g)    F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(a),strlen(d),strlen(e),strlen(f))

#define DEFSSPPPSP(UN,LN,a,la,b,lb,c,d,e,f,lf,g)             STDCALL(UN,LN)(a,b,c,d,e,f,g,la,lb,lf)
#define CALLSSPPPSP(UN,LN,a,b,c,d,e,f,g)                     F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(a),strlen(b),strlen(f))
#define CALLSSPPPPP(UN,LN,a,b,c,d,e,f,g)                     F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(a),strlen(b))
#define DEFSPSP(UN,LN,a,la,b,c,lc,d)                      STDCALL(UN,LN)(a,b,c,d,la,lc)
#define CALLSPSP(UN,LN,a,b,c,d)                           F_FUNC(UN,LN)(a,b,c,d,strlen(a),strlen(c))
#define CALLSSSP(UN,LN,a,b,c,d)                           F_FUNC(UN,LN)(a,b,c,d,strlen(a),strlen(b),strlen(c))
#define CALLSSSSP(UN,LN,a,b,c,d,e)                           F_FUNC(UN,LN)(a,b,c,d,e,strlen(a),strlen(b),strlen(c),strlen(d))
#define CALLSSSSPPS(UN,LN,a,b,c,d,e,f,g)                  F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(a),strlen(b),strlen(c),strlen(d),strlen(g))
#define DEFSPSPP(UN,LN,a,la,b,c,lc,d,e)                      STDCALL(UN,LN)(a,b,c,d,e,la,lc)
#define DEFSPPSSP(UN,LN,a,la,b,c,d,ld,e,le,f)                      STDCALL(UN,LN)(a,b,c,d,e,f,la,ld,le)
#define DEFSSS(UN,LN,a,la,b,lb,c,lc)    STDCALL(UN,LN)(a,b,c,la,lb,lc)
#define CALLSSS(UN,LN,a,b,c)    F_FUNC(UN,LN)(a,b,c,strlen(a),strlen(b),strlen(c))
#define DEFPS(UN,LN,a,b,lb)                      STDCALL(UN,LN)(a,b,lb)
#define DEFPSP(UN,LN,a,b,lb,c)                      STDCALL(UN,LN)(a,b,c,lb)
#define DEFPSSP(UN,LN,a,b,lb,c,lc,d)    STDCALL(UN,LN)(a,b,c,d,lb,lc)
#define CALLPPS(UN,LN,a,b,c)    F_FUNC(UN,LN)(a,b,c,strlen(c))
#define DEFPPS(UN,LN,a,b,c,lc)    STDCALL(UN,LN)(a,b,c,lc)
#define DEFSSSP(UN,LN,a,la,b,lb,c,lc,d)    STDCALL(UN,LN)(a,b,c,d,la,lb,lc)
#define DEFSSSSP(UN,LN,a,la,b,lb,c,lc,d,ld,e)    STDCALL(UN,LN)(a,b,c,d,e,la,lb,lc,ld)
#define DEFSSSSPPS(UN,LN,a,la,b,lb,c,lc,d,ld,e,f,g,lg)    STDCALL(UN,LN)(a,b,c,d,e,f,g,la,lb,lc,ld,lg)
#define DEFSSPSPP(UN,LN,a,la,b,lb,c,d,ld,e,f)    STDCALL(UN,LN)(a,b,c,d,e,f,la,lb,ld)
#define FCALLSSPSPP(UN,LN,a,la,b,lb,c,d,ld,e,f)    F_FUNC(UN,LN)(a,b,c,d,e,f,la,lb,ld)
#define DEFSPSPPPS(UN,LN,a,la,b,c,lc,d,e,f,g,lg)    STDCALL(UN,LN)(a,b,c,d,e,f,g,la,lc,lg)
#define FCALLSPSPPPS(UN,LN,a,la,b,c,lc,d,e,f,g,lg)    F_FUNC(UN,LN)(a,b,c,d,e,f,g,la,lc,lg)
#define DEFSPSSPPP(UN,LN,a,la,b,c,lc,d,ld,e,f,g)    STDCALL(UN,LN)(a,b,c,d,e,f,g,la,lc,ld)
#define CALLSPSSPPP(UN,LN,a,b,c,d,e,f,g)    F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(a),strlen(c),strlen(d))
#define FCALLSSS(UN,LN,a,la,b,lb,c,lc)    F_FUNC(UN,LN)(a,b,c,la,lb,lc)
#define FCALLPSSP(UN,LN,a,b,lb,c,lc,d)    F_FUNC(UN,LN)(a,b,c,d,lb,lc)

#elif defined PPRO_NT
#define STDCALL(UN,LN)                           __stdcall F_FUNC(UN,LN)

#define CALLSS(UN,LN,a,b)                        F_FUNC(UN,LN)(a,strlen(a),b,strlen(b))
#define CALLSSP(UN,LN,a,b,c)                        F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c)
#define CALLSSPP(UN,LN,a,b,c,d)                        F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,d)
#define CALLSSPPP(UN,LN,a,b,c,d,e)                        F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,d,e)
#define CALLS(UN,LN,a)                     F_FUNC(UN,LN)(a,strlen(a))
#define CALLSP(UN,LN,a,b)                     F_FUNC(UN,LN)(a,strlen(a),b)
#define CALLSPP(UN,LN,a,b,c)                     F_FUNC(UN,LN)(a,strlen(a),b,c)
#define CALLSPPP(UN,LN,a,b,c,d)                     F_FUNC(UN,LN)(a,strlen(a),b,c,d)
#define CALLSPPPP(UN,LN,a,b,c,d,e)                     F_FUNC(UN,LN)(a,strlen(a),b,c,d,e)

#define CALLSPPPPS(UN,LN,a,b,c,d,e,f)                     F_FUNC(UN,LN)(a,strlen(a),b,c,d,e,f,strlen(f))
#define CALLPPPSP(UN,LN,a,b,c,d,e)                        F_FUNC(UN,LN)(a,b,c,d,strlen(d),e)
#define DEFPPPSP(UN,LN,a,b,c,d,ld,e)                        STDCALL(UN,LN)(a,b,c,d,ld,e)

#define DEFS(UN,LN,a,la)                      STDCALL(UN,LN)(a,la)
#define DEFSP(UN,LN,a,la,b)                      STDCALL(UN,LN)(a,la,b)
#define DEFSPP(UN,LN,a,la,b,c)                      STDCALL(UN,LN)(a,la,b,c)
#define DEFSPPP(UN,LN,a,la,b,c,d)                   STDCALL(UN,LN)(a,la,b,c,d)
#define DEFSPPPP(UN,LN,a,la,b,c,d,e)                   STDCALL(UN,LN)(a,la,b,c,d,e)

#define DEFSPPPPS(UN,LN,a,la,b,c,d,e,f,lf)           STDCALL(UN,LN)(a,la,b,c,d,e,f,lf)
#define DEFSSPPPPP(UN,LN,a,la,b,lb,c,d,e,f,g)    STDCALL(UN,LN)(a,la,b,lb,c,d,e,f,g)
#define DEFSSPPPP(UN,LN,a,la,b,lb,c,d,e,f)    STDCALL(UN,LN)(a,la,b,lb,c,d,e,f)
#define DEFSSPPP(UN,LN,a,la,b,lb,c,d,e)    STDCALL(UN,LN)(a,la,b,lb,c,d,e)
#define DEFSSPP(UN,LN,a,la,b,lb,c,d)    STDCALL(UN,LN)(a,la,b,lb,c,d)
#define DEFSSP(UN,LN,a,la,b,lb,c)    STDCALL(UN,LN)(a,la,b,lb,c)
#define DEFSS(UN,LN,a,la,b,lb)    STDCALL(UN,LN)(a,la,b,lb)

#define DEFSSSPPPPS(UN,LN,a,la,b,lb,c,lc,d,e,f,g,h,lh)    STDCALL(UN,LN)(a,la,b,lb,c,lc,d,e,f,g,h,lh)
#define CALLSSSPPPPS(UN,LN,a,b,c,d,e,f,g,h)    F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,strlen(c),d,e,f,g,h,strlen(h))

#define DEFSPPSSSP(UN,LN,a,la,b,c,d,ld,e,le,f,lf,g)    STDCALL(UN,LN)(a,la,b,c,d,ld,e,le,f,lf,g)
#define CALLSPPSSSP(UN,LN,a,b,c,d,e,f,g)    F_FUNC(UN,LN)(a,strlen(a),b,c,d,strlen(d),e,strlen(e),f,strlen(f),g)

#define DEFSSPPPSP(UN,LN,a,la,b,lb,c,d,e,f,lf,g)    STDCALL(UN,LN)(a,la,b,lb,c,d,e,f,lf,g)
#define CALLSSPPPSP(UN,LN,a,b,c,d,e,f,g)                     F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,d,e,f,strlen(f),g)
#define CALLSSPPPPP(UN,LN,a,b,c,d,e,f,g)                     F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,d,e,f,g)
#define DEFSPSP(UN,LN,a,la,b,c,lc,d)                      STDCALL(UN,LN)(a,la,b,c,lc,d)
#define CALLSPSP(UN,LN,a,b,c,d)                     F_FUNC(UN,LN)(a,strlen(a),b,c,strlen(c),d)
#define CALLSSSP(UN,LN,a,b,c,d)                     F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,strlen(c),d)
#define CALLSSSSP(UN,LN,a,b,c,d,e)                     F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,strlen(c),d,strlen(d),e)
#define CALLSSSSPPS(UN,LN,a,b,c,d,e,f,g)                     F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,strlen(c),d,strlen(d),e,f,g,strlen(g))
#define DEFSPSPP(UN,LN,a,la,b,c,lc,d,e)                      STDCALL(UN,LN)(a,la,b,c,lc,d,e)
#define DEFSPPSSP(UN,LN,a,la,b,c,d,ld,e,le,f)                      STDCALL(UN,LN)(a,la,b,c,d,ld,e,le,f)
#define DEFSSS(UN,LN,a,la,b,lb,c,lc)    STDCALL(UN,LN)(a,la,b,lb,c,lc)
#define CALLSSS(UN,LN,a,b,c)    F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,strlen(c))
#define DEFPS(UN,LN,a,b,lb)                      STDCALL(UN,LN)(a,b,lb)
#define DEFPSP(UN,LN,a,b,c,lb)                      STDCALL(UN,LN)(a,b,c,lb)
#define DEFPSSP(UN,LN,a,b,lb,c,lc,d)    STDCALL(UN,LN)(a,b,lb,c,lc,d)
#define CALLPPS(UN,LN,a,b,c)    F_FUNC(UN,LN)(a,b,c,strlen(c))
#define DEFPPS(UN,LN,a,b,c,lc)    STDCALL(UN,LN)(a,b,c,lc)
#define DEFSSSP(UN,LN,a,la,b,lb,c,lc,d)    STDCALL(UN,LN)(a,la,b,lb,c,lc,d)
#define DEFSSSSP(UN,LN,a,la,b,lb,c,lc,d,ld,e)    STDCALL(UN,LN)(a,la,b,lb,c,lc,d,ld,e)
#define DEFSSSSPPS(UN,LN,a,la,b,lb,c,lc,d,ld,e,f,g,lg)    STDCALL(UN,LN)(a,la,b,lb,c,lc,d,ld,e,f,g,lg)
#define DEFSSPSPP(UN,LN,a,la,b,lb,c,d,ld,e,f)    STDCALL(UN,LN)(a,la,b,lb,c,d,ld,e,f)
#define FCALLSSPSPP(UN,LN,a,la,b,lb,c,d,ld,e,f)    F_FUNC(UN,LN)(a,la,b,lb,c,d,ld,e,f)
#define DEFSPSPPPS(UN,LN,a,la,b,c,lc,d,e,f,g,lg)    STDCALL(UN,LN)(a,la,b,c,lc,d,e,f,g,lg)
#define FCALLSPSPPPS(UN,LN,a,la,b,c,lc,d,e,f,g,lg)    F_FUNC(UN,LN)(a,la,b,c,lc,d,e,f,g,lg)
#define DEFSPSSPPP(UN,LN,a,la,b,c,lc,d,ld,e,f,g)    STDCALL(UN,LN)(a,la,b,c,lc,d,ld,e,f,g)
#define CALLSPSSPPP(UN,LN,a,b,c,d,e,f,g)    F_FUNC(UN,LN)(a,strlen(a),b,c,strlen(c),d,strlen(d),e,f,g)
#define FCALLSSS(UN,LN,a,la,b,lb,c,lc)    F_FUNC(UN,LN)(a,la,b,lb,c,lc)
#define FCALLPSSP(UN,LN,a,b,lb,c,lc,d)    F_FUNC(UN,LN)(a,b,lb,c,lc,d)

#endif

#define CALLP(UN,LN,a)                        F_FUNC(UN,LN)(a)
#define CALLPP(UN,LN,a,b)                        F_FUNC(UN,LN)(a,b)
#define CALLPPPP(UN,LN,a,b,c,d)                        F_FUNC(UN,LN)(a,b,c,d)
#define DEFP(UN,LN,a)                            STDCALL(UN,LN)(a)
#define DEFPP(UN,LN,a,b)                            STDCALL(UN,LN)(a,b)
#define DEFPPP(UN,LN,a,b,c)                            STDCALL(UN,LN)(a,b,c)
#define DEFPPPP(UN,LN,a,b,c,d)                            STDCALL(UN,LN)(a,b,c,d)

/* FIN DE pour définir les appels et signatures de fonctions appelables en Fortran */

/* Fonction retournant PI en R8 */
#define R8PI() F_FUNC(R8PI,r8pi)()
extern double STDCALL(R8PI,r8pi)();

/* pour representer les logical sur toutes les stations {*/

/* FORTRAN_TRUE = -1 sur HP-UX avec l'option de compilation +DAportable +apollo */
enum ENUM_LOGICAL { FORTRAN_TRUE=-1, FORTRAN_FALSE=0} ;
#define FORTRAN_LOGICAL enum ENUM_LOGICAL

/*                                                      }*/

/* pour representer les entiers sur toutes les stations. {*/

#if defined(SOLARIS) || defined(P_LINUX) || defined(PPRO_NT) || defined(HPUX)
#define INTEGER long
#else
#if defined IRIX || TRU64 || SOLARIS64
#define INTEGER long
#else
#error Environnement INDEFINI pour INTEGER
#endif
#endif

/*                                                       }*/



/* pour preciser quel fichier affiche les  messages et les valeurs */

#define INTERRUPTION(code) { ICI ; fprintf(stderr,"INTERRUPTION - code retour %d\n",code) ;abort() ; }

#ifdef _DEBUT
#error _DEBUT est deja definie
#endif
#ifdef _FIN
#error _FIN est deja definie
#endif

#ifdef _DEBOG_        /*{*/

#define ICI fflush(stdout);fprintf( stderr, "%s  %d : " , __FILE__ , __LINE__  ) ; fflush(stderr) ;
#define MESSAGE(chaine) ICI ; fprintf( stderr , "%s\n" , chaine ) ; fflush(stderr) ;

#ifndef ASSERT        /*{*/
#define ASSERT(condition) if( !(condition) ){ ICI ; fprintf(stderr,"condition %s VIOLEE\n",#condition);INTERRUPTION(17);}
#endif                /*}# ifndef ASSERT*/

#define TAB fflush(stdout);fprintf( stderr, "\t" );ICI
#define RES fflush(stdout);fprintf( stderr, "\t RESULTAT >> " );ICI
#define ISCRUTE(entier) TAB ; fprintf(stderr,"%s = %ld\n",#entier,(INTEGER)entier) ; fflush(stderr);
#define TISCRUTE(n,entier) TAB ; fprintf(stderr,"%s = %ld",#n,(INTEGER)n) ; \
                           if(n>0) fprintf(stderr,", %s[0] = %ld",#entier,(INTEGER)entier[0]) ; \
                           fprintf(stderr,"\n");fflush(stderr);
#define REFSCRUTE(objet) ISCRUTE(objet->ob_refcnt) ;
#define DSCRUTE(reel) TAB ; fprintf(stderr,"%s = %f\n",#reel,reel) ; fflush(stderr);
#define TDSCRUTE(n,reel) TAB ; fprintf(stderr,"%s = %ld",#n,(INTEGER)n) ; \
                           if(n>0) fprintf(stderr,", %s[0] = %f",#reel,reel[0]) ; \
                           fprintf(stderr,"\n");fflush(stderr);
#define OBSCRUTE(obj) TAB ; fprintf(stderr,"%s = ",#obj) ; PyObject_Print(obj, stderr, 0); fprintf(stderr,"\n");fflush(stderr);
#define SSCRUTE(chaine) TAB ; fprintf(stderr,"%s = ",#chaine) ; if (chaine){fprintf(stderr,"\"%s\"\n",chaine);}else{fprintf(stderr,"(char*)0\n");} ; fflush(stderr);
#define FSSCRUTE(chaine,longueur) TAB ; fprintf(stderr,"%s = ",#chaine) ; fflush(stderr) ; AfficheChaineFortran(chaine,longueur) ;
#define _DEBUT(nom) fprintf( stderr , "\n\n\n") ; ICI ; fprintf( stderr , "{ DEBUT %s\n" , #nom ) ; fflush(stderr) ;
#define _FIN(nom) ICI ; fprintf( stderr , "} FIN %s\n\n\n" , #nom ) ; fflush(stderr) ;

#else                /*}# ifdef _DEBOG_{*/

#define ICI
#define TAB
#define RES
#define MESSAGE(chaine)
#define ISCRUTE(entier)
#define TISCRUTE(n,entier)
#define DSCRUTE(reel)
#define TDSCRUTE(n,reel)
#define OBSCRUTE(obj)
#define SSCRUTE(chaine)
#define REFSCRUTE(objet)
#define FSSCRUTE(chaine,longueur)
#define ASSERT(condition)
#define _DEBUT(nom)
#define _FIN(nom)

#endif                /*}# ifdef _DEBOG_*/
#endif                /*}# ifndef _UTILITES_*/


/* fin du fichier UTILITE.h */






#define EstValide(c) (isprint((int)c) && (isalnum((int)c) || (c=='_') || (c==' ')))




/* --- declarations des interfaces des fonctions de ce fichier --- */
/*{*/

static PyObject *aster_argv( _UNUSED  PyObject *self, _IN PyObject *args ) ;
const char *aster_ident() ;

int EstPret( _IN char *chaine , _IN int longueur ) ;
long FindLength( _IN char *chaineFortran , _IN INTEGER longueur ) ;
void AfficheChaineFortran( _IN char *chaine , _IN int longueur ) ;
void TraiteMessageErreur( _IN char* ) ;
void PRE_myabort( _IN const char *nomFichier , _IN const int numeroLigne , _IN const char *message ) ;
#define MYABORT(message) PRE_myabort( __FILE__ , __LINE__ , message )

PyObject * MakeTupleString(long nbval,char *kval,int lkval,INTEGER *lval) ;
PyObject * MakeTupleString_pour_putvid(long nbval,char *kval,int lkval) ;


char * fstring2c( _IN char *s, _IN int l) ;
char * fstr1( _IN char *s, _IN int l) ;
char * fstr2( _IN char *s, _IN int l) ;
char * fstr3( _IN char *s, _IN int l) ;
void convert( _IN int nval, _IN PyObject *tup, _OUT INTEGER *val) ;
void convertxt( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN int taille) ;
void converltx( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN int taille) ;

void AjoutChaineA( _INOUT char **base , _IN char *supplement ) ;

void TraitementFinAster( _IN int val ) ;



/*}*/
/* --- FIN declarations des interfaces des fonctions de ce fichier --- */



#define _UTILISATION_SETJMP_
/*
 *   Emulation d'exceptions en C : on utilise le couple de fonctions systemes setjmp/longjmp
 *   pour reproduire le comportement des exceptions en C.
 *   Pour initier une exception, le Fortran doit appeler la fonction XFINI
 *   avec en argument le code de l'exception.
 *   La fonction XFINI fait appel à longjmp pour effectuer le debranchement necessaire.
 *
 *   La variable exception_flag indique comment toute anomalie intervenant pendant
 *   le try doit etre traitée :  par une exception (si try(1)) ou par un abort (si try(0))
 */

#define CodeFinAster       19
#define CodeAbortAster     20
#define CodeErrorAster     21
#define CodeNonConvergenceAster           22
#define CodeEchecComportementAster        23
#define CodeBandeFrequenceVideAster       24
#define CodeMatriceSinguliereAster        25
#define CodeTraitementContactAster        26
#define CodeMatriceContactSinguliereAster 27
#define CodeArretCPUAster                 28

int exception_status=-1;
#define REASONMAX 800
static char exception_reason[REASONMAX+1];

#define NIVMAX 10
static int niveau=0;

#ifdef _UTILISATION_SETJMP_
#include <setjmp.h>

static jmp_buf env[NIVMAX+1] ;           /* utilise par longjmp, le type jmp_buf est defini dans setjmp.h */
static int exception_flag[NIVMAX+1];

#define try(val) exception_flag[niveau]=val;if((exception_status = setjmp(env[niveau])) == 0)
#define catch(val) else if (exception_status == val)
#define throw(val) longjmp(env[niveau],val)
#define finally else

void TraiteErreur( _IN int code )
{
   _DEBUT("TraiteErreur");
        if(exception_flag[niveau]==1){
          exception_flag[niveau]=0;
          throw(code);
        }
        else{
          abort();
        }
   _FIN("TraiteErreur");
}

#else
#define try(val) if(1)
#define catch(val) else if (0)
#define throw(val)
#define finally else

void TraiteErreur( _IN int code )
{
        switch( code ){

        case CodeFinAster :
                exit(0);
                break ;
        case CodeAbortAster :
                abort();
                break ;
        case CodeErrorAster :
                abort();
                break ;

        /* exceptions particularisées */
        case CodeNonConvergenceAster :
                abort();
                break ;
        case CodeEchecComportementAster :
                abort();
                break ;
        case CodeBandeFrequenceVideAster :
                abort();
                break ;
        case CodeMatriceSinguliereAster :
                abort();
                break ;
        case CodeTraitementContactAster :
                abort();
                break ;
        case CodeMatriceContactSinguliereAster :
                abort();
                break ;
        case CodeArretCPUAster :
                abort();
                break ;
        default :
                MESSAGE("code erreur INCONNU !!!!") ;
                ISCRUTE(*code) ;
                INTERRUPTION(1) ;
                break ;
        }

}

#endif                /* #ifdef _UTILISATION_SETJMP_ */

void STDCALL(XFINI,xfini)(_IN INTEGER *code)
{
   _DEBUT("XFINI");
   switch( *code ){
        case CodeFinAster :
                strcpy(exception_reason,"exit ASTER");
                break ;
        case CodeAbortAster :
                strcpy(exception_reason,"abort ASTER");
                break ;
        default:
                *code=CodeAbortAster;
                strcpy(exception_reason,"abort ASTER");
                break ;
        }
   TraiteErreur(*code);
   _FIN("XFINI");
}

/* Fin emulation exceptions en C */



/* --- liste des variables globales au fonctions  de ce fichier --- */ /*{*/



/* commande (la commande courante) est definie par les fonctions aster_debut et aster_oper */
static PyObject *commande       = (PyObject*)0 ;
static PyObject *pile_commandes = (PyObject*)0 ;

/* NomCas est initialise dans aster_debut() */
/* NomCas est initialise a blanc pour permettre la recuperation de la
   trace des commandes lors de l'appel a debut ou poursuite. On ne connait
   pas encore NomCas qui sera initialise lors de l'appel a RecupNomCas */
static char *NomCas          = "        ";

/*
    Les exceptions levees dans le Fortran par les developpeurs
    doivent etre des objets de la classe AsterError (numero equivalent 21) ou d'une classe
    derivee.
 */
/* exceptions de base */
static PyObject *AsterError = (PyObject*)0 ; /* Ce type d'exception est levee sur appel de XFINI avec le parametre 21 */
static PyObject *FatalError = (PyObject*)0 ; /* Ce type d'exception (derive de AsterError) est levee sur appel de XFINI avec le parametre 20 */

/* exceptions particularisées */
static PyObject *NonConvergenceError = (PyObject*)0 ;           /* Exception non convergence */
static PyObject *EchecComportementError = (PyObject*)0 ;        /* Exception échec intégration du comportement */
static PyObject *BandeFrequenceVideError = (PyObject*)0 ;       /* Exception bande de fréquence vide */
static PyObject *MatriceSinguliereError = (PyObject*)0 ;        /* Exception matrice singuliere */
static PyObject *TraitementContactError = (PyObject*)0 ;        /* Exception échec de traitement du contact */
static PyObject *MatriceContactSinguliereError = (PyObject*)0 ; /* Exception matrice de contact non inversible */
static PyObject *ArretCPUError = (PyObject*)0 ;                 /* Exception manque de temps CPU */

void initExceptions(PyObject *dict)
{
        AsterError = PyErr_NewException("aster.error", NULL, NULL);
        if(AsterError != NULL) PyDict_SetItemString(dict, "error", AsterError);
        /* type d'exception Fatale derivee de AsterError */
        FatalError = PyErr_NewException("aster.FatalError", AsterError, NULL);
        if(FatalError != NULL) PyDict_SetItemString(dict, "FatalError", FatalError);
        
        /* Exceptions particularisées */
        NonConvergenceError = PyErr_NewException("aster.NonConvergenceError", AsterError, NULL);
        if(NonConvergenceError != NULL) PyDict_SetItemString(dict, "NonConvergenceError", NonConvergenceError);

        EchecComportementError = PyErr_NewException("aster.EchecComportementError", AsterError, NULL);
        if(EchecComportementError != NULL) PyDict_SetItemString(dict, "EchecComportementError", EchecComportementError);
        
        BandeFrequenceVideError = PyErr_NewException("aster.BandeFrequenceVideError", AsterError, NULL);
        if(BandeFrequenceVideError != NULL) PyDict_SetItemString(dict, "BandeFrequenceVideError", BandeFrequenceVideError);
        
        MatriceSinguliereError = PyErr_NewException("aster.MatriceSinguliereError", AsterError, NULL);
        if(MatriceSinguliereError != NULL) PyDict_SetItemString(dict, "MatriceSinguliereError", MatriceSinguliereError);
        
        TraitementContactError = PyErr_NewException("aster.TraitementContactError", AsterError, NULL);
        if(TraitementContactError != NULL) PyDict_SetItemString(dict, "TraitementContactError", TraitementContactError);
        
        MatriceContactSinguliereError = PyErr_NewException("aster.MatriceContactSinguliereError", AsterError, NULL);
        if(MatriceContactSinguliereError != NULL) PyDict_SetItemString(dict, "MatriceContactSinguliereError", MatriceContactSinguliereError);
        
        ArretCPUError = PyErr_NewException("aster.ArretCPUError", AsterError, NULL);
        if(ArretCPUError != NULL) PyDict_SetItemString(dict, "ArretCPUError", ArretCPUError);
}

/*
  Subroutine appelable depuis le Fortran pour demander la levee d'une exception de type exc_type
  Une chaine de charactere (reason) ajoute un commentaire au type d'exception
*/
void DEFPS(UEXCEP,uexcep,_IN INTEGER *exc_type,  _IN char *reason , _IN int lreason )
{
   int l;
   _DEBUT("UEXCEP");
   l=min(FindLength(reason,lreason),REASONMAX);
   strncpy(exception_reason,reason,l);
   exception_reason[l]='\0';
   TraiteErreur(*exc_type);
   _FIN("UEXCEP");
}


/* Pour initialiser statiquement une chaine avec des blancs d'une longueur suffisante */
static char * blan="                                                                                                            \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                \
                                                                                                                                ";


/*
#define STRING_FCPY(dest,taille,src,longueur) \
   strncpy(dest,blan,taille);strncpy(dest,src,longueur);
*/

#define BLANK(dest,taille) memset(dest,' ',taille)
#define STRING_FCPY(dest,taille,src,longueur) \
   memcpy(dest,src,min(taille,longueur));taille>longueur?memset(dest+longueur,' ',taille-longueur):0;
#define CSTRING_FCPY(dest,taille,src) STRING_FCPY(dest,taille,src,strlen(src))

#define PRINTERR if(PyErr_Occurred()){ \
            fprintf(stderr,"Warning: une exception n'a pas ete traitée\n"); \
            PyErr_Print(); \
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait du etre traitée avant\n"); \
            PyErr_Clear(); \
        }


static char nom_fac[256];        /* utilise par fstr1 */
static char nom_cle[256];        /* utilise par fstr2 */
static char nom_cmd[256];        /* utilise par fstr3 */


/*}*/ /* --- FIN liste des variables globales au fonctions  de ce fichier --- */


/*
 *   Ce module crée de nombreux objets Python. Il doit respecter les règles
 *   générales de création des objets et en particulier les règles sur le
 *   compteur de références associé à chaque objet.
 *   Tous les objets sont partagés. Seules des références à des objets peuvent
 *   etre acquises.
 *   Si une fonction a acquis une référence sur un objet elle doit la traiter
 *   proprement, soit en la transférant (habituellement à l'appelant), soit en
 *   la relachant (par appel à Py_DECREF ou Py_XDECREF).
 *   Quand une fonction transfere la propriété d'une référence, l'appelant recoit
 *   une nouvelle référence. Quand la propriété n'est pas transférée, l'appelant
 *   emprunte la référence.
 *   Dans l'autre sens, quand un appelant passe une référence à une fonction, il y a
 *   deux possibilités : la fonction vole une référence à l'objet ou elle ne le fait
 *   pas. Peu de fonctions (de l'API Python) volent des références : les deux exceptions
 *   les plus notables sont PyList_SetItem() et PyTuple_SetItem() qui volent une
 *   référence à l'item qui est inséré dans la liste ou dans le tuple.
 *   Ces fonctions qui volent des références existent, en général, pour alléger
 *   la programmation.
 */





void TraiteMessageErreur( _IN char * message )
{
        printf("%s\n",message);
        if(PyErr_Occurred())PyErr_Print();
        abort();
        if(exception_flag[niveau]==1){
          int l;
          exception_flag[niveau]=0;
          l=min(REASONMAX,strlen(message));
          strncpy(exception_reason,message,l);
          exception_reason[l+1]='\0';
          throw(CodeAbortAster);
        }
        else{
          abort();
        }
}





void PRE_myabort( _IN const char *nomFichier , _IN const int numeroLigne , _IN const char *message )
{

        /*
        Procedure : PRE_myabort
        Intention
                Cette procedure prepare la chaine de caracteres affichee par TraiteMessageErreur()
                en ajoutant devant cette chaine, le nom du fichier source et le numero
                de la ligne a partir desquels PRE_myabort a ete appelee.
                Puis elle appelle elle-meme TraiteMessageErreur().
                Voir aussi la macro MYABORT qui permet de generer automatiquement le nom
                du fichier et le numero de la ligne.
        */

        char *chaine = (char*)0 ;
        int longueur = 0 ;

                                                        ASSERT(numeroLigne>0);
                                                        ASSERT(((int)log10((float)numeroLigne))<=5);
                                                        ASSERT(nomFichier!=(char*)0) ;
        longueur += strlen( nomFichier ) ;
        longueur += 1 ; /* pour le blanc de separation */
        longueur += 5 ; /* pour le numero de la ligne */
        longueur += 3 ; /* pour les deux points entre deux blancs */
                                                        ASSERT(message!=(const char*)0);
        longueur += ( message != (const char*)0 ) ? strlen( message ) : 0 ;
        longueur += 1 ; /* pour le caractere de fin de chaine */

        chaine = (char*)(malloc(longueur*sizeof(char))) ;
                                                        ASSERT(chaine!=(char*)0);

        sprintf( chaine , "%s %u : %s" , nomFichier , numeroLigne , message ) ;
        TraiteMessageErreur( chaine ) ;

        free( chaine )   ;
        chaine=(char*)0 ;
        longueur = 0     ;
}

void DEFSSPPPPP(GETLTX,getltx,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_OUT INTEGER *isval, _OUT INTEGER *nbval )
{
        /*
        Procedure : getltx_ (appelee par le fortran sous le nom GETLTX)
        Intention

        */
        PyObject *res = (PyObject*)0 ;
        PyObject *tup = (PyObject*)0 ;
        char *mfc     = (char*)0 ;
        char *mcs     = (char*)0 ;
        int ok        = 0 ;
        int nval      = 0 ;

        _DEBUT("getltx_") ;
                                                        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ; ISCRUTE(*iocc) ;

        mfc=fstr1(motfac,lfac);
                                                        ASSERT(mfc!=(char*)0);

                                                        ASSERT(EstPret(motcle,lcle)!=0);
        mcs=fstr2(motcle,lcle);
                                                        ASSERT(mcs!=(char*)0);

                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getltx","ssiii",mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");


        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        convert(nval,tup,isval);
                                                        ISCRUTE(nbval);
                                                        TISCRUTE(nval,isval);
        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getltx_) ;
        return ;
}




char * fstring2c( _IN char *s, _IN int l)
{
        char *fs;
                                                        ASSERT(EstPret(s,l)!=0);
        fs=(char *)malloc(l+1);
        if(fs == NULL){
                MYABORT("impossible d allouer de la memoire");
        }
        strncpy(fs, s, l );
        fs[l]='\0';
        return fs;
}







char * fstr1( _IN char *s, _IN int l)
{
        /*
        copie l caracteres d'une chaine de caracteres fortran s dans la chaine
        statique globale nom_fac, et retourne un pointeur sur nom_fac
        */
        strncpy(nom_fac, s, l );
        nom_fac[l]='\0';
        return nom_fac;
}
char * fstr2( _IN char *s, _IN int l)
{
        /*
        copie l caracteres d'une chaine de caracteres fortran s dans la chaine
        statique globale nom_cle, et retourne un pointeur sur nom_cle
        */
        strncpy(nom_cle, s, l );
        nom_cle[l]='\0';
        return nom_cle;
}
char * fstr3( _IN char *s, _IN int l)
{
        /*
        copie l caracteres d'une chaine de caracteres fortran s dans la chaine
        statique globale nom_cmd, et retourne un pointeur sur nom_cmd
        */
        strncpy(nom_cmd, s, l );
        nom_cmd[l]='\0';
        return nom_cmd;
}



void DEFSP(GETFAC,getfac,_IN char *nomfac, _IN int lfac, _OUT INTEGER *occu)
{
        /*
          Procedure GETFAC pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : nomfac (string)
          Retourne :
            le nombre d occurence de ce mot cle dans les args : occu (entier)
            dans l'etape (ou la commande) courante
        */
        PyObject *res  = (PyObject*)0 ;

        _DEBUT(getfac_) ;
                                                        FSSCRUTE(nomfac,lfac) ;
                                                        ASSERT(EstPret(nomfac,lfac)!=0);
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getfac","s",fstr1(nomfac,lfac));

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        *occu=PyInt_AsLong(res);
                                                        ISCRUTE(*occu);

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getfac_) ;
        return ;
}




void convc8( _IN int nval, _IN PyObject *tup, _OUT double *val)
{

        /*
                  tup est un tuple de tuples internes, chaque tuple
                interne contenant le type et les deux parties du complexe.
        */

        int    i = 0 ;
        int    k = 0 ;
        int conv_un_c8( _IN PyObject *tup, _OUT double *val) ;

                                                                    ASSERT(PyTuple_Check(tup)) ;
                                                                    OBSCRUTE(tup) ;
        if(nval != 0){
                PyObject *v = (PyObject*)0 ;
                                                                    ASSERT(nval>0) ;
                for(i=0;i<nval;i++){
                                                                    ISCRUTE(i) ;
                        v=PyTuple_GetItem(tup,i);
                        k += conv_un_c8( v , val+k ) ;
                }
        }
        return ;
}
int conv_un_c8( _IN PyObject *tup, _OUT double *val)
{

        /* Enrichissement des complexes stockes dans val a partir du tuple tup */

        char *repres = (char*)0 ; /* representation "RI" (reelle/imaginaire) ou "MP" (module phase) */

        double x = 0.0 ;
        double y = 0.0 ;
        double *rho = &x ;
        double *theta = &y ;
                                                                    OBSCRUTE(tup) ;
        if(PyComplex_Check(tup)){
           /* On est dans le cas d'un objet Python complexe */
           /* representation : partie reelle/partie imaginaire */
           *val    =PyComplex_RealAsDouble(tup)  ;
           *(val+1)=PyComplex_ImagAsDouble(tup)  ;
                                                               DSCRUTE(*val);DSCRUTE(*(val+1));
        }
        else if(PyTuple_Check(tup)){
           /* On est dans le cas d'un complexe représenté par un triplet : "RI" ou "MP",x,y */
           if(!PyArg_ParseTuple(tup,"sdd",&repres,&x,&y))
                     MYABORT("erreur dans la partie Python");
                                                                                     SSCRUTE(repres) ;
                                                                                     ASSERT((strcmp(repres,"RI")==0)||(strcmp(repres,"MP")==0)) ;
                                                                                     DSCRUTE(x) ;
                                                                                     DSCRUTE(y) ;
                                                                                     ISCRUTE(strcmp(repres,"RI"))
           if (strcmp(repres,"RI")==0){
                /* representation : partie reelle/partie imaginaire */
                *val    =x ;
                *(val+1)=y ;
           }
           else{
                /* representation RHO,THETA (les angles sont fournis en degres) */
                *val    =*rho * cos( *theta /180. * R8PI()) ;
                *(val+1)=*rho * sin( *theta /180. * R8PI()) ;
           }
        }
        else {
           MYABORT("erreur dans la partie Python");
        }
        return 2 ;
}




void convr8( _IN int nval, _IN PyObject *tup, _OUT double *val)
{

        /* Convertit un Tuple en tableau de double */

        int i;
        PyObject *v = (PyObject*)0 ;
        if(nval == 0)return;
        if (!PyTuple_Check(tup)){
                printf("tup : ");
                PyObject_Print(tup, stdout, 0);
                printf("\n ");
                MYABORT("erreur sur le type : devrait etre un tuple");
        }
        for(i=0;i<nval;i++){
                v=PyTuple_GetItem(tup,i);
                val[i]=PyFloat_AsDouble(v);
        }
        return ;
}




void convert( _IN int nval, _IN PyObject *tup, _OUT INTEGER *val)
{

        /* Convertit un Tuple en tableau d entier */

        int i;
        PyObject *v = (PyObject*)0 ;
        if(nval == 0)return;
        if (!PyTuple_Check(tup)){
                printf("tup : ");
                PyObject_Print(tup, stdout, 0);
                printf("\n ");
                MYABORT("erreur sur le type : devrait etre un tuple");
        }
        for(i=0;i<nval;i++){
                v=PyTuple_GetItem(tup,i);
                val[i]=PyInt_AsLong(v);
        }
        return ;
}





void convertxt( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN int taille)
{
        /*
        Convertit un Tuple en tableau de chaines
        Pour retour au Fortran : le tableau existe deja (val)
           nval   : indique le nombre d'elements du tuple a convertir
           tup    : est le tuple Python a convertir
           val    : est le tableau de chaines Fortran a remplir
           taille : indique la taille des chaines
        */
                                                                   ASSERT(PyTuple_Check(tup)) ;
                                                                   ISCRUTE(nval) ;
                                                                   ISCRUTE(taille) ;
                                                                   OBSCRUTE(tup) ;
        if(nval != 0){
                PyObject *v  = (PyObject*)0 ;
                int i;
                char *s      = (char*)0 ;
                char *val_i      = (char*)0 ;
                int longueur = 0 ;
                                                                   ASSERT(nval>0) ;
                                                                   ASSERT(taille>0) ;
                if (!PyTuple_Check(tup)){
                        printf("tup : ");
                        PyObject_Print(tup, stdout, 0);
                        printf("\n ");
                        MYABORT("erreur sur le type : devrait etre un tuple");
                }
                for(i=0;i<nval;i++){
                        v=PyTuple_GetItem(tup,i);
                        /*                               v=PySequence_GetItem(tup,i); */
                        s=PyString_AsString(v);
                        if(s == NULL){
                                printf("s : ");
                                PyObject_Print(v, stdout, 0);
                                printf("\n ");
                                MYABORT("erreur sur le type : devrait etre une string");
                        }

                        /* le fortran attend des chaines de caracteres completees par des blancs */
                        longueur=strlen(s);
                        val_i=&val[i*taille];
                        STRING_FCPY(val_i,taille,s,longueur);
                }
        }
}




void converltx( _IN int nval, _IN PyObject *tup, _OUT char *val, _IN int taille)
{
        /*
        Convertit une Liste  en tableau de chaines
        Pour retour au Fortran : le tableau existe deja (val)
        */

        PyObject *v = (PyObject*)0 ;
        int i;
        char *s = (char*)0 ;
        char *val_i      = (char*)0 ;
        int longueur=0 ;

        if(nval != 0){
                if (!PyList_Check(tup)){
                        printf("tup : ");
                        PyObject_Print(tup, stdout, 0);
                        printf("\n ");
                        MYABORT("erreur sur le type : devrait etre une liste");
                }
                for(i=0;i<nval;i++){
                        v=PyList_GetItem(tup,i);
                        /* v=PySequence_GetItem(tup,i); */
                        s=PyString_AsString(v);
                        if(s == NULL){
                                printf("s : ");
                                PyObject_Print(v, stdout, 0);
                                printf("\n ");
                                MYABORT("erreur sur le type : devrait etre une string");
                        }

                        /* le fortran attend des chaines de caracteres completees par des blancs */
                        longueur=strlen(s);
                        val_i=&val[i*taille];
                        STRING_FCPY(val_i,taille,s,longueur);
                }
        }
        return ;
}


void STDCALL(GETRAN,getran)(_OUT double *rval)
{
        /*
          Procedure GETRAN pour le FORTRAN : recupere un réel aleatoire (loi uniforme 0-1) du module python Random
          Entrees :
            neant
          Retourne :
            un reel tiré au hasard
        */

        PyObject *res  = (PyObject*)0 ;
        PyObject *val  = (PyObject*)0 ;
        int ok=0;
        int nval=0;
        int nbval=0;

        _DEBUT(getran) ;

        res=PyObject_CallMethod(commande,"getran","");

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"O",&val);
        if(!ok)MYABORT("erreur dans la partie Python");

        *rval=PyFloat_AsDouble(val);

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getran) ;
        return ;
}



void DEFP(INIRAN,iniran,_IN INTEGER *jump)
{
        /*
          Procedure INIRAN pour le FORTRAN : recupere un réel aleatoire (loi uniforme 0-1) du module python Random
          avec un shift eventuel de jump termes
        */

        PyObject *res  = (PyObject*)0 ;

        _DEBUT(iniran) ;
                                                           ISCRUTE(*jump);
        res=PyObject_CallMethod(commande,"iniran","i",*jump);
                                                           ISCRUTE(*jump);
        _FIN(iniran) ;
        return ;
}



void DEFSS(GETTCO,gettco,_IN char *nomobj, _IN int lnom, _OUT char *typobj, _IN int ltyp)
{
        /*
        Procedure gettco_
          remplace le sous-programme fortran  GETTCO

         BUT :
          retrouver le type "superviseur" du concept nomobj.

        cf. cas : hpla100a
        */

        char *mcs      = (char*)0 ;
        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        char *nomType  = (char*)0 ;
        int longueur   = 0 ;
        int ok         = 0 ;
        int nval       = 1 ;
        int k          = 0 ;

        _DEBUT("gettco_") ;
                                                              ASSERT(lnom>0) ;
                                                              FSSCRUTE(nomobj,lnom);
        mcs=fstr2(nomobj,lnom);

        /*
        recherche dans le jeu de commandes python du nom du type de
         du concept Aster de nom nomobj
        */
                                                              ASSERT(commande!=(PyObject*)0);
                                                              SSCRUTE(mcs) ;
        res=PyObject_CallMethod(commande,"gettco","s",mcs);
        if (res == (PyObject*)0)MYABORT("erreur dans la partie Python (gettco)");
                                                              OBSCRUTE(res);
                                                              ASSERT( PyString_Check(res) )
        nomType=PyString_AsString(res);
                                                              SSCRUTE(nomType);
                                                              ASSERT(nomType!=(char*)0) ;
        longueur = strlen(nomType) ;
                                                              ASSERT(longueur>0) ;
                                                              ASSERT(longueur<=ltyp) ;
        STRING_FCPY(typobj,ltyp,nomType,longueur);
                                                              ASSERT(EstPret(typobj,ltyp)) ;

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN("gettco_") ;
        return ;
}

void DEFPS(GETMAT,getmat,_OUT INTEGER *nbarg,_OUT char *motcle,_IN int lcle)
{

        /*
          Procedure GETMAT pour le FORTRAN
          Routine a l usage de DEFI_MATERIAU : consultation du catalogue (et non de l etape)
          Retourne :
            le nombre de mots cles facteur sous la commande, y compris en eliminant les blocs
            la liste de leur noms
        */

        PyObject *res   = (PyObject*)0 ;
        PyObject *lnom  = (PyObject*)0 ; /* liste python des noms */
        int       nval = 0 ;
        int          k = 0 ;


        _DEBUT(getmat_) ;
                                                                        ISCRUTE(lcle);
                                                                        ASSERT(lcle>0);
        for ( k=0 ;k<lcle ; k++ ) motcle[k]=' ' ;
                                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getmat","");
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
        /*  si non impression du retour */

        if(!PyArg_ParseTuple(res,"O",&lnom)) MYABORT("erreur dans la partie Python");
        nval=PyList_Size(lnom);
                                                                        ISCRUTE(nval) ;
        *nbarg = nval ;
                                                                        ISCRUTE(*nbarg) ;

        if ( nval > 0 ){
                converltx(nval,lnom,motcle,lcle); /* conversion  */
        }

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN("getmat_") ;
        return ;
}

void DEFSPPSSP(GETMJM,getmjm,_IN char *nomfac,_IN int lfac,_IN INTEGER *iocc,_IN INTEGER *nbval,
                            _OUT char *motcle,_IN int lcle,_OUT char *type,_IN int ltyp, _OUT INTEGER *nbarg)
{

        /*
          Procedure GETMJM : emule la procedure equivalente ASTER
           Retourne les nbval premiers mots cles du mot cle facteur nomfac du catalogue de la commande en cours
          Entrees :
           nomfac : nom du mot cle facteur
           iocc   : numero d occurence du mot cle facteur
           nbval  : nombre de mots cles facteurs demandes
          Retourne :
           motcle : liste des mots cles du mot cle facteur demande
           type   : liste des types des mots cles du mot cle facteur demande
                    R8 , R8L : un reel ou une liste de reels ;
                    C8 , C8L : un complexe ou une liste de complexes ;
                     ...
                    CO , COL : un concept ou une liste de concepts.
           nbarg  : nombre d arguments des mots cles du mot cle facteur
        */

        PyObject *res   = (PyObject*)0 ;
        PyObject *lnom  = (PyObject*)0 ;
        PyObject *lty   = (PyObject*)0 ; /* liste python des noms */
        int       nval = 0 ;
        int          k = 0 ;


        _DEBUT(getmjm_) ;
                                                                        ISCRUTE(*iocc);
                                                                        ISCRUTE(*nbval);
                                                                        FSSCRUTE(nomfac,lfac) ; ISCRUTE(ltyp);
                                                                        ASSERT(ltyp>0);
        for ( k=0 ;k<ltyp ; k++ ) type[k]=' ' ;
                                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getmjm","sii",fstr2(nomfac,lfac),*iocc,*nbval);
                                                                        ISCRUTE(*iocc);
                                                                        ISCRUTE(*nbval);
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
        /*  si non impression du retour */


        if(!PyArg_ParseTuple(res,"OO",&lnom,&lty)) MYABORT("erreur dans la partie Python");
        nval=PyList_Size(lnom);

                                                                        ISCRUTE(nval) ; ISCRUTE(*nbval) ;

        *nbarg = (nval > *nbval) ? -nval : nval ;
                                                                        ISCRUTE(*nbarg) ;
                                                                        ASSERT(((nval<=*nbval)&&(*nbarg==nval))||(*nbarg==-nval)) ;

        if(*nbarg < 0)nval=*nbval;
                                                                        ISCRUTE(nval) ;

        if ( nval > 0 ){
                converltx(nval,lnom,motcle,lcle); /* conversion  */
                converltx(nval,lty,type,ltyp);
       }


        /*
        A.Y.
        A la demande des developpeurs (J. Pellet), le nom des concepts retourne par
        la methode EXECUTION.getmjm (par exemple grma) est ici remplace par
        la chaine CO (pour COncept).
        les types retournes sont donc parmi les valeurs : R8 , C8 , IS , TX et CO.
        */

        for( k=0 ; k<nval*ltyp ; k+=ltyp ){
                char     *mot = (char*)0 ;
                mot           = type+k ;
                if ( strncmp( mot , "R8" , 2 )!=0 && strncmp( mot , "IS" , 2 )!=0 && strncmp( mot , "TX" , 2 )!=0 && strncmp( mot , "C8" , 2 )!=0 ){
                        int j=0 ;

                        ASSERT(ltyp>2);
                        mot[0]='C' ;
                        mot[1]='O' ;
                        for ( j=2 ; j<ltyp ; j++ ) mot[j]=' ' ;
                }
        }

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN("getmjm_") ;
        return ;
}

FORTRAN_LOGICAL DEFSS( GETEXM ,getexm, _IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle)
{
        /*
          Procedure GETEXM pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
          Retourne :
            0 si n existe pas 1 si existe

          ATTENTION : la valeur C 0 correspond a le valeur Fortran .FORTRAN_FALSE.
        */
        PyObject *res  = (PyObject*)0 ;
        FORTRAN_LOGICAL presence     = FORTRAN_FALSE;

        _DEBUT(getexm_) ;
                                                                        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ;
                                                                        ASSERT(motcle!=(char*)0);
                                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getexm","ss",
                                fstr1(motfac,lfac),fstr2(motcle,lcle));
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
                                                                        OBSCRUTE(res);
        presence=PyInt_AsLong(res) ? FORTRAN_TRUE : FORTRAN_FALSE ;
        /*  decrement sur le refcount du retour */
                                                                        ISCRUTE(presence) ;
                                                                        FSSCRUTE(motcle,lcle) ;
        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getexm_) ;
        return presence;
}


void DEFSSS( GETRES ,getres, _OUT char *nomres, _IN int lres, _OUT char *concep, _IN int lconc, _OUT char *nomcmd, _IN int lcmd)
{
        /*
          Procedure GETRES pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Retourne
            le nom utilisateur du resultat : nomres (string)
            le nom du concept resultat     : concep (string)
            le nom de la commande          : nomcmd (string)
        */
        PyObject *res  = (PyObject*)0 ;
        int ok;
        int s1,s2,s3;
        char *ss1,*ss2,*ss3;

        _DEBUT(getres_) ;
                                                       ISCRUTE(lres) ; ISCRUTE(lconc) ; ISCRUTE(lcmd) ;
        if(commande == (PyObject*)0){
          /* Aucune commande n'est active on retourne des chaines blanches */
          BLANK(nomres,lres);
          BLANK(concep,lconc);
          BLANK(nomcmd,lcmd);
          return ;
        }
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getres","");
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"s#s#s#",&ss1,&s1,&ss2,&s2,&ss3,&s3);
        if (!ok)MYABORT("erreur dans la partie Python");


        /* le fortran attend des chaines de caracteres completees par des blancs */
                                                       ISCRUTE(s1) ; SSCRUTE(ss1) ;
        STRING_FCPY(nomres,lres,ss1,s1);
                                                       FSSCRUTE(nomres,lres) ;

                                                       ISCRUTE(s2) ; SSCRUTE(ss2) ;
        STRING_FCPY(concep,lconc,ss2,s2);
                                                       FSSCRUTE(concep,lconc) ;

                                                       ISCRUTE(s3) ; SSCRUTE(ss3) ;
        STRING_FCPY(nomcmd,lcmd,ss3,s3);
                                                       FSSCRUTE(nomcmd,lcmd) ;

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getres_) ;
        return ;
}

void DEFSSPPPPP(GETVC8,getvc8,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT double *val,_OUT INTEGER *nbval)
{
        /*
          Procedure GETVC8 pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
            le numero de l occurence du mot cle facteur : iocc (entier)
            le numero de l argument demande (obsolete =1): iarg (entier)
            le nombre max de valeur attendues dans val : mxval (entier)
          Retourne :
            le tableau des valeurs attendues : val (2 reels (double) par complexe)
            le nombre de valeurs effectivement retournees : nbval (entier)
               si pas de valeur nbval =0
               si plus de valeur que mxval nbval <0 et valeur abs = nbre valeurs
               si moins de valeurs que mxval nbval>0 et egal au nombre retourne
        */

        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        int ok         = 0 ;
        int nval       = 0 ;
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;

        _DEBUT(getvc8_)
                                                        SSCRUTE(PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                                                        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ; ISCRUTE(*iocc) ;
                                                        ASSERT(EstPret(motcle,lcle)!=0);

        mfc=fstr1(motfac,lfac);
                                                        ASSERT(mfc!=(char*)0);
        mcs=fstr2(motcle,lcle);

        /*
                VERIFICATION
                Si le mot-cle simple est recherche sous un mot-cle facteur et uniquement dans ce cas,
                le numero d'occurrence (*iocc) doit etre strictement positif.
                Si le mot-cle simple est recherche sans un mot-cle facteur iocc n'est pas utilise

        */
        if( isalpha(mfc[0])&&(*iocc<=0) )
        {
                printf( "<F> GETVC8 : le numero d'occurence (IOCC=%d) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }

                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvc8","ssiii",
            mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
                                                        ASSERT(PyTuple_Check(res)) ;


        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if(!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;

        convc8(nval,tup,val);
                                                        ISCRUTE(*nbval) ;
                                                        TDSCRUTE(nval,val) ;

        Py_DECREF(res);
        _FIN(getvc8_) ;
        return ;
}

void DEFSSPPPPP(GETVR8,getvr8,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT double *val,_OUT INTEGER *nbval)
{
        /*
          Procedure GETVR8 pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
            le numero de l occurence du mot cle facteur : iocc (entier)
            le numero de l argument demande (obsolete =1): iarg (entier)
            le nombre max de valeur attendues dans val : mxval (entier)
          Retourne :
            le tableau des valeurs attendues : val (tableau de R8    )
            le nombre de valeurs effectivement retournees : nbval (entier)
               si pas de valeur nbval =0
               si plus de valeur que mxval nbval <0 et valeur abs = nbre valeurs
               si moins de valeurs que mxval nbval>0 et egal au nombre retourne
        */

        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        int ok         = 0 ;
        int nval       = 0 ;
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;

        _DEBUT(getvr8) ;
                                                        SSCRUTE(PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                                                        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ; ISCRUTE(*iocc) ;
                                                        ASSERT(EstPret(motcle,lcle)!=0);
        mfc=fstr1(motfac,lfac); /* conversion chaine fortran en chaine C */
                                                        ASSERT(mfc!=(char*)0);
        mcs=fstr2(motcle,lcle);

        /*
                VERIFICATION
                Si le mot-cle simple est recherche sous un mot-cle facteur et uniquement dans ce cas,
                le numero d'occurrence (*iocc) doit etre strictement positif.
                Si le mot-cle simple est recherche sans un mot-cle facteur iocc n'est pas utilise

        */
        if( isalpha(mfc[0])&&(*iocc<=0) )
        {
                printf( "<F> GETVR8 : le numero d'occurence (IOCC=%d) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }


                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvr8","ssiii",
                                                  mfc,mcs,*iocc,*iarg,*mxval);
        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");
                                                       OBSCRUTE(res);
                                                       ASSERT(PyTuple_Check(res)) ;
        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if(!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        if ( nval>0 ){
                convr8(nval,tup,val);
        }
                                                        ISCRUTE(*nbval) ;
                                                        TDSCRUTE(nval,val) ;

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getvr8) ;
        return ;
}

void DEFSSPPPPP(GETVIS,getvis,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT INTEGER *val,_OUT INTEGER *nbval )
{
        /*
          Procedure GETVIS pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
            le numero de l occurence du mot cle facteur : iocc (entier)
            le numero de l argument demande (obsolete =1): iarg (entier)
            le nombre max de valeur attendues dans val : mxval (entier)
          Retourne :
            le tableau des valeurs attendues : val (tableau d entier )
            le nombre de valeurs effectivement retournees : nbval (entier)
               si pas de valeur nbval =0
               si plus de valeur que mxval nbval <0 et valeur abs = nbre valeurs
               si moins de valeurs que mxval nbval>0 et egal au nombre retourne
        */
        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        int ok         = 0 ;
        int nval       = 0 ;
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;

        _DEBUT(getvis_) ;
                                                        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ;
                                                        ASSERT((*iocc>0)||(FindLength(motfac,lfac)==0));
                                                        ASSERT(EstPret(motcle,lcle)!=0);


        mfc=fstr1(motfac,lfac); /* conversion chaine fortran en chaine C */
                                                        ASSERT(mfc!=(char*)0);
        mcs=fstr2(motcle,lcle);

        /*
                VERIFICATION
                Si le mot-cle simple est recherche sous un mot-cle facteur et uniquement dans ce cas,
                le numero d'occurrence (*iocc) doit etre strictement positif.
                Si le mot-cle simple est recherche sans un mot-cle facteur iocc n'est pas utilise

        */
        if( isalpha(mfc[0])&&(*iocc<=0) )
        {
                printf( "<F> GETVIS : le numero d'occurence (IOCC=%d) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }

                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvis","ssiii",
                                mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

                                                        OBSCRUTE(res) ;
        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        convert(nval,tup,val);

                                                        ISCRUTE(*nbval) ;
                                                        TISCRUTE(nval,val) ;

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getvis_) ;
        return ;
}


void DEFPS(GETVLI,getvli,_OUT INTEGER *unite , _OUT char *cas , _IN int lcas )
{
        /*
        Cette fonction est destinee a etre utilisee pour le fichier "*.code" (fort.15)
        */
        _DEBUT(getvli) ;
                                                        ISCRUTE(lcas);
                                                        ASSERT(NomCas!=(char*)0) ;
        *unite = 15 ;
        CSTRING_FCPY(cas,lcas,NomCas);
                                                        FSSCRUTE(cas,lcas);
        _FIN(getvli) ;
        return ;
}

void DEFSSPPPPP(GETVLS,getvls,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT INTEGER *val,_OUT INTEGER *nbval )
{
        /*
          Procedure GETVLS pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
            le numero de l occurence du mot cle facteur : iocc (entier)
            le numero de l argument demande (obsolete =1): iarg (entier)
            le nombre max de valeur attendues dans val : mxval (entier)
          Retourne :
            le tableau des valeurs attendues : val (tableau de logical )
            le nombre de valeurs effectivement retournees : nbval (entier)
               si pas de valeur nbval =0
               si plus de valeur que mxval nbval <0 et valeur abs = nbre valeurs
               si moins de valeurs que mxval nbval>0 et egal au nombre retourne
        */

        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        int ok         = 0 ;
        int nval       = 0 ;
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;

        _DEBUT(getvls_) ;
                                                        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ;
                                                        ASSERT((*iocc>0)||(FindLength(motfac,lfac)==0));
                                                        ASSERT(EstPret(motcle,lcle)!=0);


        mfc=fstr1(motfac,lfac); /* conversion chaine fortran en chaine C */
                                                        ASSERT(mfc!=(char*)0);
        mcs=fstr2(motcle,lcle);

        /*
                VERIFICATION
                Si le mot-cle simple est recherche sous un mot-cle facteur et uniquement dans ce cas,
                le numero d'occurrence (*iocc) doit etre strictement positif.
                Si le mot-cle simple est recherche sans un mot-cle facteur iocc n'est pas utilise

        */
        if( isalpha(mfc[0])&&(*iocc<=0) )
        {
                printf( "<F> GETVLS : le numero d'occurence (IOCC=%d) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }

                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvls","ssiii",
            mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
        convert(nval,tup,val);
                                                        ISCRUTE(*nbval) ;
                                                        TISCRUTE(nval,val) ;

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getvls_) ;
        return ;
}

void DEFSSPPPSP(GETVTX,getvtx,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT char *txval,_IN int ltx,_OUT INTEGER *nbval)
{
        /*
          Procedure GETVTX pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
            le numero de l occurence du mot cle facteur : iocc (entier)
            le numero de l argument demande (obsolete =1): iarg (entier)
            le nombre max de valeur attendues dans val : mxval (entier)
          Retourne :
            le tableau des valeurs attendues : txval (tableau de string)
            ATTENTION : txval arrive avec une valeur par defaut
            le nombre de valeurs effectivement retournees : nbval (entier)
               si pas de valeur nbval =0
               si plus de valeur que mxval nbval <0 et valeur abs = nbre valeurs
               si moins de valeurs que mxval nbval>0 et egal au nombre retourne

        */
        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        int ok         = 0 ;
        int nval       = 0 ;
        int k          = 0 ;
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;

        _DEBUT(getvtx_) ;
                                                        SSCRUTE(PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                                                        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ; ISCRUTE(ltx) ; ISCRUTE(*iocc) ;
                                                        /*ASSERT((*iocc>0)||(FindLength(motfac,lfac)==0));*/


        mfc=fstr1(motfac,lfac); /* conversion chaine fortran en chaine C */
                                                        ASSERT(mfc!=(char*)0);
        mcs=fstr2(motcle,lcle);

        /*
                VERIFICATION
                Si le mot-cle simple est recherche sous un mot-cle facteur et uniquement dans ce cas,
                le numero d'occurrence (*iocc) doit etre strictement positif.
                Si le mot-cle simple est recherche sans un mot-cle facteur iocc n'est pas utilise

        */
        if( isalpha(mfc[0])&&(*iocc<=0) )
        {
                printf( "<F> GETVTX : le numero d'occurence (IOCC=%d) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }

                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvtx","ssiii",mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

                                                        OBSCRUTE(res);

        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur au decodage d'une chaine dans le module C aster.getvtx");

                                                        ISCRUTE(*nbval);
                                                        ISCRUTE(*mxval);
                                                        ISCRUTE(ltx);
                                                        OBSCRUTE(tup);
        nval=*nbval;
        if(*nbval < 0)nval=*mxval;

        if( nval > 0 ){
                convertxt(nval,tup,txval,ltx);
                                                        FSSCRUTE(txval,nval*ltx) ;
        }

        /* ATTENTION : il ne faut decrementer le compteur de references de res
         *             qu'apres en avoir fini avec l'utilisation de tup.
         *             NE PAS decrementer le compteur de references de tup car
         *             la disparition de res entrainera un decrement automatique
         *             du compteur de tup (res=(nbval,tup))
         */
        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getvtx_) ;
        return ;
}

void DEFSSPPPSP(GETFTX,getftx,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT char *txval,_IN int ltx,_OUT INTEGER *nbval)
{
        /*

          Procedure GETFTX pour le FORTRAN : destinee a l'usage (exclusif) par OPS005
          Cette fonction retourne - dans une tableau de chaines de caracteres fortran -
          une formule ASTER stockee dans le jeu de commande.

          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
            le numero de l occurence du mot cle facteur : iocc (entier)
            le numero de l argument demande (obsolete =1): iarg (entier)
            le nombre max de valeur attendues dans val : mxval (entier)
          Retourne :
            le tableau des valeurs attendues : txval (tableau de string)
            ATTENTION : txval arrive avec une valeur par defaut
            le nombre de valeurs effectivement retournees : nbval (entier)
               si pas de valeur nbval =0
               si plus de valeur que mxval nbval <0 et valeur abs = nbre valeurs
               si moins de valeurs que mxval nbval>0 et egal au nombre retourne
        */
        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        int ok         = 0 ;
        int nval       = 0 ;
        int k          = 0 ;
        char *mfc      = (char*)0 ;
        char *mcs      = (char*)0 ;

        _DEBUT(getftx_) ;
                                                        SSCRUTE(PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                                                        FSSCRUTE(motfac,lfac); FSSCRUTE(motcle,lcle);
                                                        ISCRUTE(*mxval);ISCRUTE(ltx) ; ISCRUTE(*iocc);
                                                        /*ASSERT((*iocc>0)||(FindLength(motfac,lfac)==0));*/

        mfc=fstr1(motfac,lfac);
        mcs=fstr2(motcle,lcle);
                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvtx","ssiii",mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

                                                        OBSCRUTE(res);

        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur au decodage d'une chaine dans le module C aster.getftx");

                                                        ISCRUTE(*nbval) ;
                                                        OBSCRUTE(tup);
        nval=*nbval;
        if(*nbval < 0)nval=*mxval;

        if( nval > 0 ){
                /*
                le tableau de mxval chaines et interprete comme une chaine de mxval*ltx
                caracteres.
                */
                                                        ISCRUTE(PyString_Size(PyTuple_GetItem(tup,0))) ;
                                                        ASSERT(PyString_Size(PyTuple_GetItem(tup,0))<=(*mxval)*ltx) ;
                convertxt(1,tup,txval,(*mxval)*ltx);
                                                        ISCRUTE((*mxval)*ltx) ;
                                                        FSSCRUTE(txval,(*mxval)*ltx) ;
        }


        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getftx_) ;
        return ;
}



void DEFSSPPPSP(GETVID,getvid,_IN char *motfac,_IN int lfac,_IN char *motcle,_IN int lcle,_IN INTEGER *iocc,
                              _IN INTEGER *iarg,_IN INTEGER *mxval,_INOUT char *txval,_IN int ltx,_OUT INTEGER *nbval)
{
        /*
          Procedure GETVID pour le FORTRAN : emule le fonctionnement
          de la procedure equivalente ASTER
          Entrees :
            le nom d un mot cle facteur : motfac (string)
            le nom d un mot cle simple ou sous mot cle : motcle (string)
            le numero de l occurence du mot cle facteur : iocc (entier)
            le numero de l argument demande (obsolete =1): iarg (entier)
            le nombre max de valeur attendues dans val : mxval (entier)
          Retourne :
            le tableau des valeurs attendues : val (tableau de string)
            le nombre de valeurs effectivement retournees : nbval (entier)
               si pas de valeur nbval =0
               si plus de valeur que mxval nbval <0 et valeur abs = nbre valeurs
               si moins de valeurs que mxval nbval>0 et egal au nombre retourne
        */
        PyObject *res  = (PyObject*)0 ;
        PyObject *tup  = (PyObject*)0 ;
        int ok,nval;
        char *mfc;
        char *mcs;


        _DEBUT(getvid_) ;
                                                        SSCRUTE(PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                                                        FSSCRUTE(motfac,lfac) ; FSSCRUTE(motcle,lcle) ;
                                                        ASSERT((*iocc>0)||(FindLength(motfac,lfac)==0));



        mfc=fstr1(motfac,lfac); /* conversion chaine fortran en chaine C */
                                                        ASSERT(mfc!=(char*)0);
        mcs=fstr2(motcle,lcle);

        /*
                VERIFICATION
                Si le mot-cle simple est recherche sous un mot-cle facteur et uniquement dans ce cas,
                le numero d'occurrence (*iocc) doit etre strictement positif.
                Si le mot-cle simple est recherche sans un mot-cle facteur iocc n'est pas utilise

        */
        if( isalpha(mfc[0])&&(*iocc<=0) )
        {
                printf( "<F> GETVID : le numero d'occurence (IOCC=%d) est invalide\n",*iocc) ;
                printf( "             commande : %s\n",PyString_AsString(PyObject_CallMethod(commande,"retnom",""))) ;
                printf( "             mot-cle facteur : %s\n",mfc) ;
                printf( "             mot-cle simple  : %s\n",mcs) ;
                MYABORT( "erreur d'utilisation detectee") ;
        }



                                                        ASSERT(commande!=(PyObject*)0);
        res=PyObject_CallMethod(commande,"getvid","ssiii",mfc,mcs,*iocc,*iarg,*mxval);

        /*  si le retour est NULL : exception Python a transferer
            normalement a l appelant mais FORTRAN ??? */
        if (res == NULL)MYABORT("erreur dans la partie Python");

                                                        OBSCRUTE(res);

        ok = PyArg_ParseTuple(res,"lO",nbval,&tup);
        if (!ok)MYABORT("erreur dans la partie Python");

                                                        ISCRUTE((INTEGER)*nbval) ;

        nval=*nbval;
        if(*nbval < 0)nval=*mxval;
                                                        ISCRUTE(nval) ;
        if ( nval > 0 ){
                convertxt(nval,tup,txval,ltx);
                                                        ISCRUTE(ltx) ;
                                                        ISCRUTE(nval*ltx) ;
                                                        FSSCRUTE(txval,nval*ltx) ;
        }

        Py_DECREF(res);                /*  decrement sur le refcount du retour */
        _FIN(getvid_) ;
        return ;
}


void DEFPPP(SMCDEL,smcdel,INTEGER *iold,INTEGER *inew,INTEGER *ierusr)
{
        /*
          Entrees:
            iold ancien numero d ordre de la commande
            inew nouveau numero d ordre de la commande (si inew < iold, commande detruite)
          Sorties:
            ierusr code retour d erreur incremente
        */
        PyObject *res  = (PyObject*)0 ;

        /*
           Normalement on doit utiliser l dans le format pour des entiers de type long (INTEGER==long)
        */
        _DEBUT("smcdel_")
        res=PyObject_CallMethod(commande,"smcdel","ll",*iold,*inew);

        /*
            Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)MYABORT("erreur a l appel de smcdel dans la partie Python");
        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        _FIN("smcdel_")
}


void DEFPSSP(SMDCMD ,smdcmd,INTEGER *jcmd,char *result,int lresult,char *comman,int lcomman,INTEGER *ierusr)
{
        /*
          Entrees:
            jcmd numero d ordre de la commande en cours
            result nom du resultat produit par la commande
            comman nom de la commande a debuter

          Sorties:
            ierusr code retour d erreur incremente
        */
        PyObject * res;
        _DEBUT("smdcmd_");
        res = PyObject_CallMethod(commande,"smdcmd","ls#s#",*jcmd,result,lresult,comman,lcomman);
        /*
            Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
           MYABORT("erreur a l appel de smdcmd dans la partie Python");
        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        _FIN("smdcmd_") ;
}


void DEFP(SMFCMD,smfcmd,INTEGER *ierusr)
{
        /*
          Entrees:

          Sorties:
            ierusr code retour d erreur incremente
        */
        PyObject * res;
        _DEBUT("smfcmd_");
        res = PyObject_CallMethod(commande,"smfcmd","");
        /*
            Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
           MYABORT("erreur a l appel de smfcmd dans la partie Python");

        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        _FIN("smfcmd_" ) ;
}


void DEFSP( SMDMCF ,smdmcf,char * motf,int lmotf,INTEGER *ierusr)
{
        /*
          Entrees:
            motf mot cle facteur a debuter

          Sorties:
            ierusr code retour d erreur incremente
        */
        PyObject * res;
        _DEBUT("smdmcf_");
        res = PyObject_CallMethod(commande,"smdmcf","s#",motf,lmotf);
        /*
            Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
           MYABORT("erreur a l appel de smdmcf dans la partie Python");

        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        _FIN("smdmcf_") ;
}


void DEFP(SMFMCF,smfmcf,INTEGER *ierusr)
{
        /*
          Entrees:

          Sorties:
            ierusr code retour d erreur incremente
        */
        PyObject * res ;
        _DEBUT("smfmcf_");
        res = PyObject_CallMethod(commande,"smfmcf","");
        /*
            Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
           MYABORT("erreur a l appel de smfmcf dans la partie Python");

        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        _FIN("smfmcf_") ;
}

long FindLength( _IN char *chaineFortran , _IN INTEGER longueur )
{
        /*
        Fonction  : FindLength
        Intention
                Retourne la taille exacte de la chaine de caracteres fortran
                chaineFortran contenant eventuellement des bmancs de fin de ligne..
                La taille exacte est la longueur de la chaine du debut au
                dernier caractere non blanc.
        */

        long k = longueur-1 ;
        if ( ! chaineFortran ) return 0 ;

        while( k>=0 && chaineFortran[k]==' ' ) k-- ;
        return k+1 ;
}

PyObject * MakeTupleString(long nbval,char *kval,int lkval,INTEGER *lval)
{
        /*
                  Entrees:
                    nbval nombre de chaines dans kval
                    kval  tableau de nbval chaines FORTRAN
                    lkval longueur des chaines FORTRAN (compilateur)
                    lval  longueur des nbval chaines FORTRAN (utilisateur)
                  Sorties:
                    RETOUR fonction : tuple de string Python de longueur nbval
                  Fonction:
                    Convertir un tableau de chaines FORTRAN en un tuple de string Python de meme longueur
        */
        int i;
        char *deb=kval;
        if(nbval == 1){
                return PyString_FromStringAndSize(deb,FindLength(deb,*lval));
        }
        else{
                PyObject *t=PyTuple_New(nbval);
                for(i=0;i<nbval;i++){
                        if(PyTuple_SetItem(t,i,PyString_FromStringAndSize(deb,FindLength(deb,lval[i]))))return NULL;
                        deb=deb+lkval;
                }
                return t;
        }
}
PyObject * MakeTupleString_pour_putvid(long nbval,char *kval,int lkval)
{
        /*
                  Entrees:
                    nbval nombre de chaines dans kval
                    kval  tableau de nbval chaines FORTRAN
                    lkval longueur des chaines FORTRAN (compilateur)
                  Sorties:
                    RETOUR fonction : tuple de string Python de longueur nbval
                  Fonction:
                    Convertir un tableau de chaines FORTRAN en un tuple de string Python de meme longueur
        */
        int i;
        char *deb=kval;
        if(nbval == 1){
                return PyString_FromStringAndSize(deb,FindLength(deb,lkval));
        }
        else{
                PyObject *t=PyTuple_New(nbval);
                for(i=0;i<nbval;i++){
                        if(PyTuple_SetItem(t,i,PyString_FromStringAndSize(deb,FindLength(deb,lkval))))return NULL;
                        deb=deb+lkval;
                }
                return t;
        }
}



PyObject * MakeTupleInt(long nbval,long* kval)
{
        /*
                  Entrees:
                    nbval nombre d'entiers dans kval
                    kval  tableau de nbval long FORTRAN
                  Sorties:
                    RETOUR fonction : tuple de int Python de longueur nbval
                  Fonction:
                    Convertir un tableau de long FORTRAN en un tuple de int Python de meme longueur
        */
        int i;
        if(nbval == 1){
                return PyInt_FromLong(*kval);
        }
        else{
                PyObject * t=PyTuple_New(nbval);
                for(i=0;i<nbval;i++){
                        if(PyTuple_SetItem(t,i,PyInt_FromLong(kval[i])))return NULL;
                }
                return t;
        }
}

PyObject * MakeTupleFloat(long nbval,double * kval)
{
        /*
                  Entrees:
                    nbval nombre de reels dans kval
                    kval  tableau de nbval double FORTRAN
                  Sorties:
                    RETOUR fonction : tuple de float Python de longueur nbval
                  Fonction:
                    Convertir un tableau de double FORTRAN en un tuple de float Python de meme longueur
        */
        int i;
        if(nbval == 1){
                return PyFloat_FromDouble(*kval);
        }
        else{
                PyObject * t=PyTuple_New(nbval);
                for(i=0;i<nbval;i++){
                        if(PyTuple_SetItem(t,i,PyFloat_FromDouble(kval[i])))return NULL;
                }
                return t;
        }
}


void DEFSPSP(PUTVID,putvid,char *motcle,int lmotcle,INTEGER *nbval,char *kval,int lkval,INTEGER *ierusr)
{
        /*
                  Entrees:
                    motcle nom du mot cle
                    nbval nombre de concepts
                    kval liste des concepts
                  Sorties:
                    ierusr code retour d erreur incremente
                  Fonction:
                    Rentrer une liste d argument de type identificateur (concept)
        */
        PyObject *res = (PyObject*)0 ;
        _DEBUT("putvid_") ;{
        if(*nbval>0)
        {
                PyObject * t=MakeTupleString_pour_putvid(*nbval,kval,lkval);
                if (t == NULL)
                        MYABORT("erreur a l appel de putvid : impossible de creer un tuple");

                                                                                     ASSERT(motcle!=NULL) ;
                                                                                     ASSERT(lmotcle>0) ;
                res = PyObject_CallMethod(commande,"putvid","s#lO",motcle,FindLength(motcle,lmotcle),*nbval,t);
                Py_DECREF(t);
                                                                                     REFSCRUTE(t) ;
                /*
                            Si le retour est NULL : une exception a ete levee dans le code Python appele
                            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                            On produit donc un abort en ecrivant des messages sur la stdout
                */
                if (res == NULL)
                        MYABORT("erreur a l appel de putvid dans la partie Python");

                                                                                     REFSCRUTE(t);
                *ierusr=*ierusr+PyInt_AsLong(res);
                Py_DECREF(res);
        }
        }_FIN("putvid_") ;
}


void DEFSPPP(PUTVIS,putvis,char *motcle,int lmotcle,INTEGER *nbval,INTEGER *ival,INTEGER *ierusr)
{
        /*
                  Entrees:
                    motcle nom du mot cle
                    nbval nombre d entiers
                    ival liste des entiers
                  Sorties:
                    ierusr code retour d erreur incremente
                  Fonction:
                    Rentrer une liste d argument de type  entier
        */
        PyObject *res = (PyObject*)0 ;
        _DEBUT("putvis_") ;{
        PyObject * t=MakeTupleInt(*nbval,ival);
        if (t == NULL)
                MYABORT("erreur a l appel de putvis : impossible de creer un tuple");

        res = PyObject_CallMethod(commande,"putvis","s#lO",motcle,FindLength(motcle,lmotcle),*nbval,t);
        Py_DECREF(t);
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de putvis dans la partie Python");

                                                                                    REFSCRUTE(t);

        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        }_FIN("putvis_") ;
}



void DEFSPPP(PUTVR8,putvr8,char *motcle,int lmotcle,INTEGER *nbval,double *rval,INTEGER *ierusr)
{
        /*
                  Entrees:
                    motcle nom du mot cle
                    nbval nombre de reels
                    rval liste des reels
                  Sorties:
                    ierusr code retour d erreur incremente
                  Fonction:
                    Rentrer une liste d argument de type  reel
        */
        PyObject *res = (PyObject*)0 ;
        _DEBUT("putvr8_") ;{
        PyObject * t=MakeTupleFloat(*nbval,rval);
        if (t == NULL)
                MYABORT("erreur a l appel de putvr8 : impossible de creer un tuple");

        res = PyObject_CallMethod(commande,"putvr8","s#lO",motcle,FindLength(motcle,lmotcle),*nbval,t);
        Py_DECREF(t);
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de putvr8 dans la partie Python");

                                                                                    REFSCRUTE(t);

        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        }_FIN("putvr8_") ;
}


void DEFSPSPP(PUTVTX,putvtx,char *motcle,int lmotcle,INTEGER *nbval,char *kval,int lkval,INTEGER *ival,INTEGER *ierusr)
{
        /*
                  Entrees:
                    motcle nom du mot cle
                    nbval nombre de texte
                    kval liste des nbval textes
                    ival liste des nbval longueurs de texte
                  Sorties:
                    ierusr code retour d erreur incremente
                  Fonction:
                    Rentrer une liste d argument de type texte
        */
        PyObject * res = (PyObject*)0 ;
        _DEBUT("putvtx_") ; FSSCRUTE(motcle,lmotcle);FSSCRUTE(kval,lkval);ISCRUTE(*ival) ; ISCRUTE(lkval);{

        PyObject * t=MakeTupleString(*nbval,kval,lkval,ival);
        if (t == NULL)
                MYABORT("erreur a l appel de putvtx : impossible de creer un tuple");

        res = PyObject_CallMethod(commande,"putvtx","s#lOl",motcle,FindLength(motcle,lmotcle),*nbval,t,*ival);
        Py_DECREF(t);
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de putvtx dans la partie Python");

                                                                                                 REFSCRUTE(t);
        *ierusr=*ierusr+PyInt_AsLong(res);
        Py_DECREF(res);
        }_FIN("putvtx_") ;
}


void DEFPSSP(GCUCON,gcucon,INTEGER *icmd, char *resul, int lresul, char *concep, int lconcep, INTEGER *ier)
{
        /*
                  Entrees:
                    icmd    numero de la commande
                    resul   nom du concept
                    concep type du concept
                  Sorties :
                    ier     >0 le concept existe avant
                            =0 le concept n'existe pas avant
                            <0 le concept existe avant mais n'est pas du bon type
                  Fonction:
                    Verification de l existence du couple (resul,concep) dans les
                    resultats produits par les etapes precedentes
        */
        PyObject * res = (PyObject*)0 ;
        _DEBUT("gcucon_") ;
                                                                                    ASSERT(lresul) ;
                                                                                    ASSERT(lconcep) ;
        res = PyObject_CallMethod(commande,"gcucon","ls#s#",*icmd,resul,lresul,concep,lconcep);
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de gcucon dans la partie Python");

        *ier = PyInt_AsLong(res);
        Py_DECREF(res);
        _FIN("gcucon_") ;
}


void DEFPSP(GCUCDT,gcucdt,INTEGER *icmd,char *resul,int lresul,INTEGER *ier)
{
        /*
        Emulation de la fonction ASTER correspondante

        Entrees:
                icmd    numero de la commande
                resul   nom du concept
                Sorties :
                ier     >0 le concept existe avant
                        =0 le concept n'existe pas avant
                        <0 le concept existe avant mais est detruit
        Fonction:
                VERIFICATION DE L'EXISTENCE D'UN CONCEPT DANS LES
                DECLARATIONS PRECEDENTES (IE JUSQU'A L'ORDRE ICMD-1)
        Commentaire :
                L'émulation de la fonction est seulement partielle. On utilise
                la fonction gcucon qui ne donne pas l'information sur les concepts
                detruits
        */
        PyObject * res = (PyObject*)0 ;
        _DEBUT("gcucdt_") ;
        res = PyObject_CallMethod(commande,"gcucon","ls#s",*icmd,resul,lresul,"");
        /*
           Si le retour est NULL : une exception a ete levee dans le code Python appele
            Cette exception est a transferer normalement a l appelant mais FORTRAN ???
            On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de gcucdt dans la partie Python");

        *ier = PyInt_AsLong(res);
        /*
                ier= -1 indique que le concept existe mais d'un autre type. On doit donc
                retourner 1
        */
        if(*ier==-1)*ier=1;
                                                                                     ISCRUTE(*ier);
        Py_DECREF(res);
        _FIN("gcucdt_") ;
}


void DEFSSPPP(GETTVC,gettvc,char * nom,int lnom,char *ctyp,int lctyp,INTEGER *ival,double *rval,INTEGER *ier)
{
        /*
                  Entrees:
                    nom    numero de la commande
                  Sorties :
                    ctyp    type de la constante (IS,R8,C8,LS)
                    ival    valeur de la constante si IS ou LS
                    rval    valeur de la constante si R8 ou C8 (dimension 2)
                    ier     >0 la constante existe
                            =0 la constante n'existe pas
                  Fonction:
                    Retourner la valeur de la constante nom si elle existe
                    si elle n existe pas ier = 0
                  Commentaire : RAS
        */
        PyObject * res = (PyObject*)0 ;
        PyObject * valeur = (PyObject*)0 ;
        int ok=0;
        _DEBUT("gettvc_") ;
        *ier=0;
        res = PyObject_CallMethod(commande,"gettvc","s#",nom,lnom);
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de gettvc dans la partie Python");

        ok=PyArg_ParseTuple(res, "lO",ier,&valeur);
        if(!ok)MYABORT("erreur dans gettvc_ ");
                                                                              REFSCRUTE(valeur) ;
        if(PyInt_Check(valeur)){
          *ival=PyInt_AsLong(valeur);
          strncpy(ctyp,"IS  ",4);
        }
        else if(PyFloat_Check(valeur)){
          *rval=PyFloat_AsDouble(valeur);
          strncpy(ctyp,"R8  ",4);
        }
        else{
          *ier=0;
        }

        Py_DECREF(res); /* le compteur de references de valeur sera automatiquement decremente */
        _FIN("gettvc_") ;
}


void DEFPPP(GCECDU,gcecdu,INTEGER *ul,INTEGER *icmdu, INTEGER *numint)
{
        /*
          Entrees:
            ul      unite logique pour les ecritures
            icmdu   numero de la commande
          Sorties :
            numint  numero de l operateur de la commande
          Fonction:
             Ecriture d'un operateur ou d une commande utilisateur avec ses arguments (pas implemente)
             Recuperation du numero de l operateur
        */
        PyObject * res = (PyObject*)0 ;
        _DEBUT("gcecdu_") ;
        res = PyObject_CallMethod(commande,"getoper","");
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de gcecdu dans la partie Python");

        *numint = PyInt_AsLong(res);
        Py_DECREF(res);
        _FIN("gcecdu_") ;
}



void gcncon2_(char *type,char *resul,int ltype,int lresul)
{
/* CCAR : cette fonction devrait s appeler gcncon mais elle est utilisee par
          tous les operateurs (???) et pas seulement dans les macros
          Pour le moment il a ete decide de ne pas l'emuler dans le superviseur
          Python mais d'utiliser les fonctions FORTRAN existantes
          Ceci a l avantage d'assurer la coherence entre tous les operateurs
          et de conserver les fonctionnalites de poursuite pour les macros
*/
        /*
          Entrees:
            type vaut soit
                    '.' : le concept sera detruit en fin de job
                    '_' : le concept ne sera pas detruit

          Sorties:
            resul  nom d'un concept delivre par le superviseur
                   Ce nom est de la forme type // '000ijkl' ou ijkl est un nombre
                   incremente a chaque appel pour garantir l unicite des noms

          Fonction:
            Delivrer un nom de concept non encore utilise et unique
        */
        MYABORT("Cette procedure n est pas implementee");
}

/*
 * Pour conserver le lien entre le code de calcul en Fortran et le superviseur
 * en Python, on memorise la commande courante.
 * Cette commande est celle que le fortran interroge lors de ses appels à l'API
 * GETXXX.
 * Cette commande courante est enregistrée dans une pile de commandes car avec le
 * mécanisme des macros, une commande peut avoir des sous commandes qui sont
 * appelées pendant l'exécution de la commande principale.
 * La fonction empile doit etre appelée avant l'exécution d'une commande (appel à oper,
 * par exemple) et la fonction depile doit etre appelée après l'exécution de cette
 * commande.
 */
static PyObject * empile(PyObject *c)
{
        _DEBUT(empile) ;
                                                               OBSCRUTE(c) ; /*  impression de la commande courante */
                                                               REFSCRUTE(c) ; /*  impression du compteur de references de la commande courante */
        /* PyList_Append incremente de 1 le compteur de references de c (commande courante) */
        PyList_Append(pile_commandes,c);
        niveau=niveau+1;
                                                               ISCRUTE(niveau);
        if(NIVMAX < niveau){
          printf("Le nombre de niveau max prevus %d est insuffisant pour le nombre demande %d\n",NIVMAX,niveau);
          abort();
        }
                                                               REFSCRUTE(c) ;
        _FIN(empile) ;
        return c;
}

static PyObject * depile()
{
        PyObject * com;
        int l=PyList_Size(pile_commandes);
        _DEBUT(depile) ;
        niveau=niveau-1;
                                                               ISCRUTE(niveau);
                                                               ISCRUTE(l) ;
        if(l == 0){
          /* Pile vide */
          Py_INCREF( Py_None ) ;
          _FIN(depile) ;
          return Py_None;
        }
        /* Derniere commande dans la pile */
        com = PyList_GetItem(pile_commandes,l-1);
        /* PyList_GetItem n incremente pas le compteur de ref de com */
                                                               REFSCRUTE(com) ;
        /* On tronque la liste a la dimension l-1 */
        PyList_SetSlice(pile_commandes,l-1,l,NULL);
        /* Le compteur de ref de com est decremente de 1 */
                                                               REFSCRUTE(com) ;
        if(l == 1){
          /* La pile tronquee est vide */
          Py_INCREF( Py_None ) ;
          _FIN(depile) ;
          return Py_None;
        }
        /* On ne passe ici que pour les macros avec sous commandes
         * en mode commande par commande */
        /* On retourne la derniere commande de la pile */
        com = PyList_GetItem(pile_commandes,l-2);
                                                               REFSCRUTE(com) ;
        _FIN(depile) ;
        return com;
}

PyObject * get_active_command()
{
        /*
         * Retourne un pointeur sur la commande active
         */
   return commande;
}

#define CALL_PRCOCH(nomce,nomcs,nomcmp,ktype,itopo,nval,groups) CALLSSSSPPS(PRCOCH,prcoch,nomce,nomcs,nomcmp,ktype,itopo,nval,groups)
void DEFSSSSPPS(PRCOCH,prcoch,char *,int,char *,int,char *,int,char *,int,INTEGER *,INTEGER *,char *,int);

static PyObject* aster_prepcompcham(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        char *nomce;
        char *nomcs;
        char *nomcmp;
        char *ktype;
        char *groups;
        PyObject *list;
        INTEGER nval=0;
        int k;
        int long_nomcham=8;
        INTEGER itopo;

        _DEBUT(aster_prepcompcham) ;

        if (!PyArg_ParseTuple(args, "sssslO:prepcompcham",&nomce,&nomcs,&nomcmp,&ktype,&itopo,&list)) return NULL;

        nval=PyList_Size(list);


        if (nval > 0) {
          groups = (char *)malloc(nval*long_nomcham*sizeof(char));
          converltx(nval,list,groups,long_nomcham); /* conversion  */
        }
        /* on ne peut passer a fortran une chaine non allouee
           a cause du strlen() que l'on va faire dessus au moment du passage
           c -> fortran
        */
        else {
          groups = (char *)malloc(long_nomcham*sizeof(char));
          groups = strcpy(groups,"        ");
        }



        try(1){
                                SSCRUTE(nomce);
                                SSCRUTE(nomcs);
                                SSCRUTE(nomcmp);
                                SSCRUTE(ktype);
                                ISCRUTE(itopo);
                                ISCRUTE(nval);
                                FSSCRUTE(groups,nval*long_nomcham);
          CALL_PRCOCH(nomce,nomcs,nomcmp,ktype,&itopo,&nval,groups);
          Py_INCREF( Py_None ) ;
          _FIN(aster_prepcompcham) ;
          free(groups);
          return Py_None;
        }
        catch(CodeAbortAster){
          /* une exception a ete levee, elle est destinee a etre traitee dans l'appelant */
          PyErr_SetString(PyExc_KeyError, "Concept inexistant");
          _FIN(aster_prepcompcham) ;
          return NULL;
        }
}

#define CALL_GETCON(nomsd,iob,ctype,lcon,iaddr,nomob) CALLSPPPPS(GETCON,getcon,nomsd,iob,ctype,lcon,iaddr,nomob)
void DEFSPPPPS(GETCON,getcon,char *,int,INTEGER *,INTEGER *,INTEGER *,char **,char *,int);
#define CALL_JELIBE(nomsd) CALLS(JELIBE,jelibe,nomsd)
void DEFS(JELIBE,jelibe,char *,int);

static char getvectjev_doc[]=
"getvectjev(nomsd)->valsd      \n\
\n\
Retourne la valeur du concept nomsd \n\
dans un tuple.";

static PyObject* aster_getvectjev(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        char *nomsd;
        char nomob[8];
        double *f;
        INTEGER *l;
        char *kvar;
        PyObject *tup;
        INTEGER lcon, iob;
        INTEGER ctype=0;
        int i;
        char *iaddr;

        _DEBUT(aster_getvectjev) ;

        if (!PyArg_ParseTuple(args, "s:getvectjev",&nomsd)) return NULL;

        try(1){
          iob=0 ;
                                   SSCRUTE(nomsd);
                                   ISCRUTE(iob);
          CALL_GETCON(nomsd,&iob,&ctype,&lcon,&iaddr,nomob);
                                   ISCRUTE(lcon);
                                   ISCRUTE(ctype);
                                   ISCRUTE(iaddr);
                                   SSCRUTE(nomob);
          if(ctype < 0){
            /* Erreur */
            PyErr_SetString(PyExc_KeyError, "Concept inexistant");
            _FIN(aster_getvectjev) ;
            return NULL;
          }
          else if(ctype == 0){
            Py_INCREF( Py_None ) ;
            _FIN(aster_getvectjev) ;
            return Py_None;
          }
          else if(ctype == 1){
            /* REEL */
            f = (double *)iaddr;
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyFloat_FromDouble(f[i]) ) ;
            }
            _FIN(aster_getvectjev) ;
            return tup;
          }
          else if(ctype == 2){
            /* ENTIER */
            l = (INTEGER*)iaddr;
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyInt_FromLong(l[i]) ) ;
            }
            _FIN(aster_getvectjev) ;
            return tup;
          }
          else if(ctype == 3){
            /* COMPLEXE */
            f = (double *)iaddr;
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyComplex_FromDoubles(f[2*i],f[2*i+1]) ) ;
            }
            _FIN(aster_getvectjev) ;
            return tup;
          }
          else if(ctype == 4){
            /* CHAINE K8 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
                                        OBSCRUTE(tup);
               kvar = iaddr + i*8;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,8) ) ;
            }
            _FIN(aster_getvectjev) ;
            return tup;
          }
          else if(ctype == 5){
            /* CHAINE K16 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
                                        OBSCRUTE(tup);
               kvar = iaddr + i*16;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,16) ) ;
            }
            _FIN(aster_getvectjev) ;
            return tup;
          }
          else if(ctype == 6){
            /* CHAINE K24 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
                                        OBSCRUTE(tup);
               kvar = iaddr + i*24;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,24) ) ;
            }
            _FIN(aster_getvectjev) ;
            return tup;
          }
          else if(ctype == 7){
            /* CHAINE K32 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
                                        OBSCRUTE(tup);
               kvar = iaddr + i*32;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,32) ) ;
            }
            _FIN(aster_getvectjev) ;
            return tup;
          }
          else if(ctype == 8){
            /* CHAINE K80 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
                                        OBSCRUTE(tup);
               kvar = iaddr + i*80;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,80) ) ;
            }
            _FIN(aster_getvectjev) ;
            return tup;
          }
                                   SSCRUTE(nomsd);
          CALL_JELIBE(nomsd);
        }
        catch(CodeAbortAster){
          /* une exception a ete levee, elle est destinee a etre traitee dans l'appelant */
          PyErr_SetString(PyExc_KeyError, "Concept inexistant");
          _FIN(aster_getvectjev) ;
          return NULL;
        }
}

#define CALL_TAILSD(nom, nomsd, val, nbval) CALLSSPP(TAILSD,tailsd,nom, nomsd, val, nbval)
void DEFSSPP(TAILSD,tailsd,char *,int,char *,int,INTEGER *, INTEGER *);
#define CALL_JELIBE(nomsd) CALLS(JELIBE,jelibe,nomsd)
void DEFS(JELIBE,jelibe,char *,int);

static char getcolljev_doc[]=
"getcolljev(nomsd)->valsd      \n\
\n\
Retourne la valeur du concept nomsd \n\
dans un tuple.";

static PyObject* aster_getcolljev(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        char *nomsd, *nom;
        char nomob[8];
        double *f;
        INTEGER *l;
        char *kvar;
        PyObject *tup, *dico, *key;
        INTEGER iob,j;
        INTEGER lcon;
        INTEGER ctype=0;
        INTEGER *val, nbval;
        int i;
        char *iaddr;

        _DEBUT(aster_getcolljev) ;

        if (!PyArg_ParseTuple(args, "s:getcolljev",&nomsd)) return NULL;

/* Taille de la collection */
        nbval = 1;
        val = (INTEGER *)malloc((nbval)*sizeof(INTEGER));
        nom = (char *)malloc(24*sizeof(char));
        strcpy(nom, "LIST_COLLECTION");
        CALL_TAILSD(nom, nomsd, val, &nbval);
        iob=val[0];

        dico = PyDict_New();
        try(1){
          for(j=1;j<iob+1;j++){
                                   SSCRUTE(nomsd);
                                   ISCRUTE(j);
          CALL_GETCON(nomsd,&j,&ctype,&lcon,&iaddr,nomob);
                                   ISCRUTE(lcon);
                                   ISCRUTE(ctype);
                                   ISCRUTE(iaddr);
                                   SSCRUTE(nomob);
          if(nomob[1] == ' '){
             key=PyInt_FromLong(j);
          }
          else {
             key=PyString_FromStringAndSize(nomob,8);
          }
          if(ctype < 0){
            /* Erreur */
            PyErr_SetString(PyExc_KeyError, "Concept inexistant");
            _FIN(aster_getcolljev) ;
            return NULL;
          }
          else if(ctype == 0){
            Py_INCREF( Py_None );
            PyDict_SetItem(dico,key,Py_None);
          }
          else if(ctype == 1){
            /* REEL */
            f = (double *)iaddr;
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyFloat_FromDouble(f[i]) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 2){
            /* ENTIER */
            l = (INTEGER*)iaddr;
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyInt_FromLong(l[i]) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 3){
            /* COMPLEXE */
            f = (double *)iaddr;
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
               PyTuple_SetItem( tup, i, PyComplex_FromDoubles(f[2*i],f[2*i+1]) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 4){
            /* CHAINE K8 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
                                        OBSCRUTE(tup);
               kvar = iaddr + i*8;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,8) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 5){
            /* CHAINE K16 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
                                        OBSCRUTE(tup);
               kvar = iaddr + i*16;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,16) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 6){
            /* CHAINE K24 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
                                        OBSCRUTE(tup);
               kvar = iaddr + i*24;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,24) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 7){
            /* CHAINE K32 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
                                        OBSCRUTE(tup);
               kvar = iaddr + i*32;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,32) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          else if(ctype == 8){
            /* CHAINE K80 */
            tup = PyTuple_New( lcon ) ;
            for(i=0;i<lcon;i++){
                                        OBSCRUTE(tup);
               kvar = iaddr + i*80;
               PyTuple_SetItem( tup, i, PyString_FromStringAndSize(kvar,80) ) ;
            }
            PyDict_SetItem(dico,key,tup);
          }
          CALL_JELIBE(nomsd);
         }
         return dico;
         _FIN(aster_getcolljev) ;
       }
        catch(CodeAbortAster){
          /* une exception a ete levee, elle est destinee a etre traitee dans l'appelant */
          PyErr_SetString(PyExc_KeyError, "Concept inexistant");
          _FIN(aster_getcolljev) ;
          return NULL;
        }
}

void DEFSSPSPP(LIMAGP,limagp,char *, int, char *, int, INTEGER *, char *, int, INTEGER *, INTEGER *);
void DEFSPSPPPS(RSACCH,rsacch,char *, int, INTEGER *, char *,int,INTEGER *, INTEGER *, INTEGER *, char *,int);
void DEFSPSSPPP(RSACVA,rsacva,char *, int,INTEGER *, char *,int,char *,int,INTEGER *, double *,INTEGER *);
void DEFSPSSPPP(RSACPA,rsacpa,char *, int,INTEGER *, char *,int,char *,int,INTEGER *, double *,INTEGER *);

#define CALL_LIMAGP(noma, ligrno, linbno, ligrma, linbma, lidima) \
                  FCALLSSPSPP(LIMAGP,limagp,noma,strlen(noma),ligrno,8,linbno,ligrma,8,linbma,lidima)
#define CALL_RSACCH(nomsd, numch, nomch, nbord, liord, nbcmp, liscmp) \
                  FCALLSPSPPPS(RSACCH,rsacch,nomsd,strlen(nomsd),numch, nomch,16,nbord, liord, nbcmp, liscmp,8)
#define CALL_RSACVA(nomsd, numva, nomva, ctype, ival, rval, ier) \
                  CALLSPSSPPP(RSACVA,rsacva,nomsd, numva, nomva, ctype, ival, rval, ier)
#define CALL_RSACPA(nomsd, numva, nomva, ctype, ival, rval, ier) \
                  CALLSPSSPPP(RSACPA,rsacpa,nomsd, numva, nomva, ctype, ival, rval, ier)



static PyObject* aster_GetMaillage(self, args)
PyObject *self; /* Not used */
PyObject *args;

/* Retourne les informations relatives a une SD maillage

   Arguments :
     IN Nom du maillage
     IN Nature des informations recherchees
          GROUP_MA   -> Liste des groupes de mailles
          GROUP_NO   -> Liste des groupes de noeuds

     OUT liste
       'GROUP_MA' -> Liste des groupes de mailles
       'GROUP_NO' -> Liste des groupes de noeuds

*/

{

   INTEGER *val, nbval, nbgpno, nbgpma;
   char *noma, *nom, *nomsd, *mode;
        int i, nb, lo;
        char *ligpno, *ligpma;
        INTEGER *linbma, *linbno, *lidima;
   PyObject *liste, *tuple;

   if (!PyArg_ParseTuple(args, "ss",&noma, &mode)) return NULL;

/* Identifiant de la SD Maillage */
   nbval = 2;
   val = (INTEGER *)malloc((nbval)*sizeof(INTEGER));
   nom = (char *)malloc(24*sizeof(char));
   strcpy(nom, "LIST_GROUP");

/* Taille de la SD maillage : nbr GROUP_NO, nbr GROUP_MA */
        CALL_TAILSD(nom, noma, val, &nbval);

   nbgpno = val[0];
   nbgpma = val[1];
        ligpno = (char *)malloc((1+nbgpno)*8*sizeof(char));
        ligpma = (char *)malloc((1+nbgpma)*8*sizeof(char));
   linbno = (INTEGER *)malloc((1+nbgpno)*sizeof(INTEGER));
   linbma = (INTEGER *)malloc((1+nbgpma)*sizeof(INTEGER));
   lidima = (INTEGER *)malloc((1+nbgpma)*sizeof(INTEGER));
   CALL_LIMAGP(noma, ligpno, linbno, ligpma, linbma, lidima);

   liste = PyList_New(0);

/* Liste des GROUP_MA */

     if (strcmp(mode,"GROUP_MA") == 0)
          {
          for (i=0; i<nbgpma; i++)
          {
       tuple = PyTuple_New(3);
            nom = &(ligpma[i*8]);
       lo = 8; while (nom[lo-1] == ' ')  lo--;
            PyTuple_SetItem(tuple, 0, PyString_FromStringAndSize(nom,lo));
            PyTuple_SetItem(tuple, 1, PyInt_FromLong(linbma[i]));
            PyTuple_SetItem(tuple, 2, PyInt_FromLong(lidima[i]));
            PyList_Append(liste,tuple);
            };
          }

/* Liste des GROUP_NO */

        if (strcmp(mode,"GROUP_NO") == 0)
          {
          for (i=0; i<nbgpno; i++)
     {
       tuple = PyTuple_New(2);
            nom = &(ligpno[i*8]);
       lo = 8; while (nom[lo-1] == ' ')  lo--;
            PyTuple_SetItem(tuple, 0, PyString_FromStringAndSize(nom,lo));
            PyTuple_SetItem(tuple, 1, PyInt_FromLong(linbno[i]));
            PyList_Append(liste,tuple);
            };
          }

          free(ligpno);
          free(linbno);
          free(ligpma);
          free(linbma);
          free(lidima);
          return liste;

}

static PyObject* aster_GetResu(self, args)
PyObject *self; /* Not used */
PyObject *args;

/* Construit sous forme d'un dictionnaire Python l'architecture d'une SD resultat

   Arguments :
     IN Nom de la SD resultat
     IN Nature des informations recherchees
          CHAMPS      -> Champs de resultats
          COMPOSANTES -> Liste des composantes des champs
          VARI_ACCES  -> Variables d'acces
          PARAMETRES  -> Parametres


     OUT dico
       Si 'CHAMPS'
       dico['NOM_CHAM'] -> [] si le champ n'est pas calcule
                        -> Liste des numeros d'ordre ou le champ est calcule

       Si 'COMPOSANTES'
       dico['NOM_CHAM'] -> [] si le champ n'est pas calcule
                        -> Liste des composantes du champ (enveloppe sur tous les instants)

       Si 'VARI_ACCES'
       dico['NOM_VA']   -> Liste des valeurs de la variable d'acces

       Si 'PARAMETRES'
       dico['NOM_VA']   -> Liste des valeurs du parametre

*/

{
   INTEGER nbchmx, nbpamx, nbord, numch, numva, ier, nbcmp ;
   INTEGER *liord, *ival;
   INTEGER *val, nbval ;
   double *rval;
   char *nomsd, *mode, *liscmp, *nom ;
   char nomch[16], ctype, nomva[16];
   int i, lo, nb;
   PyObject *dico, *liste, *key;

   if (!PyArg_ParseTuple(args, "ss",&nomsd, &mode)) return NULL;

/* Identifiant de la SD resultat */	
   nbval = 1;
   val = (INTEGER *)malloc((nbval)*sizeof(INTEGER));
   nom = (char *)malloc(24*sizeof(char));
   strcpy(nom, "LIST_RESULTAT");

/* Taille de la SD resultat : nbr champs, nbr paras, nbr numeros d'ordre */
	CALL_TAILSD(nom, nomsd, val, &nbval);
   nbchmx = val[0];
   nbpamx = val[1];
   nbord  = val[2];

   if (strcmp(mode,"CHAMPS") == 0 || strcmp(mode,"COMPOSANTES") == 0)

/* Construction du dictionnaire : cle d'acces = nom du champ */

          {
     liord  = (INTEGER *)malloc(nbord*sizeof(INTEGER));
     liscmp = (char *)malloc(500*8*sizeof(char));
     dico = PyDict_New();
     for (numch=1; numch<=nbchmx; numch++)
            {
       CALL_RSACCH(nomsd, &numch, nomch, &nbord, liord, &nbcmp, liscmp);

       lo = 16;
       while (nomch[lo-1] == ' ')  lo--;
       key = PyString_FromStringAndSize(nomch,lo);

       liste = PyList_New(0);

       if (strcmp(mode,"CHAMPS") == 0)
              {
              for (i=0; i<nbord; i++)
              PyList_Append(liste,PyInt_FromLong(liord[i]));
              }

            if (strcmp(mode,"COMPOSANTES") == 0)
              {
              for (i=0; i<nbcmp; i++)
                {
                nom = &(liscmp[i*8]);
                lo = 8; while (nom[lo-1] == ' ')  lo--;
                PyList_Append(liste,PyString_FromStringAndSize(nom,lo));
                }
              }

            PyDict_SetItem(dico,key,liste);
            };

          free(liord);
          }


        else if (strcmp(mode,"VARI_ACCES") == 0 )

/* Extraction des variables d'acces */

          {
          ival  = (INTEGER *)malloc(nbord*sizeof(INTEGER));
          rval  = (double * )malloc(nbord*sizeof(double) );

          dico = PyDict_New();
          for (numva=0; numva<=nbpamx; numva++)
            {
            CALL_RSACVA(nomsd, &numva, nomva, &ctype, ival, rval, &ier);
            if (ier != 0) continue;

            lo = 16;
            while (nomva[lo-1] == ' ') lo--;
            key = PyString_FromStringAndSize(nomva,lo);

            liste = PyList_New(0);
            if (ctype == 'I')
              for (i=0; i<nbord; i++)
                PyList_Append(liste,PyInt_FromLong(ival[i]));
            else
              for (i=0; i<nbord; i++)
                PyList_Append(liste,PyFloat_FromDouble(rval[i]));

            PyDict_SetItem(dico,key,liste);
            };

          free(ival);
          free(rval);
          }
        else if (strcmp(mode,"PARAMETRES") == 0 )

/* Extraction des parametres */

          {
          ival  = (INTEGER *)malloc(nbord*sizeof(INTEGER));
          rval  = (double * )malloc(nbord*sizeof(double) );

          dico = PyDict_New();
          for (numva=0; numva<=nbpamx; numva++)
            {
            CALL_RSACPA(nomsd, &numva, nomva, &ctype, ival, rval, &ier);
            if (ier != 0) continue;

            lo = 16;
            while (nomva[lo-1] == ' ') lo--;
            key = PyString_FromStringAndSize(nomva,lo);

            liste = PyList_New(0);
            if (ctype == 'I')
              for (i=0; i<nbord; i++)
                PyList_Append(liste,PyInt_FromLong(ival[i]));
            else
              for (i=0; i<nbord; i++)
                PyList_Append(liste,PyFloat_FromDouble(rval[i]));

            PyDict_SetItem(dico,key,liste);
            };

          free(ival);
          free(rval);
          };

        return dico;

}


#define CALL_EXPASS(a,b,c,d)  F_FUNC(EXPASS,expass)(a,b,c,d)
extern void STDCALL(EXPASS,expass)(INTEGER* , INTEGER* , INTEGER* , INTEGER*);

static PyObject* aster_oper(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp;
        INTEGER lot=1 ; /* FORTRAN_TRUE */
        INTEGER iertot=0 ;
        INTEGER icmd=0 ;
        INTEGER ipass=0 ;

        _DEBUT(aster_oper) ;

        if (!PyArg_ParseTuple(args, "Olll",&temp,&lot,&ipass,&icmd)) return NULL;

        /* On empile le nouvel appel */
        commande=empile(temp);

        if(PyErr_Occurred()){
            fprintf(stderr,"Warning: une exception n'a pas ete traitée\n");
            PyErr_Print();
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait\n\
                            etre traitée avant\n");
            PyErr_Clear();
        }

        fflush(stderr) ;
        fflush(stdout) ;

        try(1){

                /*  appel du sous programme expass pour verif ou exec */
                CALL_EXPASS (&lot,&ipass,&icmd,&iertot);

                /* On depile l appel */
                commande = depile();

                                                                                 ISCRUTE(iertot) ;
                _FIN(aster_oper) ;
                return PyInt_FromLong(iertot); /*  retour de la fonction oper sous la forme d un entier */
        }
        finally{
                /* On depile l appel */
                commande = depile();

                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */
                TraitementFinAster( exception_status ) ;

                _FIN(aster_oper) ;
                return NULL;
        }
}

#define CALL_OPSEXE(a,b,c,d,e)  CALLPPPSP(OPSEXE,opsexe,a,b,c,d,e)
extern void DEFPPPSP(OPSEXE,opsexe,INTEGER* , INTEGER* , INTEGER* , char *,int ,INTEGER* ) ;

static PyObject* aster_opsexe(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp;
        INTEGER ier=0 ;
        INTEGER icmd=0 ;
        INTEGER oper=0 ;
        INTEGER ipass=0 ;
        char *cmdusr="                                                                          ";

        _DEBUT(aster_opsexe) ;

        if (!PyArg_ParseTuple(args, "Olll",&temp,&icmd,&ipass,&oper)) return NULL;

        /* On empile le nouvel appel */
        commande=empile(temp);

        if(PyErr_Occurred()){
            fprintf(stderr,"Warning: une exception n'a pas ete traitée\n");
            PyErr_Print();
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait\n\
                            etre traitée avant\n");
            PyErr_Clear();
        }

        fflush(stderr) ;
        fflush(stdout) ;

        try(1){
                /*  appel du sous programme opsexe */
                CALL_OPSEXE (&icmd,&ipass,&oper,cmdusr,&ier);

                /* On depile l appel */
                commande = depile();
                                                                                 ISCRUTE(ier) ;
                                                                                 PRINTERR ;
                _FIN(aster_opsexe) ;
                return PyInt_FromLong(ier); /*  retour de la fonction oper sous la forme d un entier */
        }
        finally{
                /* On depile l appel */
                commande = depile();

                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */
                TraitementFinAster( exception_status ) ;

                _FIN(aster_opsexe) ;
                return NULL;
        }
}


extern void STDCALL(IMPERS,impers)();
#define CALL_IMPERS() F_FUNC(IMPERS,impers)()

static PyObject * aster_impers(self,args)
PyObject *self; /* Not used */
{
        _DEBUT(aster_impers) ;
        CALL_IMPERS ();
      /*   impers_() ;*/
        Py_INCREF( Py_None ) ;
        _FIN(aster_impers)
        return Py_None;
}

void DEFPPS(REPOUT,repout,INTEGER *,INTEGER *,char *,int);
#define CALL_REPOUT(a,b,c) CALLPPS(REPOUT,repout,a,b,c)


static PyObject * aster_repout(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        INTEGER maj=1 ;
        INTEGER lnom=0;
        char nom[129];

        _DEBUT(aster_repout) ;
        if (!PyArg_ParseTuple(args, "")) return NULL;
                                                       ISCRUTE(maj);
        BLANK(nom,128);
        nom[128]='\0';
                                                       SSCRUTE(nom);
        CALL_REPOUT (&maj,&lnom,nom);
                                                       ISCRUTE(lnom);
                                                       FSSCRUTE(nom,128);
        temp= PyString_FromStringAndSize(nom,FindLength(nom,lnom));
                                                       OBSCRUTE(temp);
        _FIN(aster_repout)
        return temp;
}

void DEFPPS(REPDEX,repdex,INTEGER *,INTEGER *,char *,int);
#define CALL_REPDEX(a,b,c) CALLPPS(REPDEX,repdex,a,b,c)


static PyObject * aster_repdex(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        INTEGER maj=1 ;
        INTEGER lnom=0;
        char nom[129];

        _DEBUT(aster_repdex) ;
        if (!PyArg_ParseTuple(args, "")) return NULL;
                                                       ISCRUTE(maj);
        BLANK(nom,128);
        nom[128]='\0';
                                                       SSCRUTE(nom);
        CALL_REPDEX (&maj,&lnom,nom);
                                                       ISCRUTE(lnom);
                                                       FSSCRUTE(nom,128);
        temp= PyString_FromStringAndSize(nom,FindLength(nom,lnom));
                                                       OBSCRUTE(temp);
        _FIN(aster_repdex)
        return temp;
}


void DEFSPSP(MDNOMA,mdnoma,char *,int,INTEGER *,char *,int,INTEGER *);
#define CALL_MDNOMA(a,b,c,d) CALLSPSP(MDNOMA,mdnoma,a,b,c,d)


static PyObject * aster_mdnoma(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        INTEGER lnomam=0;
        INTEGER codret=0;
        char *nomast;
        char nomamd[33];

        _DEBUT(aster_mdnoma) ;
        if (!PyArg_ParseTuple(args, "s",&nomast)) return NULL;
                                                       SSCRUTE(nomast);
        BLANK(nomamd,32);
        nomamd[32]='\0';
                                                       SSCRUTE(nomamd);
        CALL_MDNOMA (nomamd,&lnomam,nomast,&codret);
                                                       ISCRUTE(lnomam);
                                                       FSSCRUTE(nomamd,32);

        temp= PyString_FromStringAndSize(nomamd,FindLength(nomamd,lnomam));
                                                       OBSCRUTE(temp);
        _FIN(aster_mdnoma)
        return temp;
}

void DEFSPPSSSP(MDNOCH,mdnoch,char *,int,INTEGER *,INTEGER *,char *,int,char *,int,char *,int,INTEGER *);
#define CALL_MDNOCH(a,b,c,d,e,f,g) CALLSPPSSSP(MDNOCH,mdnoch,a,b,c,d,e,f,g)

static PyObject * aster_mdnoch(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        INTEGER lnochm=0;
        INTEGER lresu=1 ; /* FORTRAN_TRUE */
        INTEGER codret=0;
        char *noresu;
        char *nomsym;
        char nopase[1];
        char nochmd[33];

        _DEBUT(aster_mdnoch) ;
        if (!PyArg_ParseTuple(args, "ss",&noresu,&nomsym)) return NULL;
                                                       SSCRUTE(noresu);
                                                       SSCRUTE(nomsym);
        BLANK(nochmd,32);
        nochmd[32]='\0';
        nopase[0]='\0';
                                                       SSCRUTE(nochmd);
        CALL_MDNOCH (nochmd,&lnochm,&lresu,noresu,nomsym,nopase,&codret);
                                                       ISCRUTE(lnochm);
                                                       FSSCRUTE(nochmd,32);
        temp= PyString_FromStringAndSize(nochmd,FindLength(nochmd,lnochm));
                                                       OBSCRUTE(temp);
        _FIN(aster_mdnoch)
        return temp;
}

#define CALL_MYEVAL(a,b,c,d,e)  CALLSPPPP(MYEVAL,myeval,a,b,c,d,e)
extern void DEFSPPPP(MYEVAL,myeval,char* ,int , INTEGER* , INTEGER* , double *,INTEGER* ) ;

static PyObject* aster_myeval(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        INTEGER ier=0 ;
        INTEGER iclass=0 ;
        INTEGER ival=0 ;
        double rval[2] ; /* contient un nombre reel ou un nombre complexe */
        char *cmdusr="                                                                          ";

        _DEBUT(aster_myeval) ;
        if (!PyArg_ParseTuple(args, "Os",&temp,&cmdusr)) return NULL;

        /* On empile le nouvel appel */
        commande=empile(temp);

        if(PyErr_Occurred()){
            fprintf(stderr,"Warning: une exception n'a pas ete traitée\n");
            PyErr_Print();
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait\n\
                            etre traitée avant\n");
            PyErr_Clear();
        }

        fflush(stderr) ;
        fflush(stdout) ;

        try(1){
                /*  appel du sous programme myeval */
                CALL_MYEVAL (cmdusr,&iclass,&ival,rval,&ier);

                /* On depile l appel */
                commande = depile();
                if(ier != 0){
                                                                                 ISCRUTE(ier) ;
                        PyErr_SetString(PyExc_ValueError, "erreur evaluation ASTER");
                        return NULL;
                }
                if(iclass == 1 || iclass == 2){
                        return PyFloat_FromDouble(rval[0]);
                }
                else if(iclass == 5){

                        PyObject *complexe  = (PyObject*)0 ;
                        PyObject *objet     = (PyObject*)0 ;
                        const char *repr    = "RI" ; /* type de representation */

                                                                                 DSCRUTE(rval[0]) ; DSCRUTE(rval[1]) ;

                        /* ici, rval contient la partie reelle et la partie imaginaire  du  */
                        /* complexe a stocker dans un tuple                                 */
                        /* (repre,reelle,imaginaire)), destine a etre traite par la         */
                        /* methode Traite_DEFI_VALEUR de la classe EXECUTION (commandes.py) */


                        /* Creation d'une liste et stockage du type et des deux reels */

                        complexe = PyTuple_New( 3 ) ;

                        PyTuple_SetItem( complexe, 0, (objet=PyString_FromString(repr)) ) ;
                                                                                 REFSCRUTE(objet) ;
                                                                                 ASSERT(objet->ob_refcnt==1) ;
                        PyTuple_SetItem( complexe, 1, PyFloat_FromDouble(rval[0]) ) ;
                        PyTuple_SetItem( complexe, 2, PyFloat_FromDouble(rval[1]) ) ;

                        return complexe ;
                }
                else if(iclass == 6){
                        return PyInt_FromLong(ival);
                }
                else{
                                                                                 ISCRUTE(iclass) ;
                        PyErr_SetString(PyExc_ValueError, "erreur evaluation ASTER");
                        return NULL;
                }
        }
        finally{
                /* On depile l appel */
                commande = depile();

                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */
                TraitementFinAster( exception_status ) ;

                _FIN(aster_myeval) ;
                return NULL;
        }
}



void TraitementFinAster( _IN int val )
{
        _DEBUT("TraitementFinAster") ;
                             ISCRUTE(val) ;
        switch( val ){
        case CodeFinAster :
                PyErr_SetString(PyExc_EOFError, "exit ASTER");
                break ;
        case CodeAbortAster :
                PyErr_SetString(FatalError, exception_reason);
                break ;
        case CodeErrorAster :
                PyErr_SetString(AsterError, exception_reason);
                break ;

        /* exceptions particularisées */
        case CodeNonConvergenceAster :
                PyErr_SetString(NonConvergenceError, exception_reason);
                break ;
        case CodeEchecComportementAster :
                PyErr_SetString(EchecComportementError, exception_reason);
                break ;
        case CodeBandeFrequenceVideAster :
                PyErr_SetString(BandeFrequenceVideError, exception_reason);
                break ;
        case CodeMatriceSinguliereAster :
                PyErr_SetString(MatriceSinguliereError, exception_reason);
                break ;
        case CodeTraitementContactAster :
                PyErr_SetString(TraitementContactError, exception_reason);
                break ;
        case CodeMatriceContactSinguliereAster :
                PyErr_SetString(MatriceContactSinguliereError, exception_reason);
                break ;
        case CodeArretCPUAster :
                PyErr_SetString(ArretCPUError, exception_reason);
                break ;

        default :
                MESSAGE("code erreur INCONNU !!!!") ;
                ISCRUTE(val) ;
                INTERRUPTION(1) ;
                break ;
        }
        _FIN("TraitementFinAster") ;
        return ;
}

#define CALL_GETLTX(a,b,c,d,e,f,g) CALLSSPPPPP(GETLTX,getltx,a,b,c,d,e,f,g)
#define CALL_GETVTX(a,b,c,d,e,f,g) CALLSSPPPSP(GETVTX,getvtx,a,b,c,d,e,f,g)

int RecupNomCas(void)
{
        /* recuperation du nom du cas */

                INTEGER un          = 1 ;
                INTEGER *iocc       = (INTEGER*)&un ;
                INTEGER *iarg       = (INTEGER*)&un ;
                INTEGER *mxval      = (INTEGER*)&un ;
                INTEGER nbval       = 1 ;
                int ltx       = 8 ;
                INTEGER longueur[1] ;
                                                                ASSERT(commande!=(PyObject*)0);
                CALL_GETLTX ( "CODE","NOM",iocc,iarg,mxval, longueur ,&nbval) ;
                if(nbval == 0){
                  /* Le mot cle NOM n'a pas ete fourni on donne un nom
                   * par defaut au nom du cas */
                  NomCas = strdup("??????");
                }
                else if(nbval > 0){
                                                                ISCRUTE(longueur[0]) ;
                                                                ASSERT(longueur[0]>0);
                  NomCas = (char*)(malloc((longueur[0]+1)*sizeof(char))) ;
                  BLANK(NomCas,longueur[0]); /* initialisation a blanc */
                  NomCas[longueur[0]]='\0';
                                                                ASSERT(NomCas!=(char*)0);
                  ltx = longueur[0];
                  CALL_GETVTX ( "CODE","NOM",iocc,iarg,mxval, NomCas ,&nbval) ;
                }
                else{
                  /* Erreur  */
                  PyErr_SetString(PyExc_KeyError, "Erreur a la recuperation du nom du cas");
                  return -1;
                }
                                                                SSCRUTE(NomCas) ;
                return 0;
}

void DEFPPPP(POURSU,poursu,INTEGER* , INTEGER* , INTEGER* ,INTEGER*) ;
void DEFS(GCCPTS,gccpts,char *, int );

#define CALL_POURSU(a,b,c,d) CALLPPPP(POURSU,poursu,a,b,c,d)
#define CALL_GCCPTS(a,la) F_FUNC(GCCPTS,gccpts)(a,la)

static PyObject * aster_poursu(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        /*
        FONCTIONALITE : poursuite
        est appele par cata.POURSUITE (cf. ops.py)
        */
        PyObject *temp = (PyObject*)0 ;
        PyObject *concepts = (PyObject*)0 ;
        INTEGER ipass=0;
        INTEGER lot=1 ; /* FORTRAN_TRUE */
        INTEGER ier=0 ;
        INTEGER lonuti=0 ;
        static int nbPassages=0 ;

        _DEBUT(aster_poursu) ;
                                                                SSCRUTE(aster_ident()) ;
                                                                ASSERT((nbPassages==1)||(commande==(PyObject*)0));
        nbPassages++ ;
        if (!PyArg_ParseTuple(args, "Ol",&temp,&ipass)) return NULL;

        /* On empile le nouvel appel */
        commande=empile(temp);

        if(PyErr_Occurred()){
            fprintf(stderr,"Warning: une exception n'a pas ete traitée\n");
            PyErr_Print();
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait\n\
                            etre traitée avant\n");
            PyErr_Clear();
        }

        fflush(stderr) ;
        fflush(stdout) ;

        try(1){
                /*  appel de la commande debut (effectue dans POURSU) */
                /*  La routine fortran POURSU traite aussi le cas     */
                /*  de la poursuite de calcul (en retour lonuti       */
                /*  contient le nombre de concepts crees dans le      */
                /*  calcul precedent)                                 */

                CALL_POURSU (&lot,&ipass,&ier,&lonuti);


                /* recuperation de la liste des concepts dans une     */
                /* string python                                      */

                concepts=PyString_FromStringAndSize(NULL,lonuti*80);
                CALL_GCCPTS (PyString_AsString(concepts),80);
        }
        finally{
                /* On depile l appel */
                commande = depile();

                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */
                TraitementFinAster( exception_status ) ;

                _FIN(aster_poursu) ;
                return NULL;
        }

        /* On recupere le nom du cas */
        if(RecupNomCas() == -1){
          /* Erreur a la recuperation */

          /* On depile l appel */
          commande = depile();

          _FIN(aster_poursu) ;
          return NULL;
        }
        else{
          /* On depile l appel */
          commande = depile();

          /*  retour de la fonction poursu sous la forme
           *  d'un tuple de trois entiers et un objet */
          _FIN(aster_poursu) ;
          return Py_BuildValue("(iiiO)",lot ,ier,lonuti,concepts );
        }
}

#define CALL_DEBUT(a,b,c)  F_FUNC(DEBUT,debut)(a,b,c)
extern void STDCALL(DEBUT,debut)(INTEGER* , INTEGER* , INTEGER* );

static PyObject * aster_debut(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        PyObject *temp = (PyObject*)0 ;
        INTEGER ipass=0;
        INTEGER lot=1 ; /* FORTRAN_TRUE */
        INTEGER ier=0 ;
        static int nbPassages=0 ;

        _DEBUT(aster_debut) ;
                                                                SSCRUTE(aster_ident()) ;
                                                                ASSERT((nbPassages==1)||(commande==(PyObject*)0));
        nbPassages++ ;
        if (!PyArg_ParseTuple(args, "Ol",&temp,&ipass)) return NULL;

        /* On empile le nouvel appel */
        commande=empile(temp);

        if(PyErr_Occurred()){
            fprintf(stderr,"Warning: une exception n'a pas ete traitée\n");
            PyErr_Print();
            fprintf(stderr,"Warning: on l'annule pour continuer mais elle aurait\n\
                            etre traitée avant\n");
            PyErr_Clear();
        }

        fflush(stderr) ;
        fflush(stdout) ;

        try(1){
                /*  appel de la commande debut */
                CALL_DEBUT (&lot,&ipass,&ier);
        }
        finally{
                /* On depile l appel */
                commande = depile();

                /* une exception a ete levee, elle est destinee a etre traitee dans JDC.py */
                _FIN(aster_debut) ;
                TraitementFinAster( exception_status ) ;
                return NULL;
        }

        /* On recupere le nom du cas */
        if(RecupNomCas() == -1){
          /* Erreur a la recuperation */
          /* On depile l appel */
          commande = depile();
          _FIN(aster_debut) ;
          return NULL;
        }
        else{
          /* On depile l appel */
          commande = depile();
          /*  retour de la fonction debut sous la forme d un tuple de deux entiers */
          _FIN(aster_debut) ;
          return Py_BuildValue("(ii)",lot ,ier );
        }
}

#define CALL_IBMAIN(a,b,c)  F_FUNC(IBMAIN,ibmain)(a,b,c)
extern void STDCALL(IBMAIN,ibmain)(INTEGER* , INTEGER* , INTEGER* );

static PyObject *
aster_init(self, args)
PyObject *self; /* Not used */
PyObject *args;
{
        INTEGER lot=1 ; /* FORTRAN_TRUE */
        INTEGER ier=0 ;
        INTEGER dbg=0 ; /* FORTRAN_FALSE */

        _DEBUT(aster_init)
        if (!PyArg_ParseTuple(args, "l",&dbg)) return NULL;

        fflush(stderr) ;
        fflush(stdout) ;

        CALL_IBMAIN (&lot,&ier,&dbg);

        _FIN(aster_init)
        return PyInt_FromLong(ier);
}





static PyObject *aster_argv( _UNUSED  PyObject *self, _IN PyObject *args )
{

        /*
        A partir des arguments passes au script python, construction du
        tableau de chaines de caracteres C "char** argv" et initialisation
        de "argc" : la taille de ce tableau.
        Puis appel de Code_Aster en passant ces informations en argument.
        */


        int        k       = 0 ;
        long       argc    = 0 ;
        char      *chaine  = NULL ;
        PyObject  *liste   = NULL ;
        PyObject  *string  = NULL ;
        char     **argv    = NULL ;

        void asterm( long , char** ) ;

        _DEBUT("aster_argv") ;


        /*
           la fonction aster_argv recoit un tuple d'arguments (ici de taille 1°
           dans lequel est stockee la liste, qui est extraite par l'appel a
           PyArg_ParseTuple.
        */

                                                                ISCRUTE((INTEGER)PyTuple_Size(args)) ;
        if (!PyArg_ParseTuple(args, "O" , &liste )) return NULL;


        /*  Allocation dynamique de argv : on ajoute un argument NULL */

        argc=PyList_GET_SIZE(liste) ;
                                                                ISCRUTE((INTEGER)argc) ;

        argv = (char**)malloc(1+argc*sizeof(char*)) ;
        argv[argc]=(char*)0 ;


        /* conversion de chaque element de la liste en une chaine */

        for ( k=0 ; (long)k<argc ; k++ ){
                string=PyList_GetItem(liste,k) ;
                                                                ASSERT(string!=NULL);
                                                                ASSERT(PyString_Check(string));
                argv[k]=PyString_AsString( string ) ;
                                                                ASSERT(argv[k]!=NULL);
        }
#ifdef _DEBOG_
        for ( k=0 ; (long)k<argc ; k++ ){
                ISCRUTE(k);SSCRUTE(argv[k]) ;
        }
#endif


        /* Passage des arguments a Code_Aster */

        asterm(argc,argv) ;


                                                                ASSERT(argv) ;
        free(argv);
        argv=(char**)0 ;

        Py_INCREF( Py_None ) ;
        _FIN("aster_argv") ;
        return Py_None;
}


/* List of functions defined in the module */

static PyMethodDef aster_methods[] = {
                {"init",        aster_init ,              METH_VARARGS},
                {"debut",       aster_debut ,             METH_VARARGS},
                {"poursu",      aster_poursu ,            METH_VARARGS},
                {"oper" ,       aster_oper ,              METH_VARARGS},
                {"opsexe" ,     aster_opsexe ,            METH_VARARGS},
                {"repout" ,     aster_repout ,            METH_VARARGS},
                {"impers" ,     aster_impers ,            METH_VARARGS},
                {"repdex" ,     aster_repdex ,            METH_VARARGS},
                {"mdnoma" ,     aster_mdnoma ,            METH_VARARGS},
                {"mdnoch" ,     aster_mdnoch ,            METH_VARARGS},
                {"myeval" ,     aster_myeval ,            METH_VARARGS},
                {"argv" ,       aster_argv ,              METH_VARARGS},
                {"prepcompcham",aster_prepcompcham,       METH_VARARGS},
                {"getvectjev" , aster_getvectjev ,        METH_VARARGS, getvectjev_doc},
                {"getcolljev" , aster_getcolljev ,        METH_VARARGS, getcolljev_doc},
                {"GetResu",     aster_GetResu,            METH_VARARGS},
                {"GetMaillage", aster_GetMaillage,        METH_VARARGS},
                {NULL,                NULL}/* sentinel */
};


#define CALL_VERSIO(a,b,c,d,e) CALLPPPSP(VERSIO,versio,a,b,c,d,e)

void initvers(PyObject *dict)
{
    PyObject *v;
    INTEGER vers,util,nivo;
    INTEGER exploi;
    char date[20];
    char rev[8];

    CALL_VERSIO(&vers,&util,&nivo,date,&exploi);
    sprintf(rev,"%d.%d.%d",vers,util,nivo);
    PyDict_SetItemString(dict, "__version__", v = PyString_FromString(rev));
    Py_XDECREF(v);
}


/* Initialization function for the module (*must* be called initaster) */
static char aster_module_documentation[] =
"C implementation of the Python aster module\n"
"\n"
;

DL_EXPORT(void)
initaster()
{
        PyObject *m = (PyObject*)0 ;
        PyObject *d = (PyObject*)0 ;

        _DEBUT(initaster) ;

        /* Create the module and add the functions */
        m = Py_InitModule3("aster", aster_methods,aster_module_documentation);

        /* Add some symbolic constants to the module */
        d = PyModule_GetDict(m);

        initvers(d);
        initExceptions(d);

        /* Initialisation de la pile d appel des commandes */
        pile_commandes = PyList_New(0);

        _FIN(initaster) ;
}





void AfficheChaineFortran( _IN char *chaine , _IN int longueur )
{
        /* Traitement des chaines fortran : pour le deboguage uniquement*/

        static FILE *strm ; /* le stream de sortie pointe sur la stderr */
        strm=stderr;

        if ( longueur ){
                int k=0 ;
                fprintf( strm , "'" ) ;
                for ( k=0 ; k<((longueur<=512)?longueur:512) ; k++ ){
                        fprintf( strm , "%c" , chaine[k] ) ;
                }
                fprintf( strm , "'\n" ) ;
                fflush(strm) ;
        }
        return ;
}

int EstPret( _IN char *chaine , _IN int longueur )
{
        /*
        Fonction  : EstPret
        Intention
                dit si "chaine" destinee a etre exploitee par un module fortran,
                est une commande ASTER i.e. si elle est composee uniquement de lettres,
                de chiffres, de _ et de caracteres blancs et si elle contient un
                caractere non blanc.
        */
        int pret     = 0 ;
        int k        = 0 ;
        int taille   = 0 ;

        taille = ( longueur < 1024 ) ? FindLength( chaine , longueur ) : 1024 ;
                                                                                        ASSERT(taille <= longueur ) ;

        if ( taille >= 0 ){
                pret = 1 ;
                if( isalpha(chaine[0]) ){
                        for( k=0 ; pret==1 && k<longueur ; k++ ){
                                pret = ( EstValide(chaine[k] ) ) ? 1 : 0 ;
                                if ( pret != 1 ){
                                        fprintf( stderr , "CARACTERE %d INVALIDE '%c' %d\n" , k , chaine[k] , (int)chaine[k]) ;
                                }
                        }
                }
                else{
                        fprintf( stderr , "PREMIER CARACTERE INVALIDE '%c' %d\n" , chaine[0] , (int)chaine[0]) ;
                }
                if ( pret != 1 ){
                                                                                        FSSCRUTE(chaine,longueur) ;
                }
        }
        return pret ;
}


void AjoutChaineA( _INOUT char **base , _IN char *supplement )
{


        /*
        Procedure  : AjoutChaineA
        Intention
                la chaine de caractere "base" est agrandie de la chaine
                "supplement". La zone memoire occupee par base est reallouee
                et la valeur du pointeur *base est donc modifiee.
                Dans cette operation tous les caracteres sont significatifs
                sauf le caractere NUL ('\0').

        PRENEZ GARDE ! : base doit etre une zone allouee DYNAMIQUEMENT

        */

        char *resultat = (char*)0 ;
        int ajout      = 0 ;
        int taille     = 0 ;
        int total      = 0 ;

        taille = ( *base ) ? strlen( *base ) : 0 ;

        ajout = ( supplement ) ? strlen( supplement ) : 0 ;

        if ( ajout > 0 ){
                if ( taille > 0 ){
                        total = taille + ajout ;
                        total += 1 ; /* caractere de fin de chaine */
                        resultat = (char*)(malloc(total)) ;
                        ASSERT(resultat!=NULL) ;
                        strcpy(resultat,*base) ;
                        strcat(resultat,supplement) ;
                }
                else{
                        total = ajout ;
                        total += 1 ; /* caractere de fin de chaine */
                        resultat = (char*)(malloc(total)) ;
                        ASSERT(resultat!=NULL) ;
                        strcpy(resultat,supplement) ;
                }
        }
        else{
                if ( taille > 0 ){
                        total = taille  ;
                        total += 1 ; /* caractere de fin de chaine */
                        resultat = (char*)(malloc(total)) ;
                        strcpy(resultat,*base) ;
                }
        }
        if( *base ){
                ASSERT(strlen(*base)==taille) /* verification INVARIANT !! */
                free(*base) ;
                *base=(char*)0 ;
        }
        *base = resultat ;
}







const char *aster_ident()
{
        const char *identCVS = "$Id: astermodule.c,v 1.59.12.1.2.1 2001/05/16 16:14:54 iliade Exp $ $Name:  $" ;
        return identCVS ;
}


void DEFP(GETCMC,getcmc,INTEGER *icmc)
{
        /*
          Procedure GETCMC : emule la procedure equivalente ASTER

          Entrees : aucune
          Sorties :
            icmc   : numero de la commande
          Fonction :
            Retourne le numero de la commande courante

        */
        PyObject * res = (PyObject*)0 ;
        _DEBUT("getcmc_") ;
        res = PyObject_GetAttrString(commande,"icmd");
        /*
                    Si le retour est NULL : une exception a ete levee dans le code Python appele
                    Cette exception est a transferer normalement a l appelant mais FORTRAN ???
                    On produit donc un abort en ecrivant des messages sur la stdout
        */
        if (res == NULL)
                MYABORT("erreur a l appel de getcmc dans la partie Python");

        *icmc = PyInt_AsLong(res);
                                                                                   ISCRUTE(*icmc) ;
        Py_DECREF(res);
        _FIN("getcmc_") ;
}


#define CALL_GETRES(a,la,b,lb,c,lc) FCALLSSS(GETRES,getres,a,la,b,lb,c,lc)
#define CALL_GCUCON(a,b,lb,c,lc,d) FCALLPSSP(GCUCON,gcucon,a,b,lb,c,lc,d)
#define CALL_GETCMC(a) CALLP(GETCMC,getcmc,a)


void DEFSSSSP(GETCMD,getcmd,_OUT char *nomres,int lres,_OUT char *concep,int lconc, _OUT char *nomcmd,
                       int lcmd,_OUT char *statu,int lstat, _OUT INTEGER *inum)
{
        /*
          Procedure GETCMD : emule la procedure equivalente ASTER
          Retourne des infos sur la commande courante

          Entrees : RAS

          Sorties :
            le nom du concept produit                 : nomres (string)
            le type du concept produit                : concep (string)
            le nom de la commande en cours            : nomcmd (string)
            le statut du concept produit              : statu (string)
                                   'NOUVEAU' : concept produit nouveau
                                   'MODIFIE' : concept produit modifie
                                   'ERRONE'  : concept produit existe mais pas du bon type
            le numero d'ordre de la commande courante : inum
          Fonction:
            Retourne le numero de la commande courante, le nom de la commande
                le nom du concept produit, son type, le statut du concept

          Commentaires:
            Dans l'ancienne version, "statu" qualifiait la donnee dans la memoire JEVEUX !
        */

        int k=0 ;
        INTEGER ier ;

        _DEBUT("getcmd_") ;

        CALL_GETRES(nomres,lres,concep ,  lconc , nomcmd , lcmd ) ;
        CALL_GETCMC ( inum ) ;
        CALL_GCUCON ( inum , nomres , lres , concep , lconc , &ier ) ;
                                                         ISCRUTE(ier) ;
        switch( ier )
        {
        case 0 :
                {
                        STRING_FCPY(statu,lstat,"NOUVEAU",7);
                }
                break ;
        default :
                if ( ier>0 ){
                        STRING_FCPY(statu,lstat,"MODIFIE",7);

                }
                else{
                        STRING_FCPY(statu,lstat,"ERRONE",6);
                }
                break ;
        }
                                                         FSSCRUTE(nomres,lres) ;
                                                         FSSCRUTE(concep,lconc) ;
                                                         FSSCRUTE(nomcmd,lcmd) ;
                                                         FSSCRUTE(statu,lstat) ;
                                                         ISCRUTE(*inum) ;
        _FIN("getcmd_") ;
        return ;
}
