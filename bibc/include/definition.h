/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2016  EDF R&D              WWW.CODE-ASTER.ORG */
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
/* disable the message "line too long" because the code is generated  */
/* aslint: disable=C3001 */

#ifndef DEFINITION_H
#define DEFINITION_H

#include "aster_depend.h"

/* Pour définir les appels et signatures de fonctions appelables en Fortran
 * On utilise l'operateur de concatenation ## du préprocesseur C (cpp) pour ajouter l'underscore
 * au nom en majuscule ou minuscule de la fonction à définir ou à appeler.
 * Pour les anciens compilateurs non ANSI, utiliser un commentaire vide à la place.
 * Pour appeler une subroutine Fortran de nom SUB avec un argument string et 2 arguments autres, faire:
 * #define CALL_SUB(a,b,c) CALLSPP(SUB,sub,a,b,c)
 * puis : CALL_SUB(a,b,c)
 * Pour définir une fonction C de nom SUB avec un argument string et 2 arguments autres,
 * appelable depuis le fortran, faire:
 * void DEFSPP(SUB, sub, char * nomobj, STRING_SIZE lnom, ASTERDOUBLE *d, ASTERINTEGER *i)
 * {
 * }
 * ici, lnom est l'entier qui indique la longueur de la chaine Fortran nomobj
 * Les macros définies ici ne servent qu'à former le nom de la fonction et à
 * mettre les arguments dans le bon ordre. On utilise l'ordre _WIN32 comme
 * base (pointeur char suivi de la longueur) et on reordonne pour les autres compilateurs.
 * STRING_SIZE est le type retourné par strlen.
 */

/* Operateur de concatenation */
#define  _(A,B)   A##B

/* Appels : minuscules/majuscules, avec/sans underscore */
#if defined _POSIX
#   define F_FUNC(UN,LN)           _(LN,_)
#   if defined _NO_UNDERSCORE
#       define F_FUNC(UN,LN)           LN
#   endif
#else
#   define F_FUNC(UN,LN)           UN
#endif

/* http://gcc.gnu.org/onlinedocs/cpp/Stringification.html */
#define xstr(s)                 str(s)
#define str(s)                  #s
#define S_FUNC(UN,LN)           xstr(F_FUNC(UN,LN))

/* STDCALL for "old" windows version */
#ifdef _USE_STDCALL
#   define STDCALL(UN,LN)   __stdcall F_FUNC(UN,LN)
#else
#   define STDCALL(UN,LN)   F_FUNC(UN,LN)
#endif

/* Appels et signatures avec strlen en fin de liste */
#ifdef _STRLEN_AT_END

#define DEFS(UN,LN,a,la)               STDCALL(UN,LN)(a,la)
#define CALLS(UN,LN,a)                 F_FUNC(UN,LN)(a,strlen(a))
#define DEFPS(UN,LN,a,b,lb)               STDCALL(UN,LN)(a,b,lb)
#define CALLPS(UN,LN,a,b)                 F_FUNC(UN,LN)(a,b,strlen(b))
#define DEFSP(UN,LN,a,la,b)               STDCALL(UN,LN)(a,b,la)
#define CALLSP(UN,LN,a,b)                 F_FUNC(UN,LN)(a,b,strlen(a))
#define DEFSS(UN,LN,a,la,b,lb)               STDCALL(UN,LN)(a,b,la,lb)
#define CALLSS(UN,LN,a,b)                    F_FUNC(UN,LN)(a,b,strlen(a),strlen(b))
#define DEFPSS(UN,LN,a,b,lb,c,lc)               STDCALL(UN,LN)(a,b,c,lb,lc)
#define CALLPSS(UN,LN,a,b,c)                    F_FUNC(UN,LN)(a,b,c,strlen(b),strlen(c))
#define DEFSPP(UN,LN,a,la,b,c)               STDCALL(UN,LN)(a,b,c,la)
#define CALLSPP(UN,LN,a,b,c)                 F_FUNC(UN,LN)(a,b,c,strlen(a))
#define DEFSPS(UN,LN,a,la,b,c,lc)               STDCALL(UN,LN)(a,b,c,la,lc)
#define CALLSPS(UN,LN,a,b,c)                    F_FUNC(UN,LN)(a,b,c,strlen(a),strlen(c))
#define DEFSSP(UN,LN,a,la,b,lb,c)               STDCALL(UN,LN)(a,b,c,la,lb)
#define CALLSSP(UN,LN,a,b,c)                    F_FUNC(UN,LN)(a,b,c,strlen(a),strlen(b))
#define DEFSSS(UN,LN,a,la,b,lb,c,lc)               STDCALL(UN,LN)(a,b,c,la,lb,lc)
#define CALLSSS(UN,LN,a,b,c)                       F_FUNC(UN,LN)(a,b,c,strlen(a),strlen(b),strlen(c))
#define DEFPPSP(UN,LN,a,b,c,lc,d)               STDCALL(UN,LN)(a,b,c,d,lc)
#define CALLPPSP(UN,LN,a,b,c,d)                 F_FUNC(UN,LN)(a,b,c,d,strlen(c))
#define DEFPSPP(UN,LN,a,b,lb,c,d)               STDCALL(UN,LN)(a,b,c,d,lb)
#define CALLPSPP(UN,LN,a,b,c,d)                 F_FUNC(UN,LN)(a,b,c,d,strlen(b))
#define DEFPSPS(UN,LN,a,b,lb,c,d,ld)               STDCALL(UN,LN)(a,b,c,d,lb,ld)
#define CALLPSPS(UN,LN,a,b,c,d)                    F_FUNC(UN,LN)(a,b,c,d,strlen(b),strlen(d))
#define DEFSPSP(UN,LN,a,la,b,c,lc,d)               STDCALL(UN,LN)(a,b,c,d,la,lc)
#define CALLSPSP(UN,LN,a,b,c,d)                    F_FUNC(UN,LN)(a,b,c,d,strlen(a),strlen(c))
#define DEFSSPP(UN,LN,a,la,b,lb,c,d)               STDCALL(UN,LN)(a,b,c,d,la,lb)
#define CALLSSPP(UN,LN,a,b,c,d)                    F_FUNC(UN,LN)(a,b,c,d,strlen(a),strlen(b))
#define DEFSSPS(UN,LN,a,la,b,lb,c,d,ld)               STDCALL(UN,LN)(a,b,c,d,la,lb,ld)
#define CALLSSPS(UN,LN,a,b,c,d)                       F_FUNC(UN,LN)(a,b,c,d,strlen(a),strlen(b),strlen(d))
#define DEFSSSP(UN,LN,a,la,b,lb,c,lc,d)               STDCALL(UN,LN)(a,b,c,d,la,lb,lc)
#define CALLSSSP(UN,LN,a,b,c,d)                       F_FUNC(UN,LN)(a,b,c,d,strlen(a),strlen(b),strlen(c))
#define DEFSSSS(UN,LN,a,la,b,lb,c,lc,d,ld)               STDCALL(UN,LN)(a,b,c,d,la,lb,lc,ld)
#define CALLSSSS(UN,LN,a,b,c,d)                          F_FUNC(UN,LN)(a,b,c,d,strlen(a),strlen(b),strlen(c),strlen(d))
#define DEFPPPSP(UN,LN,a,b,c,d,ld,e)               STDCALL(UN,LN)(a,b,c,d,e,ld)
#define CALLPPPSP(UN,LN,a,b,c,d,e)                 F_FUNC(UN,LN)(a,b,c,d,e,strlen(d))
#define DEFPSSSS(UN,LN,a,b,lb,c,lc,d,ld,e,le)               STDCALL(UN,LN)(a,b,c,d,e,lb,lc,ld,le)
#define CALLPSSSS(UN,LN,a,b,c,d,e)                          F_FUNC(UN,LN)(a,b,c,d,e,strlen(b),strlen(c),strlen(d),strlen(e))
#define DEFSPPPP(UN,LN,a,la,b,c,d,e)               STDCALL(UN,LN)(a,b,c,d,e,la)
#define CALLSPPPP(UN,LN,a,b,c,d,e)                 F_FUNC(UN,LN)(a,b,c,d,e,strlen(a))
#define DEFSSSPP(UN,LN,a,la,b,lb,c,lc,d,e)               STDCALL(UN,LN)(a,b,c,d,e,la,lb,lc)
#define CALLSSSPP(UN,LN,a,b,c,d,e)                       F_FUNC(UN,LN)(a,b,c,d,e,strlen(a),strlen(b),strlen(c))
#define DEFSSSSP(UN,LN,a,la,b,lb,c,lc,d,ld,e)               STDCALL(UN,LN)(a,b,c,d,e,la,lb,lc,ld)
#define CALLSSSSP(UN,LN,a,b,c,d,e)                          F_FUNC(UN,LN)(a,b,c,d,e,strlen(a),strlen(b),strlen(c),strlen(d))
#define DEFSPPSSP(UN,LN,a,la,b,c,d,ld,e,le,f)               STDCALL(UN,LN)(a,b,c,d,e,f,la,ld,le)
#define CALLSPPSSP(UN,LN,a,b,c,d,e,f)                       F_FUNC(UN,LN)(a,b,c,d,e,f,strlen(a),strlen(d),strlen(e))
#define DEFPSSSPSP(UN,LN,a,b,lb,c,lc,d,ld,e,f,lf,g)               STDCALL(UN,LN)(a,b,c,d,e,f,g,lb,lc,ld,lf)
#define CALLPSSSPSP(UN,LN,a,b,c,d,e,f,g)                          F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(b),strlen(c),strlen(d),strlen(f))
#define DEFSPPPPPP(UN,LN,a,la,b,c,d,e,f,g)               STDCALL(UN,LN)(a,b,c,d,e,f,g,la)
#define CALLSPPPPPP(UN,LN,a,b,c,d,e,f,g)                 F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(a))
#define DEFSPPSSSP(UN,LN,a,la,b,c,d,ld,e,le,f,lf,g)               STDCALL(UN,LN)(a,b,c,d,e,f,g,la,ld,le,lf)
#define CALLSPPSSSP(UN,LN,a,b,c,d,e,f,g)                          F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(a),strlen(d),strlen(e),strlen(f))
#define DEFSPSPPPS(UN,LN,a,la,b,c,lc,d,e,f,g,lg)               STDCALL(UN,LN)(a,b,c,d,e,f,g,la,lc,lg)
#define CALLSPSPPPS(UN,LN,a,b,c,d,e,f,g)                       F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(a),strlen(c),strlen(g))
#define DEFSPSPPSP(UN,LN,a,la,b,c,lc,d,e,f,lf,g)               STDCALL(UN,LN)(a,b,c,d,e,f,g,la,lc,lf)
#define CALLSPSPPSP(UN,LN,a,b,c,d,e,f,g)                       F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(a),strlen(c),strlen(f))
#define DEFSSPPPPP(UN,LN,a,la,b,lb,c,d,e,f,g)               STDCALL(UN,LN)(a,b,c,d,e,f,g,la,lb)
#define CALLSSPPPPP(UN,LN,a,b,c,d,e,f,g)                    F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(a),strlen(b))
#define DEFSSPPPSP(UN,LN,a,la,b,lb,c,d,e,f,lf,g)               STDCALL(UN,LN)(a,b,c,d,e,f,g,la,lb,lf)
#define CALLSSPPPSP(UN,LN,a,b,c,d,e,f,g)                       F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(a),strlen(b),strlen(f))
#define DEFSSSPSSP(UN,LN,a,la,b,lb,c,lc,d,e,le,f,lf,g)               STDCALL(UN,LN)(a,b,c,d,e,f,g,la,lb,lc,le,lf)
#define CALLSSSPSSP(UN,LN,a,b,c,d,e,f,g)                             F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(a),strlen(b),strlen(c),strlen(e),strlen(f))
#define DEFSSSSPPS(UN,LN,a,la,b,lb,c,lc,d,ld,e,f,g,lg)               STDCALL(UN,LN)(a,b,c,d,e,f,g,la,lb,lc,ld,lg)
#define CALLSSSSPPS(UN,LN,a,b,c,d,e,f,g)                             F_FUNC(UN,LN)(a,b,c,d,e,f,g,strlen(a),strlen(b),strlen(c),strlen(d),strlen(g))
#define DEFPPPPSPPP(UN,LN,a,b,c,d,e,le,f,g,h)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,le)
#define CALLPPPPSPPP(UN,LN,a,b,c,d,e,f,g,h)                 F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,strlen(e))
#define DEFPPPPSPSP(UN,LN,a,b,c,d,e,le,f,g,lg,h)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,le,lg)
#define CALLPPPPSPSP(UN,LN,a,b,c,d,e,f,g,h)                    F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,strlen(e),strlen(g))
#define DEFPSPSPPPP(UN,LN,a,b,lb,c,d,ld,e,f,g,h)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,lb,ld)
#define CALLPSPSPPPP(UN,LN,a,b,c,d,e,f,g,h)                    F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,strlen(b),strlen(d))
#define DEFSPPPPPPS(UN,LN,a,la,b,c,d,e,f,g,h,lh)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,la,lh)
#define CALLSPPPPPPS(UN,LN,a,b,c,d,e,f,g,h)                    F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,strlen(a),strlen(h))
#define DEFSSSPPPPP(UN,LN,a,la,b,lb,c,lc,d,e,f,g,h)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,la,lb,lc)
#define CALLSSSPPPPP(UN,LN,a,b,c,d,e,f,g,h)                       F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,strlen(a),strlen(b),strlen(c))
#define DEFSPPSPPPSP(UN,LN,a,la,b,c,d,ld,e,f,g,h,lh,i)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,i,la,ld,lh)
#define CALLSPPSPPPSP(UN,LN,a,b,c,d,e,f,g,h,i)                       F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,i,strlen(a),strlen(d),strlen(h))
#define DEFSSPSPPPPS(UN,LN,a,la,b,lb,c,d,ld,e,f,g,h,i,li)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,i,la,lb,ld,li)
#define CALLSSPSPPPPS(UN,LN,a,b,c,d,e,f,g,h,i)                          F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,i,strlen(a),strlen(b),strlen(d),strlen(i))
#define DEFPPPPPSPPPP(UN,LN,a,b,c,d,e,f,lf,g,h,i,j)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,i,j,lf)
#define CALLPPPPPSPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j)                 F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,i,j,strlen(f))
#define DEFPPPPPSPPSP(UN,LN,a,b,c,d,e,f,lf,g,h,i,li,j)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,i,j,lf,li)
#define CALLPPPPPSPPSP(UN,LN,a,b,c,d,e,f,g,h,i,j)                    F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,i,j,strlen(f),strlen(i))
#define DEFSPSPSPPPPS(UN,LN,a,la,b,c,lc,d,e,le,f,g,h,i,j,lj)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,i,j,la,lc,le,lj)
#define CALLSPSPSPPPPS(UN,LN,a,b,c,d,e,f,g,h,i,j)                          F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,i,j,strlen(a),strlen(c),strlen(e),strlen(j))
#define DEFSSPSPPSPPP(UN,LN,a,la,b,lb,c,d,ld,e,f,g,lg,h,i,j)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,i,j,la,lb,ld,lg)
#define CALLSSPSPPSPPP(UN,LN,a,b,c,d,e,f,g,h,i,j)                          F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,i,j,strlen(a),strlen(b),strlen(d),strlen(g))
#define DEFSSPPPPPPPPPPPP(UN,LN,a,la,b,lb,c,d,e,f,g,h,i,j,k,l,m,n)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,i,j,k,l,m,n,la,lb)
#define CALLSSPPPPPPPPPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j,k,l,m,n)                    F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,i,j,k,l,m,n,strlen(a),strlen(b))
#define DEFPPPPPPPPPPPPPPPPPPPSPPPPPPPPPPPPPPPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,lt,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L,lt)
#define CALLPPPPPPPPPPPPPPPPPPPSPPPPPPPPPPPPPPPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L)                 F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L,strlen(t))

/* Appels et signatures avec strlen juste après le pointeur de chaine */
#else

#define DEFS(UN,LN,a,la)               STDCALL(UN,LN)(a,la)
#define CALLS(UN,LN,a)                 F_FUNC(UN,LN)(a,strlen(a))
#define DEFPS(UN,LN,a,b,lb)               STDCALL(UN,LN)(a,b,lb)
#define CALLPS(UN,LN,a,b)                 F_FUNC(UN,LN)(a,b,strlen(b))
#define DEFSP(UN,LN,a,la,b)               STDCALL(UN,LN)(a,la,b)
#define CALLSP(UN,LN,a,b)                 F_FUNC(UN,LN)(a,strlen(a),b)
#define DEFSS(UN,LN,a,la,b,lb)               STDCALL(UN,LN)(a,la,b,lb)
#define CALLSS(UN,LN,a,b)                    F_FUNC(UN,LN)(a,strlen(a),b,strlen(b))
#define DEFPSS(UN,LN,a,b,lb,c,lc)               STDCALL(UN,LN)(a,b,lb,c,lc)
#define CALLPSS(UN,LN,a,b,c)                    F_FUNC(UN,LN)(a,b,strlen(b),c,strlen(c))
#define DEFSPP(UN,LN,a,la,b,c)               STDCALL(UN,LN)(a,la,b,c)
#define CALLSPP(UN,LN,a,b,c)                 F_FUNC(UN,LN)(a,strlen(a),b,c)
#define DEFSPS(UN,LN,a,la,b,c,lc)               STDCALL(UN,LN)(a,la,b,c,lc)
#define CALLSPS(UN,LN,a,b,c)                    F_FUNC(UN,LN)(a,strlen(a),b,c,strlen(c))
#define DEFSSP(UN,LN,a,la,b,lb,c)               STDCALL(UN,LN)(a,la,b,lb,c)
#define CALLSSP(UN,LN,a,b,c)                    F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c)
#define DEFSSS(UN,LN,a,la,b,lb,c,lc)               STDCALL(UN,LN)(a,la,b,lb,c,lc)
#define CALLSSS(UN,LN,a,b,c)                       F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,strlen(c))
#define DEFPPSP(UN,LN,a,b,c,lc,d)               STDCALL(UN,LN)(a,b,c,lc,d)
#define CALLPPSP(UN,LN,a,b,c,d)                 F_FUNC(UN,LN)(a,b,c,strlen(c),d)
#define DEFPSPP(UN,LN,a,b,lb,c,d)               STDCALL(UN,LN)(a,b,lb,c,d)
#define CALLPSPP(UN,LN,a,b,c,d)                 F_FUNC(UN,LN)(a,b,strlen(b),c,d)
#define DEFPSPS(UN,LN,a,b,lb,c,d,ld)               STDCALL(UN,LN)(a,b,lb,c,d,ld)
#define CALLPSPS(UN,LN,a,b,c,d)                    F_FUNC(UN,LN)(a,b,strlen(b),c,d,strlen(d))
#define DEFSPSP(UN,LN,a,la,b,c,lc,d)               STDCALL(UN,LN)(a,la,b,c,lc,d)
#define CALLSPSP(UN,LN,a,b,c,d)                    F_FUNC(UN,LN)(a,strlen(a),b,c,strlen(c),d)
#define DEFSSPP(UN,LN,a,la,b,lb,c,d)               STDCALL(UN,LN)(a,la,b,lb,c,d)
#define CALLSSPP(UN,LN,a,b,c,d)                    F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,d)
#define DEFSSPS(UN,LN,a,la,b,lb,c,d,ld)               STDCALL(UN,LN)(a,la,b,lb,c,d,ld)
#define CALLSSPS(UN,LN,a,b,c,d)                       F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,d,strlen(d))
#define DEFSSSP(UN,LN,a,la,b,lb,c,lc,d)               STDCALL(UN,LN)(a,la,b,lb,c,lc,d)
#define CALLSSSP(UN,LN,a,b,c,d)                       F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,strlen(c),d)
#define DEFSSSS(UN,LN,a,la,b,lb,c,lc,d,ld)               STDCALL(UN,LN)(a,la,b,lb,c,lc,d,ld)
#define CALLSSSS(UN,LN,a,b,c,d)                          F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,strlen(c),d,strlen(d))
#define DEFPPPSP(UN,LN,a,b,c,d,ld,e)               STDCALL(UN,LN)(a,b,c,d,ld,e)
#define CALLPPPSP(UN,LN,a,b,c,d,e)                 F_FUNC(UN,LN)(a,b,c,d,strlen(d),e)
#define DEFPSSSS(UN,LN,a,b,lb,c,lc,d,ld,e,le)               STDCALL(UN,LN)(a,b,lb,c,lc,d,ld,e,le)
#define CALLPSSSS(UN,LN,a,b,c,d,e)                          F_FUNC(UN,LN)(a,b,strlen(b),c,strlen(c),d,strlen(d),e,strlen(e))
#define DEFSPPPP(UN,LN,a,la,b,c,d,e)               STDCALL(UN,LN)(a,la,b,c,d,e)
#define CALLSPPPP(UN,LN,a,b,c,d,e)                 F_FUNC(UN,LN)(a,strlen(a),b,c,d,e)
#define DEFSSSPP(UN,LN,a,la,b,lb,c,lc,d,e)               STDCALL(UN,LN)(a,la,b,lb,c,lc,d,e)
#define CALLSSSPP(UN,LN,a,b,c,d,e)                       F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,strlen(c),d,e)
#define DEFSSSSP(UN,LN,a,la,b,lb,c,lc,d,ld,e)               STDCALL(UN,LN)(a,la,b,lb,c,lc,d,ld,e)
#define CALLSSSSP(UN,LN,a,b,c,d,e)                          F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,strlen(c),d,strlen(d),e)
#define DEFSPPSSP(UN,LN,a,la,b,c,d,ld,e,le,f)               STDCALL(UN,LN)(a,la,b,c,d,ld,e,le,f)
#define CALLSPPSSP(UN,LN,a,b,c,d,e,f)                       F_FUNC(UN,LN)(a,strlen(a),b,c,d,strlen(d),e,strlen(e),f)
#define DEFPSSSPSP(UN,LN,a,b,lb,c,lc,d,ld,e,f,lf,g)               STDCALL(UN,LN)(a,b,lb,c,lc,d,ld,e,f,lf,g)
#define CALLPSSSPSP(UN,LN,a,b,c,d,e,f,g)                          F_FUNC(UN,LN)(a,b,strlen(b),c,strlen(c),d,strlen(d),e,f,strlen(f),g)
#define DEFSPPPPPP(UN,LN,a,la,b,c,d,e,f,g)               STDCALL(UN,LN)(a,la,b,c,d,e,f,g)
#define CALLSPPPPPP(UN,LN,a,b,c,d,e,f,g)                 F_FUNC(UN,LN)(a,strlen(a),b,c,d,e,f,g)
#define DEFSPPSSSP(UN,LN,a,la,b,c,d,ld,e,le,f,lf,g)               STDCALL(UN,LN)(a,la,b,c,d,ld,e,le,f,lf,g)
#define CALLSPPSSSP(UN,LN,a,b,c,d,e,f,g)                          F_FUNC(UN,LN)(a,strlen(a),b,c,d,strlen(d),e,strlen(e),f,strlen(f),g)
#define DEFSPSPPPS(UN,LN,a,la,b,c,lc,d,e,f,g,lg)               STDCALL(UN,LN)(a,la,b,c,lc,d,e,f,g,lg)
#define CALLSPSPPPS(UN,LN,a,b,c,d,e,f,g)                       F_FUNC(UN,LN)(a,strlen(a),b,c,strlen(c),d,e,f,g,strlen(g))
#define DEFSPSPPSP(UN,LN,a,la,b,c,lc,d,e,f,lf,g)               STDCALL(UN,LN)(a,la,b,c,lc,d,e,f,lf,g)
#define CALLSPSPPSP(UN,LN,a,b,c,d,e,f,g)                       F_FUNC(UN,LN)(a,strlen(a),b,c,strlen(c),d,e,f,strlen(f),g)
#define DEFSSPPPPP(UN,LN,a,la,b,lb,c,d,e,f,g)               STDCALL(UN,LN)(a,la,b,lb,c,d,e,f,g)
#define CALLSSPPPPP(UN,LN,a,b,c,d,e,f,g)                    F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,d,e,f,g)
#define DEFSSPPPSP(UN,LN,a,la,b,lb,c,d,e,f,lf,g)               STDCALL(UN,LN)(a,la,b,lb,c,d,e,f,lf,g)
#define CALLSSPPPSP(UN,LN,a,b,c,d,e,f,g)                       F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,d,e,f,strlen(f),g)
#define DEFSSSPSSP(UN,LN,a,la,b,lb,c,lc,d,e,le,f,lf,g)               STDCALL(UN,LN)(a,la,b,lb,c,lc,d,e,le,f,lf,g)
#define CALLSSSPSSP(UN,LN,a,b,c,d,e,f,g)                             F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,strlen(c),d,e,strlen(e),f,strlen(f),g)
#define DEFSSSSPPS(UN,LN,a,la,b,lb,c,lc,d,ld,e,f,g,lg)               STDCALL(UN,LN)(a,la,b,lb,c,lc,d,ld,e,f,g,lg)
#define CALLSSSSPPS(UN,LN,a,b,c,d,e,f,g)                             F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,strlen(c),d,strlen(d),e,f,g,strlen(g))
#define DEFPPPPSPPP(UN,LN,a,b,c,d,e,le,f,g,h)               STDCALL(UN,LN)(a,b,c,d,e,le,f,g,h)
#define CALLPPPPSPPP(UN,LN,a,b,c,d,e,f,g,h)                 F_FUNC(UN,LN)(a,b,c,d,e,strlen(e),f,g,h)
#define DEFPPPPSPSP(UN,LN,a,b,c,d,e,le,f,g,lg,h)               STDCALL(UN,LN)(a,b,c,d,e,le,f,g,lg,h)
#define CALLPPPPSPSP(UN,LN,a,b,c,d,e,f,g,h)                    F_FUNC(UN,LN)(a,b,c,d,e,strlen(e),f,g,strlen(g),h)
#define DEFPSPSPPPP(UN,LN,a,b,lb,c,d,ld,e,f,g,h)               STDCALL(UN,LN)(a,b,lb,c,d,ld,e,f,g,h)
#define CALLPSPSPPPP(UN,LN,a,b,c,d,e,f,g,h)                    F_FUNC(UN,LN)(a,b,strlen(b),c,d,strlen(d),e,f,g,h)
#define DEFSPPPPPPS(UN,LN,a,la,b,c,d,e,f,g,h,lh)               STDCALL(UN,LN)(a,la,b,c,d,e,f,g,h,lh)
#define CALLSPPPPPPS(UN,LN,a,b,c,d,e,f,g,h)                    F_FUNC(UN,LN)(a,strlen(a),b,c,d,e,f,g,h,strlen(h))
#define DEFSSSPPPPP(UN,LN,a,la,b,lb,c,lc,d,e,f,g,h)               STDCALL(UN,LN)(a,la,b,lb,c,lc,d,e,f,g,h)
#define CALLSSSPPPPP(UN,LN,a,b,c,d,e,f,g,h)                       F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,strlen(c),d,e,f,g,h)
#define DEFSPPSPPPSP(UN,LN,a,la,b,c,d,ld,e,f,g,h,lh,i)               STDCALL(UN,LN)(a,la,b,c,d,ld,e,f,g,h,lh,i)
#define CALLSPPSPPPSP(UN,LN,a,b,c,d,e,f,g,h,i)                       F_FUNC(UN,LN)(a,strlen(a),b,c,d,strlen(d),e,f,g,h,strlen(h),i)
#define DEFSSPSPPPPS(UN,LN,a,la,b,lb,c,d,ld,e,f,g,h,i,li)               STDCALL(UN,LN)(a,la,b,lb,c,d,ld,e,f,g,h,i,li)
#define CALLSSPSPPPPS(UN,LN,a,b,c,d,e,f,g,h,i)                          F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,d,strlen(d),e,f,g,h,i,strlen(i))
#define DEFPPPPPSPPPP(UN,LN,a,b,c,d,e,f,lf,g,h,i,j)               STDCALL(UN,LN)(a,b,c,d,e,f,lf,g,h,i,j)
#define CALLPPPPPSPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j)                 F_FUNC(UN,LN)(a,b,c,d,e,f,strlen(f),g,h,i,j)
#define DEFPPPPPSPPSP(UN,LN,a,b,c,d,e,f,lf,g,h,i,li,j)               STDCALL(UN,LN)(a,b,c,d,e,f,lf,g,h,i,li,j)
#define CALLPPPPPSPPSP(UN,LN,a,b,c,d,e,f,g,h,i,j)                    F_FUNC(UN,LN)(a,b,c,d,e,f,strlen(f),g,h,i,strlen(i),j)
#define DEFSPSPSPPPPS(UN,LN,a,la,b,c,lc,d,e,le,f,g,h,i,j,lj)               STDCALL(UN,LN)(a,la,b,c,lc,d,e,le,f,g,h,i,j,lj)
#define CALLSPSPSPPPPS(UN,LN,a,b,c,d,e,f,g,h,i,j)                          F_FUNC(UN,LN)(a,strlen(a),b,c,strlen(c),d,e,strlen(e),f,g,h,i,j,strlen(j))
#define DEFSSPSPPSPPP(UN,LN,a,la,b,lb,c,d,ld,e,f,g,lg,h,i,j)               STDCALL(UN,LN)(a,la,b,lb,c,d,ld,e,f,g,lg,h,i,j)
#define CALLSSPSPPSPPP(UN,LN,a,b,c,d,e,f,g,h,i,j)                          F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,d,strlen(d),e,f,g,strlen(g),h,i,j)
#define DEFSSPPPPPPPPPPPP(UN,LN,a,la,b,lb,c,d,e,f,g,h,i,j,k,l,m,n)               STDCALL(UN,LN)(a,la,b,lb,c,d,e,f,g,h,i,j,k,l,m,n)
#define CALLSSPPPPPPPPPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j,k,l,m,n)                    F_FUNC(UN,LN)(a,strlen(a),b,strlen(b),c,d,e,f,g,h,i,j,k,l,m,n)
#define DEFPPPPPPPPPPPPPPPPPPPSPPPPPPPPPPPPPPPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,lt,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,lt,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L)
#define CALLPPPPPPPPPPPPPPPPPPPSPPPPPPPPPPPPPPPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L)                 F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,strlen(t),u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,L)

#endif

/* Appels et signatures sans chaine de caractères */
#define DEF0(UN,LN)               STDCALL(UN,LN)( void )
#define CALL0(UN,LN)              F_FUNC(UN,LN)()
#define DEFP(UN,LN,a)               STDCALL(UN,LN)(a)
#define CALLP(UN,LN,a)              F_FUNC(UN,LN)(a)
#define DEFPP(UN,LN,a,b)               STDCALL(UN,LN)(a,b)
#define CALLPP(UN,LN,a,b)              F_FUNC(UN,LN)(a,b)
#define DEFPPP(UN,LN,a,b,c)               STDCALL(UN,LN)(a,b,c)
#define CALLPPP(UN,LN,a,b,c)              F_FUNC(UN,LN)(a,b,c)
#define DEFPPPP(UN,LN,a,b,c,d)               STDCALL(UN,LN)(a,b,c,d)
#define CALLPPPP(UN,LN,a,b,c,d)              F_FUNC(UN,LN)(a,b,c,d)
#define DEFPPPPP(UN,LN,a,b,c,d,e)               STDCALL(UN,LN)(a,b,c,d,e)
#define CALLPPPPP(UN,LN,a,b,c,d,e)              F_FUNC(UN,LN)(a,b,c,d,e)
#define DEFPPPPPP(UN,LN,a,b,c,d,e,f)               STDCALL(UN,LN)(a,b,c,d,e,f)
#define CALLPPPPPP(UN,LN,a,b,c,d,e,f)              F_FUNC(UN,LN)(a,b,c,d,e,f)
#define DEFPPPPPPPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,i,j)
#define CALLPPPPPPPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j)              F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,i,j)
#define DEFPPPPPPPPPPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j,k,l,m)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,i,j,k,l,m)
#define CALLPPPPPPPPPPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j,k,l,m)              F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,i,j,k,l,m)
#define DEFPPPPPPPPPPPPPPPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)               STDCALL(UN,LN)(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)
#define CALLPPPPPPPPPPPPPPPPPP(UN,LN,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)              F_FUNC(UN,LN)(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r)

#endif
