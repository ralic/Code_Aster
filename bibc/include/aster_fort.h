/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF aster_fort include  DATE 24/08/2010   AUTEUR COURTOIS M.COURTOIS */
/* ================================================================== */
/* COPYRIGHT (C) 1991 - 2010  EDF R&D              WWW.CODE-ASTER.ORG */
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

#include "aster.h"

#ifndef ASTER_FORT_H
#define ASTER_FORT_H

/* *********************************************************************
 * 
 * Définition des interfaces aux routines fortran appelées depuis le C.
 * 
 * *********************************************************************/

/* routines UTILITAIRES */
#define CALL_R8VIDE STDCALL(R8VIDE,r8vide)
extern DOUBLE CALL_R8VIDE();

#define CALL_ISNNEM STDCALL(ISNNEM,isnnem)
extern INTEGER CALL_ISNNEM();

#define CALL_ULOPEN(a,b,c,d,e) CALLPSSSS(ULOPEN,ulopen,a,b,c,d,e)
extern void DEFPSSSS(ULOPEN,ulopen,INTEGER *,char *,STRING_SIZE,char *,STRING_SIZE, char *,STRING_SIZE,char *,STRING_SIZE);

#define CALL_FCLOSE(a) CALLP(FCLOSE,fclose,a)
extern void DEFP(FCLOSE,fclose,INTEGER *);

#define CALL_REPOUT(a,b,c) CALLPPS(REPOUT,repout,a,b,c)
extern void DEFPPS(REPOUT,repout,INTEGER *,INTEGER *,char *,STRING_SIZE);

#define CALL_REPDEX(a,b,c) CALLPPS(REPDEX,repdex,a,b,c)
extern void DEFPPS(REPDEX,repdex,INTEGER *,INTEGER *,char *,STRING_SIZE);

#define CALL_DISMOI(a,b,c,d,e,f,g) CALLSSSSPSP(DISMOI,dismoi,a,b,c,d,e,f,g)
extern void DEFSSSSPSP(DISMOI, dismoi, char *,STRING_SIZE, char *,STRING_SIZE, char *,STRING_SIZE, char *,STRING_SIZE, INTEGER *, char *,STRING_SIZE, INTEGER *);

#define CALL_MATFPE(a) CALLP(MATFPE,matfpe,a)
extern void DEFP(MATFPE, matfpe, INTEGER *);

#define CALL_VERSIO(a,b,c,d,e) CALLPPPSP(VERSIO,versio,a,b,c,d,e)
extern void DEFPPPSP(VERSIO,versio,INTEGER *,INTEGER *,INTEGER *,char *,STRING_SIZE,INTEGER *);


/* routines SUPERVISEUR */
#define CALL_EXPASS(a,b,c,d)  CALLPPPP(EXPASS,expass,a,b,c,d)
extern void DEFPPPP(EXPASS,expass, INTEGER* , INTEGER* , INTEGER* , INTEGER*);

#define CALL_OPSEXE(a,b,c,d,e)  CALLPPPSP(OPSEXE,opsexe,a,b,c,d,e)
extern void DEFPPPSP(OPSEXE,opsexe,INTEGER* , INTEGER* , INTEGER* , char *,STRING_SIZE ,INTEGER* ) ;

#define CALL_IMPERS() F_FUNC(IMPERS,impers)()
extern void STDCALL(IMPERS,impers)();

#define CALL_ONERRF(a,b,c) CALLSSP(ONERRF,onerrf,a,b,c)
extern void DEFSSP(ONERRF,onerrf,char *,STRING_SIZE, _OUT char *,STRING_SIZE, _OUT INTEGER *);

#define CALL_GCNCON(a,b) CALLSS(GCNCON,gcncon,a,b)
extern void DEFSS(GCNCON,gcncon,char *,STRING_SIZE,char *,STRING_SIZE);

#define CALL_DEBUT(a,b,c)  F_FUNC(DEBUT,debut)(a,b,c)
extern void STDCALL(DEBUT,debut)(INTEGER* , INTEGER* , INTEGER* );

#define CALL_IBMAIN(a,b,c)  F_FUNC(IBMAIN,ibmain)(a,b,c)
extern void STDCALL(IBMAIN,ibmain)(INTEGER* , INTEGER* , INTEGER* );

#define CALL_POURSU(a,b,c,d) CALLPPPP(POURSU,poursu,a,b,c,d)
extern void DEFPPPP(POURSU,poursu,INTEGER* , INTEGER* , INTEGER* ,INTEGER*) ;

#define CALL_GCCPTS(a,la) F_FUNC(GCCPTS,gccpts)(a,la)
extern void DEFS(GCCPTS,gccpts,char *, STRING_SIZE );

#define CALL_GCUGEN(a,b,c,d,e) CALLPSSSP(GCUGEN,gcugen,a,b,c,d,e)
extern void DEFPSSSP(GCUGEN, gcugen, INTEGER*, char*, STRING_SIZE, char*, STRING_SIZE, char*,  STRING_SIZE, INTEGER*);


/* routines JEVEUX */
#define CALL_JEMARQ() STDCALL(JEMARQ, jemarq)()
extern void CALL_JEMARQ();

#define CALL_JEDEMA() STDCALL(JEDEMA, jedema)()
extern void CALL_JEDEMA();

#define CALL_JEDETR(nom) CALLS(JEDETR, jedetr, nom)
extern void DEFS(JEDETR, jedetr, char *, STRING_SIZE);

#define CALL_JELST3(a,b,c,d)  CALLSSPP(JELST3,jelst3,a,b,c,d)
extern void DEFSSPP(JELST3,jelst3, char*, STRING_SIZE, char*, STRING_SIZE, INTEGER*, INTEGER*);

#define CALL_JELIRA(a,b,c,d)  CALLSSPS(JELIRA,jelira,a,b,c,d)
extern void DEFSSPS(JELIRA,jelira, char*, STRING_SIZE, char*, STRING_SIZE, INTEGER*, char*, STRING_SIZE );

#define CALL_JEEXIN(a,b)  CALLSP(JEEXIN,jeexin,a,b)
extern void DEFSP(JEEXIN,jeexin, char*, STRING_SIZE, INTEGER* );

#define CALL_JEINFO(a) CALLP(JEINFO,jeinfo,a)
extern void DEFP(JEINFO, jeinfo, DOUBLE *);


/* routines d'accès aux OBJETS JEVEUX (vecteurs, collections, champs) */
#define CALL_GETCON(nomsd,iob,ishf,ilng,ctype,lcon,iaddr,nomob) CALLSPPPPPPS(GETCON,getcon,nomsd,iob,ishf,ilng,ctype,lcon,iaddr,nomob)
extern void DEFSPPPPPPS(GETCON,getcon,char *,STRING_SIZE,INTEGER *,INTEGER *,INTEGER *,INTEGER *,INTEGER *,char **,char *,STRING_SIZE);

#define CALL_PUTCON(nomsd,nbind,ind,valr,valc,num,iret) CALLSPPPPPP(PUTCON,putcon,nomsd,nbind,ind,valr,valc,num,iret)
extern void DEFSPPPPPP(PUTCON,putcon,char *,STRING_SIZE,INTEGER *,INTEGER *,DOUBLE *,DOUBLE *,INTEGER *,INTEGER *);

#define CALL_TAILSD(nom, nomsd, val, nbval) CALLSSPP(TAILSD,tailsd,nom, nomsd, val, nbval)
extern void DEFSSPP(TAILSD,tailsd,char *,STRING_SIZE,char *,STRING_SIZE,INTEGER *, INTEGER *);

#define CALL_PRCOCH(nomce,nomcs,nomcmp,ktype,itopo,nval,groups) CALLSSSSPPS(PRCOCH,prcoch,nomce,nomcs,nomcmp,ktype,itopo,nval,groups)
extern void DEFSSSSPPS(PRCOCH,prcoch,char *,STRING_SIZE,char *,STRING_SIZE,char *,STRING_SIZE,char *,STRING_SIZE,INTEGER *,INTEGER *,char *,STRING_SIZE);


/* routines de manipulation de la SD RESULTAT */
extern void DEFSPPSPPPSP(RSACPA,rsacpa,char *, STRING_SIZE, INTEGER *, INTEGER *, char *, STRING_SIZE, INTEGER *, INTEGER *, DOUBLE *, char *, STRING_SIZE, INTEGER *);
#define CALL_RSACPA(nomsd, numva, icode, nomva, ctype, ival, rval, kval, ier) \
                  CALLSPPSPPPSP(RSACPA,rsacpa, nomsd, numva, icode, nomva, ctype, ival, rval, kval, ier)

/* particulier car on passe les longueurs des chaines en dur */
extern void DEFSPSPPPS(RSACCH,rsacch,char *, STRING_SIZE, INTEGER *, char *,STRING_SIZE,INTEGER *, INTEGER *, INTEGER *, char *,STRING_SIZE);
#ifdef _POSIX
#define CALL_RSACCH(nomsd, numch, nomch, nbord, liord, nbcmp, liscmp) \
                  F_FUNC(RSACCH,rsacch)(nomsd,numch,nomch,nbord,liord,nbcmp,liscmp, strlen(nomsd),16,8)
#else
#define CALL_RSACCH(nomsd, numch, nomch, nbord, liord, nbcmp, liscmp) \
                  F_FUNC(RSACCH,rsacch)(nomsd,strlen(nomsd),numch, nomch,16,nbord, liord, nbcmp, liscmp,8)
#endif


/* routines de manipulation de la SD MATERIAU */
#define CALL_RCVALE(a,b,c,d,e,f,g,h,i,j) CALLSSPSPPSPSS(RCVALE,rcvale,a,b,c,d,e,f,g,h,i,j)
extern void DEFSSPSPPSPSS(RCVALE, rcvale, char *,STRING_SIZE, char *,STRING_SIZE, INTEGER *, char *,STRING_SIZE, DOUBLE *, INTEGER *, char *,STRING_SIZE, DOUBLE *, char *, STRING_SIZE, char *, STRING_SIZE);


/* routines d'impression des MESSAGES */
#define CALL_AFFICH(a,b) CALLSS(AFFICH,affich,a,b)
extern void DEFSS(AFFICH,affich,char *,STRING_SIZE,char *,STRING_SIZE);

#define CALL_U2MESS(cod, idmess) CALLSS(U2MESS, u2mess, cod, idmess)
extern void DEFSS(U2MESS, u2mess, char *, STRING_SIZE, char *, STRING_SIZE);

/* particulier car on fixe les longueurs des chaines valk */
#define VALK_SIZE 128
extern void DEFSSPSPPPP(U2MESG, u2mesg, char *, STRING_SIZE, char *, STRING_SIZE, INTEGER *, char *, STRING_SIZE, INTEGER *, INTEGER *, INTEGER *, DOUBLE *);
#ifdef _POSIX
#define CALL_U2MESG(cod, idmess, nk, valk, ni, vali, nr, valr) \
                F_FUNC(U2MESG, u2mesg)(cod, idmess, nk, valk, ni, vali, nr, valr, strlen(cod), strlen(idmess), VALK_SIZE)
#else
#define CALL_U2MESG(cod, idmess, nk, valk, ni, vali, nr, valr) \
                F_FUNC(U2MESG, u2mesg)(cod, strlen(cod), idmess, strlen(idmess), nk, valk, VALK_SIZE, ni, vali, nr, valr)
#endif


/* routines UTILITAIRES pour MED */
#define CALL_MDNOMA(a,b,c,d) CALLSPSP(MDNOMA,mdnoma,a,b,c,d)
extern void DEFSPSP(MDNOMA,mdnoma,char *,STRING_SIZE,INTEGER *,char *,STRING_SIZE,INTEGER *);

#define CALL_MDNOCH(a,b,c,d,e,f,g) CALLSPPSSSP(MDNOCH,mdnoch,a,b,c,d,e,f,g)
extern void DEFSPPSSSP(MDNOCH,mdnoch,char *,STRING_SIZE,INTEGER *,INTEGER *,char *,STRING_SIZE,char *,STRING_SIZE,char *,STRING_SIZE,INTEGER *);


/* FIN ASTER_FORT_H */
#endif
