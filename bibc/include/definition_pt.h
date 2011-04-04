/*           CONFIGURATION MANAGEMENT OF EDF VERSION                  */
/* MODIF definition_pt include  DATE 04/04/2011   AUTEUR COURTOIS M.COURTOIS */
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
/* ALONG WITH THIS PROGRAM; IF NOT, WRITE TO : EDF R&D CODE_ASTER,    */
/*    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.     */
/* ================================================================== */

#include "definition.h"

#ifndef DEFINITION_PT_H
#define DEFINITION_PT_H

/* Appels et signatures avec strlen en fin de liste */
#ifdef _STRLEN_AT_END
#define DEF_P_PPPPSPSP(NAME,a,b,c,d,e,le,f,g,lg,h)               (NAME)(a,b,c,d,e,f,g,h,le,lg)
#define CALL_P_PPPPSPSP(NAME,a,b,c,d,e,f,g,h)                    (NAME)(a,b,c,d,e,f,g,h,strlen(e),strlen(g))
#define DEF_P_PPPPSPPP(NAME,a,b,c,d,e,le,f,g,h)               (NAME)(a,b,c,d,e,f,g,h,le)
#define CALL_P_PPPPSPPP(NAME,a,b,c,d,e,f,g,h)                 (NAME)(a,b,c,d,e,f,g,h,strlen(e))
#define DEF_P_PPPPPSPPSP(NAME,a,b,c,d,e,f,lf,g,h,i,li,j)               (NAME)(a,b,c,d,e,f,g,h,i,j,lf,li)
#define CALL_P_PPPPPSPPSP(NAME,a,b,c,d,e,f,g,h,i,j)                    (NAME)(a,b,c,d,e,f,g,h,i,j,strlen(f),strlen(i))
#define DEF_P_PPPPPSPPPP(NAME,a,b,c,d,e,f,lf,g,h,i,j)               (NAME)(a,b,c,d,e,f,g,h,i,j,lf)
#define CALL_P_PPPPPSPPPP(NAME,a,b,c,d,e,f,g,h,i,j)                 (NAME)(a,b,c,d,e,f,g,h,i,j,strlen(f))

#define DEFUMAT(NAME,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,ls,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K)               (NAME)(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,ls)
#define CALLUMAT(NAME,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K)                 (NAME)(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K,strlen(s))

/* Appels et signatures avec strlen juste après le pointeur de chaine */
#else
#define DEF_P_PPPPSPSP(NAME,a,b,c,d,e,le,f,g,lg,h)               (NAME)(a,b,c,d,e,le,f,g,lg,h)
#define CALL_P_PPPPSPSP(NAME,a,b,c,d,e,f,g,h)                    (NAME)(a,b,c,d,e,strlen(e),f,g,strlen(g),h)
#define DEF_P_PPPPSPPP(NAME,a,b,c,d,e,le,f,g,h)               (NAME)(a,b,c,d,e,le,f,g,h)
#define CALL_P_PPPPSPPP(NAME,a,b,c,d,e,f,g,h)                 (NAME)(a,b,c,d,e,strlen(e),f,g,h)
#define DEF_P_PPPPPSPPSP(NAME,a,b,c,d,e,f,lf,g,h,i,li,j)               (NAME)(a,b,c,d,e,f,lf,g,h,i,li,j)
#define CALL_P_PPPPPSPPSP(NAME,a,b,c,d,e,f,g,h,i,j)                    (NAME)(a,b,c,d,e,f,strlen(f),g,h,i,strlen(i),j)
#define DEF_P_PPPPPSPPPP(NAME,a,b,c,d,e,f,lf,g,h,i,j)               (NAME)(a,b,c,d,e,f,lf,g,h,i,j)
#define CALL_P_PPPPPSPPPP(NAME,a,b,c,d,e,f,g,h,i,j)                 (NAME)(a,b,c,d,e,f,strlen(f),g,h,i,j)

#define DEFUMAT(NAME,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,ls,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K)               (NAME)(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,ls,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K)
#define CALLUMAT(NAME,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K)                 (NAME)(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,strlen(s),t,u,v,w,x,y,z,A,B,C,D,E,F,G,H,I,J,K)

#endif

#endif
