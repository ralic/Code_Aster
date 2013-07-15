subroutine sh2rig(xetemp, para, dsde, option, re)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.
!
! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.
!
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!               ELEMENT SHB20
!
    implicit none
#include "jeveux.h"
#include "asterfort/asbgl2.h"
#include "asterfort/mulmat.h"
#include "asterfort/r8inir.h"
#include "asterfort/rloshb.h"
#include "asterfort/s2calb.h"
#include "asterfort/sh2ksi.h"
#include "asterfort/tran63.h"
    character(len=16) :: option
    integer :: p, q
    real(kind=8) :: para(2)
    real(kind=8) :: xe(60), re(60, 60), lambda
    real(kind=8) :: xcoq(3, 4), bksip(3, 20, 20), b(3, 20)
    real(kind=8) :: xcent(3), ppp(3, 3), pppt(3, 3)
    real(kind=8) :: xl(3, 4), xxx(3), yyy(3), som, pppt1, pppt2, pppt3
    real(kind=8) :: tmptab(6, 60), tmpke(60, 60), cmatlo(6, 6)
    real(kind=8) :: tmpke2(60, 60), xetemp(*)
    real(kind=8) :: xxg5(20), xyg5(20), xzg5(20), pxg5(20), em2(3, 3, 3, 3)
    real(kind=8) :: bgl(6, 60), bglt(60, 6), em(6, 6), em3333(3, 3, 3, 3)
    real(kind=8) :: dsde(20,6,6)
!
!
!
!CCCCCCCCCCCCC ENTREES CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!          ICLE=2    ON CALCULE LA MATRICE DE RAIDEUR
!CCCCCCCCCCCCC SORTIE CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! INITIALISATIONS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! INFOS:
! XE EST RANGE: NOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2 ZNOEUD2,...)
! DANS SHB8_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! ON DEFINIT LES POINTS DE GAUSS ET LES POIDS
!
! Des points de gauss sur la facette 1-2-3:
!
!-----------------------------------------------------------------------
    integer :: i, ip, iz, j, k, l, m
    integer :: n
    real(kind=8) :: ajac, coela1, coela2, elt, rbid, tt1, tt2
    real(kind=8) :: xmu, xnu, xxl1, xxl2, zeta, zlamb
!
!-----------------------------------------------------------------------
    xzg5(1) = -0.906179845938664d0
    xzg5(2) = -0.538469310105683d0
    xzg5(3) = 0.d0
    xzg5(4) = 0.538469310105683d0
    xzg5(5) = 0.906179845938664d0
!
    pxg5(1) = 0.236926885056189d0
    pxg5(2) = 0.478628670499366d0
    pxg5(3) = 0.568888888888889d0
    pxg5(4) = 0.478628670499366d0
    pxg5(5) = 0.236926885056189d0
!
    do 8 iz = 1, 5
        xxg5(iz) = -0.577350269189625d0
        xxg5(iz+5) = 0.577350269189625d0
        xxg5(iz+10) = 0.577350269189625d0
        xxg5(iz+15) = -0.577350269189625d0
        xyg5(iz) = -0.577350269189625d0
        xyg5(iz+5) = -0.577350269189625d0
        xyg5(iz+10) = 0.577350269189625d0
        xyg5(iz+15) = 0.577350269189625d0
        xzg5(iz+5) = xzg5(iz)
        pxg5(iz+5) = pxg5(iz)
        xzg5(iz+10) = xzg5(iz)
        pxg5(iz+10) = pxg5(iz)
        xzg5(iz+15) = xzg5(iz)
        pxg5(iz+15) = pxg5(iz)
 8  continue
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 9 i = 1, 60
        xe(i) = xetemp(i)
 9  continue
!
!
! INTIALISATION LONGUEUR DES COTES
! CALCUL DES COEFF D ELANCEMENT A METTRE DANS LA MATRICE DE CPT
!
    xxl1 = 0.d0
    xxl2 = 0.d0
    tt1 = 0.d0
    tt2 = 0.d0
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! STABILISATION ADAPTATIVE EN FONCTION DE LA DISTORTION DE L'ELEMENT
!
    do 10 i = 1, 3
! DISTANCE ENTRE 1 ET 5 (EPAISSEUR)
        tt1 = tt1+(xe(i+12)-xe(i))**2
! DISTANCE ENTRE 3 ET 7 (EPAISSEUR)
        tt2 = tt2+(xe(i+18)-xe(i+6))**2
! DISTANCE ENTRE 1 ET 2
        xxl1 = xxl1+(xe(i+3)-xe(i))**2
! DISTANCE ENTRE 2 ET 3
        xxl2 = xxl2+(xe(i+6)-xe(i+3))**2
10  continue
    xxl1 = sqrt(xxl1)
    xxl2 = sqrt(xxl2)
    tt1 = 0.5d0*(sqrt(tt1)+sqrt(tt2))
    coela1 = 5.d0/6.d0
    coela2 = 5.d0/6.d0
! ELANCEMENT DANS DIRECTION 2
    elt = 6.d0*tt1/xxl1
    if (coela1 .gt. elt) coela1=elt
! ELANCEMENT DANS DIRECTION 1
    elt = 6.d0*tt1/xxl2
    if (coela2 .gt. elt) coela2=elt
! POUR L'INSTANT, ON NE MET PAS EN SERVICE:
    coela1 = 1.d0
    coela2 = 1.d0
!
    call r8inir(3600, 0.d0, re, 1)
    call r8inir(36, 0.d0, cmatlo, 1)
!
! ON DEFINIT CMATLOC: MATRICE DE COMPORTEMENT
!
    xnu = para(2)
    lambda = para(1)*para(2)/(1.d0-para(2)*para(2))
    xmu = 0.5d0*para(1)/(1.d0+para(2))
    cmatlo(1,1) = lambda + 2.d0*xmu
    cmatlo(2,2) = lambda + 2.d0*xmu
    cmatlo(3,3) = para(1)
    cmatlo(1,2) = lambda
    cmatlo(2,1) = lambda
    cmatlo(4,4) = xmu
    cmatlo(5,5) = xmu
    cmatlo(6,6) = xmu
!
! CALCUL DE BKSIP(3,20,IP) DANS REPERE DE REFERENCE
!      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
!      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
!      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
!
    call sh2ksi(20, xxg5, xyg5, xzg5, bksip)
!
! DEBUT DE LA BOUCLE SUR LES 5 PTS GAUSS
!
    do 240 ip = 1, 20

      if(option.ne.'RIGI_MECA') then
        cmatlo(1,1) = dsde(ip,1,1)
        cmatlo(2,1) = dsde(ip,2,1)
        cmatlo(4,1) = dsde(ip,4,1)/2.D0
        cmatlo(1,2) = dsde(ip,1,2)
        cmatlo(2,2) = dsde(ip,2,2)
        cmatlo(4,2) = dsde(ip,4,2)/2.D0
        cmatlo(1,4) = dsde(ip,1,4)/2.D0
        cmatlo(2,4) = dsde(ip,2,4)/2.D0
        cmatlo(4,4) = dsde(ip,4,4)/2.D0
      endif
!
! DEFINITION DES 4 POINTS  COQUES
!
        zeta = xzg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 30 i = 1, 4
            do 20 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe(i*3+ 9+j)
20          continue
30      continue
!
! CALCUL DE PPP 3*3 PASSAGE DE GLOBAL A LOCAL,COQUE
! XCENT : COORD GLOBAL DU CENTRE DE L'ELEMENT
!
        call rloshb(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!
! CALCUL DE B EN GLOBAL
!
! ATTENTION A L'ORDRE DE EPSILON:
!  FARID DANS SON PAPIER: 11 22 33 12 13 23
!  HARID DANS PLEXUS:     11 22 33 12 23 13
!
        call s2calb(bksip(1, 1, ip), xe, b, ajac)
        call r8inir(360, 0.d0, bgl, 1)
        call asbgl2(bgl, b)
        call r8inir(360, 0.d0, bglt, 1)
        do 50 i = 1, 6
            do 40 j = 1, 60
                bglt(j,i) = bgl(i,j)
40          continue
50      continue
!
! IL NE RESTE PLUS QU'A FAIRE: BGLT * C * BGL
!
        do 70 i = 1, 6
            do 60 j = 1, 6
                em(i,j)=cmatlo(i,j)
60          continue
70      continue
!
! Passer EM du repère d'élément au repère global
!
        call r8inir(81, 0.d0, em3333, 1)
        call tran63(em, em3333, 2)
        do 90 i = 1, 3
            do 80 j = 1, 3
                pppt(j,i) = ppp(i,j)
80          continue
90      continue
!
        do 170 k = 1, 3
            do 160 l = 1, 3
                do 150 p = 1, 3
                    do 140 q = 1, 3
!
                        som=0.d0
                        do 130 j = 1, 3
                            pppt1 = pppt(j,l)
                            do 120 i = 1, 3
                                pppt2 = pppt(i,k)
                                do 110 m = 1, 3
                                    pppt3 = pppt(m,p)
                                    do 100 n = 1, 3
                                        som=som+ pppt1*pppt2*em3333(i,&
                                        j,m,n)*pppt3*pppt(n,q)
100                                  continue
110                              continue
120                          continue
130                      continue
                        em2(k,l,p,q)=som
!
140                  continue
150              continue
160          continue
170      continue
        call tran63(em, em2, 1)
!
        call r8inir(360, 0.d0, tmptab, 1)
        call r8inir(3600, 0.d0, tmpke, 1)
        call r8inir(3600, 0.d0, tmpke2, 1)
        call mulmat(6, 6, 60, em, bgl,&
                    tmptab)
        call mulmat(60, 6, 60, bglt, tmptab,&
                    tmpke2)
!
! ASSEMBLAGE: KE=KE + POIDS*JACOBIAN*TMPKE
!
        do 190 j = 1, 20
            do 180 i = 1, 60
                tmpke(i,(j-1)*3+1)=tmpke2(i,j)
                tmpke(i,(j-1)*3+2)=tmpke2(i,j+20)
                tmpke(i,(j-1)*3+3)=tmpke2(i,j+40)
180          continue
190      continue
        call r8inir(3600, 0.d0, tmpke2, 1)
        do 210 i = 1, 20
            do 200 j = 1, 60
                tmpke2((i-1)*3+1,j)=tmpke(i,j)
                tmpke2((i-1)*3+2,j)=tmpke(i+20,j)
                tmpke2((i-1)*3+3,j)=tmpke(i+40,j)
200          continue
210      continue
        do 230 j = 1, 60
            do 220 i = 1, 60
                re(i,j)=re(i,j) + ajac*pxg5(ip)*tmpke2(i,j)
220          continue
230      continue
240  continue
end subroutine
