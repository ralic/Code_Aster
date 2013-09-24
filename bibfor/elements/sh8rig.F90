subroutine sh8rig(xetemp, para, dsde, option, re)
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
!               ELEMENT SHB8
!
    implicit none
#include "jeveux.h"
#include "asterfort/houxgb.h"
#include "asterfort/mulmat.h"
#include "asterfort/r8inir.h"
#include "asterfort/rloshb.h"
#include "asterfort/shaksg.h"
#include "asterfort/shakst.h"
#include "asterfort/shasbg.h"
#include "asterfort/shbbar.h"
#include "asterfort/shbksi.h"
#include "asterfort/shbrot.h"
#include "asterfort/shcalb.h"
#include "asterfort/shvrot.h"
    character(len=16) :: option
    real(kind=8) :: para(2)
    real(kind=8) :: xe(24), re(24, 24)
    real(kind=8) :: xxg5(5), pxg5(5), xcoq(3, 4), bksip(3, 8, 5), b(3, 8)
    real(kind=8) :: xeloc(24), xcent(3), ppp(3, 3)
    real(kind=8) :: xl(3, 4), xxx(3), yyy(3), bgloc(6, 24)
    real(kind=8) :: bgloct(24, 6), tmptab(6, 24), tmpke(24, 24), cmatlo(6, 6)
    real(kind=8) :: xxvb(3), hij(6), xkstab(24, 24), tmpke2(24, 24)
    real(kind=8) :: gb(8, 4), gs(8, 4), xxgb(3, 4)
    real(kind=8) :: xk11(8, 8), xk22(8, 8), xk33(8, 8), rr2(3, 3), xk12(8, 8)
    real(kind=8) :: xk21(8, 8), xk13(8, 8), xk23(8, 8), xk31(8, 8), xk32(8, 8)
    real(kind=8) :: lambda
    real(kind=8) :: xetemp(*)
    real(kind=8) :: dsde(20,6,6)
!
!
!CCCCCCCCCCCCC ENTREES CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!          ON CALCULE LA MATRICE DE RAIDEUR
!          PARA    PARAMETRES (VOIR te0473.f)
!CCCCCCCCCCCCC SORTIE CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!          RE        MATRICE DE RAIDEUR
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
! INITIALISATIONS
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
! INFOS:
! XE EST RANGE COMME CA:
! (XNOEUD1 YNOEUD1 ZNOEUD1, XNOEUD2 YNOEUD2 ZNOEUD2,...)
! DANS SHB8_TEST_NUM: ATTENTION A LA NUMEROTATION DES NOEUDS
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!      IF (NOMSHB.EQ.'SHB8') THEN
!
!-----------------------------------------------------------------------
    integer :: i, ia, ip, j
    real(kind=8) :: ajac, coela1, coela2, elt, rbid, tt1, tt2
    real(kind=8) :: uns3, uns8, vol, xk1101, xk1102, xk2201
    real(kind=8) :: xk2202, xk3301, xk3302, xmu, xnu, xxl1, xxl2
    real(kind=8) :: youngt, zeta, zlamb
!-----------------------------------------------------------------------
    data gb/1.d0,1.d0,-1.d0,-1.d0,-1.d0,-1.d0,1.d0,1.d0,1.d0,-1.d0,&
     &     -1.d0,1.d0,-1.d0,1.d0,1.d0,-1.d0,1.d0,-1.d0,1.d0,-1.d0,1.d0,&
     &     -1.d0,1.d0,-1.d0,-1.d0,1.d0,-1.d0,1.d0,1.d0,-1.d0,1.d0,-1.d0/
!
! VB: COORD DES NOEUDS DANS REPERE DE REFERENCE
!
!      DATA VB/-1.d0,1.d0,1.d0,-1.d0,-1.d0,1.d0,1.d0,-1.d0,-1.d0,-1.d0,
!     &    1.d0,1.d0,-1.d0,-1.d0,1.d0,1.d0,-1.d0,-1.d0,-1.d0,-1.d0,1.d0,
!     &    1.d0,1.d0,1.d0/
!
! ON DEFINIT LES POINTS DE GAUSS ET LES POIDS
!
    xxg5(1) = -0.906179845938664d0
    xxg5(2) = -0.538469310105683d0
    xxg5(3) = 0.d0
    xxg5(4) = 0.538469310105683d0
    xxg5(5) = 0.906179845938664d0
!
    pxg5(1) = 0.236926885056189d0
    pxg5(2) = 0.478628670499366d0
    pxg5(3) = 0.568888888888889d0
    pxg5(4) = 0.478628670499366d0
    pxg5(5) = 0.236926885056189d0
!
    uns8 = 1.d0/8.d0
    uns3 = 1.d0/3.d0
!
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 10 i = 1, 24
        xe(i) = xetemp(i)
10  continue
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
    do 40 i = 1, 3
! DISTANCE ENTRE 1 ET 5 (EPAISSEUR)
        tt1 = tt1 + (xe(i+12)-xe(i))**2
! DISTANCE ENTRE 3 ET 7 (EPAISSEUR)
        tt2 = tt2 + (xe(i+18)-xe(i+6))**2
! DISTANCE ENTRE 1 ET 2
        xxl1 = xxl1 + (xe(i+3)-xe(i))**2
! DISTANCE ENTRE 2 ET 3
        xxl2 = xxl2 + (xe(i+6)-xe(i+3))**2
40  continue
    xxl1 = sqrt(xxl1)
    xxl2 = sqrt(xxl2)
    tt1 = 0.5d0* (sqrt(tt1)+sqrt(tt2))
    coela1 = 5.d0/6.d0
    coela2 = 5.d0/6.d0
! ELANCEMENT DANS DIRECTION 2
    elt = 6.d0*tt1/xxl1
    if (coela1 .gt. elt) coela1 = elt
! ELANCEMENT DANS DIRECTION 1
    elt = 6.d0*tt1/xxl2
    if (coela2 .gt. elt) coela2 = elt
! POUR L'INSTANT, ON NE MET PAS EN SERVICE:
    coela1 = 1.d0
    coela2 = 1.d0
!
    call r8inir(576, 0.d0, re, 1)
    call r8inir(36, 0.d0, cmatlo, 1)
! ON DEFINIT CMATLO: MATRICE DE COMPORTEMENT
!
    xnu = para(2)
    lambda = para(1)*para(2)/(1-para(2)*para(2))
    xmu = 0.5d0*para(1)/(1+para(2))
    cmatlo(1,1) = lambda + 2.d0*xmu
    cmatlo(2,2) = lambda + 2.d0*xmu
    cmatlo(3,3) = para(1)
    cmatlo(1,2) = lambda
    cmatlo(2,1) = lambda
    cmatlo(4,4) = xmu
    cmatlo(5,5) = xmu
    cmatlo(6,6) = xmu
!
! CALCUL DE BKSIP(3,8,IP) DANS REPERE DE REFERENCE
!      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
!      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
!      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
!
    call shbksi(5, xxg5, bksip)
!
! DEBUT DE LA BOUCLE SUR LES 5 PTS GAUSS
!
    do 160 ip = 1, 5
!
      if(option.ne.'RIGI_MECA') then
        cmatlo(1,1) = dsde(ip,1,1)
        cmatlo(2,1) = dsde(ip,2,1)
        cmatlo(4,1) = dsde(ip,4,1)/2.d0
        cmatlo(1,2) = dsde(ip,1,2)
        cmatlo(2,2) = dsde(ip,2,2)
        cmatlo(4,2) = dsde(ip,4,2)/2.d0
        cmatlo(1,4) = dsde(ip,1,4)/2.d0
        cmatlo(2,4) = dsde(ip,2,4)/2.d0
        cmatlo(4,4) = dsde(ip,4,4)/2.d0
      endif
!
! DEFINITION DES 4 POINTS  COQUES
!
        zeta = xxg5(ip)
        zlamb = 0.5d0* (1.d0-zeta)
        do 70 i = 1, 4
            do 60 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe((i- 1+4)*3+j)
60          continue
70      continue
!
! CALCUL DE PPP 3*3 PASSAGE DE GLOBAL A LOCAL,COQUE
! XCENT : COORD GLOBAL DU CENTRE DE L'ELEMENT
!
        call rloshb(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!
! CALCUL DE B EN GLOBAL
! ATTENTION A L'ORDRE DE EPSILON:
!  FARID DANS SON PAPIER: 11 22 33 12 13 23
!  HARID DANS PLEXUS:     11 22 33 12 23 13
! ON RAJOUTE LES TERMES H1,X . G1  , H2,X . G2
!                   ET  H1,Y . G1  , H2,Y . G2
! AVEC H1   = Y.Z    H2   = X.Z
! DONC H1,X =0       H2,X = Z
! ET   H1,Y =Z       H2,Y = 0
!
! DONC IL NE RESTE PLUS QU'A CALCULER G1 ET G2, ET A AJOUTER A BKSIP
!
        call shcalb(bksip(1, 1, ip), xe, b, ajac)
!
! IL FAUT:  EPS_LOCAL=BGLOB .U_NODAL_GLOBAL
! ON CALCULE BGLOC LA MATRICE B(6,24) UGLOBAL ---> EPS LOCAL
!
        call shasbg(bgloc, b, ppp)
!
! IL FAUT TRANSPOSER BGLOC
!
        do 90 i = 1, 6
            do 80 j = 1, 24
                bgloct(j,i) = bgloc(i,j)
80          continue
90      continue
! IL NE RESTE PLUS QU'A FAIRE: BGLOCT * C * BGLOC
        call r8inir(144, 0.d0, tmptab, 1)
        call r8inir(576, 0.d0, tmpke, 1)
        call r8inir(576, 0.d0, tmpke2, 1)
        call mulmat(6, 6, 24, cmatlo, bgloc,&
                    tmptab)
        call mulmat(24, 6, 24, bgloct, tmptab,&
                    tmpke2)
!
! ASSEMBLAGE: KE=KE + POIDS*JACOBIAN*TMPKE
!
        do 110 j = 1, 8
            do 100 i = 1, 24
                tmpke(i, (j-1)*3+1) = tmpke2(i,j)
                tmpke(i, (j-1)*3+2) = tmpke2(i,j+8)
                tmpke(i, (j-1)*3+3) = tmpke2(i,j+16)
100          continue
110      continue
!
        call r8inir(576, 0.d0, tmpke2, 1)
        do 130 i = 1, 8
            do 120 j = 1, 24
                tmpke2((i-1)*3+1,j) = tmpke(i,j)
                tmpke2((i-1)*3+2,j) = tmpke(i+8,j)
                tmpke2((i-1)*3+3,j) = tmpke(i+16,j)
120          continue
130      continue
        do 150 j = 1, 24
            do 140 i = 1, 24
                re(i,j) = re(i,j) + 4.d0*ajac*pxg5(ip)*tmpke2(i,j)
140          continue
150      continue
160  continue
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!                                                                  C
! MATRICE DE STABILISATION : PAS DE BOUCLE SUR LES POINTS DE GAUSS C
!                                                                  C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!  ON A BESOIN DES VECTEURS GAMMA(8, ALPHA=1 A 4) = GS(8,4)
!
    do 170 i = 1, 24
        xeloc(i) = xe(i)
170  continue
!
! ATTENTION, RR, MATRICE DU CORROTATIONNEL EST RANGEE PAR LIGNES!
!
    call shbrot(xeloc, rr2)
    call shvrot(rr2, xeloc, 1)
    call shbbar(xeloc, b, vol)
!
! CALCUL DES FONCTIONS DE FORME  GS
! XXGB = X  * GB
!
    do 190 j = 1, 3
        do 180 ia = 1, 4
            xxgb(j,ia) = houxgb(xeloc(j),ia)
180      continue
190  continue
!
! GS = (BBB)  * XXGB
!
    do 210 i = 1, 8
        do 200 j = 1, 4
            gs(i,j) = 0.d0
200      continue
210  continue
    do 240 j = 1, 3
        do 230 ia = 1, 4
            do 220 i = 1, 8
                gs(i,ia) = gs(i,ia) + b(j,i)*xxgb(j,ia)
220          continue
230      continue
240  continue
!
! GS = GB - GS
!
    do 260 i = 1, 4
        do 250 j = 1, 8
            gs(j,i) = (gb(j,i)-gs(j,i))*uns8
250      continue
260  continue
!
! CALCUL DE XXVB = X * VB
!
    xxvb(1) = -xeloc(1) + xeloc(4) + xeloc(7) - xeloc(10) - xeloc(13) + xeloc(16) + xeloc(19) - x&
              &eloc(22)
    xxvb(2) = -xeloc(2) - xeloc(5) + xeloc(8) + xeloc(11) - xeloc(14) - xeloc(17) + xeloc(20) + x&
              &eloc(23)
    xxvb(3) = -xeloc(3) - xeloc(6) - xeloc(9) - xeloc(12) + xeloc(15) + xeloc(18) + xeloc(21) + x&
              &eloc(24)
!
! CALCUL DES RELATIONS CONTRAINTES ET DEFORMATIONS GENERALISEES
!
    hij(1) = uns3*xxvb(2)*xxvb(3)/xxvb(1)
    hij(2) = uns3*xxvb(1)*xxvb(3)/xxvb(2)
    hij(3) = uns3*xxvb(2)*xxvb(1)/xxvb(3)
    hij(4) = uns3*xxvb(3)
    hij(5) = uns3*xxvb(1)
    hij(6) = uns3*xxvb(2)
    youngt = para(1)
! POUR RESOUDRE LE CISAILLEMENT TRANSVERSE:
!
    youngt = youngt*0.5d0*(coela1+coela2)
    lambda = youngt*para(2)/(1-para(2)*para(2))
    xmu = 0.5d0*youngt/(1+para(2))
    xk1101 = (lambda+2.d0*xmu)*hij(1)
    xk1102 = uns3*((lambda+2.d0*xmu)*hij(1))
    xk2201 = (lambda+2.d0*xmu)*hij(2)
    xk2202 = uns3*((lambda+2.d0*xmu)*hij(2))
    xk3301 = 0.d0
    xk3302 = xmu*hij(1)*uns3
!
    call r8inir(64, 0.d0, xk12, 1)
    call r8inir(64, 0.d0, xk21, 1)
    call r8inir(64, 0.d0, xk13, 1)
    call r8inir(64, 0.d0, xk23, 1)
    call r8inir(64, 0.d0, xk31, 1)
    call r8inir(64, 0.d0, xk32, 1)
    call r8inir(64, 0.d0, xk33, 1)
    do 280 j = 1, 8
        do 270 i = 1, 8
!
! IL FAUT CALCULER K11 K22 K33 MATRICES 8*8
!
            xk11(i,j) = xk1101*gs(i,3)*gs(j,3) + xk1102*gs(i,4)*gs(j, 4)
            xk22(i,j) = xk2201*gs(i,3)*gs(j,3) + xk2202*gs(i,4)*gs(j, 4)
            xk33(i,j) = xk3301*gs(i,3)*gs(j,3) + xk3302*gs(i,4)*gs(j, 4)
270      continue
280  continue
!
! ASSEMBLAGE DE KSTAB
!
    call shakst(xkstab, xk11, xk22, xk33, xk12,&
                xk21, xk13, xk23, xk31, xk32)
!
! REMISE EN ORDRE DE KSTAB
!
    do 300 j = 1, 8
        do 290 i = 1, 24
            tmpke(i, (j-1)*3+1) = xkstab(i,j)
            tmpke(i, (j-1)*3+2) = xkstab(i,j+8)
            tmpke(i, (j-1)*3+3) = xkstab(i,j+16)
290      continue
300  continue
!
    call r8inir(576, 0.d0, xkstab, 1)
    do 320 i = 1, 8
        do 310 j = 1, 24
            xkstab((i-1)*3+1,j) = tmpke(i,j)
            xkstab((i-1)*3+2,j) = tmpke(i+8,j)
            xkstab((i-1)*3+3,j) = tmpke(i+16,j)
310      continue
320  continue
!
! IL FAUT REPASSER DANS LE REPERE GLOBAL AVEC RR2^T . KSTAB . RR2
! EN FAIT C'EST RR2. KSTAB .RR2^T CAR RR2 RANGEE PAR LIGNES
!
    call shaksg(xkstab, rr2)
!
! RAJOUT DE KSTAB A KE
!
    do 340 j = 1, 24
        do 330 i = 1, 24
            re(i,j) = re(i,j) + xkstab(i,j)
330      continue
340  continue
!
!      K = 0
!      DO 750 I = 1,24
!          DO 740 J = 1,I
!            K = K + 1
!            REV(K) = RE(I,J)
!  740     CONTINUE
!  750 CONTINUE
end subroutine
