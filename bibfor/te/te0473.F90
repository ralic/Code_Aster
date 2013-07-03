subroutine te0473(option, nomte)
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
    implicit none
!
!          ELEMENT SHB
!    FONCTION REALISEE:
!            OPTION : 'RIGI_MECA      '
!                            CALCUL DES MATRICES ELEMENTAIRES  3D
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!              ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
#include "jeveux.h"
!
!-----------------------------------------------------------------------
#include "asterfort/elref4.h"
#include "asterfort/idsshb.h"
#include "asterfort/jevech.h"
#include "asterfort/moytem.h"
#include "asterfort/rcvalb.h"
#include "asterfort/sh1rig.h"
#include "asterfort/sh2rig.h"
#include "asterfort/sh6rig.h"
#include "asterfort/sh8rig.h"
    integer :: i, idfde, igeom, imate, imatuu, ipoids, iret
    integer :: ivf, j, jgano, k, lag, nbinco, nbres
    integer :: ndim, nno, nnos, npg
    real(kind=8) :: tempm, ygot
!-----------------------------------------------------------------------
    parameter (nbres=2)
    character(len=4) :: fami
    integer :: icodre(nbres)
    character(len=8) :: nomres(nbres), famil, poum
    character(len=16) :: nomte, option, nomshb
    real(kind=8) :: para(11), re20(60, 60)
    real(kind=8) :: valres(nbres), re(24, 24), re6(18, 18)
    integer :: nbv, kpg, spt
    real(kind=8) :: nu, e, re15(45, 45)
!
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
    call idsshb(ndim, nno, npg, nomshb)
    nbinco = ndim*nno
    do 10 i = 1, 11
        para(i) = 0.d0
10  end do
    famil='FPG1'
    kpg=1
    spt=1
    poum='+'
    if (option .eq. 'RIGI_MECA') then
! ----  RECUPERATION DES COORDONNEES DES CONNECTIVITES
        call jevech('PGEOMER', 'L', igeom)
! ----  RECUPERATION DU MATERIAU DANS ZI(IMATE)
        call jevech('PMATERC', 'L', imate)
        nomres(1) = 'E'
        nomres(2) = 'NU'
        nbv = 2
! ----  INTERPOLATION DES COEFFICIENTS EN FONCTION DE LA TEMPERATURE
! ----  ET DU TEMPS
!
        call moytem(fami, npg, 1, '+', tempm,&
                    iret)
!
        call rcvalb(famil, kpg, spt, poum, zi(imate),&
                    ' ', 'ELAS', 1, 'TEMP', tempm,&
                    nbv, nomres, valres, icodre, 1)
        e = valres(1)
        nu = valres(2)
! ----  PARAMETRES MATERIAUX
        ygot = e
! ----  PARAMETRES MATERIAUX POUR LE CALCUL DE LA
! ----  MATRICE TANGENTE PLASTIQUE
!       MATRICE TANGENTE PLASTIQUE SI WORK(13)=1
!       LAG=0 LAGRANGIEN REACTUALISE (EPS=EPSLIN)
!       LAG=1 LAGRANGIEN TOTAL (EPS=EPSLIN+EPSNL)
        lag = 0
        para(1) = e
        para(2) = nu
        para(3) = ygot
! PARA(4) = WORK(13)
        para(4) = 0
! PARA(5) = WORK(150)
        para(5) = 1
! PARA(6) = LAG
        para(6) = lag
    endif
!
!  ===========================================
!  -- MATRICE DE RIGIDITE
!  ===========================================
    if (option .eq. 'RIGI_MECA') then
        if (nomshb .eq. 'SHB8') then
            do 30 i = 1, nbinco
                do 20 j = 1, nbinco
                    re(i,j) = 0.d0
20              continue
30          continue
            call sh8rig(zr(igeom), para, re)
!        RECUPERATION ET AFFECTATION DU VECTEUR EN SORTIE
!        DEMI-MATRICE DE RIGIDITE
            call jevech('PMATUUR', 'E', imatuu)
            k = 0
            do 50 i = 1, nbinco
                do 40 j = 1, i
                    k = k + 1
                    zr(imatuu+k-1) = re(i,j)
40              continue
50          continue
        else if (nomshb.eq.'SHB6') then
            do 70 i = 1, nbinco
                do 60 j = 1, nbinco
                    re6(i,j) = 0.d0
60              continue
70          continue
            call sh6rig(zr(igeom), para, re6)
            call jevech('PMATUUR', 'E', imatuu)
            k = 0
            do 90 i = 1, nbinco
                do 80 j = 1, i
                    k = k + 1
                    zr(imatuu+k-1) = re6(i,j)
80              continue
90          continue
        else if (nomshb.eq.'SHB15') then
            do 110 i = 1, nbinco
                do 100 j = 1, nbinco
                    re15(i,j) = 0.d0
100              continue
110          continue
!
            call sh1rig(zr(igeom), para, re15)
            call jevech('PMATUUR', 'E', imatuu)
            k = 0
            do 130 i = 1, nbinco
                do 120 j = 1, i
                    k = k + 1
                    zr(imatuu+k-1) = re15(i,j)
120              continue
130          continue
        else if (nomshb.eq.'SHB20') then
            do 150 i = 1, 60
                do 140 j = 1, 60
                    re20(i,j) = 0.d0
140              continue
150          continue
            call sh2rig(zr(igeom), para, re20)
            call jevech('PMATUUR', 'E', imatuu)
            k = 0
            do 170 i = 1, 60
                do 160 j = 1, i
                    k = k + 1
                    zr(imatuu+k-1) = re20(i,j)
160              continue
170          continue
        endif
    endif
end subroutine
