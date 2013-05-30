subroutine te0474(option, nomte)
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
    include 'jeveux.h'
!
    include 'asterfort/elref4.h'
    include 'asterfort/idsshb.h'
    include 'asterfort/jevech.h'
    include 'asterfort/sh1mek.h'
    include 'asterfort/sh2mek.h'
    include 'asterfort/sh6mek.h'
    include 'asterfort/sh8mek.h'
    character(len=4) :: fami
    character(len=16) :: nomte, nomshb, option
    real(kind=8) :: sigma(120), re(24, 24), re6(18, 18)
    real(kind=8) :: re20(60, 60)
    real(kind=8) :: re15(45, 45)
!      REAL*8 RE(24,24)
!-----------------------------------------------------------------------
    integer :: i, icont, idfde, igeom, imatuu, ipoids, ivf
    integer :: j, jgano, k, ndim, nno, nnos, npg
!
!-----------------------------------------------------------------------
    fami = 'RIGI'
    call elref4(' ', fami, ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
! --- INITIALISATIONS :
    call idsshb(ndim, nno, npg, nomshb)
!
!  ===========================================
!  -- MATRICE DE RIGIDITE GEOMETRIQUE
!  ===========================================
    if (option .eq. 'RIGI_MECA_GE') then
!        RECUPERATION DES COORDONNEES DES CONNECTIVITES
!        GEOMETRIE Dans ZR(IGEOM)
        call jevech('PGEOMER', 'L', igeom)
!        RECUPERATION DES CONTRAINTES DANS ZR(ICONT)
        call jevech('PCONTRR', 'L', icont)
        if (nomshb .eq. 'SHB8') then
            do 20 i = 1, 24
                do 10 j = 1, 24
                    re(i,j) = 0.d0
10              continue
20          continue
            do 50 i = 1, 5
                do 40 j = 1, 6
                    sigma(6*(i-1)+j)=zr(icont+18*(i-1)+j-1)
40              continue
50          continue
            call sh8mek(zr(igeom), sigma, re)
            call jevech('PMATUUR', 'E', imatuu)
            k = 0
            do 70 i = 1, 24
                do 60 j = 1, i
                    k = k + 1
                    zr(imatuu+k-1) = re(i,j)
60              continue
70          continue
        else if (nomshb.eq.'SHB6') then
            do 90 i = 1, 18
                do 80 j = 1, 18
                    re6(i,j) = 0.d0
80              continue
90          continue
            do 110 i = 1, 5
                do 100 j = 1, 6
                    sigma(6*(i-1)+j)=zr(icont+18*(i-1)+j-1)
100              continue
110          continue
!
            call sh6mek(zr(igeom), sigma, re6)
            call jevech('PMATUUR', 'E', imatuu)
            k = 0
            do 130 i = 1, 18
                do 120 j = 1, i
                    k = k + 1
                    zr(imatuu+k-1) = re6(i,j)
120              continue
130          continue
!
        else if (nomshb.eq.'SHB15') then
            do 150 i = 1, 45
                do 140 j = 1, 45
                    re15(i,j) = 0.d0
140              continue
150          continue
            do 170 i = 1, 15
                do 160 j = 1, 6
                    sigma(6*(i-1)+j)=zr(icont+18*(i-1)+j-1)
160              continue
170          continue
!
            call sh1mek(zr(igeom), sigma, re15)
            call jevech('PMATUUR', 'E', imatuu)
            k = 0
            do 190 i = 1, 45
                do 180 j = 1, i
                    k = k + 1
                    zr(imatuu+k-1) = re15(i,j)
180              continue
190          continue
!
        else if (nomshb.eq.'SHB20') then
            do 210 i = 1, 60
                do 200 j = 1, 60
                    re20(i,j) = 0.d0
200              continue
210          continue
            do 230 i = 1, 20
                do 220 j = 1, 6
                    sigma(6*(i-1)+j)=zr(icont+18*(i-1)+j-1)
220              continue
230          continue
            call sh2mek(zr(igeom), sigma, re20)
            call jevech('PMATUUR', 'E', imatuu)
            k = 0
            do 250 i = 1, 60
                do 240 j = 1, i
                    k = k + 1
                    zr(imatuu+k-1) = re20(i,j)
240              continue
250          continue
        endif
    endif
end subroutine
