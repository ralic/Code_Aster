subroutine eclapp(ndim, nno2, lonmin, coor)
!
    implicit none
    include 'asterc/matfpe.h'
    include 'asterfort/r8inir.h'
    include 'blas/dnrm2.h'
    integer :: ndim, nno2
    real(kind=8) :: coor(ndim, nno2), lonmin
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!        CONTROLE DE L'APPLATISSEMENT DES ELEMENTS AVEC ECLA_PG
! ----------------------------------------------------------------------
! IN  NDIM    I  DIMENSION DE L'ESPACE
! IN  NNO2    I  NOMBRE DE NOEUDS DE L'ELEMENT
! IN  LONMIN  R  LONGUEUR MINIMALE (LONGUEUR DES MEDIANES)
! VAR COOR    R  COORDONNEES DES NOEUDS
!                 IN  -> VALEURS INITIALES
!                 OUT -> VALEURS APRES CORRECTION (AFFINITE)
! ----------------------------------------------------------------------
!
    real(kind=8) :: corr, l1, l2, d1(3), d2(3), prec
    integer :: i
    parameter (prec = 1.d-5)
! ----------------------------------------------------------------------
    call matfpe(-1)
!
    if (nno2 .eq. 4) then
!
        do 10 i = 1, ndim
            d1(i) = (coor(i,2)+coor(i,3)-coor(i,1)-coor(i,4))/2
            d2(i) = (coor(i,3)+coor(i,4)-coor(i,1)-coor(i,2))/2
10      continue
!
        l1 = dnrm2(ndim,d1,1)
        l2 = dnrm2(ndim,d2,1)
!
!      ELEMENTS PLATS
        if (min(l1,l2)/max(l1,l2) .lt. prec) then
            if (l1 .lt. l2) then
                call r8inir(ndim, 0.d0, d1, 1)
                d1(1) = d2(2)
                d1(2) = -d2(1)
                corr = lonmin/2.d0/dnrm2(ndim,d1,1)
                do 12 i = 1, ndim
                    coor(i,1) = coor(i,1) - corr*d1(i)
                    coor(i,2) = coor(i,2) + corr*d1(i)
                    coor(i,3) = coor(i,3) + corr*d1(i)
                    coor(i,4) = coor(i,4) - corr*d1(i)
12              continue
            else
                call r8inir(ndim, 0.d0, d2, 1)
                d2(1) = -d1(2)
                d2(2) = d1(1)
                corr = lonmin/2.d0/dnrm2(ndim,d2,1)
                do 15 i = 1, ndim
                    coor(i,1) = coor(i,1) - corr*d2(i)
                    coor(i,2) = coor(i,2) - corr*d2(i)
                    coor(i,3) = coor(i,3) + corr*d2(i)
                    coor(i,4) = coor(i,4) + corr*d2(i)
15              continue
            endif
            goto 9999
        endif
!
!      ELEMENTS EPAIS
        if (l1 .lt. lonmin) then
            corr = lonmin/l1
            do 20 i = 1, ndim
                coor(i,1) = corr*coor(i,1)+(1-corr)/2*(coor(i,1)+coor( i,2))
                coor(i,2) = corr*coor(i,2)+(1-corr)/2*(coor(i,1)+coor( i,2))
                coor(i,3) = corr*coor(i,3)+(1-corr)/2*(coor(i,3)+coor( i,4))
                coor(i,4) = corr*coor(i,4)+(1-corr)/2*(coor(i,3)+coor( i,4))
20          continue
        endif
!
        if (l2 .lt. lonmin) then
            corr = lonmin/l2
            do 30 i = 1, ndim
                coor(i,1) = corr*coor(i,1)+(1-corr)/2*(coor(i,1)+coor( i,4))
                coor(i,2) = corr*coor(i,2)+(1-corr)/2*(coor(i,2)+coor( i,3))
                coor(i,3) = corr*coor(i,3)+(1-corr)/2*(coor(i,2)+coor( i,3))
                coor(i,4) = corr*coor(i,4)+(1-corr)/2*(coor(i,1)+coor( i,4))
30          continue
        endif
!
    endif
!
9999  continue
!
    call matfpe(1)
!
end subroutine
