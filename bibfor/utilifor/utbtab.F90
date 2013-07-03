subroutine utbtab(raz, na, mb, a, b,&
                  xab, btab)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterfort/r8inir.h"
    character(len=*) :: raz
    integer :: na, mb
    real(kind=8) :: a(na, na), b(na, mb), xab(na, mb)
    real(kind=8) :: btab(mb, mb)
!     ------------------------------------------------------------------
!     PRODUIT BT . A . B - A CARREE - B RECTANGULAIRE
!     ------------------------------------------------------------------
!IN   K4  RAZ  'ZERO' : ON FAIT BTAB = 0    + BT*A.B
!              'CUMU' : ON FAIT BTAB = BTAB + BT*A.B
!IN   I   NA   ORDRE DE A
!IN   I   MB   NB DE COLONNES DE B
!IN   R   A    MATRICE A           (NA,NA)
!IN   R   B    MATRICE B           (NA,MB)
!IN   R   XAB  ZONE DE TRAVAIL XAB (NA,MB)
!OUT  R   BTAB PRODUIT BT . A . B  (MB,MB)
!     ------------------------------------------------------------------
    character(len=4) :: raz2
! --DEB
!-----------------------------------------------------------------------
    integer :: i, j, k
!-----------------------------------------------------------------------
    raz2=raz
!
    call r8inir(na*mb, 0.0d0, xab, 1)
    do 15 i = 1, na
        do 15 k = 1, na
            do 15 j = 1, mb
                xab(i,j) = xab(i,j) + a(i,k) * b(k,j)
15          continue
!
    if (raz2 .eq. 'ZERO') call r8inir(mb*mb, 0.0d0, btab, 1)
!
    do 25 i = 1, mb
        do 25 k = 1, na
            do 25 j = 1, mb
                btab(i,j) = btab(i,j) + b(k,i) * xab(k,j)
25          continue
end subroutine
