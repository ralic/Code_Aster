subroutine sigbar(sigma, barsig)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
!
! ......................................................................
!     FONCTION  :  CALCUL DE
!
!                         (   SIGMA  0       0     )
!      BARS   ( 9 , 9 ) = (   0      SIGMA   0     )
!                         (   0      0       SIGMA )
!
!      AVEC
!
!      SIGMA  ( 3 , 3 )
!
!                  POUR LA PARTIE GEOMETRIQUE DE LA MATRICE TANGENTE
!                  COQUE_3D
!
! ......................................................................
!
    include 'asterfort/r8inir.h'
    integer :: i
!
    integer :: ii, jj
!
    real(kind=8) :: sigma ( 3 , 3 )
!
    real(kind=8) :: barsig ( 9 , 9 )
!
! DEB
!
    call r8inir(9 * 9, 0.d0, barsig, 1)
!
    do 200 i = 1, 3
        do 210 jj = 1, 3
            do 220 ii = 1, 3
                barsig ( ( i - 1 ) * 3 + ii , ( i - 1 ) * 3 + jj ) =&
                sigma ( ii , jj )
220          continue
210      continue
200  continue
!
! FIN
!
end subroutine
