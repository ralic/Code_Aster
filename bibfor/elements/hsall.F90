subroutine hsall(vectt, hstout)
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
!
! ......................................................................
!     FONCTION :  CALCUL DE LA MATRICE
!
!                 HSTOUT ( 5 , 9 ) = H ( 5 , 6 ) * S ( 6 , 9 )
!
!                 AU POINTS D INTEGRATION NORMALE
! ......................................................................
!
!
!
    implicit none
!
    include 'asterfort/hfmss.h'
    integer :: i, j
!
    real(kind=8) :: vectt ( 3 , 3 )
!
    real(kind=8) :: hstout ( 5 , 9 )
!
    real(kind=8) :: hsfm ( 3 , 9 )
!
    real(kind=8) :: hss ( 2 , 9 )
!
!
!DEB
!
!---- CALCUL DE HSFM ( 3 , 9 ) ET HSS  ( 2 , 9 )
!
    call hfmss(1, vectt, hsfm, hss)
!
!
!                                       (  HSFM ( 3 , 9 )  )
!---- REMLISSAGE DE  HSTOUT ( 5 , 9 ) = (------------------)
!                                       (  HSS  ( 2 , 9 )  )
!
!
!                 HSTOUT ( 5 , 9 ) = H ( 5 , 6 ) * S ( 6, 9 )
!
!
!
    do 100 j = 1, 9
        do 110 i = 1, 3
            hstout ( i , j ) = hsfm ( i , j )
            if (i .le. 2) hstout ( i + 3 , j ) = hss ( i , j )
110      continue
100  end do
!
!
!FIN
!
end subroutine
