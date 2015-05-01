subroutine sigvte(sigmtd, sigmt)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     FONCTION  :  CALCUL DE   SIGMT  ( 3 , 3 ) TENSEUR 3D
!
!                  A PARTIR DE SIGMTD  ( 5 ) VECTEUR CONTRAINTES PLANES
!
! ......................................................................
!
    real(kind=8) :: sigmtd ( 5 )
!
    real(kind=8) :: sigmt ( 3 , 3 )
!
!
!
! DEB
!
!
!---- CONTRAINTES NORMALES
!
    sigmt ( 1 , 1 ) = sigmtd ( 1 )
    sigmt ( 2 , 2 ) = sigmtd ( 2 )
!
    sigmt ( 3 , 3 ) = 0.d0
!
!---- CISAILLEMENT
!
    sigmt ( 1 , 2 ) = sigmtd ( 3 )
    sigmt ( 1 , 3 ) = sigmtd ( 4 )
    sigmt ( 2 , 3 ) = sigmtd ( 5 )
!
!---- SYMETRISATION
!
    sigmt ( 2 , 1 ) = sigmt ( 1 , 2 )
    sigmt ( 3 , 1 ) = sigmt ( 1 , 3 )
    sigmt ( 3 , 2 ) = sigmt ( 2 , 3 )
!
! FIN
!
end subroutine
