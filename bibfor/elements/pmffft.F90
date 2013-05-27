subroutine pmffft(fv, sv)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    -------------------------------------------------------------------
!     CALCUL DE : FV FOIS FV TRANSPOSE
!      FV VECTEUR (12)
!
!    * REMARQUE :
!      LA MATRICE EST STOCKEE TRIANGULAIRE INFERIEURE DANS UN TABLEAU
!      UNICOLONNE
!    -------------------------------------------------------------------
!  DONNEES NON MODIFIEES
!
! IN TYPE ! NOM    ! TABLEAU !             SIGNIFICATION
! IN -------------------------------------------------------------------
! IN R*8  ! FV   !     12   ! VECTEUR INT(BT KS G)
!
! OUT TYPE ! NOM   ! TABLEAU !             SIGNIFICATION
! OUT ------------------------------------------------------------------
! OUT R*8 !  SF   ! (12)    ! MATRICE ELEMENTAIRE UNICOLONNE
!
!
! LOC TYPE !  NOM  ! TABLEAU !              SIGNIFICATION
! LOC ------------------------------------------------------------------
! LOC I   ! IP     !   12    ! POINTEUR SUR L'ELEMENT DIAGONAL PRECEDENT
!
    integer :: ip(12), i, j
    real(kind=8) :: fv(12), sv(78)
    data ip/0,1,3,6,10,15,21,28,36,45,55,66/
!
!
    do 10 i = 1, 12
        do 20 j = 1, i
            sv(ip(i)+j)=fv(i)*fv(j)
20      continue
10  end do
!
end subroutine
