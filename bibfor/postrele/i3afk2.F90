subroutine i3afk2(s, fk, iret)
    implicit none
!
    integer :: iret
    real(kind=8) :: s(3, *), fk(4, *)
!
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!     ------------------------------------------------------------------
!     ASSEMBLAGE DE LA TRANSFO GEOM POUR UN QUADRANGLE
!     ------------------------------------------------------------------
! IN  S      : R : TABLE(1..3,1..4) DES COORDONEES DES SOMMETS
! OUT FK     : R : TABLE(1..4,1..3) DES COEF DES FK
! OUT IRET   : I : CODE RETOUR : -1 --> DEGENERESCENCE
!            :   :                0 --> RAS
!     ------------------------------------------------------------------
!
    real(kind=8) :: a, b, c, d
    integer :: i
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    iret = 0
    do 10, i = 1, 3, 1
    a = s(i,1)
    b = s(i,2)
    c = s(i,3)
    d = s(i,4)
    fk(1,i) = ( a + b + c + d)*0.25d0
    fk(2,i) = (-a + b + c - d)*0.25d0
    fk(3,i) = (-a - b + c + d)*0.25d0
    fk(4,i) = ( a - b + c - d)*0.25d0
    10 end do
end subroutine
