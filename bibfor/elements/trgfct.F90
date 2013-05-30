subroutine trgfct(fcttab)
!_____________________________________________________________________
!
!     TRGFCT
!
!      CALCUL DES PARAMETRES TRIGONOMÉTRIQUES POUR LES (36) FACETTES
!
!_____________________________________________________________________
! ======================================================================
! COPYRIGHT (C) 1991 - 2010  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
!
!
!     ! FACETTES POUR METHODE DE CAPRA ET MAURY
!
!       36 FACETTES
!       NOMBRE DE DIVISIONS ENTRE -PI/2 ET +PI/2
    real(kind=8) :: fcttab(36, 3)
!
!       ANGLE DE LA FACETTE (-PI/2 <= X < +PI/2)
    real(kind=8) :: angle
    real(kind=8) :: pas
    integer :: i
!
    fcttab(1,1) = 0d0
!
    fcttab(1,2) = 1d0
    fcttab(1,3) = 0d0
!
!       -PI
    angle = -4d0 * atan2(1.d0,1.d0)
!       2PI/N
    pas = -2d0 * angle / 36d0
!
!       POUR CHAQUE FACETTE, LES VALEURS SONT
!         C = COS^2
!         S = SIN^2
!         R = 2 SIN COS
    do 20 i = 2, 36
        angle = angle + pas
        fcttab(i,1) = 0.5d0 * (1d0 + cos(angle))
        fcttab(i,2) = 1d0 - fcttab(i,1)
        fcttab(i,3) = sin(angle)
20  continue
end subroutine
