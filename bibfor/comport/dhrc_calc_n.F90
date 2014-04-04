subroutine dhrc_calc_n(eps, vint, b, c, neta1,&
                  neta2)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: sebastien.fayolle at edf.fr
!
    implicit none
!
#include "asterfort/r8inir.h"
    real(kind=8) :: b(6, 2, 2), c(2, 2, 2)
    real(kind=8) :: vint(7), eps(6)
!
    real(kind=8) :: neta1(2), neta2(2)
!
! ----------------------------------------------------------------------
!
!      CALCUL DES FORCES THERMODYNAMIQUES ASSOCIEES A LA PLASTICITE
!      APPELE PAR "SEUGLC"
!
! IN:
!       EPS     : TENSEUR DE DEFORMATIONS
!       B       : TENSEUR ASSOCIE AUX DEFORMATIONS PLASTIQUES
!       C       : TENSEUR DE RAIDEUR D'ÉCROUISSAGE PLASTIQUE
!       VINT   : VECTEUR DES VARIABLES INTERNES
!                VINT=(D1,D2,EPSP1X,EPSP1Y,EPSP2X,EPSP2Y)
!
! OUT:
!       NETA1   : CONTRAINTE ASSOCIEE AU GLISSEMENT SUR LA PARTIE 1 DE
!                 LA PLAQUE
!       NETA2   : CONTRAINTE ASSOCIEE AU GLISSEMENT SUR LA PARTIE 2 DE
!                 LA PLAQUE
!
! ----------------------------------------------------------------------
!
    integer :: i, k
!
!     INITIALISATION
    call r8inir(2, 0.0d0, neta1, 1)
    call r8inir(2, 0.0d0, neta2, 1)
!
!
    do k = 1, 2
!     CALCUL DE NETA1 ET NETA2
        do i = 1, 6
            neta1(k) = neta1(k)-eps(i)*b(i,k,1)*0.5d0
            neta2(k) = neta2(k)-eps(i)*b(i,k,2)*0.5d0
            if (i .lt. 3) then
                neta1(k) = neta1(k)-vint(i+2)*c(i,k,1)
                neta2(k) = neta2(k)-vint(i+4)*c(i,k,2)
            endif
        end do
    end do
end subroutine
