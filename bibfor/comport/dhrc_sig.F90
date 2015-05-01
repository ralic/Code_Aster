subroutine dhrc_sig(eps, vint, a, b, sig)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sebastien.fayolle at edf.fr
!
    implicit none
!
#include "asterfort/r8inir.h"
    real(kind=8) :: a(6, 6), b(6, 2, 2)
    real(kind=8) :: vint(*), eps(8)
    real(kind=8) :: sig(8)
!
! ----------------------------------------------------------------------
!
!      CALCUL DES FORCES THERMODYNAMIQUES ASSOCIEES A LA PLASTICITE
!      APPELE PAR "SEUGLC"
!
! IN:
!       EPS     : TENSEUR DE DEFORMATIONS
!       A       : TENSEUR DE RAIDEUR ELASTIQUE ENDOMMAGEE
!       B       : TENSEUR ASSOCIE AUX DEFORMATIONS PLASTIQUES
!       C       : TENSEUR DE RAIDEUR D'ÉCROUISSAGE PLASTIQUE
!       VINT    : VECTEUR DES VARIABLES INTERNES
!                VINT=(D1,D2,EPSP1X,EPSP1Y,EPSP2X,EPSP2Y)
!
! OUT:
!       SIG     : CONTRAINTES GENERALISEES ASSOCIEES AUX
!       DEFORMATIONS MEMBRANAIRE, DE FLEXION ET DE GLISSEMENT
!
! ----------------------------------------------------------------------
!
    integer :: i, k
!
!     INITIALISATION
    call r8inir(8, 0.0d0, sig, 1)
!
    do k = 1, 6
!     CALCUL DE SIG
        do i = 1, 6
            sig(k) = sig(k)+a(k,i)*eps(i)
            if (i .lt. 3) then
                sig(k) = sig(k)+(b(k,i,1)*vint(i+2) +b(k,i,2)*vint(i+4))*0.5d0
            endif
        end do
end do
!
end subroutine
