subroutine vallib_3d(khi, dallib, alf, alfeq, dafm,&
                     casol, nasol, alsol)
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
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!      provient de rsi_3d :
!     calcul de la vitesse de fixation de l'aluminium dans les csh
!=====================================================================
    implicit none
    real(kind=8) :: khi
    real(kind=8) :: dallib
    real(kind=8) :: alf
    real(kind=8) :: alfeq
    real(kind=8) :: dafm
    real(kind=8) :: alsol
    real(kind=8) :: casol
    real(kind=8) :: nasol
!     on ne fixe de l alu dans les csh ou on les relargue
!     on fait en sorte que cette vitesse soit grande devant
!     celle de formation d aft et afm de façon a la priovilegier
    dallib=100.d0*khi*(dlog10(alfeq)-dlog10(alf))
end subroutine
