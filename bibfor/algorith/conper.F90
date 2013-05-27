subroutine conper(macoc, i1, i2, i3, i4)
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
!
!  ROUTINE CONPER
!    PERMUTATION CIRCULAIRE
!  DECLARATIONS
!    KDUM   : CHARACTER DE TRAVAIL
!    MACOC  : TABLEAU DES NOMS DES NOEUDS   POUR UNE MAILLE FISSURE
!
!  MOT_CLEF : ORIE_FISSURE
!
    implicit none
!
!     ------------------------------------------------------------------
!
    character(len=8) :: kdum
    character(len=8) :: macoc(*)
!
!-----------------------------------------------------------------------
    integer :: i1, i2, i3, i4
!-----------------------------------------------------------------------
    kdum = macoc(i1+2)
    macoc(i1+2) = macoc(i2+2)
    macoc(i2+2) = macoc(i3+2)
    macoc(i3+2) = macoc(i4+2)
    macoc(i4+2) = kdum
!
end subroutine
