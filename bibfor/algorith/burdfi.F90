subroutine burdfi(bfi, cfi, nr, yd, dy)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: alexandre.foucault at edf.fr
!
!=====================================================================
! ROUTINE QUI CALCUL L INCREMENT DE DEFORMATIONS DE FLUAGE PROPRE
!
! IN  BFI      : MATRICE FLUAGE PROPRE IRREVERSIBLE
!     CFI      : MATRICE FLUAGE PROPRE IRREVERSIBLE
!     NR       : DIMENSION VECTEUR YD ET DY
!     YD       : VECTEUR DES INCONNUES INITIALES
!     DY       : INCREMENT DU VECTEUR DES INCONNUES AVEC SIGMA MAJ
! OUT DY       : INCREMENT DU VECTEUR DES INCONNUES AVEC VARI MAJ
!_______________________________________________________________________
!
    implicit none
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcsove.h'
    integer :: ndt, ndi, i, nr
    real(kind=8) :: bfi(6, 6), cfi(6, 6)
    real(kind=8) :: yd(nr), dy(nr)
    real(kind=8) :: bnsigd(6), sigf(6), cnsigf(6)
    real(kind=8) :: depsf(6)
!
!     ----------------------------------------------------------------
    common /tdim/   ndt ,ndi
!     ----------------------------------------------------------------
!
! === =================================================================
! --- CONSTRUCTION DES PARTIES PRENANTES POUR DEFORMATION IRREVERSIBLE
! === =================================================================
! === =================================================================
! --- CONSTRUCTION TENSEUR BFI*SIGD
! === =================================================================
    call lcprmv(bfi, yd, bnsigd)
! === =================================================================
! --- CONSTRUCTION TENSEUR CFI*SIGF
! === =================================================================
    call lcsove(yd, dy, sigf)
    call lcprmv(cfi, sigf, cnsigf)
! === =================================================================
! --- CONSTRUCTION TENSEUR CNSIGF+BNSIGD EQUIVAUT A BFI*SIGD+CFI*SIGF
! === =================================================================
    call lcsove(bnsigd, cnsigf, depsf)
! === =================================================================
! --- AFFECTATION DES VALEURS A DY + MISE A L'ECHELLE
! === =================================================================
    do 2 i = 1, ndt
        dy(ndt+i) = depsf(i)
 2  end do
!
end subroutine
