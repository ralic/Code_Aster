subroutine burafr(vin, nvi, materd, materf, nmat,&
                  timed, timef, afr, bfr, cfr)
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
! person_in_charge: alexandre.foucault at edf.fr
!=======================================================================
!
! ROUTINE QUI CALCULE LES MATRICES DE DEFORMATION
!  DE FLUAGE PROPRE SPHERIQUE ET DEVIATORIQUE REVERSIBLE
!   D APRES LE MODELE BETON_BURGER_FP
!
! IN  VIN      : VARIABLES INTERNES INITIALES
!     NVI      : DIMENSION DES VECTEURS VARIABLES INTERNES
!     MATERD   : VECTEUR DE PARAMETRES MATERIAU A T
!     MATERF   : VECTEUR DE PARAMETRES MATERIAU A T+DT
!     NMAT     : DIMENSION DE CMAT
!     TIMED    : INSTANT T
!     TIMEF    : INSTANT T+DT
! OUT AFR      : VECTEUR LIE A LA DEFOR. REV. DE FLUAGE PROPRE
!     BFR      : MATRICE LIEE A LA DEFOR. REV. DE FLUAGE PROPRE
!     CFR      : MATRICE LIEE A LA DEFOR. REV. DE FLUAGE PROPRE
!=======================================================================
    implicit none
#include "asterfort/burard.h"
#include "asterfort/burars.h"
    integer :: nvi, nmat, ndt, ndi, i, j
    real(kind=8) :: vin(*)
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: timed, timef
    real(kind=8) :: afr(6), bfr(6, 6), cfr(6, 6)
    real(kind=8) :: afrs, bfrs, cfrs
    real(kind=8) :: afrd(6), bfrd, cfrd
!
    common /tdim/   ndt,ndi
!
! === =================================================================
! INITIALISATION DES VARIABLES
! === =================================================================
    do 1 i = 1, ndt
        afrd(i) = 0.d0
        afr(i) = 0.d0
        do 2 j = 1, ndt
            bfr(i,j) = 0.d0
            cfr(i,j) = 0.d0
 2      continue
 1  end do
! === =================================================================
! CALCUL DE LA MATRICE DES DEFORMATIONS REVERSIBLES
!          DE FLUAGE PROPRE SPHERIQUE INCREMENTALES
! === =================================================================
    call burars(vin, nvi, materd, materf, nmat,&
                timed, timef, afrs, bfrs, cfrs)
! === =================================================================
! CALCUL DE LA MATRICE DES DEFORMATIONS REVERSIBLES
!          DE FLUAGE PROPRE DEVIATORIQUE INCREMENTALES
! === =================================================================
    call burard(vin, nvi, materd, materf, nmat,&
                timed, timef, afrd, bfrd, cfrd)
! === =================================================================
! CONSTRUCTION DE LA MATRICE DES DEFORMATIONS REVERSIBLES
!          DE FLUAGE PROPRE INCREMENTALES
! === =================================================================
    do 3 i = 1, ndi
        afr(i) = afrs + afrd(i)
        afr(i+3) = afrd(i+3)
        bfr(i,i) = (bfrs+2.d0*bfrd)/3.d0
        cfr(i,i) = (cfrs+2.d0*cfrd)/3.d0
        bfr(i+ndi,i+ndi) = bfrd
        cfr(i+ndi,i+ndi) = cfrd
 3  end do
    bfr(1,2) = (bfrs-bfrd)/3
    cfr(1,2) = (cfrs-cfrd)/3
    bfr(2,1) = bfr(1,2)
    cfr(2,1) = cfr(1,2)
    bfr(3,1) = bfr(1,2)
    cfr(3,1) = cfr(1,2)
    bfr(1,3) = bfr(1,2)
    cfr(1,3) = cfr(1,2)
    bfr(2,3) = bfr(1,2)
    cfr(2,3) = cfr(1,2)
    bfr(3,2) = bfr(1,2)
    cfr(3,2) = cfr(1,2)
!
end subroutine
