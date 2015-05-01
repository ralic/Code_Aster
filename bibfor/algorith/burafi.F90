subroutine burafi(vin, nvi, materd, materf, nmat,&
                  timed, timef, afi, bfi, cfi)
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
! person_in_charge: alexandre.foucault at edf.fr
!=======================================================================
!
! ROUTINE QUI CALCULE LES MATRICES DE DEFORMATION
!  DE FLUAGE PROPRE SPHERIQUE ET DEVIATORIQUE IRREVERSIBLE LINEARISEE
!
! IN  VIN      : VARIABLES INTERNES INITIALES
!     NVI      : DIMENSION DES VECTEURS VARIABLES INTERNES
!     MATERD   : VECTEUR DE PARAMETRES MATERIAU A T
!     MATERF   : VECTEUR DE PARAMETRES MATERIAU A T+DT
!     NMAT     : DIMENSION DE CMAT
!     TIMED    : INSTANT T
!     TIMEF    : INSTANT T+DT
! OUT AFI      : VECTEUR LIE A LA DEFOR. IRREV. DE FLUAGE PROPRE
!     BFI      : MATRICE LIEE A LA DEFOR. IRREV. DE FLUAGE PROPRE
!     CFI      : MATRICE LIEE A LA DEFOR. IRREV. DE FLUAGE PROPRE
!=======================================================================
    implicit none
#include "asterfort/burail.h"
    integer :: nvi, nmat, ndt, ndi, i, j
    real(kind=8) :: vin(*)
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: timed, timef
    real(kind=8) :: afi(6), bfi(6, 6), cfi(6, 6)
    real(kind=8) :: bfis, cfis
    real(kind=8) :: bfid, cfid
!
    common /tdim/   ndt,ndi
!
! === =================================================================
! INITIALISATION DES VARIABLES
! === =================================================================
    do 1 i = 1, ndt
        afi(i) = 0.d0
        do 2 j = 1, ndt
            bfi(i,j) = 0.d0
            cfi(i,j) = 0.d0
 2      continue
 1  end do
! === =================================================================
! CALCUL DE LA MATRICE DES DEFORMATIONS IRREVERSIBLES LINEARISEE
!          DE FLUAGE PROPRE SPHERIQUE INCREMENTALES
! === =================================================================
    call burail(vin, nvi, materd, materf, nmat,&
                timed, timef, 'SPH', bfis, cfis)
! === =================================================================
! CALCUL DE LA MATRICE DES DEFORMATIONS IRREVERSIBLES LINEARISEE
!          DE FLUAGE PROPRE DEVIATORIQUE INCREMENTALES
! === =================================================================
    call burail(vin, nvi, materd, materf, nmat,&
                timed, timef, 'DEV', bfid, cfid)
! === =================================================================
! CONSTRUCTION DE LA MATRICE DES DEFORMATIONS IRREVERSIBLES
!          DE FLUAGE PROPRE INCREMENTALES
! === =================================================================
    do 3 i = 1, ndi
        bfi(i,i) = (bfis+2.d0*bfid)/3.d0
        cfi(i,i) = (cfis+2.d0*cfid)/3.d0
        bfi(i+ndi,i+ndi) = bfid
        cfi(i+ndi,i+ndi) = cfid
 3  end do
    bfi(1,2) = (bfis-bfid)/3
    cfi(1,2) = (cfis-cfid)/3
    bfi(2,1) = bfi(1,2)
    cfi(2,1) = cfi(1,2)
    bfi(3,1) = bfi(1,2)
    cfi(3,1) = cfi(1,2)
    bfi(1,3) = bfi(1,2)
    cfi(1,3) = cfi(1,2)
    bfi(2,3) = bfi(1,2)
    cfi(2,3) = cfi(1,2)
    bfi(3,2) = bfi(1,2)
    cfi(3,2) = cfi(1,2)
!
end subroutine
