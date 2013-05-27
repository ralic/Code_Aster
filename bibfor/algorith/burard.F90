subroutine burard(vin, nvi, materd, materf, nmat,&
                  timed, timef, an, bn, cn)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ROUTINE QUI CALCULE LES MATRICES DE DEFORMATION DE FLUAGE PROPRE
!   DEVIATOIRE REVERSIBLE D APRES LE MODELE BETON_BURGER_FP
!
! IN  VIN      : VARIABLES INTERNES INITIALES
!     NVI      : DIMENSION DES VECTEURS VARIABLES INTERNES
!     MATERD   : VECTEUR DE PARAMETRES MATERIAU A T
!     MATERF   : VECTEUR DE PARAMETRES MATERIAU A T+DT
!     NMAT     : DIMENSION DE CMAT
!     TIMED    : INSTANT T
!     TIMEF    : INSTANT T+DT
! OUT AN       : SCALAIRE LIE A LA DEFOR. REV. DE FLUAGE PROPRE DEV.
!     BN       : SCALAIRE LIE A LA DEFOR. REV. DE FLUAGE PROPRE DEV.
!     CN       : SCALAIRE LIE A LA DEFOR. REV. DE FLUAGE PROPRE DEV.
!=======================================================================
    implicit none
    integer :: nvi, nmat, i
    real(kind=8) :: vin(nvi)
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: timed, timef, tdt
    real(kind=8) :: an(6), bn, cn
    real(kind=8) :: krd, etard, hini, hfin
    real(kind=8) :: erd(6)
    real(kind=8) :: tdev, tdexp
!
! === =================================================================
! --- RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
! === =================================================================
    krd = materd(4,2)
    etard = materd(5,2)
!
    hini = materd(6,1)
    hfin = materf(6,1)
!
! === =================================================================
! --- CALCUL VARIATION DU TEMPS
! === =================================================================
    tdt = timef - timed
! === =================================================================
! --- INITIALISATION VARIABLES DE SORTIE
! === =================================================================
    do 10 i = 1, 6
        an(i) = 0.d0
10  end do
    bn = 0.d0
    cn = 0.d0
! === =================================================================
! --- RECUPERATION DEFORMATION REVERSIBLE DEVIATOIRE
! === =================================================================
    erd(1) = vin(3)
    erd(2) = vin(5)
    erd(3) = vin(7)
    erd(4) = vin(12)
    erd(5) = vin(14)
    erd(6) = vin(16)
! === =================================================================
! --- CONSTRUCTION DE LA MATRICE DEVIATOIRE REVERSIBLE
! === =================================================================
    tdev = etard / krd
    tdexp = exp(-tdt/tdev)
    do 20 i = 1, 6
        an(i) = (tdexp - 1.d0) * erd(i)
20  end do
    bn = 1.d0/krd*(&
         tdexp*(&
         -hini*(2*tdev/tdt+1.d0) + hfin*tdev/tdt) + hini*(2.d0*tdev/tdt-1.d0) + hfin*(1.d0-tdev/t&
         &dt&
         )&
         )
    cn = hini/(tdt*krd)*( tdev*tdexp - tdev + tdt )
!
end subroutine
