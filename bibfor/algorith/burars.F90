subroutine burars(vin, nvi, materd, materf, nmat,&
                  timed, timef, an, bn, cn)
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
! ROUTINE QUI CALCULE LES MATRICES DE DEFORMATION DE FLUAGE PROPRE
!  SPHERIQUE REVERSIBLE D APRES LE MODELE BETON_BURGER_FP
!
! IN  VIN      : VARIABLES INTERNES INITIALES
!     NVI      : DIMENSION DES VECTEURS VARIABLES INTERNES
!     MATERD   : VECTEUR DE PARAMETRES MATERIAU A T
!     MATERF   : VECTEUR DE PARAMETRES MATERIAU A T+DT
!     NMAT     : DIMENSION DE CMAT
!     TIMED    : INSTANT T
!     TIMEF    : INSTANT T+DT
! OUT AN       : SCALAIRE LIE A LA DEFOR. REV. DE FLUAGE PROPRE SPH.
!     BN       : SCALAIRE LIE A LA DEFOR. REV. DE FLUAGE PROPRE SPH.
!     CN       : SCALAIRE LIE A LA DEFOR. REV. DE FLUAGE PROPRE SPH.
!=======================================================================
    implicit none
    integer :: nvi, nmat
    real(kind=8) :: vin(*)
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: timed, timef, tdt
    real(kind=8) :: an, bn, cn
    real(kind=8) :: krs, etars, hini, hfin
    real(kind=8) :: ersp
    real(kind=8) :: tsph, tsexp
!
! === =================================================================
! --- RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
! === =================================================================
    krs = materd(1,2)
    etars = materd(2,2)
!
    hini = materd(6,1)
    hfin = materf(6,1)
!
! === =================================================================
! --- CALCUL VARIATION DE L'HUMIDITE ET DU TEMPS
! === =================================================================
    tdt = timef - timed
! === =================================================================
! --- INITIALISATION VARIABLES DE SORTIE
! === =================================================================
    an = 0.d0
    bn = 0.d0
    cn = 0.d0
! === =================================================================
! --- RECUPERATION DEFORMATION REVERSIBLE SPHERIQUE
! === =================================================================
    ersp = vin(1)
! === =================================================================
! --- CONSTRUCTION DE LA MATRICE SPHERIQUE REVERSIBLE
! === =================================================================
    tsph = etars / krs
    tsexp = exp(-tdt/tsph)
    an = (tsexp - 1.d0) * ersp
    bn = 1.d0/krs*(&
         tsexp*(&
         -hini*(2*tsph/tdt+1.d0) + hfin*tsph/tdt) + hini*(2.d0*tsph/tdt-1.d0) + hfin*(1.d0-tsph/t&
         &dt&
         )&
         )
    cn = hini/(tdt*krs)*( tsph*tsexp - tsph + tdt )
!
end subroutine
