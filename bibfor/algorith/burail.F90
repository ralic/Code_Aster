subroutine burail(vin, nvi, materd, materf, nmat,&
                  timed, timef, part, bn, cn)
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
!=======================================================================
!
! ROUTINE QUI CALCULE LES MATRICES LINEARISEES DE DEFORMATION
!  DE FLUAGE PROPRE IRREVERSIBLE D APRES LE MODELE BETON_BURGER_FP
!
! IN  VIN      : VARIABLES INTERNES INITIALES
!     NVI      : DIMENSION DES VECTEURS VARIABLES INTERNES
!     MATERD   : VECTEUR DE PARAMETRES MATERIAU A T
!     MATERF   : VECTEUR DE PARAMETRES MATERIAU A T+DT
!     NMAT     : DIMENSION DE CMAT
!     TIMED    : INSTANT T
!     TIMEF    : INSTANT T+DT
!     PART     : PARTIE DEVIATORIQUE ('DEV') OU SPHERIQUE ('SPH')
! OUT BN       : SCALAIRE LIE A LA DEFOR. IRREV. DE FLUAGE PROPRE SPH.
!     CN       : SCALAIRE LIE A LA DEFOR. IRREV. DE FLUAGE PROPRE SPH.
!=======================================================================
    implicit none
    include 'asterfort/assert.h'
    include 'asterfort/lcprsc.h'
    integer :: nvi, nmat, ndt, ndi, i
    real(kind=8) :: vin(nvi)
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    real(kind=8) :: timed, timef, tdt
    real(kind=8) :: bn, cn
    real(kind=8) :: eta, kappa, hini, hfin
    real(kind=8) :: epsim(6), neir
    character(len=3) :: part
    common /tdim/   ndt,ndi
!
! === =================================================================
! --- RECUPERATION DES VALEURS DES PARAMETRES MATERIAU
! === =================================================================
    if (part(1:3) .eq. 'SPH') then
        eta = materd(3,2)
    else if (part(1:3).eq.'DEV') then
        eta = materd(6,2)
    else
        call assert(.false.)
    endif
    kappa = materd(7,2)
!
    hini = materd(6,1)
    hfin = materf(6,1)
! === =================================================================
! --- CALCUL VARIATION DU TEMPS
! === =================================================================
    tdt = timef - timed
! === =================================================================
! --- CONSTRUCTION TENSEUR DES DEFORMATIONS FLUAGE IRREVERSIBLES
! === =================================================================
    do 1 i = 1, ndi
        epsim(i) = vin(2)+vin(2*i+2)
 1  end do
    epsim(4) = vin(13)
    epsim(5) = vin(15)
    epsim(6) = vin(17)
! === =================================================================
! --- CALCUL NORME TENSEUR DES DEFORMATIONS FLUAGE IRREVERSIBLES
! === =================================================================
    call lcprsc(epsim, epsim, neir)
    neir = sqrt(neir)
    if(neir.lt.vin(21))neir = vin(21)
! === =================================================================
! --- CONSTRUCTION DE LA MATRICE IRREVERSIBLE
! === =================================================================
    bn = 1.d0/eta*exp(-neir/kappa)*tdt/2.d0*hfin
    cn = 1.d0/eta*exp(-neir/kappa)*tdt/2.d0*hini
!
end subroutine
