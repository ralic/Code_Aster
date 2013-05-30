subroutine lkimat(mod, imat, nmat, materd, materf,&
                  matcst, ndt, ndi, nvi, nr)
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
    implicit  none
! person_in_charge: alexandre.foucault at edf.fr
!       --------------------------------------------------------------
!       RECUPERATION PROPRIETES MATERIAU POUR LETK ET DIMENSION NR
!       IN  MOD    :  TYPE DE MODELISATION
!           IMAT   :  ADRESSE DU MATERIAU CODE
!           NMAT   :  DIMENSION 1 DE MATER
!       OUT MATERD :  COEFFICIENTS MATERIAU A T
!           MATERF :  COEFFICIENTS MATERIAU A T+DT
!                     MATER(*,I) = CARACTERISTIQUES MATERIAU
!                                    I = 1  CARACTERISTIQUES ELASTIQUES
!                                    I = 2  CARACTERISTIQUES PLASTIQUES
!           MATCST :  'OUI' SI  MATERIAU A T = MATERIAU A T+DT
!                     'NON' SINON OU 'NAP' SI NAPPE DANS 'VECMAT.F'
!           NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!           NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
!           NR     :  NB DE COMPOSANTES SYSTEME NL (9 OU 10)
!           NVI    :  NB DE VARIABLES INTERNES
!       --------------------------------------------------------------
    include 'asterfort/lklmat.h'
    integer :: imat, nmat, ndt, ndi, nr, nvi
    real(kind=8) :: materd(nmat, 2), materf(nmat, 2)
    character(len=8) :: mod
    character(len=3) :: matcst
!
    integer :: indal
!
    call lklmat(mod, imat, nmat, 0.d0, materd,&
                materf, matcst, ndt, ndi, nvi,&
                indal)
!
! --- L'INCONNUE DU SYSTEME NL EST COMPOSEE :
! --- DES CONTRAINTES + DLAMBDA + XIP + XIVP
    nr = ndt + 3
!
end subroutine
