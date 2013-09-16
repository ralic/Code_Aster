subroutine lclbr2(fami, kpg, ksp, imate, compor,&
                  ndim, epsm, t, e, sigmt,&
                  sigmc, epsic, compn, gamma)
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
    implicit none
#include "asterfort/rcvalb.h"
    character(len=16) :: compor(*)
    character(len=*) :: fami
    integer :: imate, ndim, t(3, 3), kpg, ksp
    real(kind=8) :: epsm(6), e, sigmt, sigmc, gamma, compn, epsic
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT BETON REGLEMENTAIRE : INITIALISATION
!
! IN  COMPOR     : NOM DE LA LOI DE COMPORTEMENT
! IN  IMATE      : CODE MATERIAU
! IN  EPSM       : DEFORMATION AU TEMPS MOINS
! OUT T          : TENSEUR DE PLACEMENT (PASSAGE VECT -> MATRICE)
! OUT LAMBDA
! OUT DEUXMU
! OUT ALPHA
! OUT GAMMA
! OUT SEUIL
! ----------------------------------------------------------------------
    integer :: icodre(6)
    character(len=8) :: nomres(6)
    real(kind=8) :: valres(6)
!
    t(1,1)=1
    t(1,2)=4
    t(1,3)=5
    t(2,1)=4
    t(2,2)=2
    t(2,3)=6
    t(3,1)=5
    t(3,2)=6
    t(3,3)=3
!
!    LECTURE DES CARACTERISTIQUES DU MATERIAU
    nomres(1) = 'E'
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'ELAS', 0, ' ', [0.d0],&
                1, nomres, valres, icodre, 1)
    e = valres(1)
!    LECTURE DES CARACTERISTIQUES D'ENDOMMAGEMENT
    nomres(1) = 'D_SIGM_EPSI'
    nomres(2) = 'SYT'
    nomres(3) = 'SYC'
    nomres(4) = 'EPSC'
    nomres(5) = 'N'
    call rcvalb(fami, kpg, ksp, '+', imate,&
                ' ', 'BETON_REGLE_PR', 0, ' ', [0.d0],&
                5, nomres, valres, icodre, 1)
    gamma = - e/valres(1)
    sigmt = valres(2)
    sigmc = -valres(3)
    epsic = -valres(4)
    compn = valres(5)
!
end subroutine
