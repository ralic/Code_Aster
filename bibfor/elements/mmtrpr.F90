subroutine mmtrpr(ndim, lpenaf, djeut, dlagrf, coefaf,&
                  tau1, tau2, ladhe, rese, nrese)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/assert.h"
    integer :: ndim
    real(kind=8) :: dlagrf(2)
    real(kind=8) :: coefaf
    real(kind=8) :: djeut(3)
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: rese(3), nrese
    logical :: lpenaf, ladhe
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! ETAT D'ADHERENCE DU POINT DE CONTACT
!
! ----------------------------------------------------------------------
!
!
!  CALCUL DE P_B(0,1)(LAMDBA+ RHO C [[DELTA X]])
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  LPENAF : .TRUE. SI FROTTEMENT PENALISE
! IN  DLAGRF : INCREMENT DEPDEL DES LAGRANGIENS DE FROTTEMENT
! IN  DJEUT  : INCREMENT DEPDEL DU JEU TANGENT
! IN  COEFAF : COEF_AUGM_FROT
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! OUT LADHE  : .TRUE. SI ADHERENCE
! OUT RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! OUT NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!
! ----------------------------------------------------------------------
!
    integer :: idim
!
! ----------------------------------------------------------------------
!
    nrese = 0.d0
    do 10 idim = 1, 3
        rese(idim) = 0.d0
10  end do
!
! --- SEMI-MULTIPLICATEUR DE FROTTEMENT RESE
!
    if (lpenaf) then
        do 32 idim = 1, 3
            rese(idim) = coefaf*djeut(idim)
32      continue
    else
        if (ndim .eq. 2) then
            do 30 idim = 1, 2
                rese(idim) = dlagrf(1)*tau1(idim)+coefaf*djeut(idim)
30          continue
        else if (ndim.eq.3) then
            do 31 idim = 1, 3
                rese(idim) = dlagrf(1)*tau1(idim)+ dlagrf(2)*tau2( idim)+ coefaf*djeut(idim)
31          continue
        else
            call assert(.false.)
        endif
    endif
!
! --- CALCUL DU COEF D'ADHERENCE
!
    do 40 idim = 1, 3
        nrese = rese(idim)*rese(idim) + nrese
40  end do
    nrese = sqrt(nrese)
!
! --- ADHERENCE OU GLISSEMENT ?
!
    if (nrese .le. 1.d0) then
        ladhe = .true.
    else
        ladhe = .false.
    endif
!
end subroutine
