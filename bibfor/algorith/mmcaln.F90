subroutine mmcaln(ndim, tau1, tau2, norm, mprojn,&
                  mprojt)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "asterfort/matini.h"
#include "asterfort/mmnorm.h"
#include "asterfort/vecini.h"
    integer :: ndim
    real(kind=8) :: tau1(3), tau2(3)
    real(kind=8) :: norm(3)
    real(kind=8) :: mprojn(3, 3), mprojt(3, 3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - UTILITAIRE)
!
! CALCUL DE LA NORMALE ET DES MATRICES DE PROJECTION
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE LA MAILLE DE CONTACT
! IN  TAU1   : PREMIERE TANGENTE EXTERIEURE
! IN  TAU2   : SECONDE TANGENTE EXTERIEURE
! OUT NORM   : NORMALE INTERIEURE
! OUT MPROJN : MATRICE DE PROJECTION NORMALE
! OUT MPROJT : MATRICE DE PROJECTION TANGENTE
!
! ----------------------------------------------------------------------
!
    integer :: i, j
    real(kind=8) :: noor
!
! ----------------------------------------------------------------------
!
    call matini(3, 3, 0.d0, mprojt)
    call matini(3, 3, 0.d0, mprojn)
    call vecini(3, 0.d0, norm)
!
! --- NORMALE
!
    call mmnorm(ndim, tau1, tau2, norm, noor)
    if (noor .le. r8prem()) call assert(.false.)
!
! --- MATRICE DE PROJECTION NORMALE
!
    do 321 i = 1, ndim
        do 311 j = 1, ndim
            mprojn(i,j) = norm(i)*norm(j)
311      continue
321  end do
!
! --- MATRICE DE PROJECTION TANGENTE
!
    do 125 i = 1, ndim
        do 115 j = 1, ndim
            mprojt(i,j) = -1.d0*norm(i)*norm(j)
115      continue
125  end do
!
    do 330 i = 1, ndim
        mprojt(i,i) = 1.d0 + mprojt(i,i)
330  end do
!
end subroutine
