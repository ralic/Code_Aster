subroutine mmmlcf(coefff, coefac, coefaf, lpenac, lpenaf,&
                  iresof, iresog, lambds)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
#include "asterfort/jevech.h"
    real(kind=8) :: coefac, coefaf
    real(kind=8) :: coefff, lambds
    logical :: lpenac, lpenaf
    integer :: iresof, iresog
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
!
! PREPARATION DES CALCULS - RECUPERATION DES COEFFICIENTS
!
! ----------------------------------------------------------------------
!
!
! OUT COEFFF : COEFFICIENT DE FROTTEMENT DE COULOMB
! OUT COEFAC : COEF_AUGM_CONT
! OUT COEFAF : COEF_AUGM_FROT
! OUT LPENAC : .TRUE. SI CONTACT PENALISE
! OUT LPENAF : .TRUE. SI FROTTEMENT PENALISE
! OUT IRESOF : ALGO. DE RESOLUTION POUR LE FROTTEMENT
!              0 - POINT FIXE
!              1 - NEWTON
! OUT IRESOG : ALGO. DE RESOLUTION POUR LA GEOMETRIE
!              0 - POINT FIXE
!              1 - NEWTON
!
! ----------------------------------------------------------------------
!
    integer :: jpcf
    integer :: ialgoc, ialgof
!
! ----------------------------------------------------------------------
!
    call jevech('PCONFR', 'L', jpcf)
!
! --- RECUPERATION DES DONNEES DU CHAM_ELEM DU CONTACT
!
    coefac = zr(jpcf-1+16)
    coefaf = zr(jpcf-1+19)
    coefff = zr(jpcf-1+20)
    ialgoc = nint(zr(jpcf-1+15))
    ialgof = nint(zr(jpcf-1+18))
    iresof = nint(zr(jpcf-1+17))
    iresog = nint(zr(jpcf-1+25))
    lambds = zr(jpcf-1+13)
!
! --- PENALISATION ?
!
    lpenaf = (ialgof.eq.3)
    lpenac = (ialgoc.eq.3)
!
end subroutine
