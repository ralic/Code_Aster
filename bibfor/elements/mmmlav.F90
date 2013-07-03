subroutine mmmlav(ldyna, lfovit, jeusup, ndexfr, coefac,&
                  coefaf)
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
    logical :: lfovit, ldyna
    real(kind=8) :: jeusup
    integer :: ndexfr
    real(kind=8) :: coefac, coefaf
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
!
! PREPARATION DES CALCULS - LECTURE FONCTIONNALITES AVANCEES
!
! ----------------------------------------------------------------------
!
!
! OUT LDYNA  : .TRUE. SI DYNAMIQUE
! OUT LFOVIT : .TRUE. SI FORMULATION EN VITESSE
! OUT JEUSUP : JEU SUPPLEMENTAIRE PAR DIST_ESCL/DIST_MAIT
! OUT NDEXFR : ENTIER CODE POUR EXCLUSION DIRECTION DE FROTTEMENT
! I/O COEFAC : COEF_AUGM_CONT
! I/O COEFAF : COEF_AUGM_FROT
!
! ----------------------------------------------------------------------
!
    integer :: jpcf
    integer :: iform
    real(kind=8) :: theta, deltat
!
! ----------------------------------------------------------------------
!
    call jevech('PCONFR', 'L', jpcf)
!
! --- RECUPERATION DES DONNEES DU CHAM_ELEM DU CONTACT
!
    jeusup = zr(jpcf-1+14)
    ndexfr = nint(zr(jpcf-1+21))
    iform = nint(zr(jpcf-1+22))
    deltat = zr(jpcf-1+23)
    theta = zr(jpcf-1+24)
!
! --- FONCTIONNALITES ACTIVEES
!
    lfovit = iform.eq.2
    ldyna = iform.ne.0
!
! --- COEFFICIENTS MODIFIES POUR FORMULATION EN THETA-VITESSE
!
    if (lfovit) then
        coefaf = coefaf/deltat/theta
        coefac = coefac/deltat/theta
    endif
!
end subroutine
