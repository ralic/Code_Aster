subroutine mmmsta(ndim, leltf, lpenaf, loptf, djeut,&
                  dlagrf, coefaf, tau1, tau2, lcont,&
                  ladhe, lambda, rese, nrese, l_previous)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: ayaovi-dzifa.kudawoo at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/mmtrpr.h"
!
    integer :: ndim
    real(kind=8) :: dlagrf(2), djeut(3)
    aster_logical :: loptf, lpenaf, leltf, l_previous
    real(kind=8) :: tau1(3), tau2(3)
    aster_logical :: lcont, ladhe
    real(kind=8) :: rese(3), nrese, lambda
    real(kind=8) :: coefaf
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - CALCUL)
!
! PREPARATION DES CALCULS - LECTURE DES STATUTS
!
! ----------------------------------------------------------------------
!
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  LPENAF : .TRUE. SI FROTTEMENT PENALISE
! IN  LELTF  : .TRUE. SI ELEMENT DE FROTTEMENT
! IN  LOPTF  : .TRUE. SI OPTION  DE FROTTEMENT
! IN  DLAGRF : INCREMENT DEPDEL DES LAGRANGIENS DE FROTTEMENT
! IN  DJEUT  : INCREMENT DEPDEL DU JEU TANGENT
! IN  COEFAF : COEF_AUGM_FROT
! IN  TAU1   : PREMIER VECTEUR TANGENT
! IN  TAU2   : SECOND VECTEUR TANGENT
! OUT LCONT  : .TRUE. SI CONTACT (SU=1)
! OUT LADHE  : .TRUE. SI ADHERENCE
! OUT LAMBDA : VALEUR DU MULT. DE CONTACT (SEUIL FIXE)
! OUT RESE   : SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!               GTK = LAMBDAF + COEFAF*VITESSE
! OUT NRESE  : NORME DU SEMI-MULTIPLICATEUR GTK DE FROTTEMENT
!
!
!
!
    integer :: jpcf
    integer :: indco
    integer :: indadhe
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    lcont = .false.
    ladhe = .false.
    nrese = 0.d0
    rese(1) = 0.d0
    rese(2) = 0.d0
    rese(3) = 0.d0
!
! --- RECUPERATION DES STATUTS
!
    call jevech('PCONFR', 'L', jpcf)
    if (l_previous) then 
        indco = nint(zr(jpcf-1+27))
        indadhe = nint(zr(jpcf-1+44))
    else 
        indco = nint(zr(jpcf-1+12))   
    endif
!
! --- STATUT DU CONTACT
!
    lcont = indco.eq.1
!
! --- PAS DE FROTTEMENT SI CALCUL OPTION CONTACT
!
    if (.not.loptf) then
        leltf = .false.
    endif
!
! --- STATUT DU CONTACT - CAS DU FROTTEMENT
!
!
    if (loptf) then
! This test influence highly the NON_REGRESSION     
!        if (lambda .lt. 1.d-30) lcont = .false.
        if ( (abs(lambda) .ge. 0.0d0-1d-30) .and. &
            (abs(lambda)  .le. 0.0d0+1d-30)) lcont = .false.
    endif
!
! --- ETAT D'ADHERENCE DU POINT DE CONTACT
!
    if (loptf .and. lcont) then
        call mmtrpr(ndim, lpenaf, djeut, dlagrf, coefaf,&
                    tau1, tau2, ladhe, rese, nrese)
        if (indadhe .eq. 1 .and. l_previous) ladhe = .true. 
    endif
!
!
end subroutine
