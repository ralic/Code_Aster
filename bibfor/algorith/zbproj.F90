subroutine zbproj(rho, echec)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2007  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterf_types.h"
    real(kind=8) :: rho
    aster_logical :: echec
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (RECH. LINE. - METHODE MIXTE)
!
! PROJECTION DE LA SOLUTION SUR LES BORNES ADMISSIBLES
! REACTUALISATION DES BORNES ADMISSIBLES
!
! ----------------------------------------------------------------------
!
!  I/O RHO      : SOLUTION COURANTE
!  OUT ECHEC : .TRUE. SI LA RECHERCHE DE RACINE A ECHOUE
!
! ----------------------------------------------------------------------
!
    real(kind=8) :: rhoneg, rhopos
    real(kind=8) :: parmul, fneg, fpos
    integer :: dimcpl, nbcpl
    aster_logical :: bpos, lopti
    common /zbpar/ rhoneg,rhopos,&
     &               parmul,fneg  ,fpos  ,&
     &               dimcpl,nbcpl ,bpos  ,lopti
!
! ----------------------------------------------------------------------
!
!
! --- BORNE MIN
!
    if (rho .lt. rhoneg) then
        if (bpos) then
            rho = (rhoneg+rhopos)/2
        else
            echec = .true.
        endif
    endif
!
! --- BORNE MAX
!
    if (bpos) then
        if (rho .gt. rhopos) then
            rho = (rhoneg+rhopos)/2
        endif
    endif
!
end subroutine
