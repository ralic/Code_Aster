subroutine caramx(char, iform, nzoco)
!
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterfort/assert.h"
#include "asterfort/caracc.h"
#include "asterfort/caracd.h"
#include "asterfort/caracm.h"
#include "asterfort/caracp.h"
#include "asterfort/caracx.h"
    character(len=8) :: char
    integer :: nzoco
    integer :: iform
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
!
! CREATION DES SDS DE DEFINITION DU CONTACT (DEFICO)
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  IFORM  : TYPE DE FORMULATION
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
!
! ----------------------------------------------------------------------
!
!
! --- CREATION SD PARAMETRES GENERAUX (NE DEPENDANT PAS DE LA ZONE)
!
    call caracp(char)
!
! --- CREATION SD PARAMETRES GENERAUX (DEPENDANT DE LA ZONE)
!
    call caracm(char, nzoco, iform)
!
! --- CREATION DES SD DEDIEES PAR FORMULATION
!
    if (iform .eq. 1) then
        call caracd(char, nzoco)
    else if (iform.eq.2) then
        call caracc(char, nzoco)
    else if (iform.eq.3) then
        call caracx(char, nzoco)
    else
        ASSERT(.false.)
    endif
!
end subroutine
