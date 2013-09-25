function mminfi(defico, questz, izone)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: mminfi
#include "asterfort/mminfp.h"
    character(len=24) :: defico
    integer :: izone
    character(len=*) :: questz
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE POUR LE CONTACT (TOUTES METHODES)
!
! REPOND A UNE QUESTION SUR UNE OPTION/CARACTERISTIQUE DU CONTACT
! VARIABLE SUIVANT LA ZONE
! REPONSE INTEGER
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD POUR LA DEFINITION DU CONTACT
! IN  IZONE  : NUMERO DE LA ZONE DE CONTACT QU'ON INTERROGE
! IN  QUESTI : QUESTION POSEE
!
! ----------------------------------------------------------------------
!
    integer :: irep(1)
    real(kind=8) :: r8bid(1)
    logical :: lbid(1)
!
! ----------------------------------------------------------------------
!
    call mminfp(izone, defico, questz, irep, r8bid, lbid)
    mminfi = irep(1)
end function
