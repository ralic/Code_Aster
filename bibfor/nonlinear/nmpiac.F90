subroutine nmpiac(sdpilo, eta)
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
#include "jeveux.h"
#include "asterfort/jeveuo.h"
    character(len=19) :: sdpilo
    real(kind=8) :: eta
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - PILOTAGE)
!
! REACTUALISATION DES BORNES DE PILOTAGE SI DEMANDE
!
! ----------------------------------------------------------------------
!
!
! IN  SDPILO : SD PILOTAGE
! IN  ETA    : PARAMETRE DE PILOTAGE
!
!
!
!
    character(len=24) :: evolpa, typsel, typpil
    real(kind=8), pointer :: plir(:) => null()
    character(len=24), pointer :: pltk(:) => null()
!
! ----------------------------------------------------------------------
!
    call jeveuo(sdpilo(1:19)// '.PLTK', 'L', vk24=pltk)
    typpil = pltk(1)
    typsel = pltk(6)
    evolpa = pltk(7)
    call jeveuo(sdpilo(1:19)// '.PLIR', 'E', vr=plir)
    if (typsel .eq. 'ANGL_INCR_DEPL' .and.&
        (typpil.eq.'LONG_ARC' .or.typpil.eq.'SAUT_LONG_ARC')) then
        plir(6)=plir(1)
    endif
!
!
    if (evolpa .eq. 'SANS') goto 9999
    if (evolpa .eq. 'CROISSANT') then
        plir(5) = eta
    else
        plir(4) = eta
    endif
!
9999  continue
end subroutine
