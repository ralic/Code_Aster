function dinins(sddisc, nume_inst)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utdidt.h"
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
    integer :: dinins
    integer, intent(in) :: nume_inst
    character(len=19), intent(in) :: sddisc
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (UTILITAIRE - DISCRETISATION)
!
! ACCES AU NIVEAU DE SUBDIVISION D'UN INSTANT
!
! --------------------------------------------------------------------------------------------------
!
! In  sddisc           : datastructure for time discretization
! IN  nume_inst        : NUMERO DE L'INSTANT
! OUT DININS           : NIVEAU DE SUBDIVISION DE L'INSTANT (1=PAS REDECOUPE)
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sddisc_dini
    integer, pointer :: v_sddisc_dini(:) => null()
    character(len=16) :: metlis
!
! --------------------------------------------------------------------------------------------------
!
!
! --- LA NOTION DE SOUS-NIVEAU N'EXISTE PAS EN GESTION AUTO
!
    call utdidt('L', sddisc, 'LIST', 'METHODE',&
                valk_ = metlis)
    if (metlis .eq. 'AUTO') then
        dinins = 1
        goto 999
    endif
!
! --- ACCES SD LISTE D'INSTANTS
!
    sddisc_dini = sddisc(1:19)//'.DINI'
    call jeveuo(sddisc_dini, 'L', vi = v_sddisc_dini)
    ASSERT(nume_inst.ge.1)
    dinins = v_sddisc_dini(nume_inst)
!
999 continue
end function
