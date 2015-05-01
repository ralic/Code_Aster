subroutine comp_ther_read(list_vale)
!
    implicit none
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/getvtx.h"
#include "asterc/lccree.h"
#include "asterc/lcinfo.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
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
    character(len=19), intent(in) :: list_vale
!
! --------------------------------------------------------------------------------------------------
!
! COMPOR <CARTE>
!
! Read informations for thermics
!
! --------------------------------------------------------------------------------------------------
!
! In  list_vale   : list of informations to save
!
! --------------------------------------------------------------------------------------------------
!
    character(len=16) :: keywordfact
    character(len=16) :: comp_code, rela_comp
    integer :: iocc, nocc
    integer :: nume_comp, nb_vari
    integer :: j_lvali, j_lvalk
!
! --------------------------------------------------------------------------------------------------
!
    keywordfact = 'COMPORTEMENT'
    call getfac(keywordfact, nocc)
!
! - List contruction
!
    call wkvect(list_vale(1:19)//'.VALI', 'V V I'  , nocc, j_lvali)
    call wkvect(list_vale(1:19)//'.VALK', 'V V K24', nocc, j_lvalk)
!
! - Read informations
!
    do iocc = 1, nocc
!
! ----- Get options from command file
!
        call getvtx(keywordfact, 'RELATION', iocc = iocc, &
                    scal = rela_comp)
        call lccree(1, rela_comp, comp_code)
        call lcinfo(comp_code, nume_comp, nb_vari)
!
! ----- Save options in list
!
        zi(j_lvali -1 + 1) = nb_vari
        zk24(j_lvalk -1 + 1)  = rela_comp
    enddo

end subroutine
