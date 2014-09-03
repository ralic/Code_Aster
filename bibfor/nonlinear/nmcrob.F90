subroutine nmcrob(meshz  , modelz, result, sddisc, sd_inout,&
                  sd_obsv)
!
implicit none
!
#include "asterc/getfac.h"
#include "asterfort/assert.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcroi.h"
#include "asterfort/nmcrot.h"
#include "asterfort/nmextr.h"
#include "asterfort/nmobno.h"
#include "asterfort/utmess.h"
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
    character(len=*), intent(in) :: meshz
    character(len=*), intent(in) :: modelz
    character(len=8), intent(in) :: result
    character(len=19), intent(in) :: sddisc
    character(len=24), intent(in) :: sd_inout
    character(len=19), intent(out) :: sd_obsv
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Init
!
! Create observation datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  result           : name of results datastructure
! In  sddisc           : datastructure for discretization
! in  sd_inout         : datastructure for input/output parameters
! Out sd_obsv          : datastructure for observation parameters
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_obsv, nbocc, nume_reuse
    character(len=19) :: sdarch
    character(len=16) :: keyw_fact
    character(len=24) :: arch_info
    integer, pointer :: v_arch_info(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_obsv   = 0
    sd_obsv   = '&&NMCROB.OBSV'
    keyw_fact = 'OBSERVATION'
    call getfac(keyw_fact, nbocc)
    ASSERT(nbocc.le.99)
!
! - Access to storage datasstructure
!
    sdarch    = sddisc(1:14)//'.ARCH'
    arch_info = sdarch(1:19)//'.AINF'
    call jeveuo(arch_info, 'L', vi = v_arch_info)
!
! - Get reuse index in OBSERVATION table
!
    nume_reuse = v_arch_info(3)
!
! - Read datas for extraction
!
    call nmextr(meshz, modelz    , sd_obsv, sd_inout, keyw_fact,&
                nbocc, nume_reuse, nb_obsv)
!
! - Read parameters
!
    if (nb_obsv .ne. 0) then
!
        call utmess('I', 'OBSERVATION_3', si=nb_obsv)   
!
! ----- Read time list
!
        call nmcroi(sd_obsv, keyw_fact, nbocc)
!
! ----- Read name of columns
!
        call nmobno(sd_obsv, keyw_fact, nbocc)
!
! ----- Create table
!
        call nmcrot(result, sd_obsv)
    endif
!
end subroutine
