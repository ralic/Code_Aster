subroutine romVariParaRead(ds_varipara, keywfact, iocc)
!
use Rom_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/as_allocate.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/getvis.h"
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
! person_in_charge: mickael.abbas at edf.fr
!
    type(ROM_DS_VariPara), intent(inout) :: ds_varipara
    character(len=16), intent(in) :: keywfact
    integer, intent(in) :: iocc
!
! --------------------------------------------------------------------------------------------------
!
! Model reduction
!
! Variation of parameters for multiparametric problems - Read data
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_varipara      : datastructure for multiparametric problems - Variations
! In  keywfact         : name of factor keyword
! In  iocc             : index of factor keyword
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_vale_para, nbret
!
! --------------------------------------------------------------------------------------------------
!
    nb_vale_para = 0
!
! - Read name of parameters
!
    call getvtx(keywfact, 'NOM_PARA', iocc=iocc, nbret=nbret)
    nbret = abs(nbret)
    ASSERT(nbret .le. 1)
    if (nbret .gt. 0) then
        call getvtx(keywfact, 'NOM_PARA', iocc=iocc, scal = ds_varipara%para_name)
    endif
!
! - Read value of parameters
!
    call getvr8(keywfact, 'VALE_PARA', iocc=iocc, nbret=nb_vale_para)
    nb_vale_para = abs(nb_vale_para)
    ASSERT(nb_vale_para .ge. 1)
    AS_ALLOCATE(vr = ds_varipara%para_vale, size = nb_vale_para)
    call getvr8(keywfact, 'VALE_PARA', iocc=iocc, nbval = nb_vale_para,&
                vect = ds_varipara%para_vale)
    call getvr8(keywfact, 'VALE_INIT', iocc=iocc, scal = ds_varipara%para_init)
!
! - Total number of value of parameters
!
    ds_varipara%nb_vale_para = nb_vale_para
!
end subroutine
