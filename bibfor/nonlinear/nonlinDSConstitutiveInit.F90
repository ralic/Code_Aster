subroutine nonlinDSConstitutiveInit(model, cara_elem, ds_constitutive)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/nmdoco.h"
#include "asterfort/nmcpqu.h"
#include "asterfort/dismoi.h"
#include "asterfort/jeveuo.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: cara_elem
    type(NL_DS_Constitutive), intent(inout) :: ds_constitutive
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Constitutive laws
!
! Initializations for constitutive laws management datastructure
!
! --------------------------------------------------------------------------------------------------
!
! In  model            : name of the model
! In  cara_elem        : name of elementary characteristics (field)
! IO  ds_constitutive  : datastructure for constitutive laws management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: ifm, niv
    character(len=8) :: answer
    integer :: nb_affe, i_affe
    character(len=16), pointer :: v_compor_vale(:) => null()
    integer, pointer :: v_compor_desc(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call infdbg('MECANONLINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> . Initializations for constitutive laws'
    endif
!
! - Construct CHEM_ELEM_S
!
    call nmdoco(model, cara_elem, ds_constitutive%compor)
!
! - Some functionnalities
!
    call nmcpqu(ds_constitutive%compor, 'C_PLAN', 'DEBORST', ds_constitutive%l_deborst)
    call nmcpqu(ds_constitutive%compor, 'RELCOM', 'DIS_CHOC', ds_constitutive%l_dis_choc)
    call dismoi('POST_INCR', ds_constitutive%carcri, 'CARTE_CARCRI', repk=answer)
    ds_constitutive%l_post_incr = answer .eq. 'OUI'
!
! - Look if geometric matrix is included in global tangent matrix
!
    call jeveuo(ds_constitutive%compor(1:19)//'.VALE', 'L', vk16 = v_compor_vale)
    call jeveuo(ds_constitutive%compor(1:19)//'.DESC', 'L', vi   = v_compor_desc)
    nb_affe = v_compor_desc(3)
    do i_affe = 1, nb_affe
        if ((v_compor_vale(1+2+20*(i_affe-1)).eq.'GROT_GDEP') .or.&
            (v_compor_vale(1+2+20*(i_affe-1)).eq.'SIMO_MIEHE') .or.&
            (v_compor_vale(1+2+20*(i_affe-1)).eq.'GDEF_LOG')) then
            ds_constitutive%l_matr_geom = .true.
        endif
    enddo
!
end subroutine
