subroutine nmimre(ds_print, sdconv)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmimck.h"
#include "asterfort/nmimcr.h"
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
    type(NL_DS_Print), intent(inout) :: ds_print
    character(len=24), intent(in) :: sdconv
!
! --------------------------------------------------------------------------------------------------
!
! MECA_NON_LINE - Print management
!
! Set value of residuals informations in convergence table
!
! --------------------------------------------------------------------------------------------------
!
! IO  ds_print         : datastructure for printing parameters
! In  sdconv           : name of datastructure convergence
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: sdconv_lieu, sdconv_vale, sdconv_name
    integer :: i_resi, nb_resi
    real(kind=8) :: vale
    character(len=16) :: dof
    character(len=9) :: resi_name
    character(len=16), pointer :: v_sdconv_lieu(:) => null()
    character(len=16), pointer :: v_sdconv_name(:) => null()
    real(kind=8), pointer :: v_sdconv_vale(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    sdconv_lieu = sdconv(1:19)//'.LIEU'
    sdconv_vale = sdconv(1:19)//'.VALE'
    sdconv_name = sdconv(1:19)//'.NAME'
    call jeveuo(sdconv_lieu, 'L', vk16 = v_sdconv_lieu)
    call jeveuo(sdconv_vale, 'L', vr   = v_sdconv_vale)
    call jeveuo(sdconv_name, 'L', vk16 = v_sdconv_name)
    call jelira(sdconv_name, 'LONMAX', ival=nb_resi)
!
! - Loop on residuals
!
    do i_resi = 1, nb_resi
        resi_name = v_sdconv_name(i_resi)(1:9)
        vale      = v_sdconv_vale(i_resi)
        dof       = v_sdconv_lieu(i_resi)
        if (resi_name .eq. 'RESI_RELA') then
            call nmimcr(ds_print, 'RESI_RELA', vale, l_affe = .true._1)
            call nmimck(ds_print, 'RELA_NOEU', dof , l_affe = .true._1)
        else if (resi_name.eq.'RESI_MAXI') then
            call nmimcr(ds_print, 'RESI_MAXI', vale, l_affe = .true._1)
            call nmimck(ds_print, 'MAXI_NOEU', dof , l_affe = .true._1)
        else if (resi_name.eq.'RESI_REFE') then
            call nmimcr(ds_print, 'RESI_REFE', vale, l_affe = .true._1)
            call nmimck(ds_print, 'REFE_NOEU', dof , l_affe = .true._1)
        else if (resi_name.eq.'RESI_COMP') then
            call nmimcr(ds_print, 'RESI_COMP', vale, l_affe = .true._1)
            call nmimck(ds_print, 'COMP_NOEU', dof , l_affe = .true._1)
        else if (resi_name.eq.'FROT_NEWT') then
            call nmimcr(ds_print, 'FROT_NEWT', vale, l_affe = .true._1)
            call nmimck(ds_print, 'FROT_NOEU', dof , l_affe = .true._1)
        else if (resi_name.eq.'GEOM_NEWT') then
            call nmimcr(ds_print, 'GEOM_NEWT', vale, l_affe = .true._1)
            call nmimck(ds_print, 'GEOM_NOEU', dof , l_affe = .true._1)
        else
! --------- Only printing: no error !
        endif
    end do
!
end subroutine
