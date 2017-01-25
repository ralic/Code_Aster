subroutine afvarc_shrink(chmate, varc_affe)
!
use Material_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/juveca.h"
#include "asterfort/afva01.h"
#include "asterfort/jelira.h"
#include "asterfort/wkvect.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
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
!
    character(len=8), intent(in) :: chmate
    type(Mat_DS_VarcListAffe), intent(in) :: varc_affe
!
! --------------------------------------------------------------------------------------------------
!
! Material - External state variables (VARC)
!
! Shrink number of components to save memory
!
! --------------------------------------------------------------------------------------------------
!
! In  chmate           : name of material field (CHAM_MATER)
! In  varc_affe        : datastructure for external state variables affected
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_varc_cmp, nb_suppr, nb_cmp, n1
    integer :: i_varc_cmp
    integer :: nbgdmx, nbgdut, ico
    character(len=19) :: cart2 
    character(len=24) :: cvnom, cvvar, cvgd, cvcmp
    character(len=8), pointer :: v_cvnom(:) => null()
    character(len=8), pointer :: v_cvvar(:) => null()
    character(len=8), pointer :: v_cvgd(:) => null()
    character(len=8), pointer :: v_cvcmp(:) => null()
    integer, pointer :: v_suppr(:) => null()
    character(len=8) :: varc_name, varc_cmp
    aster_logical :: l_suppr, l_other
    integer, pointer :: v_desc(:) => null()
    character(len=16), pointer :: v_vale(:) => null()
    character(len=16) :: type_affe, para_1, para_2
!
! --------------------------------------------------------------------------------------------------
!
    nb_varc_cmp  = varc_affe%nb_varc_cmp
!
! - Access to main objects
!
    cvnom = chmate//'.CVRCNOM'
    cvvar = chmate//'.CVRCVARC'
    cvgd  = chmate//'.CVRCGD'
    cvcmp = chmate//'.CVRCCMP'
    call jeveuo(cvnom, 'E', vk8 = v_cvnom)
    call jeveuo(cvvar, 'E', vk8 = v_cvvar)
    call jeveuo(cvgd , 'E', vk8 = v_cvgd)
    call jeveuo(cvcmp, 'E', vk8 = v_cvcmp)
!
! - POUR NE PAS PENALISER LES CALCULS N'AYANT QUE LA COMPOSANTE
! - TEMP (POUR NOM_VARC='TEMP'), ON VA TENTER DE REDUIRE NBCVRC
!
    call wkvect('&&AFVARC.ADETR', 'V V I', nb_varc_cmp, vi = v_suppr)
    nb_suppr=0
    do i_varc_cmp = 1, nb_varc_cmp
        varc_name = v_cvvar(i_varc_cmp)
        varc_cmp  = v_cvnom(i_varc_cmp)
        if (varc_name .eq. 'TEMP') then
            if (varc_cmp .ne. 'TEMP') then
                nb_suppr            = nb_suppr+1
                v_suppr(i_varc_cmp) = 1
            endif
        endif
    end do
    if (nb_suppr .ne. 0) then
! ----- PEUT-ON REELLEMENT SUPPRIMER CES CVRC ?
        l_suppr = .true.
        cart2   = chmate//'.TEMP    .2'
        call jeveuo(cart2//'.DESC', 'L', vi   = v_desc)
        call jeveuo(cart2//'.VALE', 'L', vk16 = v_vale)
        call jelira(jexnom('&CATA.GD.NOMCMP', 'NEUT_K16'), 'LONMAX', nb_cmp)
        nbgdmx = v_desc(2)
        nbgdut = v_desc(3)
        call jelira(chmate//'.TEMP    .2.VALE', 'LONMAX', n1)
        ASSERT(n1 .eq. nbgdmx*nb_cmp)
! ----- ON PARCOURT LES SD STOCKEES DANS LA CARTE ET ON REGARDE S'IL EXISTE D'AUTRES
! ----- CMPS QUE TEMP ET LAGR :  LAUTR=.TRUE.
        do i_varc_cmp = 1, nbgdut
            varc_name = v_vale(nb_cmp*(i_varc_cmp-1)+1)(1:8)
            ASSERT(varc_name .eq. 'TEMP')
            type_affe = v_vale(nb_cmp*(i_varc_cmp-1)+2)
            para_1    = v_vale(nb_cmp*(i_varc_cmp-1)+3)
            para_2    = v_vale(nb_cmp*(i_varc_cmp-1)+4)
            call afva01(type_affe, para_1, para_2, l_other)
            if (l_other) then
                l_suppr = .false.
                exit
            endif
        end do
! ----- ON SUPPRIME CE QUI NE SERT A RIEN
        if (l_suppr) then
            ico=0
            do i_varc_cmp = 1, nb_varc_cmp
                if (v_suppr(i_varc_cmp) .eq. 0) then
                    ico=ico+1
                    v_cvnom(ico) = v_cvnom(i_varc_cmp)
                    v_cvvar(ico) = v_cvvar(i_varc_cmp)
                    v_cvgd(ico)  = v_cvgd(i_varc_cmp)
                    v_cvcmp(ico) = v_cvcmp(i_varc_cmp)
                endif
            end do
            ASSERT(ico.eq.nb_varc_cmp-nb_suppr)
            call juveca(cvnom, ico)
            call juveca(cvvar, ico)
            call juveca(cvgd , ico)
            call juveca(cvcmp, ico)
        endif
    endif
!
end subroutine
