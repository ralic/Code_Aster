subroutine numcch(modelz, list_loadz, list_ligr, nb_ligr)
!
implicit none
!
#include "asterfort/dismoi.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/as_allocate.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: modelz
    character(len=*), intent(in) :: list_loadz
    character(len=24), pointer, intent(out) :: list_ligr(:)
    integer, intent(out) :: nb_ligr
!
! --------------------------------------------------------------------------------------------------
!
! Factor
!
! Create list of LIGREL for numbering - For loads
!
! --------------------------------------------------------------------------------------------------
!
! In  model          : name of model
! In  load_list      : list of loads
! In  list_ligr      : pointer to list of LIGREL
! In  nb_ligr        : number of LIGREL in list
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: ligr_name, list_load
    integer :: nb_list_ligr
    integer :: i_load, iret, ier
    character(len=24), pointer :: l_load_name(:) => null()
    character(len=8), pointer :: load_type(:) => null()
    character(len=24) :: list_load_name
    character(len=8) :: model, load_name
    integer :: nb_load
!
! --------------------------------------------------------------------------------------------------
!
    list_load = list_loadz
    model     = modelz
    nb_ligr   = 0
!
! - List of load names
!
    list_load_name = list_load//'.LCHA'
    nb_load = 0
    call jeexin(list_load_name, iret)
    if (iret .ne. 0) then
        call jelira(list_load_name, 'LONMAX', nb_load)
        call jeveuo(list_load_name, 'L', vk24 = l_load_name)
    endif
    nb_list_ligr = nb_load+1
!
! - Create object
!
    AS_ALLOCATE(vk24 = list_ligr, size = nb_list_ligr)
!
! - LIGREL for model
!
    call jeexin(model//'.MODELE    .NBNO', iret)
    if (iret .gt. 0) then
        nb_ligr = 1
        list_ligr(nb_ligr) = model(1:8)//'.MODELE'
    endif
!
! - LIGREL for loads
!
    do i_load = 1, nb_load
        load_name = l_load_name(i_load)(1:8)
        call jeexin(load_name(1:8)//'.TYPE', ier)
        if (ier .gt. 0) then
            call jeveuo(load_name(1:8)//'.TYPE', 'L', vk8 = load_type)
            ligr_name = load_name(1:8)//'.CH'//load_type(1)(1:2)// '.LIGRE'
            call jeexin(ligr_name(1:19)//'.LIEL', iret)
        else
            iret = 0
        endif
        if (iret .gt. 0) then
            nb_ligr = nb_ligr + 1
            list_ligr(nb_ligr) = ligr_name
        endif
    end do
!
end subroutine
