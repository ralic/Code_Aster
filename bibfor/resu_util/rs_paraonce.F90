subroutine rs_paraonce(result   , nb_para      , list_para,&
                       nb_store_, v_list_store_)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterfort/utmess.h"
#include "asterfort/rs_get_liststore.h"
#include "asterfort/rsadpa.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in) :: result
    integer, intent(in) :: nb_para
    character(len=*), intent(in) :: list_para(*)
    integer, optional, intent(in) :: nb_store_
    integer, pointer, optional, intent(in) :: v_list_store_(:)
!
! --------------------------------------------------------------------------------------------------
!
! Results datastructure - Utility
!
! Check if parameters are the same on all storing index
! /!\ only CHARACTERs parameters. If not => error !
!
! --------------------------------------------------------------------------------------------------
!
! In  result           : name of results datastructure
! In  nb_para          : number of parameters to check
! In  list_para        : list of parameters to check
! In  nb_store         : number if times stored in results datastructures
! In  v_list_store     : pointer to list of times stored in results datastructures
! 
! --------------------------------------------------------------------------------------------------
!
    integer :: i_store, nume_store, nume_store0, i_para, nb_store
    character(len=3) :: ctyp
    character(len=16) :: valk(2)
    character(len=80) :: para_refe, para_curr
    integer :: jv_para
    integer, pointer :: v_list_store(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - List of storing index
!
    if (present(nb_store_)) then
        nb_store     = nb_store_
        v_list_store = v_list_store_
    else
        call rs_get_liststore(result, nb_store)
        AS_ALLOCATE(vi = v_list_store, size = nb_store)
        call rs_get_liststore(result, nb_store, v_list_store)
    endif
!
! - Check unicity
!
    if (nb_store .gt. 1) then
        do i_para = 1, nb_para
            nume_store0 = v_list_store(1)
            call rsadpa(result, 'L', 1, list_para(i_para), nume_store0,&
                        1, sjv=jv_para, styp=ctyp)
            para_refe = ' '
            if (ctyp(1:3) .eq. 'K80') then
                para_refe = zk80(jv_para)
            else if (ctyp(1:3) .eq. 'K32') then
                para_refe = zk32(jv_para)
            else if (ctyp(1:3) .eq. 'K24') then
                para_refe = zk24(jv_para)
            else if (ctyp(1:3) .eq. 'K16') then
                para_refe = zk16(jv_para)
            else if (ctyp(1:2) .eq. 'K8') then
                para_refe = zk8(jv_para)
            else
                ASSERT(.false.)
            endif
            do i_store = 2, nb_store
                nume_store = v_list_store(i_store)
                call rsadpa(result, 'L', 1, list_para(i_para), nume_store,&
                            1, sjv=jv_para, styp=ctyp)
                para_curr = ' '
                if (ctyp(1:3) .eq. 'K80') then
                    para_curr = zk80(jv_para)
                else if (ctyp(1:3) .eq. 'K32') then
                    para_curr = zk32(jv_para)
                else if (ctyp(1:3) .eq. 'K24') then
                    para_curr = zk24(jv_para)
                else if (ctyp(1:3) .eq. 'K16') then
                    para_curr = zk16(jv_para)
                else if (ctyp(1:2) .eq. 'K8') then
                    para_curr = zk8(jv_para)
                else
                    ASSERT(.false.)
                endif
            end do
            if (para_curr .ne. para_refe) then
                valk(1) = result
                valk(2) = list_para(i_para)
                call utmess('F', 'RESULT1_2', nk = 2, valk = valk)
            endif
        end do
    endif
!
    if (.not.present(nb_store_)) then
        AS_DEALLOCATE(vi = v_list_store)
    endif
!
    call jedema()
!
end subroutine
