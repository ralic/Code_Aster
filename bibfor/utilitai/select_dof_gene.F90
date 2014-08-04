subroutine select_dof_gene(prof_genez, nb_cmp, cata_cmp, list_cmp, list_equa,&
                           tabl_equa)
!
implicit none
!
#include "asterfort/nueq_chck.h"
#include "asterfort/jeveuo.h"
#include "asterfort/assert.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: prof_genez
    integer, intent(in) :: nb_cmp
    character(len=8), optional, pointer, intent(in) :: cata_cmp(:)
    character(len=8), optional, pointer, intent(in) :: list_cmp(:)
    integer, pointer, optional, intent(inout) :: list_equa(:)
    integer, pointer, optional, intent(inout) :: tabl_equa(:,:)
!
! --------------------------------------------------------------------------------------------------
!
! Select dof from list of nodes and components from PROF_GENE
!
! --------------------------------------------------------------------------------------------------
!
! Output:
!    list_equa    : vector on complete numbering [1:nb_equa]
!                   for ieq =  [1:nb_equa]
!                      list_equa[ieq] = 0 if node+component not present
!                      list_equa[ieq] = 1 if node+component is present
!    tabl_equa    : table on complete numbering [1:nb_equa, 1:nb_cmp]
!                   for ieq = [1:nb_equa]
!                      for icmp = [1:nb_cmp]
!                         tabl_equa[ieq,icmp] = 0 if node+component not present
!                         tabl_equa[ieq,icmp] = 1 if node+component is present
!
! In  prof_gene     : name of profile (PROF_GENE)
! IO  list_equa     : list of equations
! IO  tabl_equa     : table of equations by components
! In  nb_cmp        : number of components
! In  cata_cmp      : list of components in catalog (name)
! In  list_cmp      : list of components (name)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_equa, i_cmp, nb_equa
    integer :: node_nume
    character(len=8) :: name_cmp
    character(len=19) :: prof_gene
    integer, pointer :: v_desc(:) => null()
    integer, pointer :: v_deeq(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    prof_gene = prof_genez
    call nueq_chck(prof_gene, nb_equa)
    call jeveuo(prof_gene//'.DESC', 'L', vi = v_desc)
    ASSERT(v_desc(1).eq.2)
!
    call jeveuo(prof_gene//'.DEEQ', 'L', vi = v_deeq)
    do i_equa = 1, nb_equa
        node_nume = v_deeq(2*i_equa)
        do i_cmp = 1, nb_cmp
            if (present(list_cmp)) then
                name_cmp = list_cmp(i_cmp)
            else
                name_cmp = cata_cmp(i_cmp)
            endif
            if (name_cmp .eq. 'LAGR' .and. node_nume .lt. 0) then
                if (present(tabl_equa)) then
                    tabl_equa(i_equa,i_cmp) = 1
                elseif (present(list_equa)) then
                    list_equa(i_equa) = 1
                else
                    ASSERT(.false.)
                endif
            endif
            if (name_cmp .eq. 'GENE' .and. node_nume .gt. 0) then
                if (present(tabl_equa)) then
                    tabl_equa(i_equa,i_cmp) = 1
                elseif (present(list_equa)) then
                    list_equa(i_equa) = 1
                else
                    ASSERT(.false.)
                endif
            endif
        end do
    end do

end subroutine
