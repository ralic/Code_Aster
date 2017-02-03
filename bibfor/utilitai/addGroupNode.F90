subroutine addGroupNode(mesh, nb_add)
!
implicit none
!
#include "asterfort/assert.h"
#include "asterfort/cpclma.h"
#include "asterfort/jeexin.h"
#include "asterfort/jedetr.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jeecra.h"
#include "asterfort/jecrec.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in) :: mesh
    integer, intent(in) :: nb_add
!
! --------------------------------------------------------------------------------------------------
!
! Mesh management
!
! Add group in list of GROUP_NO
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh         : name of mesh
! In  nb_add       : number of groups to add
!
! --------------------------------------------------------------------------------------------------
!
    character(len=24) :: grpnoe, gpptnn, grpnov
    character(len=24) :: group_name
    integer :: iret, nb_group, nb_group_new, nb_enti, i_enti, i_group
    integer, pointer :: v_list_old(:) => null()
    integer, pointer :: v_list_new(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    grpnoe = mesh//'.GROUPENO'
    gpptnn = mesh//'.PTRNOMNOE'
    grpnov = '&&ADDGRE.GROUPENO'
!
    call jeexin(grpnoe, iret)
    if (iret .eq. 0) then
        call jedetr(gpptnn)
        call jecreo(gpptnn, 'G N K24')
        call jeecra(gpptnn, 'NOMMAX', 1, ' ')
        call jecrec(grpnoe, 'G V I', 'NO '//gpptnn, 'DISPERSE', 'VARIABLE', 1)
    else
        call jelira(grpnoe, 'NOMUTI', nb_group)
        nb_group_new = nb_group + nb_add
        call cpclma(mesh, '&&ADDGRE', 'GROUPENO', 'V')
        call jedetr(grpnoe)
        call jedetr(gpptnn)
        call jecreo(gpptnn, 'G N K24')
        call jeecra(gpptnn, 'NOMMAX', nb_group_new, ' ')
        call jecrec(grpnoe, 'G V I', 'NO '//gpptnn, 'DISPERSE', 'VARIABLE', nb_group_new)
        do i_group = 1, nb_group
            call jenuno(jexnum(grpnov, i_group), group_name)
            call jecroc(jexnom(grpnoe, group_name))
            call jeveuo(jexnum(grpnov, i_group), 'L', vi = v_list_old)
            call jelira(jexnum(grpnov, i_group), 'LONUTI', nb_enti)
            call jeecra(jexnom(grpnoe, group_name), 'LONMAX', max(nb_enti, 1))
            call jeecra(jexnom(grpnoe, group_name), 'LONUTI', nb_enti)
            call jeveuo(jexnom(grpnoe, group_name), 'E', vi = v_list_new)
            do i_enti = 1, nb_enti
                v_list_new(i_enti) = v_list_old(i_enti)
            enddo
        enddo
    endif
!
    call jedetr(grpnov)
!
end subroutine
