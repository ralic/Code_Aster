subroutine carc_init(mesh, carcri, nb_cmp)
!
    implicit none

#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
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
!
      character(len=8) , intent(in) :: mesh
      character(len=19) , intent(in) :: carcri
      integer, intent(out) :: nb_cmp
!
! --------------------------------------------------------------------------------------------------
!
! <CARTE> CARCRI
!
! Initialization
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh     : namle of mesh
! In  compor   : name of <CARTE> CARCRI
! Out nb_cmp   : number of components in <CARTE>
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nume_gd
    integer :: nb_cmp_max, icmp
    character(len=8) :: name_gd
    real(kind=8)    , pointer :: p_carcri_valv(:) => null()
    character(len=8), pointer :: p_cata_nomcmp(:) => null()
    character(len=8), pointer :: p_carcri_ncmp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_cmp  = 0
    name_gd = 'CARCRI'  
!
! - Read catalog
!
    call jenonu(jexnom('&CATA.GD.NOMGD', name_gd), nume_gd)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', nume_gd), 'L', vk8 = p_cata_nomcmp)
    call jelira(jexnum('&CATA.GD.NOMCMP', nume_gd), 'LONMAX', nb_cmp_max)
    ASSERT(nb_cmp_max .le. 21)
!
! - Allocate <CARTE>
!
    call alcart('V', carcri, mesh, name_gd)
!
! - Acces to <CARTE>
!
    call jeveuo(carcri(1:19)//'.NCMP', 'E', vk8 = p_carcri_ncmp)
    call jeveuo(carcri(1:19)//'.VALV', 'E', vr  = p_carcri_valv)
!
! - Init <CARTE>
!
    do icmp = 1, nb_cmp_max
        p_carcri_ncmp(icmp) = p_cata_nomcmp(icmp)
        p_carcri_valv(icmp) = 0.d0
    enddo
!
! - Default values
!
    p_carcri_valv(1) = 10
    p_carcri_valv(2) = 0
    p_carcri_valv(3) = 1.d-6
    p_carcri_valv(4) = 1.d0
!
    nb_cmp = nb_cmp_max

end subroutine
