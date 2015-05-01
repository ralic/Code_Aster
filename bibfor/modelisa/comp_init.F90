subroutine comp_init(mesh, compor, base, nb_cmp)
!
    implicit none
!
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
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
      character(len=19) , intent(in) :: compor
      character(len=1) , intent(in) :: base
      integer, intent(out) :: nb_cmp
!
! --------------------------------------------------------------------------------------------------
!
! <CARTE> COMPOR
!
! Initialization
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh     : name of mesh
! In  compor   : name of <CARTE> COMPOR
! In  base     : base where create <CARTE> COMPOR
! Out nb_cmp   : number of components in <CARTE> COMPOR
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nume_gd
    integer :: nb_cmp_max, icmp
    character(len=8) :: name_gd
    character(len=16), pointer :: p_compor_valv(:) => null()
    character(len=8) , pointer :: p_cata_nomcmp(:) => null()
    character(len=8) , pointer :: p_compor_ncmp(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    nb_cmp  = 0
    name_gd = 'COMPOR'   
!
! - Read catalog
!
    call jenonu(jexnom('&CATA.GD.NOMGD', name_gd), nume_gd)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', nume_gd), 'L', vk8 = p_cata_nomcmp)
    call jelira(jexnum('&CATA.GD.NOMCMP', nume_gd), 'LONMAX', nb_cmp_max)
!
! - Allocate <CARTE>
!
    call alcart(base, compor, mesh, name_gd)
!
! - Acces to <CARTE>
!
    call jeveuo(compor(1:19)//'.NCMP', 'E', vk8  = p_compor_ncmp)
    call jeveuo(compor(1:19)//'.VALV', 'E', vk16 = p_compor_valv)
!
! - Init <CARTE>
!
    do icmp = 1, nb_cmp_max
        p_compor_ncmp(icmp) = p_cata_nomcmp(icmp)
        p_compor_valv(icmp) = ' '
    enddo
!
    nb_cmp = nb_cmp_max

end subroutine
