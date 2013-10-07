subroutine comp_init(mesh, compor, base, nb_cmp)
!
    implicit none
!
#include "jeveux.h"
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
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! In  mesh     : namle of mesh
! In  compor   : name of <CARTE> COMPOR
! In  base     : base where create <CARTE> COMPOR
! Out nb_cmp   : number of components in <CARTE> COMPOR
!
! --------------------------------------------------------------------------------------------------
!
    integer :: nume_gd
    integer :: j_cmp, icmp, j_cart_cmp, j_cart_val
    integer :: nb_cmp_max
    character(len=8) :: k8dummy
    character(len=8) :: name_gd
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    nb_cmp  = 0
    name_gd = 'COMPOR'   
!
! - Read catalog
!
    call jenonu(jexnom('&CATA.GD.NOMGD', name_gd), nume_gd)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', nume_gd), 'L', j_cmp)
    call jelira(jexnum('&CATA.GD.NOMCMP', nume_gd), 'LONMAX', nb_cmp_max, k8dummy)
!
! - Allocate <CARTE>
!
    call alcart(base, compor, mesh, name_gd)
!
! - Acces to <CARTE>
!
    call jeveuo(compor(1:19)//'.NCMP', 'E', j_cart_cmp)
    call jeveuo(compor(1:19)//'.VALV', 'E', j_cart_val)
!
! - Init <CARTE>
!
    do icmp = 1, nb_cmp_max
        zk8(j_cart_cmp-1+icmp)  = zk8(j_cmp-1+icmp)
        zk16(j_cart_val-1+icmp) = ' '
    enddo
!
    nb_cmp = nb_cmp_max
!
    call jedema()
end subroutine
