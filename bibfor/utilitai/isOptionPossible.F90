subroutine isOptionPossible(ligrel_, option_, para_name_,&
                            l_all_ , l_some_)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/nbgrel.h"
#include "asterfort/typele.h"
#include "asterfort/modat2.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/teattr.h"
#include "asterfort/jeveuo.h"
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
    character(len=*), intent(in) :: ligrel_
    character(len=*), intent(in) :: option_
    character(len=*), intent(in) :: para_name_
    aster_logical, optional, intent(out) :: l_all_
    aster_logical, optional, intent(out) :: l_some_
!
! --------------------------------------------------------------------------------------------------
!
! Utility
!
! Check if option is possible on all elements in LIGREL
!
! --------------------------------------------------------------------------------------------------
!
! In  ligrel           : list of GRoup of ELements
! In  option           : name of option to check
! In  para_name        : name of parameter to describe field
! Out l_all            : on all elements
! Out l_some           : on some elements
!
! --------------------------------------------------------------------------------------------------
!
    character(len=19) :: ligrel
    character(len=16) :: option, nomte
    character(len=8) :: para_name, dmo, dma
    integer :: indx_option
    integer :: igrel, ngrel, te, mode, nel, ier
    integer, pointer :: v_liel(:) => null()
    aster_logical :: l_all, l_some
!
! --------------------------------------------------------------------------------------------------
!
    ligrel    = ligrel_
    option    = option_
    para_name = para_name_
    l_all     = .true.
    l_some    = .false.
!
! - Access to catalog
!
    call jenonu(jexnom('&CATA.OP.NOMOPT', option), indx_option)
!
! - Loop on GREL
!
    ngrel     = nbgrel(ligrel)
    do igrel = 1, ngrel
        te    = typele(ligrel, igrel)
        mode  = modat2(indx_option, te, para_name) 
        call jeveuo(jexnum(ligrel//'.LIEL', igrel), 'L', vi = v_liel)
        call jelira(jexnum(ligrel//'.LIEL', igrel), 'LONMAX', nel)
        call jenuno(jexnum('&CATA.TE.NOMTE', v_liel(nel)), nomte)
        call teattr('S', 'DIM_TOPO_MODELI', dmo, ier, typel=nomte)
        call teattr('S', 'DIM_TOPO_MAILLE', dma, ier, typel=nomte)
        if (dmo .eq. dma) then
            if (mode .eq. 0) then
                l_all  = .false.
            else
                l_some = .true.
            endif
        endif
    end do
!
    if (present(l_all_)) then
        l_all_  = l_all
    endif
    if (present(l_some_)) then
        l_some_ = l_some
    endif
!
end subroutine
