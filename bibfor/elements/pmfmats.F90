subroutine pmfmats(icdmat, nomats)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! ----------------------------------------------------------------------------
!
!  Retourne le nom du materiau "section" (MATER_SEC) de l'element PMF courant
!  Si l'element n'est pas PMF, retourne : ' '
!
! ----------------------------------------------------------------------------
!   in
!       icdmat  : materiau code (zi(imate))
!   out
!       nomats     : nom du materiau "section"
! ----------------------------------------------------------------------------
!
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lteatt.h"
!
    integer, intent(in) :: icdmat
    character(len=*), intent(out) :: nomats
    integer :: icompo, isdcom, nbgfmx
    integer, pointer :: cpri(:) => null()
!-----------------------------------------------------------------------
    if (.not.lteatt('TYPMOD2','PMF')) then
        nomats=' '
    else
        call jevech('PCOMPOR', 'L', icompo)
        call jeveuo(zk16(icompo-1+7), 'L', isdcom)

        call jeveuo(zk16(icompo-1+7)(1:8)//'.CPRI', 'L', vi=cpri)
        nbgfmx=cpri(3)
        nomats=zk24(isdcom-1+nbgfmx*6+1)(1:8)
    endif

end subroutine
