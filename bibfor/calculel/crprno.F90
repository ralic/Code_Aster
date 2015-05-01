subroutine crprno(prof_chnoz, base, meshz, gran_namez, nb_equa)
!
implicit none
!
#include "asterfort/dismoi.h"
#include "asterfort/assert.h"
#include "asterfort/jenonu.h"
#include "asterfort/profchno_crsd.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=*), intent(in) :: prof_chnoz
    character(len=1), intent(in) :: base
    character(len=*), intent(in) :: gran_namez
    character(len=*), intent(in) :: meshz
    integer, intent(in) :: nb_equa
!
! --------------------------------------------------------------------------------------------------
!
! Create PROF_CHNO only on mesh
!
! --------------------------------------------------------------------------------------------------
!
! In  prof_chno   : name of PROF_CHNO
! In  base        : JEVEUX base to create PROF_CHNO
! In  nb_equa     : number of equations
! In  gran_name   : name of GRANDEUR
! In  mesh        : name of mesh
!
! --------------------------------------------------------------------------------------------------
!
    integer :: i_ligr_mesh
    character(len=24) :: lili
!
! --------------------------------------------------------------------------------------------------
!
    call profchno_crsd(prof_chnoz, base, nb_equa, meshz = meshz, &
                       gran_namez = gran_namez)

    lili = prof_chnoz(1:19)//'.LILI'
    call jenonu(jexnom(lili, '&MAILLA'), i_ligr_mesh)
    ASSERT(i_ligr_mesh.eq.1)

end subroutine
