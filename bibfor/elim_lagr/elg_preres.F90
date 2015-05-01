subroutine elg_preres(solve1, base, iret, matpre, matas1,&
                      npvneg, istop)
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/copisd.h"
#include "asterfort/elg_calc_matk_red.h"
#include "asterfort/elg_gest_common.h"
#include "asterfort/gcncon.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/prere1.h"
!-----------------------------------------------------------------------
! But : faire "preres" si ELIM_MAGR='OUI'
!-----------------------------------------------------------------------
!
    character(len=19) :: matas1, solve1
    character(len=*) :: base, matpre
    integer :: istop, iret
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
    character(len=19) :: matas2, solve2
    integer ::   npvneg
    character(len=24), pointer :: slvk(:) => null()
    character(len=24), pointer :: refa(:) => null()
!
!
    call jemarq()
!
!
!   -- ON CREE LA MATRICE (REDUITE) MATAS2
    call gcncon('_', matas2)
    call elg_gest_common('NOTE', matas1, matas2, ' ')
    call elg_calc_matk_red(matas1, solve1, matas2, 'V')
!
!   -- ON DUPLIQUE SOLVE1 EN CHANGEANT ELIM_LAGR: OUI -> NON
    call gcncon('_', solve2)
    call copisd('SOLVEUR', 'V', solve1, solve2)
    call jeveuo(solve2//'.SLVK', 'L', vk24=slvk)
    slvk(13)='NON'
    call jeveuo(matas2//'.REFA', 'E', vk24=refa)
    refa(7)=solve2
!
!   --  ON APPELLE PRERE1 AVEC MATAS2 ET SOLVE2 :
    call prere1(' ', base, iret, matpre, matas2,&
                npvneg, istop)
!
!
    call jedema()
end
