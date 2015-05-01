subroutine elg_kellag(matass, solveu, kellag)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
!-----------------------------------------------------------------------
! But : determiner si la fonctionnalite ELIM_LAGR est demandee
!-----------------------------------------------------------------------
    character(len=*), intent(in) :: matass
    character(len=*), intent(in) :: solveu
    character(len=3), intent(out) :: kellag
!
!-----------------------------------------------------------------------
! ----------------------------------------------------------------------
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
    integer ::   n1
    character(len=19) :: matas1, solve1
    character(len=3) :: kbid
    character(len=24), pointer :: slvk(:) => null()
! ----------------------------------------------------------------------
!
    call jemarq()
!
!
!     1. CALCUL DE KELLAG :
!     -------------------------------------
    matas1=matass
    solve1=solveu
    if (solve1 .eq. ' ') call dismoi('SOLVEUR', matas1, 'MATR_ASSE', repk=solve1)
    call jeveuo(solve1//'.SLVK', 'L', vk24=slvk)
    call jelira(solve1//'.SLVK', 'LONMAX', n1, kbid)
    ASSERT(n1.eq.14)
    kellag=slvk(13)(1:3)
    ASSERT(kellag.eq.' '.or.kellag.eq.'OUI'.or.kellag.eq.'NON')
!
    call jedema()
end
