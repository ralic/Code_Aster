subroutine op0158()
    implicit none
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
! person_in_charge: jacques.pellet at edf.fr
!     ------------------------------------------------------------------
!     OPERATEUR APPL_CINE_MATR
!     BUT: APPLIQUER LES CHAR_CINE SUR LA MATRICE
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtmchc.h"
#include "asterfort/titre.h"
#include "asterfort/utmess.h"
    character(len=19) :: mass
    character(len=16) :: concep, nomcmd
    character(len=8) :: matass, matfac
!
    integer :: ibid, ifm
    integer :: niv
    character(len=24), pointer :: refa(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(matfac, concep, nomcmd)
    call getvid('  ', 'MATR_ASSE', scal=matass, nbret=ibid)
    ASSERT(matass.eq.matfac)
    mass = matass
!
    call jeveuo(mass//'.REFA', 'L', vk24=refa)
    if (refa(3) .eq. 'ELIML') call mtmchc(mass, 'ELIMF')
    ASSERT(refa(3).ne.'ELIML')
!
!
    call titre()
    call jedema()
end subroutine
