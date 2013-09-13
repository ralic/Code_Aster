subroutine op0158()
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/u2mess.h"
    character(len=19) :: mass
    character(len=16) :: concep, nomcmd
    character(len=8) :: matass, matfac
    integer :: jrefa
!
    character(len=16) :: metres
    integer :: ibid, ifm
    integer :: niv
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
    call dismoi('F', 'METH_RESO', mass, 'MATR_ASSE', ibid,&
                metres, ibid)
!
    if (metres .ne. 'LDLT' .and. metres .ne. 'MULT_FRONT' .and. metres .ne. 'MUMPS') then
        call u2mess('F', 'ALGELINE4_1')
    endif
!
    call jeveuo(mass//'.REFA', 'L', jrefa)
    if (zk24(jrefa-1+3) .eq. 'ELIML') call mtmchc(mass, 'ELIMF')
    ASSERT(zk24(jrefa-1+3).ne.'ELIML')
!
!
    call titre()
    call jedema()
end subroutine
