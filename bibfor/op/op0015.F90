subroutine op0015()
    implicit none
!     ------------------------------------------------------------------
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
!     OPERATEUR RESOUDRE
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/chpver.h"
#include "asterfort/copisd.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/resoud.h"
#include "asterfort/titre.h"
!
    integer :: ifm, niv, nb, j1, mxiter, ier
    character(len=8) :: xsol, secmbr, matr, vcine, matf, metres, kvari
    character(len=16) :: concep, nomcmd
    character(len=19) :: solve1, solve2
    complex(kind=8) :: cbid
    real(kind=8) :: eps
    integer :: iret
    cbid = dcmplx(0.d0, 0.d0)
!     ------------------------------------------------------------------
    call jemarq()
!
!
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(xsol, concep, nomcmd)
!
    call getvid('  ', 'MATR', scal=matr, nbret=nb)
    ASSERT(nb.eq.1)
!
    matf=' '
    call getvid(' ', 'MATR_PREC', scal=matf, nbret=nb)
!
    call getvid('  ', 'CHAM_NO', scal=secmbr, nbret=nb)
    ASSERT(nb.eq.1)
    call chpver('F', secmbr, 'NOEU', '*', ier)
!
    vcine = ' '
    call getvid('  ', 'CHAM_CINE', scal=vcine, nbret=nb)
    if (nb .eq. 1) call chpver('F', vcine, 'NOEU', '*', ier)
!
!
!   -- CREATION D'1 SOLVEUR TEMPORAIRE : SOLVE2 (SAUF SI MUMPS)
    if (matf.eq.' ') then
        call dismoi('SOLVEUR', matr, 'MATR_ASSE', repk=solve1)
    else
        call dismoi('SOLVEUR', matf, 'MATR_ASSE', repk=solve1)
    endif
    call jeveuo(solve1//'.SLVK', 'E', j1)
    metres=zk24(j1-1+1)
    if (metres .ne. 'MUMPS' .and. metres .ne. 'PETSC') then
        solve2='&&OP0015.SOLVEUR'
        call copisd('SOLVEUR', 'V', solve1, solve2)
    else
!       -- MUMPS COMME PETSC VERIFIENT QUE LE SOLVEUR LORS DE RESOUD
!          EST LE MEME QUE CELUI DE PRERES. ON EST DONC OBLIGE DE LE
!          MODIFIER
        solve2=solve1
    endif
!
!     -- MODIFICATION DU SOLVEUR DU FAIT DE CERTAINS MOTS CLES :
    call getvr8(' ', 'RESI_RELA', scal=eps, nbret=nb)
    if (nb .eq. 1) then
        call jeveuo(solve2//'.SLVR', 'E', j1)
        zr(j1-1+2)=eps
    endif
    call getvtx(' ', 'POSTTRAITEMENTS', scal=kvari, nbret=nb)
    if (nb .eq. 1) then
        call jeveuo(solve2//'.SLVK', 'E', j1)
        zk24(j1-1+11)=kvari
    endif
    call getvis(' ', 'NMAX_ITER', scal=mxiter, nbret=nb)
    if (nb .eq. 1) then
        call jeveuo(solve2//'.SLVI', 'E', j1)
        zi(j1-1+2)=mxiter
    endif
    call getvtx(' ', 'ALGORITHME', scal=kvari, nbret=nb)
    if ((nb.eq.1) .and. (metres.eq.'PETSC')) then
        call jeveuo(solve2//'.SLVK', 'E', j1)
        zk24(j1-1+6)=kvari
    endif
!
!     -- APPEL A LA ROUTINE RESOUD :
    call resoud(matr, matf, solve2, vcine, 0,&
                secmbr, xsol, 'G', [0.d0], [cbid],&
                ' ', .true._1, 0, iret)
!
    if (metres .ne. 'MUMPS' .and. metres .ne. 'PETSC') then
        call detrsd('SOLVEUR', solve2)
    endif
!
!
    call titre()
    call jedema()
end subroutine
