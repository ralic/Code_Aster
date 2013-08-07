subroutine rvrecu(mcf, iocc, champ, nomvec)
    implicit none
#include "jeveux.h"
#include "asterc/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: iocc
    character(len=*) :: mcf, champ, nomvec
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
! IN  IOCC   : INDICE DE L' OCCURENCE
! IN  CHAMP  : NOM DU CHAMP A TRAITER
!     ------------------------------------------------------------------
!
    character(len=1) :: type
    character(len=8) ::  form
    character(len=19) :: nch19
    character(len=24) :: vecteu
!
    real(kind=8) :: a, b
    integer :: i, jval, kval, n1, neq
    integer :: iarg
!
!==================== CORPS DE LA ROUTINE =============================
!
    call jemarq()
    nch19 = champ
    vecteu = nomvec
    call jelira(nch19//'.VALE', 'TYPE', cval=type)
    if (type .ne. 'C') call u2mess('F', 'POSTRELE_11')
    call jelira(nch19//'.VALE', 'LONMAX', neq)
    call jeveuo(nch19//'.VALE', 'L', jval)
    call wkvect(vecteu, 'V V R', neq, kval)
!
    call getvtx(mcf, 'FORMAT_C', iocc, iarg, 1,&
                form, n1)
!
    if (form .eq. 'MODULE') then
        do 11 i = 0, neq-1
            a = dble( zc(jval+i) )
            b = dimag( zc(jval+i) )
            zr(kval+i) = sqrt( a*a + b*b )
11      continue
!
    else if (form .eq. 'REEL') then
        do 20 i = 0, neq-1
            zr(kval+i) = dble( zc(jval+i) )
20      continue
!
    else if (form .eq. 'IMAG') then
        do 30 i = 0, neq-1
            zr(kval+i) = dimag( zc(jval+i) )
30      continue
!
    else
        call u2mesk('F', 'POSTRELE_52', 1, form)
    endif
!
    call jedema()
end subroutine
