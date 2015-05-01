subroutine vpnor2(nomcon, nbmode, numord, coef)
    implicit none
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/peenc2.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
!
    integer :: nbmode, numord(*)
    real(kind=8) :: coef(*)
    character(len=*) :: nomcon
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
!     NORMALISATION DE TOUS LES CHAMPS D'UN MODE_MECA
!
! IN  NOMCON : NOM DU CONCEPT RESULTAT DE TYPE MODE_MECA
! IN  NBMODE : NOMBRE DE MODES
! IN  NUMORD : NUMERO D'ORDRE
! IN  COEF  : COEFFICIENT REEL A APLLIQUER AUX CHAMPS
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    integer :: ibid, nbnosy, isy, im, iordr, iret, lvale, neq, ieq
    real(kind=8) :: rcoef
    character(len=8) :: typmod
    character(len=16) :: nomsym
    character(len=19) :: nomd2
    character(len=24) :: vale
!     ------------------------------------------------------------------
    data  vale  /'                   .VALE'/
!     ------------------------------------------------------------------
!
    call jemarq()
!
    nomd2 = nomcon
!
    call jelira(nomd2//'.DESC', 'NOMMAX', nbnosy)
    if (nbnosy .eq. 0) goto 9999
!
    do 10 isy = 1, nbnosy
        call jenuno(jexnum(nomd2//'.DESC', isy), nomsym)
        do 12 im = 1, nbmode
            iordr = numord(im)
            call rsexch(' ', nomcon, nomsym, iordr, vale(1:19),&
                        iret)
            if (iret .eq. 0) then
                call jeexin(vale(1:19)//'.VALE', ibid)
                if (ibid .gt. 0) then
                    vale=vale(1:19)//'.VALE'
                else
                    vale=vale(1:19)//'.CELV'
                endif
!
                call jelira(vale, 'TYPE', cval=typmod)
                if (nomsym(1:4) .eq. 'EFGE' .or. nomsym(1:4) .eq. 'SIGM' .or. nomsym(1:4)&
                    .eq. 'EPSI' .or. nomsym(1:4) .eq. 'SIEF' .or. nomsym(1:4) .eq. 'FORC'&
                    .or. nomsym( 1:4) .eq. 'REAC' .or. nomsym(1:4) .eq. 'DEGE') then
                    rcoef = coef(im)
                else if (nomsym(1:4) .eq. 'EQUI') then
                    call utmess('A', 'UTILITAI5_88', sk=nomsym)
                    goto 12
                    elseif ( nomsym(1:4) .eq. 'EPOT' .or. nomsym(1:4)&
                .eq. 'ECIN' ) then
                    rcoef = coef(im) * coef(im)
                    if (typmod(1:1) .eq. 'R') then
                        call peenc2(vale(1:19), rcoef)
                    else
                        call utmess('F', 'UTILITAI5_89')
                    endif
                    goto 12
                else
                    goto 12
                endif
                call jeveuo(vale, 'E', lvale)
                call jelira(vale, 'LONMAX', neq)
                if (typmod(1:1) .eq. 'R') then
                    do 20 ieq = 0, neq-1
                        zr(lvale+ieq) = zr(lvale+ieq) * rcoef
20                  continue
                else
                    call utmess('F', 'UTILITAI5_89')
                endif
            endif
12      continue
10  end do
!
!
9999  continue
    call jedema()
end subroutine
