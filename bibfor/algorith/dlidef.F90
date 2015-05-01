subroutine dlidef()
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!
!     COMMANDE : DEFI_LIST_ENTI/OPERATION='DEFI'
!
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/getvis.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    integer :: idebut, irest, iii, ipdt
    integer :: vali(2)
    character(len=8) :: resu
    character(len=16) :: nomcmd, concep
    integer :: i, j, ndim, nbvale, nv, jval, jbor, nbocc, jnbp, n1, nbval
    integer :: kval, ico, np, jpas, iocc
!     ------------------------------------------------------------------
    call jemarq()
!
    nbval = 1
!
    call getres(resu, concep, nomcmd)
    call getvis(' ', 'VALE', nbval=0, nbret=nv)
    call getvis(' ', 'DEBUT', scal=idebut, nbret=n1)
    call getfac('INTERVALLE', nbocc)
!
    if (nv .ne. 0) then
!
    else
        call wkvect('&&DLIDEF.BORNE', 'V V I', nbocc+1, jbor)
        zi(jbor) = idebut
        do 10 iocc = 1, nbocc
            call getvis('INTERVALLE', 'JUSQU_A', iocc=iocc, scal=zi(jbor+ iocc), nbret=n1)
            iii = zi(jbor+iocc) - zi(jbor-1+iocc)
            if (iii .le. 0) then
                vali(1) = zi(jbor+iocc-1)
                vali(2) = zi(jbor+iocc)
                call utmess('F', 'ALGORITH13_78', ni=2, vali=vali)
            endif
            call getvis('INTERVALLE', 'PAS', iocc=iocc, nbval=0, nbret=np)
            if (np .ne. 0) then
                call getvis('INTERVALLE', 'PAS', iocc=iocc, scal=jpas, nbret=n1)
                jnbp = int(iii/jpas)
                irest = iii - jnbp*jpas
                if (irest .ne. 0) then
                    vali(1) = jpas
                    vali(2) = iocc
                    call utmess('F', 'ALGORITH13_79', ni=2, vali=vali)
                endif
!
            else
                call getvis('INTERVALLE', 'NOMBRE', iocc=iocc, scal=jnbp, nbret=n1)
                if (jnbp .gt. 0) then
                    ipdt = int(iii/jnbp)
                    irest = iii - jnbp*ipdt
                    if (irest .ne. 0) then
                        vali(1) = jnbp
                        vali(2) = iocc
                        call utmess('F', 'ALGORITH13_80', ni=2, vali=vali)
                    endif
                endif
            endif
10      continue
    endif
!
!
    if (nv .ne. 0) then
        nbvale = -nv
        ndim = max(1,nbvale-1)
        call wkvect(resu//'           .LPAS', 'G V I', ndim, jpas)
        call wkvect(resu//'           .NBPA', 'G V I', ndim, jnbp)
        call wkvect(resu//'           .BINT', 'G V I', nbvale, jbor)
        call wkvect(resu//'           .VALE', 'G V I', nbvale, jval)
        call wkvect('&&DLIDEF.VALE', 'V V I', nbvale, kval)
        call getvis(' ', 'VALE', nbval=nbvale, vect=zi(kval), nbret=nv)
        do 20 i = 1, nbvale - 1
            if (zi(kval+i-1) .ge. zi(kval+i)) then
                vali(1) = zi(kval+i-1)
                vali(2) = zi(kval+i)
                call utmess('F', 'ALGORITH13_81', ni=2, vali=vali)
            endif
            zi(jpas+i-1) = zi(kval+i) - zi(kval+i-1)
            zi(jnbp+i-1) = 1
            zi(jbor+i-1) = zi(kval+i-1)
            zi(jval+i-1) = zi(kval+i-1)
20      continue
        zi(jbor+nbvale-1) = zi(kval+nbvale-1)
        zi(jval+nbvale-1) = zi(kval+nbvale-1)
!
    else
!
        call wkvect(resu//'           .LPAS', 'G V I', max(1, nbocc), jpas)
        call wkvect(resu//'           .NBPA', 'G V I', max(1, nbocc), jnbp)
        call wkvect(resu//'           .BINT', 'G V I', nbocc+1, jbor)
!
        zi(jbor) = idebut
        do 30 iocc = 1, nbocc
            call getvis('INTERVALLE', 'JUSQU_A', iocc=iocc, scal=zi(jbor+ iocc), nbret=n1)
            iii = zi(jbor+iocc) - zi(jbor-1+iocc)
            call getvis('INTERVALLE', 'PAS', iocc=iocc, nbval=0, nbret=np)
            if (np .ne. 0) then
                call getvis('INTERVALLE', 'PAS', iocc=iocc, scal=zi(jpas+ iocc-1), nbret=n1)
                zi(jnbp+iocc-1) = iii/zi(jpas+iocc-1)
!
            else
                call getvis('INTERVALLE', 'NOMBRE', iocc=iocc, scal=zi(jnbp+iocc-1), nbret=n1)
                zi(jpas+iocc-1) = iii/zi(jnbp+iocc-1)
            endif
            nbval = nbval + zi(jnbp+iocc-1)
30      continue
!
!        --- ALLOCATION DE .VALE ET REMPLISSAGE DE CE DERNIER ---
        call wkvect(resu//'           .VALE', 'G V I', nbval, jval)
        zi(jval) = zi(jbor)
        ico = 0
        do 50 i = 1, nbocc
            ipdt = zi(jpas-1+i)
            do 40 j = 1, zi(jnbp-1+i) - 1
                ico = ico + 1
                zi(jval+ico) = zi(jval+ico-1) + ipdt
40          continue
            ico = ico + 1
            zi(jval+ico) = zi(jbor+i)
50      continue
    endif
!
    call jedema()
end subroutine
