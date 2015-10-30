subroutine dtmconc(sd_dtm_)
    implicit none
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
! person_in_charge: hassan.berro at edf.fr
!
! dtmconc : Append several dyna_gene data structures saved by an adaptative time-step
!           integration algorithm into a single result.
!
#include "jeveux.h"
#include "blas/dcopy.h"
#include "asterfort/codent.h"
#include "asterfort/dtmallo.h"
#include "asterfort/dtmget.h"
#include "asterfort/dtmsav.h"
#include "asterfort/jacopo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedetc.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mdtr74grd.h"
#include "asterfort/refdcp.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!   -0.1- Input/output arguments
    character(len=*), intent(in) :: sd_dtm_
!
!   -0.2- Local variables
    integer           :: nbmode, nbsauv, nbnoli, nbrede, nbrevi
    integer           :: jdepl, jvite, jacce, jdisc, jordr
    integer           :: jptem, jfcho, jdloc, jvcho, jicho
    integer           :: jvint, jredc, jredd, jrevc, jrevv
    integer           :: i, iarch_sd, decal(7), nbvint, jvir
    integer           :: nbsto1, nbstoc, copysize
    character(len=4)  :: intk
    character(len=8)  :: sd_dtm, result

    integer         , pointer :: isto(:)   => null()
    integer         , pointer :: allocs(:) => null()
    character(len=8), pointer :: nomres(:) => null()
    integer         , pointer :: sizres(:) => null()
!
#define saucho(ic,m) saucho_v((ic-1)*nbnoli+m) 

!   0 - Initializations
    call jemarq()

    sd_dtm = sd_dtm_

    call dtmget(sd_dtm, _NB_MODES, iscal=nbmode)
    call dtmget(sd_dtm, _IARCH_SD, iscal=iarch_sd)
    call dtmget(sd_dtm, _ARCH_STO, vi=isto)

!   1 - First, determine the exact size of the final data structure by looping over the
!       different &&ADXXXX dyna_gene structures

    AS_ALLOCATE(vk8=nomres, size=iarch_sd)
    AS_ALLOCATE(vi =sizres, size=iarch_sd)

    nbsauv = 0
    do i = 1, iarch_sd
        call codent(i, 'D0', intk)
        nomres(i) = '&&AD'//intk
        call jelira(nomres(i)//'           .ORDR','LONMAX', sizres(i))
        nbsauv = nbsauv + sizres(i)
        if (i.eq.iarch_sd) nbsauv = nbsauv + isto(1) - sizres(i)
    end do

    call dtmget(sd_dtm, _CALC_SD ,kscal=result)

!   2 - Allocate the final result using dtmallo, set the iarch_sd flag to 0 and the size
!       of the allocation vectors to the cumulated value
    call dtmsav(sd_dtm, _IARCH_SD, 1, iscal=0)
    call dtmsav(sd_dtm, _ARCH_NB , 1, iscal=nbsauv)
    call dtmallo(sd_dtm)

    call dtmget(sd_dtm, _IND_ALOC, vi=allocs)

    call dtmget(sd_dtm, _NB_NONLI,iscal=nbnoli)
    call dtmget(sd_dtm, _FX_NUMB,iscal=nbrede)
    call dtmget(sd_dtm, _FV_NUMB,iscal=nbrevi)

    if (nbnoli .ne. 0) call jeveuo(result//'           .VINT', 'E', jvir)

!   [jordr, jdisc, jptem, jdepl , jvite, jacce,  
!    jfcho, jdcho, jvcho, jadcho, jredc, jredd,
!    jrevc, jrevv                                 ]
    decal = [0, 0, 0, 0, 0, 0, 0]
!
!   3 - Copying all information from &&ADXXXX to the result container (CALC_SD)

    call refdcp(nomres(1), result)

    do i = 1, iarch_sd

!       --- Special care is needed for the last data structure
        copysize = sizres(i)
        if (i.eq.iarch_sd) copysize = nbsauv - decal(1)
!
        call jeveuo(nomres(i)//'           .DEPL', 'L', jdepl)
        call jeveuo(nomres(i)//'           .VITE', 'L', jvite)
        call jeveuo(nomres(i)//'           .ACCE', 'L', jacce)
        call jeveuo(nomres(i)//'           .DISC', 'L', jdisc)
        call jeveuo(nomres(i)//'           .ORDR', 'L', jordr)
        call jeveuo(nomres(i)//'           .PTEM', 'L', jptem)
!
        nbstoc=nbmode*copysize
!
        call jacopo(copysize, 'I', jordr, allocs(1)+decal(1))
        call dcopy(copysize, zr(jdisc), 1, zr(allocs(2)+decal(1)), 1)
        call dcopy(copysize, zr(jptem), 1, zr(allocs(3)+decal(1)), 1)
        call dcopy(nbstoc, zr(jdepl), 1, zr(allocs(4)+decal(2)), 1)
        call dcopy(nbstoc, zr(jvite), 1, zr(allocs(5)+decal(2)), 1)
        call dcopy(nbstoc, zr(jacce), 1, zr(allocs(6)+decal(2)), 1)
!
        call jedetr(nomres(i)//'           .DEPL')
        call jedetr(nomres(i)//'           .VITE')
        call jedetr(nomres(i)//'           .ACCE')
        call jedetr(nomres(i)//'           .DISC')
        call jedetr(nomres(i)//'           .ORDR')
        call jedetr(nomres(i)//'           .PTEM')
!
!       Nonlinearities
        if (nbnoli .ne. 0) then
            nbstoc = 3 * nbnoli * copysize
            nbsto1 = nbnoli * copysize
            nbvint = nbnoli * copysize * mdtr74grd('MAXVINT')

            call jeveuo(nomres(i)//'           .FCHO', 'L', jfcho)
            call jeveuo(nomres(i)//'           .DLOC', 'L', jdloc)
            call jeveuo(nomres(i)//'           .VCHO', 'L', jvcho)
            call jeveuo(nomres(i)//'           .ICHO', 'L', jicho)
            call jeveuo(nomres(i)//'           .VINT', 'L', jvint)
!
            call dcopy(nbstoc   , zr(jfcho), 1, zr(allocs(7)+decal(3)), 1)
            call dcopy(nbstoc   , zr(jdloc), 1, zr(allocs(8)+decal(3)), 1)
            call dcopy(nbstoc   , zr(jdloc+3*nbnoli*sizres(i)), &
                      1, zr(allocs(8)+3*nbnoli*nbsauv+decal(3)), 1)
!
            call dcopy(nbstoc   , zr(jvcho), 1, zr(allocs(9)+decal(3)), 1)
            call jacopo(nbsto1,'I', jicho, allocs(10)+decal(4))
            call dcopy(nbvint   , zr(jvint), 1, zr(jvir+decal(7)), 1)
!
            call jedetr(nomres(i)//'           .FCHO')
            call jedetr(nomres(i)//'           .DLOC')
            call jedetr(nomres(i)//'           .VCHO')
            call jedetr(nomres(i)//'           .ICHO')
            call jedetr(nomres(i)//'           .VINT')
        endif
!
!       RELA_EFFO_DEPL
        if (nbrede .ne. 0) then
            nbstoc = nbrede * copysize
            call jeveuo(nomres(i)//'           .REDC', 'L', jredc)
            call jeveuo(nomres(i)//'           .REDD', 'L', jredd)
!
            call jacopo(nbstoc, 'I', jredc, allocs(11)+decal(5))
            call dcopy(nbstoc, zr(jredd), 1, zr(allocs(12)+decal(5)), 1)
!
            call jedetr(nomres(i)//'           .REDC')
            call jedetr(nomres(i)//'           .REDD')
        endif
!
!       RELA_EFFO_VITE
        if (nbrevi .ne. 0) then
            nbstoc = nbrevi * copysize
            call jeveuo(nomres(i)//'           .REVC', 'L', jrevc)
            call jeveuo(nomres(i)//'           .REVV', 'L', jrevv)
!
            call jacopo(nbstoc, 'I', jrevc, allocs(13)+decal(6))
            call dcopy(nbstoc, zr(jrevv), 1, zr(allocs(14)+decal(6)), 1)
!
            call jedetr(nomres(i)//'           .REVC')
            call jedetr(nomres(i)//'           .REVV')
        endif

        decal(1) = decal(1) + copysize
        decal(2) = decal(2) + nbmode*copysize
        decal(3) = decal(3) + (3*nbnoli*copysize)
        decal(4) = decal(4) + (nbnoli*copysize)
        decal(5) = decal(5) + (nbrede*copysize)
        decal(6) = decal(6) + (nbrevi*copysize)
        decal(7) = decal(7) + nbnoli*copysize*mdtr74grd('MAXVINT')

        call jedetc('V', nomres(i), 1)
    enddo

    AS_DEALLOCATE(vk8=nomres)
    AS_DEALLOCATE(vi =sizres)
!
    call jedema()
end subroutine