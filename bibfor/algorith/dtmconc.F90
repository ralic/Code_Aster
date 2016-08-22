subroutine dtmconc(sd_dtm_)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/mdlibe.h"
#include "asterfort/mdtr74grd.h"
#include "asterfort/nlget.h"
#include "asterfort/refdcp.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
!
!   -0.1- Input/output arguments
    character(len=*), intent(in) :: sd_dtm_
!
!   -0.2- Local variables
    integer           :: nbmode, nbsauv, nbnoli, nbvint
    integer           :: jdepl, jvite, jacce, jdisc, jordr
    integer           :: jptem, jvint
    integer           :: i, iarch_sd, decal(3)
    integer           :: nbstoc, copysize
    character(len=4)  :: intk
    character(len=8)  :: sd_dtm, result, sd_nl

    integer         , pointer :: vindx(:)  => null()
    integer         , pointer :: isto(:)   => null()
    integer         , pointer :: allocs(:) => null()
    character(len=8), pointer :: nomres(:) => null()
    integer         , pointer :: sizres(:) => null()
!
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

!   2 - Free up the memory used by the last temporary result structure
    call dtmget(sd_dtm, _NB_NONLI,iscal=nbnoli)
    if (nbnoli.gt.0) then
        call dtmget(sd_dtm, _SD_NONL  , kscal=sd_nl)
        call mdlibe(nomres(iarch_sd), nbnoli)
    end if

    call dtmget(sd_dtm, _CALC_SD ,kscal=result)

!   3 - Allocate the final result using dtmallo, set the iarch_sd flag to 0 and the size
!       of the allocation vectors to the cumulated value
    call dtmsav(sd_dtm, _IARCH_SD, 1, iscal=0)
    call dtmsav(sd_dtm, _ARCH_NB , 1, iscal=nbsauv)
    call dtmallo(sd_dtm)

    call dtmget(sd_dtm, _IND_ALOC, vi=allocs)


!   ----------------------------------
!   decal : Array index-sliders
!   1 : ORDR, DISC, PTEM 
!   2 : DEPL, VITE, ACCE
!   3 : VINT
    decal = [0, 0, 0]
!   ------------------------------------
!
!   4 - Copying all information from &&ADXXXX to the result container (CALC_SD)

    call refdcp(nomres(1), result)

    do i = 1, iarch_sd

!       --- Special care is needed for the last data structure
        copysize = sizres(i)
        if (i.eq.iarch_sd) copysize = nbsauv - decal(1)
        nbstoc=nbmode*copysize
!
        call jeveuo(nomres(i)//'           .ORDR', 'L', jordr)
        call jacopo(copysize, 'I', jordr, allocs(1)+decal(1))
        call jedetr(nomres(i)//'           .ORDR')
!
        call jeveuo(nomres(i)//'           .DEPL', 'L', jdepl)
        call dcopy(nbstoc, zr(jdepl), 1, zr(allocs(4)+decal(2)), 1)
        call jedetr(nomres(i)//'           .DEPL')
!
        call jeveuo(nomres(i)//'           .VITE', 'L', jvite)
        call dcopy(nbstoc, zr(jvite), 1, zr(allocs(5)+decal(2)), 1)
        call jedetr(nomres(i)//'           .VITE')
!
        call jeveuo(nomres(i)//'           .ACCE', 'L', jacce)
        call dcopy(nbstoc, zr(jacce), 1, zr(allocs(6)+decal(2)), 1)
        call jedetr(nomres(i)//'           .ACCE')
!
        call jeveuo(nomres(i)//'           .DISC', 'L', jdisc)
        call dcopy(copysize, zr(jdisc), 1, zr(allocs(2)+decal(1)), 1)
        call jedetr(nomres(i)//'           .DISC')
!
        call jeveuo(nomres(i)//'           .PTEM', 'L', jptem)
        call dcopy(copysize, zr(jptem), 1, zr(allocs(3)+decal(1)), 1)
        call jedetr(nomres(i)//'           .PTEM')
!
!       Nonlinearities
        if (nbnoli .gt. 0) then
            call nlget(sd_nl, _INTERNAL_VARS_INDEX, vi=vindx)
            nbvint = vindx(nbnoli+1)-1

            call jeveuo(nomres(i)//'        .NL.VINT', 'L', jvint)
            call dcopy (nbvint*copysize, zr(jvint), 1, zr(allocs(7)+decal(3)), 1)
            call jedetr(nomres(i)//'        .NL.VINT')
!
        endif

        decal(1) = decal(1) + copysize
        decal(2) = decal(2) + nbmode*copysize
        decal(3) = decal(3) + nbvint*copysize

        call jedetc('V', nomres(i), 1)
    enddo

    AS_DEALLOCATE(vk8=nomres)
    AS_DEALLOCATE(vi =sizres)
!
    call jedema()
end subroutine
