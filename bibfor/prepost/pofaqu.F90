subroutine pofaqu()
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
!     -----------------------------------------------------------------
!     COMMANDE POST_FATIGUE
!              CHARGEMENT QUELCONQUE
!     -----------------------------------------------------------------
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/fgdomm.h"
#include "asterfort/fglema.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    integer :: n1, n2, n3, n4, n5, n6, nbf, nbptot, nbpts, i, j, ibid
    integer :: ifonc1, ifonc, nbpapf,   ivdome
    real(kind=8) :: rdomm, val(2)
    complex(kind=8) :: cbid
    character(len=8) :: k8b, nomten(6), nommat, kdomm, result, nomp, nomt, txcum
    character(len=16) :: nomcmd
    character(len=24) :: fvale(6)
!     --- POST_FATI_QUELC ----------------------------------------------
    parameter    ( nbpapf = 3  )
    character(len=1) :: typppf(nbpapf)
    character(len=16) :: nomppf(nbpapf)
    real(kind=8), pointer :: defpla(:) => null()
    real(kind=8), pointer :: ordo(:) => null()
    real(kind=8), pointer :: temp(:) => null()
    data  nomppf /  'INST' , 'DOMMAGE' , 'DOMM_CUMU' /
    data  typppf / 'R' , 'R' , 'R' /
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call getres(result, k8b, nomcmd)
!
!     --- RECUPERATION DE LA FONCTION CHARGEMENT ---
!
    call getvid('HISTOIRE', 'SIGM_XX', iocc=1, scal=nomten(1), nbret=n1)
    call getvid('HISTOIRE', 'SIGM_YY', iocc=1, scal=nomten(2), nbret=n2)
    call getvid('HISTOIRE', 'SIGM_ZZ', iocc=1, scal=nomten(3), nbret=n3)
    call getvid('HISTOIRE', 'SIGM_XY', iocc=1, scal=nomten(4), nbret=n4)
    call getvid('HISTOIRE', 'SIGM_XZ', iocc=1, scal=nomten(5), nbret=n5)
    call getvid('HISTOIRE', 'SIGM_YZ', iocc=1, scal=nomten(6), nbret=n6)
    nbf = n1 + n2 + n3 + n4 + n5 + n6
    call getvid('HISTOIRE', 'EPSP', iocc=1, scal=nomp, nbret=n1)
    call getvid('HISTOIRE', 'TEMP', iocc=1, scal=nomt, nbret=n1)
!
!     --- CHARGEMENT QUELCONQUE ---
!
    ibid=0
    cbid=(0.d0,0.d0)
    fvale(1) = nomten(1)//'           .VALE'
    call jelira(fvale(1), 'LONMAX', nbpts)
    nbptot = nbpts
    do 20 i = 2, nbf
        fvale(i) = nomten(i)//'           .VALE'
        call jelira(fvale(i), 'LONMAX', nbpts)
        if (nbpts .ne. nbptot) then
            call utmess('F', 'FATIGUE1_21')
        endif
20  end do
    AS_ALLOCATE(vr=ordo, size=nbptot/2*nbf)
    call jeveuo(fvale(1), 'L', ifonc1)
    do 30 i = 2, nbf
        call jeveuo(fvale(i), 'L', ifonc)
        do 35 j = 1, nbptot/2
            if (zr(ifonc+j-1) .ne. zr(ifonc1+j-1)) then
                call utmess('F', 'FATIGUE1_21')
            endif
            ordo(1+(j-1)*nbf+i-1) = zr(ifonc+nbptot/2+j-1)
35      continue
30  end do
    nbptot = nbptot / 2
    do 40 j = 1, nbptot
        ordo(1+(j-1)*nbf) = zr(ifonc1+nbptot+j-1)
40  end do
!
    fvale(1) = nomp//'           .VALE'
    call jelira(fvale(1), 'LONMAX', nbpts)
    if (nbpts .ne. nbptot*2) then
        call utmess('F', 'FATIGUE1_22')
    endif
    AS_ALLOCATE(vr=defpla, size=nbptot)
    call jeveuo(fvale(1), 'L', ifonc)
    do 45 j = 0, nbptot-1
        if (zr(ifonc+j) .ne. zr(ifonc1+j)) then
            call utmess('F', 'FATIGUE1_22')
        endif
        defpla(1+j) = zr(ifonc+nbptot+j)
45  end do
!
    fvale(1) = nomt//'           .VALE'
    call jelira(fvale(1), 'LONMAX', nbpts)
    if (nbpts .ne. nbptot*2) then
        call utmess('F', 'FATIGUE1_23')
    endif
    AS_ALLOCATE(vr=temp, size=nbptot)
    call jeveuo(fvale(1), 'L', ifonc)
    do 46 j = 0, nbptot-1
        if (zr(ifonc+j) .ne. zr(ifonc1+j)) then
            call utmess('F', 'FATIGUE1_23')
        endif
        temp(1+j) = zr(ifonc+nbptot+j)
46  end do
!
!     --- CREATION DE LA TABLE ---
!
    call tbcrsd(result, 'G')
    call tbajpa(result, nbpapf, nomppf, typppf)
!
    call getvid(' ', 'MATER', scal=nommat, nbret=n1)
!
!     --- CALCUL DU DOMMAGE ELEMENTAIRE ---
!
    kdomm = ' '
    call getvtx(' ', 'DOMMAGE', scal=kdomm, nbret=n1)
!
    call wkvect('&&POFAQU.DOMM.ELEM', 'V V R', nbptot, ivdome)
!
!     --- CALCUL DU DOMMAGE ELEMENTAIRE DE LEMAITRE GENERALISE
!         -----------------------------------------------------
    if (kdomm .eq. 'LEMAITRE') then
        call fglema(nbf, nbptot, ordo, defpla, temp,&
                    nommat, zr(ivdome))
    else
        call utmess('F', 'FATIGUE1_20')
    endif
!
    do 50 i = 1, nbptot
        val(1) = zr(ifonc1+i-1)
        val(2) = zr(ivdome+i-1)
        call tbajli(result, 2, nomppf, [ibid], val,&
                    [cbid], k8b, 0)
50  end do
!
!     --- CALCUL DU DOMMAGE TOTAL ---
!
    txcum = ' '
    call getvtx(' ', 'CUMUL', scal=txcum, nbret=n1)
    if (txcum .eq. 'LINEAIRE') then
!
        call fgdomm(nbptot, zr(ivdome), rdomm)
!
        call tbajli(result, 1, nomppf(3), [ibid], [rdomm],&
                    [cbid], k8b, 0)
!
    endif
!
    AS_DEALLOCATE(vr=ordo)
    AS_DEALLOCATE(vr=defpla)
    AS_DEALLOCATE(vr=temp)
    call jedetr('&&POFAQU.DOMM.ELEM')
!
    call jedema()
end subroutine
