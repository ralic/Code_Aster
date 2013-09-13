subroutine op0094()
    implicit none
! ----------------------------------------------------------------------
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
!
!     COMMANDE DEFI_TRC
!
! ----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/getvr8.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/tbajli.h"
#include "asterfort/tbajpa.h"
#include "asterfort/tbcrsd.h"
#include "asterfort/wkvect.h"
    integer :: nbhist, nbtrc, nbparr, ibid, lonmax, nbval, i, j, jvale, nbv, ind
    parameter    ( nbparr = 19 )
    real(kind=8) :: rbid, xnbv, vale(6)
    character(len=8) :: k8b, nomtrc, typarr(nbparr)
    character(len=16) :: concep, nomcmd, noparr(nbparr)
    complex(kind=8) :: c16b
    integer :: iarg
!
    data noparr / 'VITESSE' , 'PARA_EQ' , 'COEF_0' , 'COEF_1' ,&
     &              'COEF_2' , 'COEF_3' , 'COEF_4' , 'COEF_5' ,&
     &              'NB_POINT' ,&
     &              'Z1' , 'Z2' , 'Z3' , 'TEMP' ,&
     &              'SEUIL' , 'AKM' , 'BKM' , 'TPLM',&
     &              'DREF', 'A' /
    data typarr / 'R' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R' , 'R',&
     &              'R' , 'R' , 'R' , 'R' ,&
     &              'R' , 'R' , 'R' , 'R',&
     &              'R' , 'R' /
!     ------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(nomtrc, concep, nomcmd)
!
    call getfac('HIST_EXP', nbhist)
    call getfac('TEMP_MS', nbtrc)
!
!
    call tbcrsd(nomtrc, 'G')
    call tbajpa(nomtrc, nbparr, noparr, typarr)
!
    lonmax = 0
    do 100 i = 1, nbhist
        call getvr8('HIST_EXP', 'VALE', iocc=i, nbval=0, nbret=nbval)
        lonmax = max ( lonmax , -nbval )
100  end do
    call wkvect('&&OP0094.VALE', 'V V R', lonmax, jvale)
!
    do 110 i = 1, nbhist
        call getvr8('HIST_EXP', 'VALE', iocc=i, nbval=0, nbret=nbval)
        nbval = -nbval
        call getvr8('HIST_EXP', 'VALE', iocc=i, nbval=nbval, vect=zr(jvale))
        call tbajli(nomtrc, 8, noparr, ibid, zr(jvale),&
                    c16b, k8b, 0)
        xnbv = dble(( nbval - 8 ) / 4 )
        call tbajli(nomtrc, 1, noparr(9), ibid, xnbv,&
                    c16b, k8b, i)
110  end do
!
    do 120 i = 1, nbhist
        call getvr8('HIST_EXP', 'VALE', iocc=i, nbval=0, nbret=nbval)
        nbval = -nbval
        call getvr8('HIST_EXP', 'VALE', iocc=i, nbval=nbval, vect=zr(jvale))
        nbv = ( nbval - 8 ) / 4
        do 122 j = 1, nbv
            ind = jvale + 8 + 4*(j-1)
            call tbajli(nomtrc, 4, noparr(10), ibid, zr(ind),&
                        c16b, k8b, 0)
122      continue
120  end do
!
!
!
    do 200 i = 1, nbtrc
        call getvr8('TEMP_MS', 'SEUIL', iocc=i, scal=vale(1), nbret=ibid)
        call getvr8('TEMP_MS', 'AKM', iocc=i, scal=vale(2), nbret=ibid)
        call getvr8('TEMP_MS', 'BKM', iocc=i, scal=vale(3), nbret=ibid)
        call getvr8('TEMP_MS', 'TPLM', iocc=i, scal=vale(4), nbret=ibid)
        call getvr8('GRAIN_AUST', 'DREF', iocc=i, scal=vale(5), nbret=ibid)
        if (ibid .eq. 0) vale(5) = 0.d0
        call getvr8('GRAIN_AUST', 'A', iocc=i, scal=vale(6), nbret=ibid)
        if (ibid .eq. 0) vale(6) = 0.d0
        call tbajli(nomtrc, 6, noparr(14), ibid, vale,&
                    c16b, k8b, 0)
200  end do
!
    call jedema()
end subroutine
