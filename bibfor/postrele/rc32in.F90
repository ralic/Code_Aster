subroutine rc32in()
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
#include "asterfort/getvr8.h"
#include "asterfort/utmess.h"
#include "asterfort/jedema.h"
!     ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE B3200 et ZE200
!     RECUPERATION
!          DE  C1, C2, K1, K2, C3, K3      SOUS INDI_SIGM
!          DE  DIAM, INERTIE, EP           SOUS CARAC_TUYAU
!          DE  KT_SN, KT_SP                SOUS FACT_SIGM
!     ------------------------------------------------------------------
!
    integer :: ndim, jvalin, n1, ktsn, ktsp, n2, n2a, n2b, n2c, n2d
    real(kind=8) :: bid
    integer :: n2e, n2f, n2g, n2h, i, j
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    ndim = 19
    call wkvect('&&RC3200.INDI', 'V V R', ndim, jvalin)
!
! --- indices de contrainte et carac. géométriques de la tuyauterie
! ----- cas du ze200
    call getvr8('INDI_SIGM', 'K1', nbval=0, iocc=1, nbret=n1)
    if (n1 .ne. 0) then
        call getvr8('INDI_SIGM', 'K1', scal=zr(jvalin), iocc=1, nbret=n1)
        call getvr8('INDI_SIGM', 'C1', scal=zr(jvalin+1), iocc=1, nbret=n1)  
        call getvr8('INDI_SIGM', 'K2', scal=zr(jvalin+2), iocc=1, nbret=n1)  
        call getvr8('INDI_SIGM', 'C2', scal=zr(jvalin+3), iocc=1, nbret=n1)
        call getvr8('INDI_SIGM', 'K3', scal=zr(jvalin+4), iocc=1, nbret=n1)  
        call getvr8('INDI_SIGM', 'C3', scal=zr(jvalin+5), iocc=1, nbret=n1)
!
        call getvr8('TUYAU', 'R', scal=zr(jvalin+6),iocc=1, nbret=n1)
        call getvr8('TUYAU', 'EP', scal=zr(jvalin+7),iocc=1, nbret=n1)
        call getvr8('TUYAU', 'I', scal=zr(jvalin+8),iocc=1, nbret=n1)
        do 20 i =1,8
            zr(jvalin+i+10) = 1
 20     continue
        zr(jvalin+15) =0
        zr(jvalin+17) =0
!------ cas corps-tubulure
        call getvr8('CHAR_MECA', 'MX_TUBU', iocc=1, scal=bid, nbret=n2)
        if (n2 .ne. 0) then
            call getvr8('INDI_SIGM', 'K2_TUBU', scal=zr(jvalin+11), iocc=1, nbret=n2a)  
            call getvr8('INDI_SIGM', 'C2_TUBU', scal=zr(jvalin+12), iocc=1, nbret=n2b)
            call getvr8('INDI_SIGM', 'K2_CORP', scal=zr(jvalin+13), iocc=1, nbret=n2c)  
            call getvr8('INDI_SIGM', 'C2_CORP', scal=zr(jvalin+14), iocc=1, nbret=n2d)
            call getvr8('TUYAU', 'R_TUBU', scal=zr(jvalin+15),iocc=1, nbret=n2e)
            call getvr8('TUYAU', 'I_TUBU', scal=zr(jvalin+16),iocc=1, nbret=n2f)
            call getvr8('TUYAU', 'R_CORP', scal=zr(jvalin+17),iocc=1, nbret=n2g)
            call getvr8('TUYAU', 'I_CORP', scal=zr(jvalin+18),iocc=1, nbret=n2h)
            if (n2a*n2b*n2c*n2d*n2e*n2f*n2g*n2h .eq. 0) then
                call utmess('F', 'POSTRCCM_46')
            endif
        endif
! ----- cas du b3200
    else
        do 10 j =1,19
            zr(jvalin+j-1) = 1
 10     continue
        zr(jvalin+15) =0
        zr(jvalin+17) =0
    endif
!
! --- facteur de concentration de contrainte (b3200 uniquement)
    zr(jvalin+9)  = 1
    zr(jvalin+10) = 1
!
    call getvr8('FACT_SIGM', 'KT_SN', iocc=1, nbval=0, nbret=ktsn)
    if (ktsn .ne. 0) then
        call getvr8('FACT_SIGM', 'KT_SN', scal=zr(jvalin+9), iocc=1, nbret=ktsn)
    endif
!
    call getvr8('FACT_SIGM', 'KT_SP', iocc=1, nbval=0, nbret=ktsp)
    if (ktsp .ne. 0) then
        call getvr8('FACT_SIGM', 'KT_SP', scal=zr(jvalin+10), iocc=1, nbret=ktsp)
    endif                 
!
    call jedema()    
end subroutine
