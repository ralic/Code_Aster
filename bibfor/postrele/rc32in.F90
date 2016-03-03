subroutine rc32in()
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
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
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE B3200 et ZE200
!     RECUPERATION
!          DE  C1, C2, K1, K2, C3, K3      SOUS INDI_SIGM
!          DE  DIAM, INERTIE, EP           SOUS CARAC_TUYAU
!          DE  KT_SN, KT_SP                SOUS FACT_SIGM
!     ------------------------------------------------------------------
!
    integer :: ndim, jvalin, n1, ktsn, ktsp
!
! DEB ------------------------------------------------------------------
    call jemarq()
!
    ndim = 11
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
! ----- cas du b3200
    else
        zr(jvalin) = 0
        zr(jvalin+1) = 0
        zr(jvalin+2) = 0
        zr(jvalin+3) = 0
        zr(jvalin+4) = 0
        zr(jvalin+5) = 0
        zr(jvalin+6) = 1
        zr(jvalin+7) = 1
        zr(jvalin+8) = 1
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
