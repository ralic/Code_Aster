subroutine rc32in()
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/getvr8.h"
#include "asterfort/wkvect.h"
#include "asterfort/jemarq.h"
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
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_B3200
!     RECUPERATION DES FACTEURS D'INTENSITE DE CONTRAINTE 
!                  KT_SN ET KT_SP
!     ------------------------------------------------------------------
!
    character(len=16) :: motclf3    
    integer :: jvalin, ndim, n1, ik3, ik4, nb3
!
! DEB ------------------------------------------------------------------
    call jemarq()
    ndim = 11
    call wkvect('&&RC3200.INDI', 'V V R', ndim, jvalin)
    motclf3 = 'FACT_SIGM'
!
    zr(jvalin) = 0
    zr(jvalin+1) = 0
    zr(jvalin+2) = 0
    zr(jvalin+3) = 0
    zr(jvalin+4) = 0
    zr(jvalin+5) = 0
    zr(jvalin+6) = 1
    zr(jvalin+7) = 1
    zr(jvalin+8) = 1
!
    call getfac(motclf3, nb3)
    if(nb3 .eq. 0) then
        zr(jvalin+9) = 1
        zr(jvalin+10) = 1
    else 
        call getvr8(motclf3, 'KT_SN', iocc=1, nbval=0, nbret=ik3)
        if (ik3 .ne. 0) then
            call getvr8(motclf3, 'KT_SN', scal=zr(jvalin+9), iocc=1, nbret=n1)
        else
            zr(jvalin+9) = 1
        endif
!
        call getvr8(motclf3, 'KT_SP', iocc=1, nbval=0, nbret=ik4)
        if (ik4 .ne. 0) then
            call getvr8(motclf3, 'KT_SP', scal=zr(jvalin+10), iocc=1, nbret=n1)
        else
            zr(jvalin+10) = 1
        endif
    endif             
!
    call jedema()    
end subroutine

