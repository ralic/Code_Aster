subroutine rcZ2in(transif)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterc/getfac.h"
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
!     OPERATEUR POST_RCCM, TRAITEMENT DE FATIGUE_ZE200
!     RECUPERATION DES INDICES DE CONTRAINTE ET DES CARACTERISTIQUES 
!     GEOMETRIQUES DE LA TUYAUTERIE
!          DE  C1, C2, K1, K2      SOUS INDI_SIGM
!          DE  DIAM, INERTIE, EP   SOUS CARAC_TUYAU
!
!     ------------------------------------------------------------------
!
    character(len=16) :: motclf, motclf2, motclf3
    integer :: n1, jvalin, ndim
    integer :: ik3, ik4, nb3
    aster_logical :: transif
!
! DEB ------------------------------------------------------------------
    call jemarq()
    ndim = 11
    call wkvect('&&RC3200.INDI', 'V V R', ndim, jvalin)
    motclf = 'INDI_SIGM'
    motclf2 = 'TUYAU'
    motclf3 = 'FACT_SIGM'
!
    if (.not. transif) then
!       
        call getvr8(motclf, 'K1', scal=zr(jvalin), iocc=1, nbret=n1)
        call getvr8(motclf, 'C1', scal=zr(jvalin+1), iocc=1, nbret=n1)  
        call getvr8(motclf, 'K2', scal=zr(jvalin+2), iocc=1, nbret=n1)  
        call getvr8(motclf, 'C2', scal=zr(jvalin+3), iocc=1, nbret=n1)
        call getvr8(motclf, 'K3', scal=zr(jvalin+4), iocc=1, nbret=n1)  
        call getvr8(motclf, 'C3', scal=zr(jvalin+5), iocc=1, nbret=n1)
!
        call getvr8(motclf2, 'R', scal=zr(jvalin+6),iocc=1, nbret=n1)
        call getvr8(motclf2, 'EP', scal=zr(jvalin+7),iocc=1, nbret=n1)
        call getvr8(motclf2, 'I', scal=zr(jvalin+8),iocc=1, nbret=n1)
!
        zr(jvalin+9) = 1
        zr(jvalin+10) = 1
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
    endif              
!
    call jedema()    
end subroutine
