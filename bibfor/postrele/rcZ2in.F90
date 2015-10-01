subroutine rcZ2in()
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/getvr8.h"
#include "asterfort/wkvect.h"
#include "asterfort/jemarq.h"
#include "asterfort/jedema.h"
!     ------------------------------------------------------------------
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
    character(len=16) :: motclf, motclf2
    integer :: n1, jvalin, ndim, iocc
!
! DEB ------------------------------------------------------------------
    call jemarq()
    ndim = 7
    call wkvect('&&RC3200.INDI', 'V V R8', ndim, jvalin)

    motclf = 'INDI_SIGM'
    call getvr8(motclf, 'C1', scal=zr(jvalin), iocc=iocc, nbret=n1)
    call getvr8(motclf, 'C2', scal=zr(jvalin+1), iocc=iocc, nbret=n1)  
    call getvr8(motclf, 'K1', scal=zr(jvalin+2), iocc=iocc, nbret=n1)  
    call getvr8(motclf, 'K2', scal=zr(jvalin+3), iocc=iocc, nbret=n1)
!
    motclf2 = 'GEOM_TUY'
    call getvr8(motclf2, 'DIAM', scal=zr(jvalin+4),iocc=iocc, nbret=n1)
    call getvr8(motclf2, 'EP', scal=zr(jvalin+5), iocc=iocc,nbret=n1)  
    call getvr8(motclf2, 'INERTIE', scal=zr(jvalin+6),iocc=iocc, nbret=n1)
!
    call jedema()     
end subroutine

