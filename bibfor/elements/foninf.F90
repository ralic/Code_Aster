subroutine foninf(resu, typfon)
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=8) :: resu, typfon
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!       ---------------------------------------------------------------
!       STOCKAGE D'INFOS UTILES DANS LA SD EN SORTIE DE DEFI_FOND_FISS
!       ---------------------------------------------------------------
!
! IN/OUT
!       RESU   : NOM DE LA SD EN SORTIE DE DEFI_FOND_FISS
!       TYPFON : TYPE DE FOND DE FISSURE
!
!
    integer ::  ibid, jinfo
    character(len=8) :: syme, confin
!
!     -----------------------------------------------------------------
!
    call jemarq()
!
!     RECUPERATION DU MOT-CLE SYME
    call getvtx(' ', 'SYME', scal=syme, nbret=ibid)
    ASSERT(syme.eq.'OUI'.or.syme.eq.'NON')
!
!     RECUPERATION DU MOT-CLE CONFIG_INIT
    call getvtx(' ', 'CONFIG_INIT', scal=confin, nbret=ibid)
    ASSERT(confin.eq.'DECOLLEE'.or.confin.eq.'COLLEE')
!
!     CREATION DE L'OBJET .INFO DANS LA SD FOND_FISS
    call wkvect(resu//'.INFO', 'G V K8', 3, jinfo)
!
!     STOCKAGE DU MOT-CLE SYME
    zk8(jinfo-1+1) = syme
!
!     STOCKAGE DU MOT-CLE CONFIG_INIT
    zk8(jinfo-1+2) = confin
!
!     STOCKAGE DU MOT-CLE TYPE_FOND
    zk8(jinfo-1+3) = typfon
!
    call jedema()
end subroutine
