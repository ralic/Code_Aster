subroutine caveis(chargz)
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
    implicit none
!
!       CAVEIS -- TRAITEMENT DU MOT CLE FORCE_SOL
!
!      TRAITEMENT DU MOT CLE FORCE_SOL DE AFFE_CHAR_MECA
!
! -------------------------------------------------------
!  CHARGE        - IN    - K8   - : NOM DE LA SD CHARGE
!                - JXVAR -      -   LA  CHARGE STOCKE DANS L'OBJET
!                                   CHAR//'CHME.VEISS'
! -------------------------------------------------------
!
!.========================= DEBUT DES DECLARATIONS ====================
!
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/codent.h"
#include "asterfort/getvis.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/wkvect.h"
    character(len=*) :: chargz
! ------ VARIABLES LOCALES
    character(len=8) :: charge, maille
    character(len=24) :: obj, gnintf
    character(len=16) :: typbin
!
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!-----------------------------------------------------------------------
    integer :: idveis, iffor, ifmis, n1, ng, nm, nv, npasm, ibin
!
!-----------------------------------------------------------------------
    call jemarq()
!
    call getfac('FORCE_SOL', nv)
!
    if (nv .eq. 0) goto 999
!
!
    charge = chargz
    obj = charge//'.CHME.VEISS'
!
    call wkvect(obj, 'G V K24', 8, idveis)
    ifmis=0
    call getvis('FORCE_SOL', 'UNITE_RESU_RIGI', iocc=1, scal=ifmis, nbret=n1)
    call codent(ifmis, 'D', zk24(idveis))
    ifmis=0
    call getvis('FORCE_SOL', 'UNITE_RESU_MASS', iocc=1, scal=ifmis, nbret=n1)
    call codent(ifmis, 'D', zk24(idveis+1))
    ifmis=0
    call getvis('FORCE_SOL', 'UNITE_RESU_AMOR', iocc=1, scal=ifmis, nbret=n1)
    call codent(ifmis, 'D', zk24(idveis+2))
    iffor=0
    call getvis('FORCE_SOL', 'UNITE_RESU_FORC', iocc=1, scal=iffor, nbret=n1)
    call codent(iffor, 'D', zk24(idveis+3))
    gnintf = ' '
    call getvtx('FORCE_SOL', 'GROUP_NO_INTERF', iocc=1, scal=gnintf, nbret=ng)
    zk24(idveis+4) = gnintf
    maille = ' '
    call getvtx('FORCE_SOL', 'SUPER_MAILLE', iocc=1, scal=maille, nbret=nm)
    zk24(idveis+5) = maille
    call getvis('FORCE_SOL', 'NB_PAS_TRONCATURE', iocc=1, scal=npasm, nbret=n1)
    call codent(npasm, 'D', zk24(idveis+6))
    call getvtx('FORCE_SOL', 'TYPE', iocc=1, scal=typbin, nbret=nm)
    ibin = 1
    if (typbin .ne. 'BINAIRE') then
        ibin = 0
    endif
    call codent(ibin, 'D', zk24(idveis+7))
!
999 continue
!
    call jedema()
!.============================ FIN DE LA ROUTINE ======================
end subroutine
