subroutine op0089()
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
    implicit none
!     COMMANDE:  DEPL_INTERNE
!
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/getvid.h"
#include "asterc/getvtx.h"
#include "asterfort/chpver.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/pjxxpr.h"
#include "asterfort/ssdein.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    character(len=8) :: ul, ug, mail, nocas
    character(len=16) :: kbi1, kbi2, corres, tysd
    character(len=8) :: ouiri, ouima, affick(2)
!
    integer :: isma, iamacr, iarefm, ibid, ie, n1, lref
    character(len=8) :: noma, macrel, promes, modlms, noca
    character(len=19) :: method
    character(len=24) :: vref
    integer :: iarg
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
    call getres(ul, kbi1, kbi2)
    call getvid(' ', 'DEPL_GLOBAL', 1, iarg, 1,&
                ug, n1)
    call getvtx(' ', 'SUPER_MAILLE', 1, iarg, 1,&
                mail, n1)
!
    call gettco(ug, tysd)
    if (tysd(1:4) .ne. 'CHAM') then
!       UG DE TYPE RESULTAT (POUR MODIF STRUCTURALE)
        call dismoi('F', 'NOM_MAILLA', ug, 'RESULTAT', ibid,&
                    noma, ie)
        call jeveuo(noma//'.NOMACR', 'L', iamacr)
!
        call jenonu(jexnom(noma//'.SUPMAIL', mail), isma)
!
        affick(1) = mail
        affick(2) = noma
        if (isma .le. 0) call u2mesk('F', 'SOUSTRUC_26', 2, affick)
!
        macrel= zk8(iamacr-1+isma)
!
        call dismoi('F', 'NOM_PROJ_MESU', macrel, 'MACR_ELEM_STAT', ibid,&
                    promes, ie)
        if (promes .eq. ' ') call u2mess('F', 'SOUSTRUC_79')
!
        vref = macrel//'.PROJM    .PJMRF'
        call jeveuo(vref, 'L', lref)
        kbi1=zk16(lref-1 +1)
        modlms=kbi1(1:8)
!
!       VERIFIER SI LES MATRICES MASSE ET RAIDEUR CONDENSEES
!       ONT ETE CALCULEES
        call jeveuo(macrel//'.REFM', 'L', iarefm)
!
        ouiri = zk8(iarefm-1+6)
        if (ouiri .ne. 'OUI_RIGI') call u2mess('F', 'SOUSTRUC_80')
!
        ouima = zk8(iarefm-1+7)
        if (ouima .ne. 'OUI_MASS') call u2mess('F', 'SOUSTRUC_81')
!
        corres = ' '
        noca = ' '
        method = ' '
        call pjxxpr(ug, ul, noma, modlms, corres,&
                    'G', noca, method)
!
    else
!       UG DE TYPE CHAM_NO
        call chpver('F', ug, 'NOEU', 'DEPL_R', ie)
        call dismoi('F', 'NOM_MAILLA', ug, 'CHAM_NO', ibid,&
                    noma, ie)
        call getvtx(' ', 'NOM_CAS', 1, iarg, 1,&
                    nocas, n1)
        call ssdein(ul, ug, mail, nocas)
    endif
!
!
!     -- CREATION DE L'OBJET .REFD SI NECESSAIRE:
!     -------------------------------------------
!   call ajrefd(ug, ul, 'ZERO')
!
!
    call jedema()
!
end subroutine
