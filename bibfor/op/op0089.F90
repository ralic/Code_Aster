subroutine op0089()
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!     COMMANDE:  DEPL_INTERNE
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/gettco.h"
#include "asterfort/chpver.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/pjxxpr.h"
#include "asterfort/ssdein.h"
#include "asterfort/utmess.h"
!
    character(len=8) :: ul, ug, mail, nocas
    character(len=16) :: kbi1, kbi2, corres, tysd
    character(len=8) :: ouiri, ouima, affick(2)
!
    integer :: isma,   ie, n1, lref
    character(len=8) :: noma, macrel, promes, modlms, noca
    character(len=19) :: method
    character(len=24) :: vref
    character(len=8), pointer :: nomacr(:) => null()
    character(len=8), pointer :: refm(:) => null()
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
!
    call infmaj()
    call getres(ul, kbi1, kbi2)
    call getvid(' ', 'DEPL_GLOBAL', scal=ug, nbret=n1)
    call getvtx(' ', 'SUPER_MAILLE', scal=mail, nbret=n1)
!
    call gettco(ug, tysd)
    if (tysd(1:4) .ne. 'CHAM') then
!       UG DE TYPE RESULTAT (POUR MODIF STRUCTURALE)
        call dismoi('NOM_MAILLA', ug, 'RESULTAT', repk=noma)
        call jeveuo(noma//'.NOMACR', 'L', vk8=nomacr)
!
        call jenonu(jexnom(noma//'.SUPMAIL', mail), isma)
!
        affick(1) = mail
        affick(2) = noma
        if (isma .le. 0) then
            call utmess('F', 'SOUSTRUC_26', nk=2, valk=affick)
        endif
!
        macrel= nomacr(isma)
!
        call dismoi('NOM_PROJ_MESU', macrel, 'MACR_ELEM_STAT', repk=promes)
        if (promes .eq. ' ') then
            call utmess('F', 'SOUSTRUC_79')
        endif
!
        vref = macrel//'.PROJM    .PJMRF'
        call jeveuo(vref, 'L', lref)
        kbi1=zk16(lref-1 +1)
        modlms=kbi1(1:8)
!
!       VERIFIER SI LES MATRICES MASSE ET RAIDEUR CONDENSEES
!       ONT ETE CALCULEES
        call jeveuo(macrel//'.REFM', 'L', vk8=refm)
!
        ouiri = refm(6)
        if (ouiri .ne. 'OUI_RIGI') then
            call utmess('F', 'SOUSTRUC_80')
        endif
!
        ouima = refm(7)
        if (ouima .ne. 'OUI_MASS') then
            call utmess('F', 'SOUSTRUC_81')
        endif
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
        call dismoi('NOM_MAILLA', ug, 'CHAM_NO', repk=noma)
        call getvtx(' ', 'NOM_CAS', scal=nocas, nbret=n1)
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
