subroutine ssafmo(mo)
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
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterc/getfac.h"
#include "asterc/getvtx.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: mo
! ----------------------------------------------------------------------
!     BUT: TRAITER LE MOT-CLEF AFFE_SOUS_STRUC DE LA COMMANDE
!          AFFE_MODELE.
!
!
!     IN: MO : NOM DU MODELE
!
!     OUT: MO EST (EVENTUELLEMENT) ENRICHI DE L'OBJET .SSSA
!        (CET OBJET PRECISE QUELS SONT LES (SUPER)MAILLES DU MAILLAGE
!         REELLEMENT AFFECTEES PAR DES SOUS_STRUCTURES DANS LE MODELE)
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) :: ma, kbid, nosma
    character(len=24) :: valk(2)
    integer :: iarg
!
!
!
!-----------------------------------------------------------------------
    integer :: i, ialmai, iasssa, ibid, ierd, imas, ioc
    integer :: iret, n1, n2, nboc, nbsma, nbss, nl
!
!-----------------------------------------------------------------------
    call jemarq()
    call getfac('AFFE_SOUS_STRUC', nboc)
    if (nboc .eq. 0) goto 9999
!
    call dismoi('F', 'NOM_MAILLA', mo, 'MODELE', ibid,&
                ma, ierd)
    call dismoi('F', 'NB_SM_MAILLA', ma, 'MAILLAGE', nbsma,&
                kbid, ierd)
    call dismoi('F', 'NB_NL_MAILLA', ma, 'MAILLAGE', nl,&
                kbid, ierd)
    if (nbsma .eq. 0) then
        call u2mess('F', 'SOUSTRUC_30')
    endif
!
    ioc=1
    call wkvect(mo//'.MODELE    .SSSA', 'G V I', nbsma+3, iasssa)
!
!     -- CAS : TOUT: 'OUI' :
!     ----------------------
    call getvtx('AFFE_SOUS_STRUC', 'TOUT', ioc, iarg, 1,&
                kbid, n1)
    if (n1 .eq. 1) then
        do 1, i=1,nbsma
        zi(iasssa-1+i)=1
 1      continue
        nbss= nbsma
        goto 9998
    endif
!
!     -- CAS : MAILLE: L_MAIL
!     -----------------------
    call getvtx('AFFE_SOUS_STRUC', 'SUPER_MAILLE', ioc, iarg, 0,&
                kbid, n1)
    call wkvect('&&SSAFMO.LMAI', 'V V K8', -n1, ialmai)
    call getvtx('AFFE_SOUS_STRUC', 'SUPER_MAILLE', ioc, iarg, -n1,&
                zk8(ialmai), n2)
    nbss= -n1
    do 2, i=1,-n1
    nosma=zk8(ialmai-1+i)
    call jenonu(jexnom(ma//'.SUPMAIL', nosma), imas)
    if (imas .eq. 0) then
        valk(1) = nosma
        valk(2) = ma
        call u2mesk('F', 'SOUSTRUC_26', 2, valk)
    else
        zi(iasssa-1+imas)=1
    endif
    2 end do
!
!     -- ON REMPLIT LES 3 DERNIERES VALEURS:
!     --------------------------------------
9998  continue
    zi(iasssa-1+nbsma+1)=nbsma
    zi(iasssa-1+nbsma+2)=nbss
    zi(iasssa-1+nbsma+3)=nl
!
    call jeexin('&&SSAFMO.LMAI', iret)
    if (iret .gt. 0) call jedetr('&&SSAFMO.LMAI')
!
!
9999  continue
    call jedema()
end subroutine
