subroutine peenc2(champ, rcoef)
    implicit   none
#include "jeveux.h"
#include "asterfort/celver.h"
#include "asterfort/digdel.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/u2mess.h"
    character(len=*) :: champ
    real(kind=8) :: rcoef
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     FAIRE DES OPERATIONS SUR UN CHAM_ELEM DE TYPE ENERGIE
!            (NOTION D'INTEGRALE DU CHAMP SUR LE MODELE)
!     ------------------------------------------------------------------
! IN  : CHAMP  : NOM DU CHAM_ELEM
! IN  : RCOEF  : COEFFICIENT MULTIPLICATIF
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: ibid, nbgr, i, j, k, indic
    integer :: lcelk, jceld, lvale, mode, longt, nel, idecgr, icoef
    character(len=4) :: docu
    character(len=19) :: champ2, ligrel
!     ------------------------------------------------------------------
    call jemarq()
    champ2 = champ
!
!     --- ON RETROUVE LE NOM DU LIGREL ---
!
!     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
    call celver(champ2, 'NBVARI_CST', 'STOP', ibid)
    call celver(champ2, 'NBSPT_1', 'STOP', ibid)
!
    call jelira(champ2//'.CELD', 'DOCU', cval=docu)
    if (docu .ne. 'CHML') then
        call u2mess('F', 'CALCULEL3_52')
    endif
    call jeveuo(champ2//'.CELK', 'L', lcelk)
    ligrel = zk24(lcelk)(1:19)
!
    call jeveuo(champ2//'.CELD', 'L', jceld)
!
!     --- ON NE VERIFIE PAS LES LONGUEURS, CAR ELLES SONT DIFFERENTES
!         SUIVANT LE TYPE D'ELEMENT.
!     --- LA VALEUR "TOTALE" QUE L'ON VEUT RECUPERER EST PLACE EN 1
    nbgr = nbgrel( ligrel )
!
    call jeveuo(champ2//'.CELV', 'E', lvale)
    do 30 j = 1, nbgr
        mode=zi(jceld-1+zi(jceld-1+4+j) +2)
        if (mode .eq. 0) goto 30
        icoef=max(1,zi(jceld-1+4))
        longt = digdel(mode) * icoef
        nel = nbelem( ligrel, j )
        idecgr=zi(jceld-1+zi(jceld-1+4+j)+8)
        do 32 k = 1, nel
!
!            --- TOTALE ---
            i = 1
            indic = lvale-1+idecgr+(k-1)*longt+i-1
            zr(indic) = zr(indic) * rcoef
32      continue
30  continue
!
    call jedema()
end subroutine
