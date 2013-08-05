subroutine chpeva(chou)
    implicit  none
!     -----------------------------------------------------------------
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
! person_in_charge: jacques.pellet at edf.fr
!     BUT : TRAITER :
!      -  OPTION:'EVAL' DE LA COMMANDE CREA_CHAMP
!      -  COMMANDE CALC_CHAMP
!     ATTENTION: CETTE ROUTINE N'EST PAS UN UTILITAIRE :
!                ELLE FAIT DES CALL GETVXX.
!     -----------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getvid.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/ceseva.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnseva.h"
#include "asterfort/codent.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: n1, ib, jpara1, npara, jpara2, k, nncp, ibid
    character(len=4) :: typ1, typ2, knum
    character(len=8) :: kbid, chin, chou, nomgd
    character(len=19) :: ligrel, chs1, chs2, chins
    integer :: iarg
!     -----------------------------------------------------------------
!
    call jemarq()
!
!
    chins = '&&CHPEVA.CHINS'
    chs2 = '&&CHPEVA.CHS2'
!
!
! 1. CALCUL DE:
!      CHIN  : CHAMP A EVALUER (DE FONCTIONS)
!      NOMGD : GRANDEUR ASSOCIEE A CHIN
!      .LPARA1: NOMS DES CHAMPS PARAMETRES POUR LES FONCTIONS
! ------------------------------------------------------------------
!
    call getvid(' ', 'CHAM_F', 0, iarg, 1,&
                chin, ib)
!
    call dismoi('F', 'NOM_GD', chin, 'CHAMP', ib,&
                nomgd, ib)
    if (nomgd .ne. 'NEUT_F') call u2mess('F', 'MODELISA4_13')
!
    call getvid(' ', 'CHAM_PARA', 0, iarg, 0,&
                kbid, n1)
    npara = -n1
    call wkvect('&&CHPEVA.LPARA1', 'V V K8', npara, jpara1)
    call getvid(' ', 'CHAM_PARA', 0, iarg, npara,&
                zk8(jpara1), n1)
!
!
! 2.  ON VERIFIE QUE LES CHAMPS PARA ONT LA MEME DISCRETISATION:
!     ET ON LES TRANSFORME EN CHAMPS SIMPLES
!     CALCUL DE .LPARA2: NOMS DES CHAMP_S PARAMETRES POUR LES FONCTIONS
! ------------------------------------------------------------
    call wkvect('&&CHPEVA.LPARA2', 'V V K24', npara, jpara2)
    call dismoi('F', 'TYPE_CHAMP', chin, 'CHAMP', ib,&
                typ1, ib)
    do 10,k = 1,npara
    call dismoi('F', 'TYPE_CHAMP', zk8(jpara1-1+k), 'CHAMP', ib,&
                typ2, ib)
    if (typ1 .ne. typ2) call u2mess('F', 'MODELISA4_14')
!
    call codent(k, 'G', knum)
    chs1 = '&&CHPEVA.'//knum
    if (typ1 .eq. 'NOEU') then
        call cnocns(zk8(jpara1-1+k), 'V', chs1)
!
    else if (typ1.eq.'CART') then
        call carces(zk8(jpara1-1+k), 'ELEM', ' ', 'V', chs1,&
                    'A', ib)
!
    else if (typ1(1:2).eq.'EL') then
        call celces(zk8(jpara1-1+k), 'V', chs1)
    endif
    zk24(jpara2-1+k) = chs1
    10 end do
!
!
! 3.  -- ON APPELLE LA ROUTINE D'EVAL APPROPRIEE :
! ------------------------------------------------------------
    ASSERT((typ1.eq.'NOEU').or.(typ1(1:2).eq.'EL'))
    if (typ1 .eq. 'NOEU') then
        call cnocns(chin, 'V', chins)
        call cnseva(chins, npara, zk24(jpara2), chs2)
        call cnscno(chs2, ' ', 'NON', 'G', chou,&
                    'F', ibid)
        call detrsd('CHAM_NO_S', chins)
        call detrsd('CHAM_NO_S', chs2)
!
    else if (typ1(1:2).eq.'EL') then
        call celces(chin, 'V', chins)
        call ceseva(chins, npara, zk24(jpara2), chs2)
        call dismoi('F', 'NOM_LIGREL', chin, 'CHAMP', ib,&
                    ligrel, ib)
        call cescel(chs2, ligrel, ' ', ' ', 'NON',&
                    nncp, 'G', chou, 'F', ibid)
        call detrsd('CHAM_ELEM_S', chins)
        call detrsd('CHAM_ELEM_S', chs2)
!
    endif
!
!
! 7. MENAGE :
! -----------------------------------------------------
    call jedetr('&&CHPEVA.LPARA1')
    do 20,k = 1,npara
    if (typ1 .eq. 'NOEU') then
        call detrsd('CHAM_NO_S', zk24(jpara2-1+k))
!
    else
        call detrsd('CHAM_ELEM_S', zk24(jpara2-1+k))
    endif
    20 end do
    call jedetr('&&CHPEVA.LPARA2')
!
    call jedema()
!
end subroutine
