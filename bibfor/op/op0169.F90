subroutine op0169()
    implicit none
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
!     OPERATEUR FONC_FLUI_STRU
!     CREATION D UNE FONCTION CONSTANTE (CONCEPT FONCTION) DONNANT
!     LA VALEUR DU COEFFICIENT DE MASSE AJOUTEE
!     ------------------------------------------------------------------
!     OBJETS SIMPLES CREES:
!        NOMFON//'.PROL'
!        NOMFON//'.VALE
!     ------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterfort/assert.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: typfon
    character(len=16) :: cmd
    character(len=19) :: nomfon, typflu
    character(len=24) :: fsic, fsvr, prol, vale
    integer :: iarg
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibid, lfsic, lfsvr, lprol, lvale
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
    call getres(nomfon, typfon, cmd)
    call getvid(' ', 'TYPE_FLUI_STRU', 0, iarg, 1,&
                typflu, ibid)
!
! --- VERIFICATION A L EXECUTION
    fsic = typflu//'.FSIC'
    call jeveuo(fsic, 'L', lfsic)
    if (zi(lfsic) .ne. 1) then
        call u2mess('F', 'UTILITAI3_2')
    endif
!
! --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PROL
    prol = nomfon//'.PROL'
    call assert(lxlgut(nomfon).le.24)
    call wkvect(prol, 'G V K24', 6, lprol)
    zk24(lprol) = 'CONSTANT'
    zk24(lprol+1) = 'LIN LIN '
    zk24(lprol+2) = 'ABSC    '
    zk24(lprol+3) = 'COEF_MAS'
    zk24(lprol+4) = 'CC      '
    zk24(lprol+5) = nomfon
!
! --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.VALE
    fsvr = typflu//'.FSVR'
    call jeveuo(fsvr, 'L', lfsvr)
!
    vale = nomfon//'.VALE'
    call wkvect(vale, 'G V R', 2, lvale)
    zr(lvale) = 1.0d0
    zr(lvale+1) = zr(lfsvr)
!
    call jedema()
end subroutine
