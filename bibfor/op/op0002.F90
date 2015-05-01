subroutine op0002()
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     LECTURE DE LA COMMANDE DEFI_CONSTANTE
!     STOCKAGE DANS UN OBJET DE TYPE FONCTION
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
!     OBJETS SIMPLES CREES:
!        NOMFON//'.PROL'
!        NOMFON//'.VALE
!     ------------------------------------------------------------------
!
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/assert.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/titre.h"
#include "asterfort/wkvect.h"
    integer :: nbval, lval, jpro
    character(len=8) :: typfon
    character(len=16) :: consta
    character(len=19) :: nomfon
    character(len=24) :: chpro, chval, nomres
!     ------------------------------------------------------------------
    call jemarq()
    call infmaj()
!
!     CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PROL
    call getres(nomfon, typfon, consta)
    chpro = nomfon//'.PROL'
    ASSERT(lxlgut(nomfon).le.24)
    call wkvect(chpro, 'G V K24', 6, jpro)
    zk24(jpro) = 'CONSTANT'
    zk24(jpro+1) = 'LIN LIN '
    call getvtx(' ', 'NOM_RESU', scal=nomres, nbret=nbval)
    zk24(jpro+2) = 'TOUTPARA'
    zk24(jpro+3) = nomres(1:8)
    zk24(jpro+4) = 'CC      '
    zk24(jpro+5) = nomfon(1:19)
!
!     CREATION ET REMPLISSAGE DE L'OBJET NOMFON.VALE
!
    chval = nomfon//'.VALE'
    call wkvect(chval, 'G V R', 2, lval)
    zr(lval) = 1.0d0
    call getvr8(' ', 'VALE', scal=zr(lval+1), nbret=nbval)
!
!     --- LIBERATIONS ---
!
!     --- CREATION D'UN TITRE ---
    call titre()
!
    call jedema()
end subroutine
