subroutine focstc(nomfon, nomres, rval, ival, base)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/wkvect.h"
    character(len=1) :: base
    character(len=*) :: nomfon, nomres
    real(kind=8) :: rval, ival
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     CREATION D'UN OBJET DE TYPE FONCTION CONSTANTE COMPLEXE
!     ------------------------------------------------------------------
! IN  NOMFON : K19 : NOM DE LA FONCTION CONSTANTE A CREER
! IN  NOMRES : K8  : NOM_RESU DE LA FONCTION
! IN  RVAL   : R   : VALEUR REELLE DE LA CONSTANTE
! IN  IVAL   : R   : VALEUR IMAGINAIRE DE LA CONSTANTE
! IN  BASE   : K1  : TYPE DE LA BASE 'G','V'
!     ------------------------------------------------------------------
!     OBJETS SIMPLES CREES:
!        NOMFON//'.PROL'
!        NOMFON//'.VALE
!     ------------------------------------------------------------------
    character(len=19) :: nomf
    character(len=24) :: chpro, chval
    integer :: jpro, lval
    integer :: iret
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PROL ---
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    nomf = nomfon
    chpro = nomf//'.PROL'
    chval = nomf//'.VALE'
    call jeexin(chpro, iret)
    if (iret .eq. 0) then
!
        ASSERT(lxlgut(nomf).le.24)
        call wkvect(chpro, base//' V K24', 6, jpro)
        zk24(jpro) = 'FONCT_C '
        zk24(jpro+1) = 'LIN LIN '
        zk24(jpro+2) = 'TOUTPARA'
        zk24(jpro+3) = nomres
        zk24(jpro+4) = 'CC      '
        zk24(jpro+5) = nomf
!
!        --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.VALE ---
        chval(1:19) = nomf
        chval(20:24) = '.VALE'
        call wkvect(chval, base//' V R', 3, lval)
        zr(lval) = 1.0d0
        zr(lval+1) = rval
        zr(lval+2) = ival
    else
        call jeveuo(chval, 'E', lval)
        zr(lval+1) = rval
        zr(lval+2) = ival
    endif
!
!     --- LIBERATIONS ---
    call jedema()
end subroutine
