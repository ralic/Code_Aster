subroutine fozero(nomfon)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/lxlgut.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nomfon
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
!     CREATION D'UN OBJET DE TYPE FONCTION CONSTANTE DE VALEUR NULLE
!     ------------------------------------------------------------------
! IN  NOM DE LA FONCTION CONSTANTE A CREER
!     ------------------------------------------------------------------
!     OBJETS SIMPLES CREES:
!        NOMFON//'.PROL'
!        NOMFON//'.VALE
!     ------------------------------------------------------------------
!
    character(len=19) :: nomf
    character(len=24) :: chpro, chval
!-----------------------------------------------------------------------
    integer :: iret, jpro, lval
!-----------------------------------------------------------------------
    call jemarq()
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.PROL ---
    nomf = nomfon
    chpro = nomf//'.PROL'
    call jeexin(chpro, iret)
    if (iret .ne. 0) goto 9999
!
    call assert(lxlgut(nomf).le.24)
    call wkvect(chpro, 'G V K24', 6, jpro)
    zk24(jpro) = 'CONSTANT'
    zk24(jpro+1) = 'LIN LIN '
    zk24(jpro+2) = 'TOUTPARA'
    zk24(jpro+3) = 'TOUTRESU'
    zk24(jpro+4) = 'CC      '
    zk24(jpro+5) = nomf
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NOMFON.VALE ---
    chval(1:19) = nomf
    chval(20:24) = '.VALE'
    call wkvect(chval, 'G V R', 2, lval)
    zr(lval) = 1.0d0
    zr(lval+1) = 0.d0
!
!     --- LIBERATIONS ---
9999  continue
    call jedema()
end subroutine
