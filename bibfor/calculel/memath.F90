subroutine memath(option, modele, mate, cara, time,&
                  matel)
    implicit none
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/codent.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/utmess.h"
    character(len=8) :: modele, cara
    character(len=19) :: matel
    character(len=16) :: option
    character(len=24) :: time, mate
! ----------------------------------------------------------------------
!
!     CALCUL DES MATRICES ELEMENTAIRES DE MASSE_THERMIQUE
!                ( 'MASS_THER', ISO )
!
!     ENTREES:
!
!     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
!        MODELE : NOM DU MODELE
!        MATE   : CHAMP DE MATERIAUX
!        CARA   : CHAMP DE CARAC_ELEM
!        TIME   : CHAMP DE TEMPSR
!        MATEL  : NOM DU MAT_ELE(N RESUELEM) PRODUIT
!
!     SORTIES:
!        MATEL  : EST CALCULE
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    logical :: exicar
    character(len=8) :: lpain(4), lpaout(1)
    character(len=24) :: chgeom, chcara(18), lchin(4), lchout(1)
    character(len=24) :: ligrmo
!
!
!     -- ON VERIFIE LA PRESENCE PARFOIS NECESSAIRE DE CARA_ELEM
!        ET CHAM_MATER :
!-----------------------------------------------------------------------
    integer :: ilires, iret
!-----------------------------------------------------------------------
    call jemarq()
    if (modele(1:1) .eq. ' ') then
        call utmess('F', 'CALCULEL3_50')
    endif
!
    call megeom(modele, chgeom)
    call mecara(cara, exicar, chcara)
!
    call jeexin(matel//'.RERR', iret)
    if (iret .gt. 0) then
        call jedetr(matel//'.RERR')
        call jedetr(matel//'.RELR')
    endif
    call memare('G', matel, modele, mate, cara,&
                'MASS_THER')
!
    lpaout(1) = 'PMATTTR'
    lchout(1) = matel(1:8)//'.ME000'
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = mate
    lpain(3) = 'PCACOQU'
    lchin(3) = chcara(7)
    lpain(4) = 'PTEMPSR'
    lchin(4) = time
!
    ligrmo = modele//'.MODELE'
    ilires = 0
    ilires = ilires + 1
    call codent(ilires, 'D0', lchout(1) (12:14))
    call calcul('S', option, ligrmo, 4, lchin,&
                lpain, 1, lchout, lpaout, 'G',&
                'OUI')
    call reajre(matel, lchout(1), 'G')
!
    call jedema()
end subroutine
