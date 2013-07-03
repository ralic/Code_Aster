subroutine meriro(modele, cara, nchar, lchar, mate,&
                  exitim, time, compor, matel)
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
!
#include "asterfort/calcul.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/u2mess.h"
#include "asterfort/vrcins.h"
    character(len=8) :: modele, cara, lchar(*)
    character(len=19) :: matel
    character(len=24) :: mate, compor
    real(kind=8) :: time
    logical :: exitim
    integer :: nchar
! ----------------------------------------------------------------------
!
!     CALCUL DES MATRICES ELEMENTAIRES DE RAIDEUR CENTRIFUGE
!
!     ENTREES:
!
!     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
!        MODELE : NOM DU MODELE
!        NCHAR  : NOMBRE DE CHARGES
!        LCHAR  : LISTE DES CHARGES
!        MATE   : CHAMP DE MATERIAUX
!        EXITIM : VRAI SI L'INSTANT EST DONNE
!        TIME   : INSTANT DE CALCUL
!
!     SORTIES:
!
!        MATEL  : NOM DU MATEL (N RESUELEM) PRODUIT
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    logical :: exicar
    character(len=2) :: codret
    character(len=8) :: lpain(10), lpaout(1)
    character(len=16) :: option
    character(len=19) :: chvarc
    character(len=24) :: chgeom, chrota, lchin(10), lchout(10)
    character(len=24) :: ligrmo, chcara(18)
!-----------------------------------------------------------------------
    integer :: icha, iret, nbro
!-----------------------------------------------------------------------
    data chvarc /'&&MERIRO.CHVARC'/
!
!     -- ON VERIFIE LA PRESENCE PARFOIS NECESSAIRE DE CHAM_MATER
!
    call jemarq()
    if (modele(1:1) .eq. ' ') then
        call u2mess('F', 'CALCULEL3_50')
    endif
!
    call megeom(modele, chgeom)
!
    nbro = 0
    do 10 icha = 1, nchar
        call exisd('CHAMP_GD', lchar(icha)//'.CHME.ROTAT', iret)
        if (iret .ne. 0) then
            chrota = lchar(icha)//'.CHME.ROTAT.DESC'
            nbro = nbro + 1
        endif
10  end do
!
    if (nbro .ne. 1) then
        call u2mess('F', 'CALCULEL3_71')
    endif
!
    call vrcins(modele, mate, cara, time, chvarc,&
                codret)
    call mecara(cara, exicar, chcara)
!
    call jeexin(matel//'.RERR', iret)
    if (iret .gt. 0) then
        call jedetr(matel//'.RERR')
        call jedetr(matel//'.RELR')
    endif
    call memare('G', matel, modele, mate, ' ',&
                'RIGI_ROTA')
    call reajre(matel, ' ', 'G')
!
    lpaout(1) = 'PMATUUR'
    lchout(1) = matel(1:8)//'.ME001'
!
    if (modele .ne. '       ') then
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PMATERC'
        lchin(2) = mate
        lpain(3) = 'PROTATR'
        lchin(3) = chrota
        lpain(4) = 'PVARCPR'
        lchin(4) = chvarc
        lpain(5) = 'PCAGNPO'
        lchin(5) = chcara(6)
        lpain(6) = 'PCAORIE'
        lchin(6) = chcara(1)
        lpain(7) = 'PNBSP_I'
        lchin(7) = chcara(16)
        lpain(8) = 'PFIBRES'
        lchin(8) = chcara(17)
        lpain(9) = 'PCOMPOR'
        lchin(9) = compor
        ligrmo = modele//'.MODELE'
        option = 'RIGI_MECA_RO'
!
        call calcul('S', option, ligrmo, 9, lchin,&
                    lpain, 1, lchout, lpaout, 'G',&
                    'OUI')
!
        call reajre(matel, lchout(1), 'G')
    endif
!
    call detrsd('CHAMP_GD', chvarc)
    call jedema()
end subroutine
