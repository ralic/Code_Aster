subroutine meama2(modele, nchar, lchar, mate, matel,&
                  prefch)
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
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
    character(len=8) :: modele, lchar(*)
    character(len=19) :: matel, prefch
    character(len=*) :: mate
    integer :: nchar
! ----------------------------------------------------------------------
!
!*    CALCUL DES MATRICES ELEMENTAIRES D'AMORTISSEMENT ACOUSTIQUE
!*           ( ISO_FACE, 'AMOR_ACOU      R/F' )
!
!     LES RESUELEM PRODUITS S'APPELLENT :
!           PREFCH(1:8).ME000I , I=1,NCHAR
!
!     ENTREES:
!
!     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
!        MODELE : NOM DU MODELE
!        NCHAR  : NOMBRE DE CHARGES
!        LCHAR  : LISTE DES CHARGES
!*       MATE   : CARTE DE MATERIAU CODE
!        MATEL  : NOM DU MATELE (N RESUELEM) PRODUIT
!        PREFCH : PREFIXE DES NOMS DES RESUELEM STOCKES DANS MATEL
!*
!
!     SORTIES:
!        MATEL  : EST REMPLI.
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
!*
!*
    character(len=8) :: lpain(4), lpaout(1)
    character(len=16) :: option
!*
    character(len=24) :: chgeom, chimpe, lchin(3), lchout(1)
    character(len=24) :: ligrmo
!
!-----------------------------------------------------------------------
    integer :: icha, ilires, iret
!-----------------------------------------------------------------------
    call jemarq()
    call megeom(modele, chgeom)
!*
    call jeexin(matel//'.RERR', iret)
    if (iret .gt. 0) then
        call jedetr(matel//'.RERR')
        call jedetr(matel//'.RELR')
    endif
    call memare('V', matel, modele, mate, ' ',&
                'AMOR_ACOU')
!*
    lpaout(1) = 'PMATTTC'
    lchout(1) = prefch(1:8)//'.ME000'
    ilires = 0
    if (lchar(1) .ne. '        ') then
!
        ligrmo = modele//'.MODELE'
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
!**
        lpain(3) = 'PMATERC'
        lchin(3) = mate
!**
        do 10 icha = 1, nchar
!**
            option = 'AMOR_ACOU'
            lpain(2) = 'PIMPEDC'
!**
!**
            chimpe = lchar(icha)//'.CHAC.IMPED.DESC'
            call jeexin(chimpe, iret)
!**
            if (iret .eq. 0) goto 10
!*
            lchin(2) = chimpe
            ilires = ilires + 1
            call codent(ilires, 'D0', lchout(1) (12:14))
            call calcul('S', option, ligrmo, 3, lchin,&
                        lpain, 1, lchout, lpaout, 'G',&
                        'OUI')
            call reajre(matel, lchout(1), 'G')
10      continue
    endif
    call jedema()
end subroutine
