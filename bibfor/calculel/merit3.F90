subroutine merit3(modele, nchar, lchar, mate, cara,&
                  time, matel, prefch, numero, base)
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
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/mecara.h"
#include "asterfort/megeom.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/utmess.h"
    character(len=*) :: lchar(*), mate
    character(len=8) :: modele, cara
    character(len=19) :: matel, prefch
    character(len=24) :: time
    character(len=1) :: base
    integer :: nchar, numero
! ----------------------------------------------------------------------
!
!     CALCUL DES MATRICES ELEMENTAIRES DE CONVECTION NATURELLE
!
!     LES RESUELEM PRODUITS S'APPELLENT :
!           PREFCH(1:8).ME000I , I=NUMERO+1,NUMERO+N
!
!     ENTREES:
!
!     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
!        MODELE : NOM DU MODELE
!        NCHAR  : NOMBRE DE CHARGES
!        LCHAR  : LISTE DES CHARGES
!        MATE   : CHAMP DE MATERIAUX
!        CARA   : CHAMP DE CARAC_ELEM
!        TIME   : CHAMPS DE TEMPSR
!        MATEL  : NOM DU MATR_ELEM (N RESUELEM) PRODUIT
!        PREFCH : PREFIXE DES NOMS DES RESUELEM STOCKES DANS MATEL
!        NUMERO : NUMERO D'ORDRE A PARTIR DUQUEL ON NOMME LES RESUELEM
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
!
!
    character(len=8) :: nomcha, lpain(7), lpaout(1)
    character(len=8) :: vitess
    character(len=16) :: option
    character(len=24) :: lchin(7), lchout(1), chgeom, chcara(18)
    character(len=24) :: chvite, ligrmo, carte, convch
    integer :: iret, ilires
!
!
!     -- ON VERIFIE LA PRESENCE PARFOIS NECESSAIRE DE CARA_ELEM
!-----------------------------------------------------------------------
    integer :: ichar, iconv, jvites
!-----------------------------------------------------------------------
    call jemarq()
    if (modele(1:1) .eq. ' ') then
        call utmess('F', 'CALCULEL3_50')
    endif
!
    call megeom(modele, chgeom)
    call mecara(cara, chcara)
!
    call jeexin(matel//'.RERR', iret)
    if (iret .gt. 0) then
        call jedetr(matel//'.RERR')
        call jedetr(matel//'.RELR')
    endif
!
    chvite = '????'
!
    iconv = 0
!
    lpaout(1) = 'PMATTTR'
    lchout(1) = prefch(1:8)//'.ME000'
    if (lchar(1) (1:8) .ne. '        ') then
        do 10 ichar = 1, nchar
            nomcha = lchar(ichar)
            convch = nomcha//'.CHTH'//'.CONVE'//'.VALE'
            call jeexin(convch, iret)
            if (iret .gt. 0) then
                iconv = iconv + 1
                if (iconv .gt. 1) then
                    call utmess('F', 'CALCULEL3_72')
                endif
!
                option = 'RIGI_THER_CONV_D'
                call memare('V', matel, modele, mate, cara,&
                            option)
!
                call jeveuo(convch, 'L', jvites)
                vitess = zk8(jvites)
                chvite = vitess
                carte = '&&MERIT3'//'.CONVECT.DECENT'
                call mecact('V', carte, 'MODELE', modele//'.MODELE', 'NEUT_K24',&
                            ncmp=1, nomcmp='Z1', sk='NON')
                lpain(1) = 'PGEOMER'
                lchin(1) = chgeom
                lpain(2) = 'PMATERC'
                lchin(2) = mate
                lpain(3) = 'PCACOQU'
                lchin(3) = chcara(7)
                lpain(4) = 'PTEMPSR'
                lchin(4) = time
                lpain(5) = 'PVITESR'
                lchin(5) = chvite
                lpain(6) = 'PNEUK24'
                lchin(6) = carte
                lpain(7) = 'PCAMASS'
                lchin(7) = chcara(12)
                ilires = 0
                ligrmo = modele//'.MODELE'
                ilires = ilires + 1
                call codent(ilires+numero, 'D0', lchout(1) (12:14))
                call calcul('S', option, ligrmo, 7, lchin,&
                            lpain, 1, lchout, lpaout, base,&
                            'OUI')
                call reajre(matel, lchout(1), base)
            endif
10      continue
!
    endif
!
! --- MENAGE
    call detrsd('CARTE', '&&MERIT3'//'.CONVECT.DECENT')
    call jedema()
end subroutine
