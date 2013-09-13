subroutine merige(modele, cara, sigg, strx, matel,&
                  base, nh)
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/exixfe.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/mecham.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
#include "asterfort/utmess.h"
    integer :: nh
    character(len=1) :: base
    character(len=8) :: modele, cara
    character(len=*) :: sigg, strx
    character(len=19) :: matel
!     ------------------------------------------------------------------
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
!     CALCUL DES MATRICES ELEMENTAIRES DE RIGIDITE GEOMETRIQUE
!
!     ------------------------------------------------------------------
! IN  : MODELE : NOM DU MODELE
! IN  : CARA   : CHAMP DE CARAC_ELEM
! IN  : SIGG   : CHAMP DE CONTRAINTES AUX POINTS DE GAUSS
! IN  : NH     : NUMERO DE L'HARMONIQUE DE FOURIER
! VAR : MATEL  : NOM DU MATEL (N RESUELEM) PRODUIT
! IN  : BASE   : BASE POUR LA CREATION DE MATEL ('G'/'V')
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    character(len=8) :: lpain(12), lpaout(1)
    character(len=16) :: option
    character(len=24) :: ligrmo, lchin(12), lchout(1)
    character(len=24) :: chgeom, chcara(18), chharm
    character(len=19) :: pintto, cnseto, heavto, loncha, basloc, lsn, lst, stano
    character(len=19) :: pmilto
!
!-----------------------------------------------------------------------
    integer :: icode, ier
!-----------------------------------------------------------------------
    call jemarq()
    if (modele(1:1) .eq. ' ') then
        call utmess('F', 'CALCULEL2_82')
    endif
!
    option = 'RIGI_GEOM'
    call mecham(option, modele, cara, nh, chgeom,&
                chcara, chharm, icode)
!
    call memare(base, matel, modele, ' ', cara,&
                option)
!
!  -----CAS DU MODELE X-FEM-----------------------
    call exixfe(modele, ier)
    if (ier .ne. 0) then
!
        pintto = modele(1:8)//'.TOPOSE.PIN'
        cnseto = modele(1:8)//'.TOPOSE.CNS'
        heavto = modele(1:8)//'.TOPOSE.HEA'
        loncha = modele(1:8)//'.TOPOSE.LON'
        pmilto = modele(1:8)//'.TOPOSE.PMI'
        basloc = modele(1:8)//'.BASLOC'
        lsn = modele(1:8)//'.LNNO'
        lst = modele(1:8)//'.LTNO'
        stano = modele(1:8)//'.STNO'
!
        ligrmo = modele//'.MODELE'
!
! ----- REMPLISSAGE DES CHAMPS D'ENTREE
!
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PCONTRR'
        lchin(2) = sigg
        lpain(3) = 'PPINTTO'
        lchin(3) = pintto
        lpain(4) = 'PHEAVTO'
        lchin(4) = heavto
        lpain(5) = 'PLONCHA'
        lchin(5) = loncha
        lpain(6) = 'PCNSETO'
        lchin(6) = cnseto
        lpain(7) = 'PBASLOR'
        lchin(7) = basloc
        lpain(8) = 'PLSN'
        lchin(8) = lsn
        lpain(9) = 'PLST'
        lchin(9) = lst
        lpain(10) = 'PSTANO'
        lchin(10) = stano
        lpain(11) = 'PPMILTO'
        lchin(11) = pmilto
        lpain(12) = 'PSTRXRR'
        lchin(12) = strx
!
! --- CHAMPS DE SORTIE
!
        lpaout(1) = 'PMATUUR'
        lchout(1) = matel(1:15)//'.ME001'
!
        option = 'RIGI_MECA_GE'
!
        call calcul('S', option, ligrmo, 12, lchin,&
                    lpain, 1, lchout, lpaout, base,&
                    'OUI')
        call reajre(matel, lchout(1), base)
!
    else if (ier.eq.0) then
!
        lpaout(1) = 'PMATUUR'
        lchout(1) = matel(1:8)//'.ME001'
!
        ligrmo = modele//'.MODELE'
        lpain(1) = 'PGEOMER'
        lchin(1) = chgeom
        lpain(2) = 'PCONTRR'
        lchin(2) = sigg
        lpain(3) = 'PCAORIE'
        lchin(3) = chcara(1)
        lpain(4) = 'PCADISK'
        lchin(4) = chcara(2)
        lpain(5) = 'PCAGNPO'
        lchin(5) = chcara(6)
        lpain(6) = 'PCACOQU'
        lchin(6) = chcara(7)
        lpain(7) = 'PEFFORR'
        lchin(7) = sigg
        lpain(8) = 'PHARMON'
        lchin(8) = chharm
        lpain(9) = 'PNBSP_I'
        lchin(9) = chcara(16)
        lpain(10) = 'PSTRXRR'
        lchin(10) = strx
        lpain(11) = 'PFIBRES'
        lchin(11) = chcara(17)
        option = 'RIGI_MECA_GE'
        call calcul('S', option, ligrmo, 11, lchin,&
                    lpain, 1, lchout, lpaout, base,&
                    'OUI')
        call reajre(matel, lchout(1), base)
!
    endif
!
    call jedema()
end subroutine
