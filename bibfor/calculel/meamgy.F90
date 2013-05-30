subroutine meamgy(modele, mate, cara, compor, matel)
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/assert.h'
    include 'asterfort/calcul.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/mecara.h'
    include 'asterfort/megeom.h'
    include 'asterfort/memare.h'
    include 'asterfort/reajre.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: modele, cara
    character(len=19) :: matel
    character(len=24) :: mate
    character(len=*) :: compor
! ----------------------------------------------------------------------
!     CALCUL DES MATRICES ELEMENTAIRES D'AMORTISSEMENT GYROSCOPIQUE
!     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
!     ENTREES:
!        MODELE : NOM DU MODELE
!        MATE   : CHAMP DE MATERIAUX
!        CARA   : CARACTERISTIQUES ELEMENTAIRES
!     SORTIES:
!        MATEL  : NOM DU MATEL (MATRICE ELEMENTAIRE) PRODUIT
!
! ----------------------------------------------------------------------
!----------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    logical :: exicar
    character(len=8) :: lpain(10), lpaout(1)
    character(len=16) :: option
    character(len=24) :: chgeom, chcara(18), lchin(10), lchout(10)
    character(len=24) :: ligrmo
!
!
    call jemarq()
!
    call assert(modele.ne.' ')
!      CALL ASSERT(MATE.NE.' ')
    call assert(cara.ne.' ')
!
    call jedetr(matel//'.RERR')
    call jedetr(matel//'.RELR')
!
    call memare('G', matel, modele, mate, ' ',&
                'MECA_GYRO')
!
    call megeom(modele, chgeom)
    call assert(chgeom.ne.' ')
!
!
!    CHAMP DE CARACTERISTIQUES ELEMENTAIRES
    call mecara(cara, exicar, chcara)
    if (.not.exicar) call u2mess('F', 'CALCULEL2_94')
!
!
    lpaout(1) = 'PMATUNS'
    lchout(1) = matel(1:8)//'.ME001'
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PMATERC'
    lchin(2) = mate
    lpain(3) = 'PCAORIE'
    lchin(3) = chcara(1)
    lpain(4) = 'PCAGNPO'
    lchin(4) = chcara(6)
    lpain(5) = 'PCADISM'
    lchin(5) = chcara(3)
    lpain(6) = 'PCINFDI'
    lchin(6) = chcara(15)
    lpain(7) = 'PCAARPO'
    lchin(7) = chcara(9)
    lpain(8) = 'PNBSP_I'
    lchin(8) = chcara(16)
    lpain(9) = 'PFIBRES'
    lchin(9) = chcara(17)
    lpain(10) = 'PCOMPOR'
    lchin(10) = compor
    ligrmo = modele//'.MODELE'
    option = 'MECA_GYRO'
!
!
    call calcul('S', option, ligrmo, 10, lchin,&
                lpain, 1, lchout, lpaout, 'G',&
                'OUI')
!
    call reajre(matel, lchout(1), 'G')
!
    call jedema()
!
end subroutine
