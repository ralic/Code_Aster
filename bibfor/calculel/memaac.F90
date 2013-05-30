subroutine memaac(modele, mate, matel)
    implicit none
    include 'jeveux.h'
    include 'asterfort/calcul.h'
    include 'asterfort/codent.h'
    include 'asterfort/exisd.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/megeom.h'
    include 'asterfort/memare.h'
    include 'asterfort/reajre.h'
    include 'asterfort/u2mess.h'
    character(len=8) :: modele
    character(len=19) :: matel
    character(len=*) :: mate
! ----------------------------------------------------------------------
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
!     CALCUL DES MATRICES ELEMENTAIRES DE MASSE_ACOUSTIQUE
!                ( 'MASS_ACOU', ISO )
!
!     ENTREES:
!
!     LES NOMS QUI SUIVENT SONT LES PREFIXES UTILISATEUR K8:
!        MODELE : NOM DU MODELE
!*       MATE   : CARTE DE MATERIAU CODE
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
!*
!*
    character(len=8) :: lpain(4), lpaout(1)
    character(len=16) :: option
!*
    character(len=24) :: chgeom, lchin(4), lchout(1)
    character(len=24) :: ligrmo
!
!
!-----------------------------------------------------------------------
    integer :: ilires, iret
!-----------------------------------------------------------------------
    call jemarq()
    if (modele(1:1) .eq. ' ') then
        call u2mess('F', 'CALCULEL3_50')
    endif
!
    call megeom(modele, chgeom)
!
    call jeexin(matel//'.RERR', iret)
    if (iret .gt. 0) then
        call jedetr(matel//'.RERR')
        call jedetr(matel//'.RELR')
    endif
    call memare('G', matel, modele, mate, ' ',&
                'MASS_ACOU')
!
    lpaout(1) = 'PMATTTC'
    lchout(1) = matel(1:8)//'.ME000'
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
!**
    lpain(2) = 'PMATERC'
    lchin(2) = mate
!**
!
    ligrmo = modele//'.MODELE'
    option = 'MASS_ACOU'
    ilires = 0
    ilires = ilires + 1
    call codent(ilires, 'D0', lchout(1) (12:14))
    call calcul('S', option, ligrmo, 2, lchin,&
                lpain, 1, lchout, lpaout, 'G',&
                'OUI')
    call exisd('CHAMP_GD', lchout(1) (1:19), iret)
    call reajre(matel, lchout(1), 'G')
!
    call jedema()
end subroutine
