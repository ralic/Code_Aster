subroutine mecanb(modele, matel)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
!
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/calcul.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/megeom.h'
    include 'asterfort/memare.h'
    include 'asterfort/reajre.h'
    character(len=8) :: modele
    character(len=19) :: matel
! ----------------------------------------------------------------------
!
!     CALCUL DES SECONDS MEMBRES ELEMENTAIRES CONTENANT LES NORMALES
!     AUX ARETES AUX NOEUDS SOMMETS (EN FAIT LA SOMME DES 2 NORMALES)
!             ( VALABLE POUR LE 2D )
!
!     ENTREE :
!        MODELE : NOM DU MODELE
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) :: lpain(1), lpaout(1)
    character(len=16) :: option
    character(len=24) :: chgeom, lchin(1), lchout(1)
    character(len=24) :: ligrmo
!
!
!
!     -- CALCUL DE .RERR:
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    call memare('V', matel, modele, ' ', ' ',&
                'CHAR_MECA')
!
    call jedetr(matel//'.RELR')
!
    call megeom(modele, chgeom)
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpaout(1) = 'PVECTUR'
    lchout(1) = matel(1:8)//'.VE001'
    ligrmo = modele//'.MODELE'
    option = 'CALC_NOEU_BORD'
    call calcul('S', option, ligrmo, 1, lchin,&
                lpain, 1, lchout, lpaout, 'V',&
                'OUI')
    call reajre(matel, lchout(1), 'V')
!
    call jedema()
end subroutine
