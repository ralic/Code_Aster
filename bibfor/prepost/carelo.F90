subroutine carelo(modele, carele, base, chrel1, chrel2,&
                  chrel3)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/calcul.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/megeom.h'
    character(len=1) :: base
    character(len=8) :: carele, modele
    character(len=19) :: chrel1, chrel2, chrel3
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!     BUT:
!       CALCULER LES REPERES LOCAUX DES ELEMENTS
! ----------------------------------------------------------------------
!     IN MODELE  : MODELE
!     IN CARELE  : CARA_ELEM
!     IN BASE    : G/V
!     OUT CHREL1 : 1ER  VECTEUR DU REPERE LOCAL
!     OUT CHREL2 : 2EME VECTEUR DU REPERE LOCAL
!     OUT CHREL3 : 3EME VECTEUR DU REPERE LOCAL
! ---------------------------------------------------------------------
!     VARIABLES LOCALES
!
    character(len=8) :: lpain(4), lpaou(3)
    character(len=19) :: ligrmo, lchin(4), lchou(3)
    character(len=24) :: chgeom
    logical :: lret
! ----------------------------------------------------------------------
!
    call jemarq()
!
    ligrmo = modele//'.MODELE'
!
    call megeom(modele, chgeom)
    lchin(1) = chgeom
    lpain(1) = 'PGEOMER'
    lchin(2) = carele//'.CARORIEN'
    lpain(2) = 'PCAORIE'
    lchin(3) = carele//'.CARCOQUE'
    lpain(3) = 'PCACOQU'
    lchin(4) = carele//'.CARMASSI'
    lpain(4) = 'PCAMASS'
!
    lchou(1) = chrel1
    lpaou(1) = 'PREPLO1'
    lchou(2) = chrel2
    lpaou(2) = 'PREPLO2'
    lchou(3) = chrel3
    lpaou(3) = 'PREPLO3'
    call calcul('C', 'REPERE_LOCAL', ligrmo, 4, lchin,&
                lpain, 3, lchou, lpaou, base,&
                'NON')
!
    call jedema()
end subroutine
