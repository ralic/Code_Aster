subroutine contac(macor, nbcor, macoc, nbcoc, lface,&
                  lomodi, locorr, loreor, ma)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!  ROUTINE CONTAC
!    ROUTINE D ORIENTATION EN FONCTION DE KTYC
!  DECLARATIONS
!    KTYC   : NOM DU TYPE                   POUR UNE MAILLE FISSURE
!    KTYR   : NOM DU TYPE                   POUR UNE MAILLE REFERENCE
!    LOMODI : LOGICAL PRECISANT SI LA MAILLE EST UNE MAILLE MODIFIE
!    LOCORR : LOGICAL PRECISANT SI LA MAILLE EST BIEN ORIENTEE
!    LOREOR : LOGICAL PRECISANT SI LA MAILLE EST REORIENTEE
!    MA     : L OBJET DU MAILLAGE
!    MACOC  : TABLEAU DES NOMS DES NOEUDS   POUR UNE MAILLE FISSURE
!    MACOR  : TABLEAU DES NOMS DES NOEUDS   POUR UNE MAILLE REFERENCE
!    NBCOC  : NOMBRE DE CONNEX              POUR UNE MAILLE FISSURE
!    NBCOR  : NOMBRE DE CONNEX              POUR UNE MAILLE REFERENCE
!
!  MOT_CLEF : ORIE_FISSURE
!
!
    implicit none
!
!     ------------------------------------------------------------------
!
    include 'asterfort/conhex.h'
    include 'asterfort/conpen.h'
    include 'asterfort/conqua.h'
    character(len=8) :: ktyc, ktyr
    character(len=8) :: macor(nbcor+2), macoc(nbcoc+2), ma
!
    logical :: lface, lomodi, locorr, loreor
!
    logical :: qua, pen, hex
    integer :: nbcoc, nbcor
!-----------------------------------------------------------------------
!     FONCTIONS FORMULES PERMETTANT DE SAVOIR SI L'APPUI EST POSSIBLE
    qua()=(ktyc.eq.'QUAD4'.and.(ktyr.eq.'QUAD4'.or.ktyr.eq.'TRIA3'))&
     & .or. (ktyc.eq.'QUAD8'.and.(ktyr.eq.'QUAD9'.or.ktyr.eq.'QUAD8'&
     &                        .or.ktyr.eq.'TRIA6'))
    pen()=(ktyc.eq.'PENTA6 '.and.&
     &      (ktyr.eq.'PENTA6 '.or.ktyr.eq.'TETRA4'))&
     & .or. (ktyc.eq.'PENTA15'.and.&
     &      (ktyr.eq.'PENTA15'.or.ktyr.eq.'TETRA10'))
    hex()=(ktyc.eq.'HEXA8 '.and.&
     &     (ktyr.eq.'HEXA8 '.or.ktyr.eq.'PENTA6 '.or.ktyr.eq.'PYRAM5 '))&
     & .or. (ktyc.eq.'HEXA20'.and.&
     &     (ktyr.eq.'HEXA20'.or.ktyr.eq.'PENTA15'.or.ktyr.eq.'PYRAM13'))
!     ------------------------------------------------------------------
!
    ktyc = macoc(2)
    ktyr = macor(2)
!
    if (qua()) then
!
        call conqua(macor, nbcor, macoc, nbcoc, lface,&
                    lomodi, locorr, loreor, ma)
!
    else if (pen()) then
!
        call conpen(macor, nbcor, macoc, nbcoc, lface,&
                    locorr, loreor, ma)
!
    else if (hex()) then
!
        call conhex(macor, nbcor, macoc, nbcoc, lface,&
                    lomodi, locorr, loreor, ma)
!
    else
!
        loreor=.false.
!
    endif
!
!     ------------------------------------------------------------------
end subroutine
