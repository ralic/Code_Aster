function nutygm(nomtyp)
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
    integer :: nutygm
    character(len=8) :: nomtyp
!-----------------------------------------------------------------------
!
!- BUT : CETTE FONCTION RETOURNE LE TYPE GMSH DU TYPE D'ELEMENT ASTER
!
!-----------------------------------------------------------------------
! --- DESCRIPTION DES PARAMETRES
! IN   K8  NOMTYP  : NOM    ASTER DU TYPE D'ELEMENT
! OUT  I   NUTYGM  : NUMERO GMSH  DU TYPE D'ELEMENT
! ----------------------------------------------------------------------
!
    if (nomtyp .eq. 'SEG2') then
        nutygm = 1
    else if (nomtyp.eq.'TRIA3') then
        nutygm = 2
    else if (nomtyp.eq.'QUAD4') then
        nutygm = 3
    else if (nomtyp.eq.'TETRA4') then
        nutygm = 4
    else if (nomtyp.eq.'HEXA8') then
        nutygm = 5
    else if (nomtyp.eq.'PENTA6') then
        nutygm = 6
    else if (nomtyp.eq.'PYRAM5') then
        nutygm = 7
    else if (nomtyp.eq.'POI1') then
        nutygm = 15
    endif
!
end function
