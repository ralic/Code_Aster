subroutine irgmor(tord, vers)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jexnom.h'
    integer :: ntyele, neletr
    parameter (ntyele = 28)
    parameter (neletr =  8)
!
    integer :: tord(ntyele)
    integer :: vers
!     ------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ------------------------------------------------------------------
!     IN  : VERS
!     OUT : TORD
!
!     NTYELE : NOMBRE DE TYPES DE MAILLES TRAITEES
!              (MAX DE TYPE_MAILLE__.CATA)
!
!     NELETR : NOMBRE DE TYPES DE MAILLES TRANSMISES A GMSH
!
!     RETOURNE LE TABLEAU DANS L'ORDRE DANS LEQUEL ON DOIT IMPRIMER
!     LES ELEMENTS (OU PLUTOT LES VALEURS QU'ILS PORTENT)
!      TORD(1)=NUMERO DE LA MAILLE POI1...
!
!     DOC GMSH (FILE FORMATS VERSION 1.2)
!        point
!        line
!        triangle
!        quadrangle
!        tetrahedron
!        hexahedron
!        prism
!        pyramid     ET POUR CHAQUE scalar, vector, puis tensor
!
!     ------------------------------------------------------------------
    integer :: i, ind
!
! --- DATA QUI DEFINIT L'ORDRE
!     (IDENTIQUE EN VERSION 1.0 ET 1.2 PUISQUE ON AURA ZERO ELEMENT SUR
!      LES TYPES QUE LA VERSION 1.0 NE CONNAIT PAS)
    character(len=8) :: formgm(neletr)
    data formgm/'POI1',  'SEG2',   'TRIA3', 'QUAD4', 'TETRA4',&
     &            'HEXA8', 'PENTA6', 'PYRAM5'/
!     ------------------------------------------------------------------
! --- VERIF
    if (vers .ne. 1 .and. vers .ne. 2) goto 999
!
! --- REMPLISSAGE QUI POURRAIT VARIER SELON LA VERSION
    do 10 i = 1, neletr
        call jenonu(jexnom('&CATA.TM.NOMTM', formgm(i)), ind)
        if (ind .gt. ntyele) goto 999
        tord(i)=ind
10  end do
    goto 9000
!
!     VERIFICATION EMMELAGE DE PINCEAUX DU PROGRAMMEUR...
999  continue
    call assert(.false.)
!     ------------------------------------------------------------------
!
9000  continue
end subroutine
