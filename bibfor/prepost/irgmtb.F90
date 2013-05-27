subroutine irgmtb(tdec, typd, vers)
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/assert.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    integer :: ntyele, maxel, maxno
    parameter (ntyele = 28)
    parameter (maxel  = 48)
    parameter (maxno  =  8)
!
    integer :: tdec(ntyele, maxel, maxno)
    integer :: typd(ntyele, 3)
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
! TOLE CRP_20
!     ------------------------------------------------------------------
!     IN  : VERS
!     OUT : TDEC, TYPD
!
!     NTYELE : NOMBRE DE TYPES DE MAILLES TRAITEES
!              (MAX DE TYPE_MAILLE__.CATA)
!     MAXEL  : NOMBRE MAX D'ELEMENTS DES MAILLES TRAITEES
!     MAXNO  : NOMBRE MAX DE NOEUDS DES NOUVEAUX ELEMENTS
!
!     RETOURNE LE TABLEAU DE DECOUPAGE DES ELEMENTS :
!     L'INDICE i EST LE NUMERO D'ORDRE DANS LE CATALOGUE
!       TDEC(i,.,.) : POI1 (NON DECOUPE)
!            i      : SEG2 (NON DECOUPE)
!            i      : SEG3
!            i      : SEG4
!            i      : TRIA3 (NON DECOUPE)
!            i      : TRIA6
!            i      : TRIA7
!            i      : QUAD4 (NON DECOUPE EN 1.2)
!            i      : QUAD8
!            i      : QUAD9
!            i      : TETRA4 (NON DECOUPE)
!            i      : TETRA10
!            i      : PENTA6 (NON DECOUPE EN 1.2)
!            i      : PENTA15
!            i      : PYRAM5 (NON DECOUPE EN 1.2)
!            i      : HEXA8 (NON DECOUPE EN 1.2)
!            i      : HEXA20
!            i      : HEXA27
!
!     TYPD DIT EN QUOI ON A DECOUPE ET COMBIEN
!     TYPD(.,1)=i CORRESPONDANT A POI1, SEG2, TRIA3, TETRA4 EN 1.0
!                           + QUAD4, PENTA6, PYRAM5, HEXA8 EN 1.2
!     TYPD(.,2)=NOMBRE D'ELEMENTS CREES
!     TYPD(.,3)=NOMBRE DE POINTS POUR CET ELEMENT (REMPLI A LA FIN)
!
!     VERS = 1 == '1.0'    VERSION DU
!          = 2 == '1.2'    FICHIER GMSH
!     EN VERSION 1.2, ON NE DECOUPE PAS LES QUAD4, PENTA6, PYRAM5, HEXA8
!
    integer :: typpoi, typseg, typtri, typtet, ino, i, j, k, ind
!
! --- VERIF
    if (vers .ne. 1 .and. vers .ne. 2) goto 999
!
! --- INITIALISATIONS
    do 10 i = 1, ntyele
        do 11 j = 1, maxel
            do 11 k = 1, maxno
                tdec(i,j,k)=0
11          continue
        typd(i,1)=0
        typd(i,2)=0
        typd(i,3)=0
10  end do
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1' ), typpoi)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2' ), typseg)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA3' ), typtri)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TETRA4' ), typtet)
!
!
! --- ECLATEMENT POI1 EN 1 POI1
    call jenonu(jexnom('&CATA.TM.NOMTM', 'POI1'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typpoi
    typd(ind,2)=1
    tdec(ind,1,1)=1
!
! --- ECLATEMENT SEG2 EN 1 SEG2
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG2'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typseg
    typd(ind,2)=1
    tdec(ind,1,1)=1
    tdec(ind,1,2)=2
!
! --- ECLATEMENT SEG3 EN 2 SEG2
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG3'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typseg
    typd(ind,2)=2
    tdec(ind,1,1) = 1
    tdec(ind,1,2) = 3
    tdec(ind,2,1) = 3
    tdec(ind,2,2) = 2
!
! --- ECLATEMENT SEG4 EN 3 SEG2
    call jenonu(jexnom('&CATA.TM.NOMTM', 'SEG4'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typseg
    typd(ind,2)=3
    tdec(ind,1,1) = 1
    tdec(ind,1,2) = 3
    tdec(ind,2,1) = 3
    tdec(ind,2,2) = 4
    tdec(ind,3,1) = 4
    tdec(ind,3,2) = 2
!
! --- ECLATEMENT TRIA3 EN 1 TRIA3
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA3'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typtri
    typd(ind,2)=1
    tdec(ind,1,1)=1
    tdec(ind,1,2)=2
    tdec(ind,1,3)=3
!
! --- ECLATEMENT TRIA6 EN 4 TRIA3
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA6'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typtri
    typd(ind,2)=4
    tdec(ind,1,1) = 1
    tdec(ind,1,2) = 4
    tdec(ind,1,3) = 6
    tdec(ind,2,1) = 4
    tdec(ind,2,2) = 2
    tdec(ind,2,3) = 5
    tdec(ind,3,1) = 5
    tdec(ind,3,2) = 3
    tdec(ind,3,3) = 6
    tdec(ind,4,1) = 4
    tdec(ind,4,2) = 5
    tdec(ind,4,3) = 6
!
! --- ECLATEMENT TRIA7 EN 4 TRIA3 COMME TRIA6
! --- CAR DX DY DZ INCONNUS SUR LE NOEUD 7 EN COQUE_3D
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TRIA7'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typtri
    typd(ind,2)=4
    tdec(ind,1,1) = 1
    tdec(ind,1,2) = 4
    tdec(ind,1,3) = 6
    tdec(ind,2,1) = 4
    tdec(ind,2,2) = 2
    tdec(ind,2,3) = 5
    tdec(ind,3,1) = 5
    tdec(ind,3,2) = 3
    tdec(ind,3,3) = 6
    tdec(ind,4,1) = 4
    tdec(ind,4,2) = 5
    tdec(ind,4,3) = 6
!
! --- ECLATEMENT QUAD4 EN 2 TRIA3
!     OU NON DECOUPE
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD4'), ind)
    if (ind .gt. ntyele) goto 999
    if (vers .eq. 1) then
        typd(ind,1)=typtri
        typd(ind,2)=2
        tdec(ind,1,1) = 1
        tdec(ind,1,2) = 2
        tdec(ind,1,3) = 3
        tdec(ind,2,1) = 1
        tdec(ind,2,2) = 3
        tdec(ind,2,3) = 4
    else if (vers.eq.2) then
        typd(ind,1)=ind
        typd(ind,2)=1
        tdec(ind,1,1) = 1
        tdec(ind,1,2) = 2
        tdec(ind,1,3) = 3
        tdec(ind,1,4) = 4
    endif
!
! --- ECLATEMENT QUAD8 EN 6 TRIA3
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD8'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typtri
    typd(ind,2)=6
    tdec(ind,1,1) = 1
    tdec(ind,1,2) = 5
    tdec(ind,1,3) = 8
    tdec(ind,2,1) = 5
    tdec(ind,2,2) = 2
    tdec(ind,2,3) = 6
    tdec(ind,3,1) = 6
    tdec(ind,3,2) = 3
    tdec(ind,3,3) = 7
    tdec(ind,4,1) = 7
    tdec(ind,4,2) = 4
    tdec(ind,4,3) = 8
    tdec(ind,5,1) = 5
    tdec(ind,5,2) = 7
    tdec(ind,5,3) = 8
    tdec(ind,6,1) = 5
    tdec(ind,6,2) = 6
    tdec(ind,6,3) = 7
!
! --- ECLATEMENT QUAD9 EN 6 TRIA3 COMME QUAD8
! --- CAR DX DY DZ INCONNUS SUR LE NOEUD 9 EN COQUE_3D
    call jenonu(jexnom('&CATA.TM.NOMTM', 'QUAD9'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typtri
    typd(ind,2)=6
    tdec(ind,1,1) = 1
    tdec(ind,1,2) = 5
    tdec(ind,1,3) = 8
    tdec(ind,2,1) = 5
    tdec(ind,2,2) = 2
    tdec(ind,2,3) = 6
    tdec(ind,3,1) = 6
    tdec(ind,3,2) = 3
    tdec(ind,3,3) = 7
    tdec(ind,4,1) = 7
    tdec(ind,4,2) = 4
    tdec(ind,4,3) = 8
    tdec(ind,5,1) = 5
    tdec(ind,5,2) = 7
    tdec(ind,5,3) = 8
    tdec(ind,6,1) = 5
    tdec(ind,6,2) = 6
    tdec(ind,6,3) = 7
!
! --- ECLATEMENT TETRA4 EN 1 TETRA4
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TETRA4'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typtet
    typd(ind,2)=1
    tdec(ind,1,1) = 1
    tdec(ind,1,2) = 2
    tdec(ind,1,3) = 3
    tdec(ind,1,4) = 4
!
! --- ECLATEMENT TETRA10 EN 8 TETRA4
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TETRA10'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typtet
    typd(ind,2)=8
    tdec(ind,1,1) = 1
    tdec(ind,1,2) = 5
    tdec(ind,1,3) = 7
    tdec(ind,1,4) = 8
    tdec(ind,2,1) = 2
    tdec(ind,2,2) = 9
    tdec(ind,2,3) = 6
    tdec(ind,2,4) = 5
    tdec(ind,3,1) = 3
    tdec(ind,3,2) = 6
    tdec(ind,3,3) = 7
    tdec(ind,3,4) = 10
    tdec(ind,4,1) = 4
    tdec(ind,4,2) = 8
    tdec(ind,4,3) = 9
    tdec(ind,4,4) = 10
    tdec(ind,5,1) = 6
    tdec(ind,5,2) = 7
    tdec(ind,5,3) = 9
    tdec(ind,5,4) = 5
    tdec(ind,6,1) = 7
    tdec(ind,6,2) = 8
    tdec(ind,6,3) = 9
    tdec(ind,6,4) = 5
    tdec(ind,7,1) = 7
    tdec(ind,7,2) = 8
    tdec(ind,7,3) = 9
    tdec(ind,7,4) = 10
    tdec(ind,8,1) = 6
    tdec(ind,8,2) = 7
    tdec(ind,8,3) = 9
    tdec(ind,8,4) = 10
!
! --- ECLATEMENT PENTA6 EN 3 TETRA4
!     OU NON DECOUPE
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA6'), ind)
    if (ind .gt. ntyele) goto 999
    if (vers .eq. 1) then
        typd(ind,1)=typtet
        typd(ind,2)=3
        tdec(ind,1,1) = 1
        tdec(ind,1,2) = 2
        tdec(ind,1,3) = 3
        tdec(ind,1,4) = 5
        tdec(ind,2,1) = 1
        tdec(ind,2,2) = 3
        tdec(ind,2,3) = 6
        tdec(ind,2,4) = 5
        tdec(ind,3,1) = 1
        tdec(ind,3,2) = 6
        tdec(ind,3,3) = 4
        tdec(ind,3,4) = 5
    else if (vers.eq.2) then
        typd(ind,1)=ind
        typd(ind,2)=1
        tdec(ind,1,1) = 1
        tdec(ind,1,2) = 2
        tdec(ind,1,3) = 3
        tdec(ind,1,4) = 4
        tdec(ind,1,5) = 5
        tdec(ind,1,6) = 6
    endif
!
! --- ECLATEMENT PENTA15 EN 16 TETRA4
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA15'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typtet
    typd(ind,2)=16
    tdec(ind,1,1) = 4
    tdec(ind,1,2) = 10
    tdec(ind,1,3) = 13
    tdec(ind,1,4) = 15
    tdec(ind,2,1) = 1
    tdec(ind,2,2) = 7
    tdec(ind,2,3) = 9
    tdec(ind,2,4) = 13
    tdec(ind,3,1) = 1
    tdec(ind,3,2) = 9
    tdec(ind,3,3) = 15
    tdec(ind,3,4) = 13
    tdec(ind,4,1) = 1
    tdec(ind,4,2) = 15
    tdec(ind,4,3) = 10
    tdec(ind,4,4) = 13
    tdec(ind,5,1) = 5
    tdec(ind,5,2) = 11
    tdec(ind,5,3) = 13
    tdec(ind,5,4) = 14
    tdec(ind,6,1) = 2
    tdec(ind,6,2) = 7
    tdec(ind,6,3) = 8
    tdec(ind,6,4) = 13
    tdec(ind,7,1) = 2
    tdec(ind,7,2) = 8
    tdec(ind,7,3) = 14
    tdec(ind,7,4) = 13
    tdec(ind,8,1) = 2
    tdec(ind,8,2) = 14
    tdec(ind,8,3) = 11
    tdec(ind,8,4) = 13
    tdec(ind,9,1) = 6
    tdec(ind,9,2) = 14
    tdec(ind,9,3) = 15
    tdec(ind,9,4) = 12
    tdec(ind,10,1) = 14
    tdec(ind,10,2) = 13
    tdec(ind,10,3) = 15
    tdec(ind,10,4) = 12
    tdec(ind,11,1) = 3
    tdec(ind,11,2) = 8
    tdec(ind,11,3) = 14
    tdec(ind,11,4) = 7
    tdec(ind,12,1) = 3
    tdec(ind,12,2) = 14
    tdec(ind,12,3) = 13
    tdec(ind,12,4) = 7
    tdec(ind,13,1) = 3
    tdec(ind,13,2) = 13
    tdec(ind,13,3) = 9
    tdec(ind,13,4) = 7
    tdec(ind,14,1) = 3
    tdec(ind,14,2) = 14
    tdec(ind,14,3) = 12
    tdec(ind,14,4) = 15
    tdec(ind,15,1) = 3
    tdec(ind,15,2) = 14
    tdec(ind,15,3) = 13
    tdec(ind,15,4) = 15
    tdec(ind,16,1) = 2
    tdec(ind,16,2) = 13
    tdec(ind,16,3) = 15
    tdec(ind,16,4) = 9
!
! --- ECLATEMENT PENTA18 EN 8 PENTA6 24 TETRA4
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA18'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typtet
    typd(ind,2)=24
    tdec(ind,1,1) = 1
    tdec(ind,1,2) = 7
    tdec(ind,1,3) = 9
    tdec(ind,1,4) = 16
    tdec(ind,2,1) = 1
    tdec(ind,2,2) = 9
    tdec(ind,2,3) = 18
    tdec(ind,2,4) = 16
    tdec(ind,3,1) = 1
    tdec(ind,3,2) = 18
    tdec(ind,3,3) = 10
    tdec(ind,3,4) = 16
    tdec(ind,4,1) = 7
    tdec(ind,4,2) = 8
    tdec(ind,4,3) = 9
    tdec(ind,4,4) = 17
    tdec(ind,5,1) = 7
    tdec(ind,5,2) = 9
    tdec(ind,5,3) = 18
    tdec(ind,5,4) = 17
    tdec(ind,6,1) = 7
    tdec(ind,6,2) = 18
    tdec(ind,6,3) = 16
    tdec(ind,6,4) = 17
    tdec(ind,7,1) = 9
    tdec(ind,7,2) = 8
    tdec(ind,7,3) = 3
    tdec(ind,7,4) = 17
    tdec(ind,8,1) = 9
    tdec(ind,8,2) = 3
    tdec(ind,8,3) = 12
    tdec(ind,8,4) = 17
    tdec(ind,9,1) = 9
    tdec(ind,9,2) = 12
    tdec(ind,9,3) = 18
    tdec(ind,9,4) = 17
    tdec(ind,10,1) = 7
    tdec(ind,10,2) = 2
    tdec(ind,10,3) = 8
    tdec(ind,10,4) = 11
    tdec(ind,11,1) = 7
    tdec(ind,11,2) = 8
    tdec(ind,11,3) = 17
    tdec(ind,11,4) = 11
    tdec(ind,12,1) = 7
    tdec(ind,12,2) = 17
    tdec(ind,12,3) = 16
    tdec(ind,12,4) = 11
    tdec(ind,13,1) = 10
    tdec(ind,13,2) = 16
    tdec(ind,13,3) = 18
    tdec(ind,13,4) = 13
    tdec(ind,14,1) = 10
    tdec(ind,14,2) = 18
    tdec(ind,14,3) = 15
    tdec(ind,14,4) = 13
    tdec(ind,15,1) = 10
    tdec(ind,15,2) = 15
    tdec(ind,15,3) = 4
    tdec(ind,15,4) = 13
    tdec(ind,16,1) = 16
    tdec(ind,16,2) = 17
    tdec(ind,16,3) = 18
    tdec(ind,16,4) = 14
    tdec(ind,17,1) = 16
    tdec(ind,17,2) = 18
    tdec(ind,17,3) = 15
    tdec(ind,17,4) = 14
    tdec(ind,18,1) = 16
    tdec(ind,18,2) = 15
    tdec(ind,18,3) = 13
    tdec(ind,18,4) = 14
    tdec(ind,19,1) = 18
    tdec(ind,19,2) = 17
    tdec(ind,19,3) = 12
    tdec(ind,19,4) = 14
    tdec(ind,20,1) = 18
    tdec(ind,20,2) = 12
    tdec(ind,20,3) = 6
    tdec(ind,20,4) = 14
    tdec(ind,21,1) = 18
    tdec(ind,21,2) = 6
    tdec(ind,21,3) = 15
    tdec(ind,21,4) = 14
    tdec(ind,22,1) = 16
    tdec(ind,22,2) = 11
    tdec(ind,22,3) = 17
    tdec(ind,22,4) = 5
    tdec(ind,23,1) = 16
    tdec(ind,23,2) = 17
    tdec(ind,23,3) = 14
    tdec(ind,23,4) = 5
    tdec(ind,24,1) = 16
    tdec(ind,24,2) = 14
    tdec(ind,24,3) = 13
    tdec(ind,24,4) = 5
!
! --- ECLATEMENT PYRAM5 EN 2 TETRA4
!     OU NON DECOUPE
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PYRAM5'), ind)
    if (ind .gt. ntyele) goto 999
    if (vers .eq. 1) then
        typd(ind,1)=typtet
        typd(ind,2)=2
        tdec(ind,1,1) = 1
        tdec(ind,1,2) = 2
        tdec(ind,1,3) = 4
        tdec(ind,1,4) = 5
        tdec(ind,2,1) = 2
        tdec(ind,2,2) = 3
        tdec(ind,2,3) = 4
        tdec(ind,2,4) = 5
    else if (vers.eq.2) then
        typd(ind,1)=ind
        typd(ind,2)=1
        tdec(ind,1,1) = 1
        tdec(ind,1,2) = 2
        tdec(ind,1,3) = 3
        tdec(ind,1,4) = 4
        tdec(ind,1,5) = 5
    endif
!
! --- ECLATEMENT HEXA8 EN 6 TETRA4
!     OU NON DECOUPE
    call jenonu(jexnom('&CATA.TM.NOMTM', 'HEXA8'), ind)
    if (ind .gt. ntyele) goto 999
    if (vers .eq. 1) then
        typd(ind,1)=typtet
        typd(ind,2)=6
        tdec(ind,1,1) = 1
        tdec(ind,1,2) = 2
        tdec(ind,1,3) = 3
        tdec(ind,1,4) = 5
        tdec(ind,2,1) = 2
        tdec(ind,2,2) = 3
        tdec(ind,2,3) = 5
        tdec(ind,2,4) = 7
        tdec(ind,3,1) = 6
        tdec(ind,3,2) = 2
        tdec(ind,3,3) = 5
        tdec(ind,3,4) = 7
        tdec(ind,4,1) = 1
        tdec(ind,4,2) = 4
        tdec(ind,4,3) = 3
        tdec(ind,4,4) = 7
        tdec(ind,5,1) = 1
        tdec(ind,5,2) = 7
        tdec(ind,5,3) = 4
        tdec(ind,5,4) = 8
        tdec(ind,6,1) = 8
        tdec(ind,6,2) = 5
        tdec(ind,6,3) = 1
        tdec(ind,6,4) = 7
    else if (vers.eq.2) then
        typd(ind,1)=ind
        typd(ind,2)=1
        tdec(ind,1,1) = 1
        tdec(ind,1,2) = 2
        tdec(ind,1,3) = 3
        tdec(ind,1,4) = 4
        tdec(ind,1,5) = 5
        tdec(ind,1,6) = 6
        tdec(ind,1,7) = 7
        tdec(ind,1,8) = 8
    endif
!
! --- ECLATEMENT HEXA20 EN 24 TETRA4
    call jenonu(jexnom('&CATA.TM.NOMTM', 'HEXA20'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typtet
    typd(ind,2)=24
    tdec(ind,1,1) = 1
    tdec(ind,1,2) = 9
    tdec(ind,1,3) = 12
    tdec(ind,1,4) = 13
    tdec(ind,2,1) = 2
    tdec(ind,2,2) = 10
    tdec(ind,2,3) = 14
    tdec(ind,2,4) = 9
    tdec(ind,3,1) = 3
    tdec(ind,3,2) = 15
    tdec(ind,3,3) = 11
    tdec(ind,3,4) = 10
    tdec(ind,4,1) = 4
    tdec(ind,4,2) = 11
    tdec(ind,4,3) = 12
    tdec(ind,4,4) = 16
    tdec(ind,5,1) = 7
    tdec(ind,5,2) = 19
    tdec(ind,5,3) = 18
    tdec(ind,5,4) = 15
    tdec(ind,6,1) = 6
    tdec(ind,6,2) = 18
    tdec(ind,6,3) = 17
    tdec(ind,6,4) = 14
    tdec(ind,7,1) = 5
    tdec(ind,7,2) = 20
    tdec(ind,7,3) = 17
    tdec(ind,7,4) = 13
    tdec(ind,8,1) = 9
    tdec(ind,8,2) = 12
    tdec(ind,8,3) = 13
    tdec(ind,8,4) = 20
    tdec(ind,9,1) = 9
    tdec(ind,9,2) = 17
    tdec(ind,9,3) = 16
    tdec(ind,9,4) = 20
    tdec(ind,10,1) = 17
    tdec(ind,10,2) = 9
    tdec(ind,10,3) = 13
    tdec(ind,10,4) = 20
    tdec(ind,11,1) = 9
    tdec(ind,11,2) = 11
    tdec(ind,11,3) = 12
    tdec(ind,11,4) = 16
    tdec(ind,12,1) = 9
    tdec(ind,12,2) = 11
    tdec(ind,12,3) = 16
    tdec(ind,12,4) = 17
    tdec(ind,13,1) = 11
    tdec(ind,13,2) = 19
    tdec(ind,13,3) = 16
    tdec(ind,13,4) = 17
    tdec(ind,14,1) = 16
    tdec(ind,14,2) = 19
    tdec(ind,14,3) = 20
    tdec(ind,14,4) = 17
    tdec(ind,15,1) = 9
    tdec(ind,15,2) = 14
    tdec(ind,15,3) = 11
    tdec(ind,15,4) = 17
    tdec(ind,16,1) = 14
    tdec(ind,16,2) = 11
    tdec(ind,16,3) = 17
    tdec(ind,16,4) = 19
    tdec(ind,17,1) = 9
    tdec(ind,17,2) = 10
    tdec(ind,17,3) = 11
    tdec(ind,17,4) = 14
    tdec(ind,18,1) = 10
    tdec(ind,18,2) = 11
    tdec(ind,18,3) = 14
    tdec(ind,18,4) = 19
    tdec(ind,19,1) = 18
    tdec(ind,19,2) = 10
    tdec(ind,19,3) = 14
    tdec(ind,19,4) = 19
    tdec(ind,20,1) = 18
    tdec(ind,20,2) = 14
    tdec(ind,20,3) = 17
    tdec(ind,20,4) = 19
    tdec(ind,21,1) = 10
    tdec(ind,21,2) = 15
    tdec(ind,21,3) = 11
    tdec(ind,21,4) = 19
    tdec(ind,22,1) = 15
    tdec(ind,22,2) = 10
    tdec(ind,22,3) = 18
    tdec(ind,22,4) = 19
    tdec(ind,23,1) = 8
    tdec(ind,23,2) = 19
    tdec(ind,23,3) = 20
    tdec(ind,23,4) = 16
    tdec(ind,24,1) = 12
    tdec(ind,24,2) = 9
    tdec(ind,24,3) = 16
    tdec(ind,24,4) = 20
!
! --- ECLATEMENT HEXA27 EN 16 PENTA6 = 48 TETRA4
    call jenonu(jexnom('&CATA.TM.NOMTM', 'HEXA27'), ind)
    if (ind .gt. ntyele) goto 999
    typd(ind,1)=typtet
    typd(ind,2)=48
    tdec(ind,1,1) = 9
    tdec(ind,1,2) = 2
    tdec(ind,1,3) = 14
    tdec(ind,1,4) = 10
    tdec(ind,2,1) = 9
    tdec(ind,2,2) = 14
    tdec(ind,2,3) = 23
    tdec(ind,2,4) = 10
    tdec(ind,3,1) = 9
    tdec(ind,3,2) = 23
    tdec(ind,3,3) = 21
    tdec(ind,3,4) = 10
    tdec(ind,4,1) = 9
    tdec(ind,4,2) = 14
    tdec(ind,4,3) = 22
    tdec(ind,4,4) = 23
    tdec(ind,5,1) = 9
    tdec(ind,5,2) = 22
    tdec(ind,5,3) = 27
    tdec(ind,5,4) = 23
    tdec(ind,6,1) = 9
    tdec(ind,6,2) = 27
    tdec(ind,6,3) = 21
    tdec(ind,6,4) = 23
    tdec(ind,7,1) = 1
    tdec(ind,7,2) = 9
    tdec(ind,7,3) = 22
    tdec(ind,7,4) = 21
    tdec(ind,8,1) = 1
    tdec(ind,8,2) = 22
    tdec(ind,8,3) = 27
    tdec(ind,8,4) = 21
    tdec(ind,9,1) = 1
    tdec(ind,9,2) = 27
    tdec(ind,9,3) = 12
    tdec(ind,9,4) = 21
    tdec(ind,10,1) = 1
    tdec(ind,10,2) = 22
    tdec(ind,10,3) = 13
    tdec(ind,10,4) = 27
    tdec(ind,11,1) = 1
    tdec(ind,11,2) = 13
    tdec(ind,11,3) = 25
    tdec(ind,11,4) = 27
    tdec(ind,12,1) = 1
    tdec(ind,12,2) = 25
    tdec(ind,12,3) = 12
    tdec(ind,12,4) = 27
    tdec(ind,13,1) = 22
    tdec(ind,13,2) = 14
    tdec(ind,13,3) = 6
    tdec(ind,13,4) = 23
    tdec(ind,14,1) = 22
    tdec(ind,14,2) = 6
    tdec(ind,14,3) = 18
    tdec(ind,14,4) = 23
    tdec(ind,15,1) = 22
    tdec(ind,15,2) = 18
    tdec(ind,15,3) = 27
    tdec(ind,15,4) = 23
    tdec(ind,16,1) = 22
    tdec(ind,16,2) = 6
    tdec(ind,16,3) = 17
    tdec(ind,16,4) = 18
    tdec(ind,17,1) = 22
    tdec(ind,17,2) = 17
    tdec(ind,17,3) = 26
    tdec(ind,17,4) = 18
    tdec(ind,18,1) = 22
    tdec(ind,18,2) = 26
    tdec(ind,18,3) = 27
    tdec(ind,18,4) = 18
    tdec(ind,19,1) = 13
    tdec(ind,19,2) = 22
    tdec(ind,19,3) = 17
    tdec(ind,19,4) = 27
    tdec(ind,20,1) = 13
    tdec(ind,20,2) = 17
    tdec(ind,20,3) = 26
    tdec(ind,20,4) = 27
    tdec(ind,21,1) = 13
    tdec(ind,21,2) = 26
    tdec(ind,21,3) = 25
    tdec(ind,21,4) = 27
    tdec(ind,22,1) = 13
    tdec(ind,22,2) = 17
    tdec(ind,22,3) = 5
    tdec(ind,22,4) = 26
    tdec(ind,23,1) = 13
    tdec(ind,23,2) = 5
    tdec(ind,23,3) = 20
    tdec(ind,23,4) = 26
    tdec(ind,24,1) = 13
    tdec(ind,24,2) = 20
    tdec(ind,24,3) = 25
    tdec(ind,24,4) = 26
    tdec(ind,25,1) = 21
    tdec(ind,25,2) = 10
    tdec(ind,25,3) = 23
    tdec(ind,25,4) = 3
    tdec(ind,26,1) = 21
    tdec(ind,26,2) = 23
    tdec(ind,26,3) = 15
    tdec(ind,26,4) = 3
    tdec(ind,27,1) = 21
    tdec(ind,27,2) = 15
    tdec(ind,27,3) = 11
    tdec(ind,27,4) = 3
    tdec(ind,28,1) = 21
    tdec(ind,28,2) = 23
    tdec(ind,28,3) = 27
    tdec(ind,28,4) = 15
    tdec(ind,29,1) = 21
    tdec(ind,29,2) = 27
    tdec(ind,29,3) = 24
    tdec(ind,29,4) = 15
    tdec(ind,30,1) = 21
    tdec(ind,30,2) = 24
    tdec(ind,30,3) = 11
    tdec(ind,30,4) = 15
    tdec(ind,31,1) = 12
    tdec(ind,31,2) = 21
    tdec(ind,31,3) = 27
    tdec(ind,31,4) = 11
    tdec(ind,32,1) = 12
    tdec(ind,32,2) = 27
    tdec(ind,32,3) = 24
    tdec(ind,32,4) = 11
    tdec(ind,33,1) = 12
    tdec(ind,33,2) = 24
    tdec(ind,33,3) = 4
    tdec(ind,33,4) = 11
    tdec(ind,34,1) = 12
    tdec(ind,34,2) = 27
    tdec(ind,34,3) = 25
    tdec(ind,34,4) = 24
    tdec(ind,35,1) = 12
    tdec(ind,35,2) = 25
    tdec(ind,35,3) = 16
    tdec(ind,35,4) = 24
    tdec(ind,36,1) = 12
    tdec(ind,36,2) = 16
    tdec(ind,36,3) = 4
    tdec(ind,36,4) = 24
    tdec(ind,37,1) = 27
    tdec(ind,37,2) = 23
    tdec(ind,37,3) = 18
    tdec(ind,37,4) = 15
    tdec(ind,38,1) = 27
    tdec(ind,38,2) = 18
    tdec(ind,38,3) = 7
    tdec(ind,38,4) = 15
    tdec(ind,39,1) = 27
    tdec(ind,39,2) = 7
    tdec(ind,39,3) = 24
    tdec(ind,39,4) = 15
    tdec(ind,40,1) = 27
    tdec(ind,40,2) = 18
    tdec(ind,40,3) = 26
    tdec(ind,40,4) = 7
    tdec(ind,41,1) = 27
    tdec(ind,41,2) = 26
    tdec(ind,41,3) = 19
    tdec(ind,41,4) = 7
    tdec(ind,42,1) = 27
    tdec(ind,42,2) = 19
    tdec(ind,42,3) = 24
    tdec(ind,42,4) = 7
    tdec(ind,43,1) = 25
    tdec(ind,43,2) = 27
    tdec(ind,43,3) = 26
    tdec(ind,43,4) = 24
    tdec(ind,44,1) = 25
    tdec(ind,44,2) = 26
    tdec(ind,44,3) = 19
    tdec(ind,44,4) = 24
    tdec(ind,45,1) = 25
    tdec(ind,45,2) = 19
    tdec(ind,45,3) = 16
    tdec(ind,45,4) = 24
    tdec(ind,46,1) = 25
    tdec(ind,46,2) = 26
    tdec(ind,46,3) = 20
    tdec(ind,46,4) = 19
    tdec(ind,47,1) = 25
    tdec(ind,47,2) = 20
    tdec(ind,47,3) = 8
    tdec(ind,47,4) = 19
    tdec(ind,48,1) = 25
    tdec(ind,48,2) = 8
    tdec(ind,48,3) = 16
    tdec(ind,48,4) = 19
!
!     ------------------------------------------------------------------
! --- ON STOCKE LE NOMBRE DE NOEUDS POUR EVITER D'AVOIR A LE REFAIRE
!
    do 101 ind = 1, ntyele
        if (typd(ind,1) .ne. 0) then
            call jeveuo(jexnum('&CATA.TM.NBNO', typd(ind, 1)), 'L', ino)
            typd(ind,3)=zi(ino)
        endif
101  end do
!
!     ------------------------------------------------------------------
!
    goto 9000
!     VERIFICATION EMMELAGE DE PINCEAUX DU PROGRAMMEUR...
999  continue
    call assert(.false.)
!     ------------------------------------------------------------------
!
9000  continue
end subroutine
