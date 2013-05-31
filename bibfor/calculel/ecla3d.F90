subroutine ecla3d(nomte, elrefa, fapg, npg, npoini,&
                  nterm1, nsomm1, csomm1, tyma, nbno2,&
                  connx, mxnbn2, mxnbpi, mxnbte, mxnbse,&
                  nbsel, corsel)
! aslint: disable=W1501
    implicit   none
    include 'jeveux.h'
!
    include 'asterfort/eclac1.h'
    include 'asterfort/eclaco.h'
    include 'asterfort/eclan1.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenonu.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/u2mesk.h'
    integer :: mxnbn2, mxnbpi, mxnbte, mxnbse
    integer :: npg, connx(mxnbn2, mxnbse), nsomm1(mxnbpi, mxnbte)
    integer :: nterm1(mxnbpi), nbno2(mxnbse), npoini, tyma(mxnbse)
    integer :: nbsel, corsel(mxnbse)
    real(kind=8) :: csomm1(mxnbpi, mxnbte)
    character(len=16) :: nomte
    character(len=8) :: elrefa, fapg
! ---------------------------------------------------------------------
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
! ---------------------------------------------------------------------
! BUT : DECOMPOSER LES TYPE_ELEM 3D EN AUTANT DE SOUS-ELEMENTS QUE
!       DE POINTS DE GAUSS.
!
!     DECOUPAGE DU HE8, H20, H27 :
!       FPG1  :  8 NOEUDS,  1 HEXA8
!       FPG8  : 27 NOEUDS,  8 HEXA8
!       FPG27 : 64 NOEUDS, 27 HEXA8
!
!     DECOUPAGE DU TE4, T10 :
!       FPG1  :  4 NOEUDS,  1 TETRA4
!       FPG4  : 15 NOEUDS,  4 HEXA8
!       FPG5  : 10 NOEUDS,  4 TETRA4, 2 PYRAM5
!       FPG15 : 32 NOEUDS, 10 HEXA8 ET 4 PENTA6 ET 1 TETRA4
!
!     DECOUPAGE DU PE6, P15, P18 :
!       FPG1  :  6 NOEUDS,  1 PENTA6
!       FPG6  : 12 NOEUDS,  6 PENTA6
!       FPG21 : 48 NOEUDS, 18 HEXA8 ET 3 PENTA6
!
! ---------------------------------------------------------------------
! DESCRIPTION DES POINTS INTERMEDIAIRES (POINT_I) :
! ------------------------------------------------
! UN POINT_I EST DEFINI COMME UNE COMBINAISON LINEAIRE DES NOEUDS
! DE LA MAILLE SOUS-JACENTE AU TYPE_ELEM :
! POINT_I = SOMME COEF(K)*NOEUD(K)  (1<= K <=NTERMES)
!           NTERMES <= 27 (HEXA27)
!
! ---------------------------------------------------------------------
    integer :: k, ihexa8, ipent6, itetr4, ipyra5
    logical :: ltetra, lpyram
    character(len=24) :: valk(3)
!
! ---------------------------------------------------------------------
    call jemarq()
!
    call jenonu(jexnom('&CATA.TM.NOMTM', 'HEXA8' ), ihexa8)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PENTA6'), ipent6)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'TETRA4'), itetr4)
    call jenonu(jexnom('&CATA.TM.NOMTM', 'PYRAM5'), ipyra5)
!
    ltetra=.false.
    lpyram=.false.
!
!     -----------------------------------------------------------------
!     HEXAEDRES
!     -----------------------------------------------------------------
    if (elrefa .eq. 'HE8' .or. elrefa .eq. 'H20' .or. elrefa .eq. 'H27') then
!
        if (fapg .eq. 'FPG1') then
!           -----------------
            npoini = 8
            tyma(1) = ihexa8
            nbno2(1) = 8
!
!        -- DEFINITION DES POINT_I :
            nterm1(1)=1
            call eclan1(1, mxnbpi, nsomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(1, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(2)=1
            call eclan1(2, mxnbpi, nsomm1, nterm1, 2,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(2, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(3)=1
            call eclan1(3, mxnbpi, nsomm1, nterm1, 3,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(3, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(4)=1
            call eclan1(4, mxnbpi, nsomm1, nterm1, 4,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(4, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(5)=1
            call eclan1(5, mxnbpi, nsomm1, nterm1, 5,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(5, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(6)=1
            call eclan1(6, mxnbpi, nsomm1, nterm1, 6,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(6, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(7)=1
            call eclan1(7, mxnbpi, nsomm1, nterm1, 7,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(7, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(8)=1
            call eclan1(8, mxnbpi, nsomm1, nterm1, 8,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(8, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
!
!        -- CONNECTIVITE DES SOUS-ELEMENTS :
            call eclaco(1, mxnbn2, connx, nbno2, 1,&
                        2, 3, 4, 5, 6,&
                        7, 8)
!
        else if (fapg .eq. 'FPG8') then
!           -----------------
            npoini = 27
            do 10, k = 1, npg
            tyma(k) = ihexa8
            nbno2(k) = 8
10          continue
!
!        -- DEFINITION DES POINT_I :
            nterm1(1)=1
            call eclan1(1, mxnbpi, nsomm1, nterm1, 5,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(1, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(3)=1
            call eclan1(3, mxnbpi, nsomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(3, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(7)=1
            call eclan1(7, mxnbpi, nsomm1, nterm1, 8,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(7, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(9)=1
            call eclan1(9, mxnbpi, nsomm1, nterm1, 4,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(9, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(19)=1
            call eclan1(19, mxnbpi, nsomm1, nterm1, 6,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(19, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(21)=1
            call eclan1(21, mxnbpi, nsomm1, nterm1, 2,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(21, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(25)=1
            call eclan1(25, mxnbpi, nsomm1, nterm1, 7,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(25, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(27)=1
            call eclan1(27, mxnbpi, nsomm1, nterm1, 3,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(27, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(4)=2
            call eclan1(4, mxnbpi, nsomm1, nterm1, 5,&
                        8, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(4, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(10)=2
            call eclan1(10, mxnbpi, nsomm1, nterm1, 5,&
                        6, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(10, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(16)=2
            call eclan1(16, mxnbpi, nsomm1, nterm1, 7,&
                        8, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(16, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(22)=2
            call eclan1(22, mxnbpi, nsomm1, nterm1, 6,&
                        7, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(22, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(6)=2
            call eclan1(6, mxnbpi, nsomm1, nterm1, 1,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(6, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(12)=2
            call eclan1(12, mxnbpi, nsomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(12, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(24)=2
            call eclan1(24, mxnbpi, nsomm1, nterm1, 2,&
                        3, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(24, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(18)=2
            call eclan1(18, mxnbpi, nsomm1, nterm1, 3,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(18, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(8)=2
            call eclan1(8, mxnbpi, nsomm1, nterm1, 4,&
                        8, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(8, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(2)=2
            call eclan1(2, mxnbpi, nsomm1, nterm1, 1,&
                        5, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(2, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(20)=2
            call eclan1(20, mxnbpi, nsomm1, nterm1, 6,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(20, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(26)=2
            call eclan1(26, mxnbpi, nsomm1, nterm1, 3,&
                        7, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(26, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(13)=4
            call eclan1(13, mxnbpi, nsomm1, nterm1, 5,&
                        6, 7, 8, 0, 0,&
                        0, 0)
            call eclac1(13, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 1, 0, 0,&
                        0, 0)
            nterm1(15)=4
            call eclan1(15, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 0, 0,&
                        0, 0)
            call eclac1(15, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 1, 0, 0,&
                        0, 0)
            nterm1(5)=4
            call eclan1(5, mxnbpi, nsomm1, nterm1, 1,&
                        4, 8, 5, 0, 0,&
                        0, 0)
            call eclac1(5, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 1, 0, 0,&
                        0, 0)
            nterm1(23)=4
            call eclan1(23, mxnbpi, nsomm1, nterm1, 2,&
                        3, 7, 6, 0, 0,&
                        0, 0)
            call eclac1(23, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 1, 0, 0,&
                        0, 0)
            nterm1(11)=4
            call eclan1(11, mxnbpi, nsomm1, nterm1, 1,&
                        2, 6, 5, 0, 0,&
                        0, 0)
            call eclac1(11, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 1, 0, 0,&
                        0, 0)
            nterm1(17)=4
            call eclan1(17, mxnbpi, nsomm1, nterm1, 3,&
                        4, 8, 7, 0, 0,&
                        0, 0)
            call eclac1(17, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 1, 0, 0,&
                        0, 0)
!
            nterm1(14)=4
            call eclan1(14, mxnbpi, nsomm1, nterm1, 1,&
                        3, 7, 5, 0, 0,&
                        0, 0)
            call eclac1(14, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 1, 0, 0,&
                        0, 0)
!
!        -- CONNECTIVITE DES SOUS-ELEMENTS :
            call eclaco(1, mxnbn2, connx, nbno2, 12,&
                        15, 6, 3, 11, 14,&
                        5, 2)
            call eclaco(2, mxnbn2, connx, nbno2, 11,&
                        14, 5, 2, 10, 13,&
                        4, 1)
            call eclaco(3, mxnbn2, connx, nbno2, 15,&
                        18, 9, 6, 14, 17,&
                        8, 5)
            call eclaco(4, mxnbn2, connx, nbno2, 14,&
                        17, 8, 5, 13, 16,&
                        7, 4)
            call eclaco(5, mxnbn2, connx, nbno2, 21,&
                        24, 15, 12, 20, 23,&
                        14, 11)
            call eclaco(6, mxnbn2, connx, nbno2, 20,&
                        23, 14, 11, 19, 22,&
                        13, 10)
            call eclaco(7, mxnbn2, connx, nbno2, 24,&
                        27, 18, 15, 23, 26,&
                        17, 14)
            call eclaco(8, mxnbn2, connx, nbno2, 23,&
                        26, 17, 14, 22, 25,&
                        16, 13)
!
!
        else if (fapg .eq. 'FPG27') then
!               ------------------
            npoini = 64
            do 20, k = 1, npg
            tyma(k) = ihexa8
            nbno2(k) = 8
20          continue
!
!        -- DEFINITION DES POINT_I :
            nterm1(1)=1
            call eclan1(1, mxnbpi, nsomm1, nterm1, 5,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(1, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(4)=1
            call eclan1(4, mxnbpi, nsomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(4, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(13)=1
            call eclan1(13, mxnbpi, nsomm1, nterm1, 8,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(13, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(16)=1
            call eclan1(16, mxnbpi, nsomm1, nterm1, 4,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(16, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(49)=1
            call eclan1(49, mxnbpi, nsomm1, nterm1, 6,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(49, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(52)=1
            call eclan1(52, mxnbpi, nsomm1, nterm1, 2,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(52, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(61)=1
            call eclan1(61, mxnbpi, nsomm1, nterm1, 7,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(61, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(64)=1
            call eclan1(64, mxnbpi, nsomm1, nterm1, 3,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(64, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(9)=2
            call eclan1(9, mxnbpi, nsomm1, nterm1, 5,&
                        8, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(9, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(5)=2
            call eclan1(5, mxnbpi, nsomm1, nterm1, 5,&
                        8, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(5, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(2)=2
            call eclan1(2, mxnbpi, nsomm1, nterm1, 5,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(2, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(3)=2
            call eclan1(3, mxnbpi, nsomm1, nterm1, 5,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(3, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(8)=2
            call eclan1(8, mxnbpi, nsomm1, nterm1, 1,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(8, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(12)=2
            call eclan1(12, mxnbpi, nsomm1, nterm1, 1,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(12, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(15)=2
            call eclan1(15, mxnbpi, nsomm1, nterm1, 4,&
                        8, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(15, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(14)=2
            call eclan1(14, mxnbpi, nsomm1, nterm1, 4,&
                        8, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(14, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(57)=2
            call eclan1(57, mxnbpi, nsomm1, nterm1, 6,&
                        7, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(57, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(53)=2
            call eclan1(53, mxnbpi, nsomm1, nterm1, 6,&
                        7, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(53, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(50)=2
            call eclan1(50, mxnbpi, nsomm1, nterm1, 6,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(50, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(51)=2
            call eclan1(51, mxnbpi, nsomm1, nterm1, 6,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(51, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(56)=2
            call eclan1(56, mxnbpi, nsomm1, nterm1, 2,&
                        3, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(56, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(60)=2
            call eclan1(60, mxnbpi, nsomm1, nterm1, 2,&
                        3, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(60, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(63)=2
            call eclan1(63, mxnbpi, nsomm1, nterm1, 3,&
                        7, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(63, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(62)=2
            call eclan1(62, mxnbpi, nsomm1, nterm1, 3,&
                        7, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(62, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(17)=2
            call eclan1(17, mxnbpi, nsomm1, nterm1, 5,&
                        6, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(17, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(20)=2
            call eclan1(20, mxnbpi, nsomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(20, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(32)=2
            call eclan1(32, mxnbpi, nsomm1, nterm1, 4,&
                        3, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(32, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(29)=2
            call eclan1(29, mxnbpi, nsomm1, nterm1, 8,&
                        7, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(29, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(33)=2
            call eclan1(33, mxnbpi, nsomm1, nterm1, 5,&
                        6, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(33, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(36)=2
            call eclan1(36, mxnbpi, nsomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(36, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(48)=2
            call eclan1(48, mxnbpi, nsomm1, nterm1, 4,&
                        3, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(48, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(45)=2
            call eclan1(45, mxnbpi, nsomm1, nterm1, 8,&
                        7, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(45, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(6)=3
            call eclan1(6, mxnbpi, nsomm1, nterm1, 1,&
                        5, 8, 0, 0, 0,&
                        0, 0)
            call eclac1(6, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(7)=3
            call eclan1(7, mxnbpi, nsomm1, nterm1, 1,&
                        4, 5, 0, 0, 0,&
                        0, 0)
            call eclac1(7, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(11)=3
            call eclan1(11, mxnbpi, nsomm1, nterm1, 1,&
                        4, 8, 0, 0, 0,&
                        0, 0)
            call eclac1(11, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(10)=3
            call eclan1(10, mxnbpi, nsomm1, nterm1, 4,&
                        5, 8, 0, 0, 0,&
                        0, 0)
            call eclac1(10, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(54)=3
            call eclan1(54, mxnbpi, nsomm1, nterm1, 2,&
                        6, 7, 0, 0, 0,&
                        0, 0)
            call eclac1(54, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(55)=3
            call eclan1(55, mxnbpi, nsomm1, nterm1, 3,&
                        2, 6, 0, 0, 0,&
                        0, 0)
            call eclac1(55, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(59)=3
            call eclan1(59, mxnbpi, nsomm1, nterm1, 2,&
                        3, 7, 0, 0, 0,&
                        0, 0)
            call eclac1(59, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(58)=3
            call eclan1(58, mxnbpi, nsomm1, nterm1, 3,&
                        6, 7, 0, 0, 0,&
                        0, 0)
            call eclac1(58, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(24)=3
            call eclan1(24, mxnbpi, nsomm1, nterm1, 1,&
                        4, 2, 0, 0, 0,&
                        0, 0)
            call eclac1(24, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(28)=3
            call eclan1(28, mxnbpi, nsomm1, nterm1, 1,&
                        4, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(28, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(44)=3
            call eclan1(44, mxnbpi, nsomm1, nterm1, 2,&
                        3, 4, 0, 0, 0,&
                        0, 0)
            call eclac1(44, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(40)=3
            call eclan1(40, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(40, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(19)=3
            call eclan1(19, mxnbpi, nsomm1, nterm1, 1,&
                        5, 2, 0, 0, 0,&
                        0, 0)
            call eclac1(19, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(18)=3
            call eclan1(18, mxnbpi, nsomm1, nterm1, 1,&
                        5, 6, 0, 0, 0,&
                        0, 0)
            call eclac1(18, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(34)=3
            call eclan1(34, mxnbpi, nsomm1, nterm1, 2,&
                        5, 6, 0, 0, 0,&
                        0, 0)
            call eclac1(34, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(35)=3
            call eclan1(35, mxnbpi, nsomm1, nterm1, 1,&
                        2, 6, 0, 0, 0,&
                        0, 0)
            call eclac1(35, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(30)=3
            call eclan1(30, mxnbpi, nsomm1, nterm1, 4,&
                        7, 8, 0, 0, 0,&
                        0, 0)
            call eclac1(30, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(31)=3
            call eclan1(31, mxnbpi, nsomm1, nterm1, 3,&
                        4, 8, 0, 0, 0,&
                        0, 0)
            call eclac1(31, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(47)=3
            call eclan1(47, mxnbpi, nsomm1, nterm1, 3,&
                        4, 7, 0, 0, 0,&
                        0, 0)
            call eclac1(47, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(46)=3
            call eclan1(46, mxnbpi, nsomm1, nterm1, 3,&
                        7, 8, 0, 0, 0,&
                        0, 0)
            call eclac1(46, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(21)=3
            call eclan1(21, mxnbpi, nsomm1, nterm1, 5,&
                        6, 8, 0, 0, 0,&
                        0, 0)
            call eclac1(21, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(25)=3
            call eclan1(25, mxnbpi, nsomm1, nterm1, 5,&
                        7, 8, 0, 0, 0,&
                        0, 0)
            call eclac1(25, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(41)=3
            call eclan1(41, mxnbpi, nsomm1, nterm1, 6,&
                        7, 8, 0, 0, 0,&
                        0, 0)
            call eclac1(41, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(37)=3
            call eclan1(37, mxnbpi, nsomm1, nterm1, 5,&
                        6, 7, 0, 0, 0,&
                        0, 0)
            call eclac1(37, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(22)=3
            call eclan1(22, mxnbpi, nsomm1, nterm1, 4,&
                        5, 6, 0, 0, 0,&
                        0, 0)
            call eclac1(22, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(27)=3
            call eclan1(27, mxnbpi, nsomm1, nterm1, 3,&
                        4, 5, 0, 0, 0,&
                        0, 0)
            call eclac1(27, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(43)=3
            call eclan1(43, mxnbpi, nsomm1, nterm1, 3,&
                        4, 6, 0, 0, 0,&
                        0, 0)
            call eclac1(43, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(38)=3
            call eclan1(38, mxnbpi, nsomm1, nterm1, 3,&
                        5, 6, 0, 0, 0,&
                        0, 0)
            call eclac1(38, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(23)=3
            call eclan1(23, mxnbpi, nsomm1, nterm1, 1,&
                        2, 8, 0, 0, 0,&
                        0, 0)
            call eclac1(23, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(26)=3
            call eclan1(26, mxnbpi, nsomm1, nterm1, 1,&
                        7, 8, 0, 0, 0,&
                        0, 0)
            call eclac1(26, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(42)=3
            call eclan1(42, mxnbpi, nsomm1, nterm1, 2,&
                        7, 8, 0, 0, 0,&
                        0, 0)
            call eclac1(42, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(39)=3
            call eclan1(39, mxnbpi, nsomm1, nterm1, 1,&
                        2, 7, 0, 0, 0,&
                        0, 0)
            call eclac1(39, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
!
!        -- CONNECTIVITE DES SOUS-ELEMENTS :
            call eclaco(1, mxnbn2, connx, nbno2, 3,&
                        7, 8, 4, 19, 23,&
                        24, 20)
            call eclaco(2, mxnbn2, connx, nbno2, 2,&
                        6, 7, 3, 18, 22,&
                        23, 19)
            call eclaco(3, mxnbn2, connx, nbno2, 1,&
                        5, 6, 2, 17, 21,&
                        22, 18)
            call eclaco(4, mxnbn2, connx, nbno2, 7,&
                        11, 12, 8, 23, 27,&
                        28, 24)
            call eclaco(5, mxnbn2, connx, nbno2, 6,&
                        10, 11, 7, 22, 26,&
                        27, 23)
            call eclaco(6, mxnbn2, connx, nbno2, 5,&
                        9, 10, 6, 21, 25,&
                        26, 22)
            call eclaco(7, mxnbn2, connx, nbno2, 11,&
                        15, 16, 12, 27, 31,&
                        32, 28)
            call eclaco(8, mxnbn2, connx, nbno2, 10,&
                        14, 15, 11, 26, 30,&
                        31, 27)
            call eclaco(9, mxnbn2, connx, nbno2, 9,&
                        13, 14, 10, 25, 29,&
                        30, 26)
            call eclaco(10, mxnbn2, connx, nbno2, 19,&
                        23, 24, 20, 35, 39,&
                        40, 36)
            call eclaco(11, mxnbn2, connx, nbno2, 18,&
                        22, 23, 19, 34, 38,&
                        39, 35)
            call eclaco(12, mxnbn2, connx, nbno2, 17,&
                        21, 22, 18, 33, 37,&
                        38, 34)
            call eclaco(13, mxnbn2, connx, nbno2, 23,&
                        27, 28, 24, 39, 43,&
                        44, 40)
            call eclaco(14, mxnbn2, connx, nbno2, 22,&
                        26, 27, 23, 38, 42,&
                        43, 39)
            call eclaco(15, mxnbn2, connx, nbno2, 21,&
                        25, 26, 22, 37, 41,&
                        42, 38)
            call eclaco(16, mxnbn2, connx, nbno2, 27,&
                        31, 32, 28, 43, 47,&
                        48, 44)
            call eclaco(17, mxnbn2, connx, nbno2, 26,&
                        30, 31, 27, 42, 46,&
                        47, 43)
            call eclaco(18, mxnbn2, connx, nbno2, 25,&
                        29, 30, 26, 41, 45,&
                        46, 42)
            call eclaco(19, mxnbn2, connx, nbno2, 35,&
                        39, 40, 36, 51, 55,&
                        56, 52)
            call eclaco(20, mxnbn2, connx, nbno2, 34,&
                        38, 39, 35, 50, 54,&
                        55, 51)
            call eclaco(21, mxnbn2, connx, nbno2, 33,&
                        37, 38, 34, 49, 53,&
                        54, 50)
            call eclaco(22, mxnbn2, connx, nbno2, 39,&
                        43, 44, 40, 55, 59,&
                        60, 56)
            call eclaco(23, mxnbn2, connx, nbno2, 38,&
                        42, 43, 39, 54, 58,&
                        59, 55)
            call eclaco(24, mxnbn2, connx, nbno2, 37,&
                        41, 42, 38, 53, 57,&
                        58, 54)
            call eclaco(25, mxnbn2, connx, nbno2, 43,&
                        47, 48, 44, 59, 63,&
                        64, 60)
            call eclaco(26, mxnbn2, connx, nbno2, 42,&
                        46, 47, 43, 58, 62,&
                        63, 59)
            call eclaco(27, mxnbn2, connx, nbno2, 41,&
                        45, 46, 42, 57, 61,&
                        62, 58)
!
        else
            valk (1) = nomte
            valk (2) = elrefa
            valk (3) = fapg
            call u2mesk('F', 'CALCULEL5_76', 3, valk)
!
        endif
!
!     -----------------------------------------------------------------
!     TETRAEDRES
!     -----------------------------------------------------------------
    else if (elrefa .eq. 'TE4' .or. elrefa .eq. 'T10') then
        ltetra=.true.
!
        if (fapg .eq. 'FPG1') then
!           -----------------
            npoini = 4
            tyma(1) = itetr4
            nbno2(1) = 4
!
!        -- DEFINITION DES POINT_I :
            nterm1(1)=1
            call eclan1(1, mxnbpi, nsomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(1, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(2)=1
            call eclan1(2, mxnbpi, nsomm1, nterm1, 2,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(2, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(3)=1
            call eclan1(3, mxnbpi, nsomm1, nterm1, 3,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(3, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(4)=1
            call eclan1(4, mxnbpi, nsomm1, nterm1, 4,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(4, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
!
!        -- CONNECTIVITE DES SOUS-ELEMENTS :
            call eclaco(1, mxnbn2, connx, nbno2, 1,&
                        2, 3, 4, 0, 0,&
                        0, 0)
!
!
        else if (fapg .eq. 'FPG4') then
!           -----------------
            npoini = 15
            do 30, k = 1, npg
            tyma(k) = ihexa8
            nbno2(k) = 8
30          continue
!
!        -- DEFINITION DES POINT_I :
            nterm1(1)=1
            call eclan1(1, mxnbpi, nsomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(1, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(2)=1
            call eclan1(2, mxnbpi, nsomm1, nterm1, 2,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(2, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(3)=1
            call eclan1(3, mxnbpi, nsomm1, nterm1, 3,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(3, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(4)=1
            call eclan1(4, mxnbpi, nsomm1, nterm1, 4,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(4, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(5)=2
            call eclan1(5, mxnbpi, nsomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(5, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(6)=2
            call eclan1(6, mxnbpi, nsomm1, nterm1, 2,&
                        3, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(6, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(7)=2
            call eclan1(7, mxnbpi, nsomm1, nterm1, 3,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(7, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(8)=2
            call eclan1(8, mxnbpi, nsomm1, nterm1, 1,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(8, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(9)=2
            call eclan1(9, mxnbpi, nsomm1, nterm1, 2,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(9, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(10)=2
            call eclan1(10, mxnbpi, nsomm1, nterm1, 3,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(10, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(11)=3
            call eclan1(11, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(11, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(12)=3
            call eclan1(12, mxnbpi, nsomm1, nterm1, 1,&
                        2, 4, 0, 0, 0,&
                        0, 0)
            call eclac1(12, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(13)=3
            call eclan1(13, mxnbpi, nsomm1, nterm1, 2,&
                        3, 4, 0, 0, 0,&
                        0, 0)
            call eclac1(13, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(14)=3
            call eclan1(14, mxnbpi, nsomm1, nterm1, 1,&
                        3, 4, 0, 0, 0,&
                        0, 0)
            call eclac1(14, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(15)=4
            call eclan1(15, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 0, 0,&
                        0, 0)
            call eclac1(15, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 1, 0, 0,&
                        0, 0)
!
!        -- CONNECTIVITE DES SOUS-ELEMENTS :
            call eclaco(1, mxnbn2, connx, nbno2, 3,&
                        10, 14, 7, 6, 13,&
                        15, 11)
            call eclaco(2, mxnbn2, connx, nbno2, 2,&
                        5, 12, 9, 6, 11,&
                        15, 13)
            call eclaco(3, mxnbn2, connx, nbno2, 1,&
                        7, 14, 8, 5, 11,&
                        15, 12)
            call eclaco(4, mxnbn2, connx, nbno2, 4,&
                        8, 14, 10, 9, 12,&
                        15, 13)
!
!
        else if (fapg .eq. 'FPG5') then
!           -----------------
            npoini = 10
            do 301, k = 1, 4
            tyma(k) = itetr4
            nbno2(k) = 4
301          continue
            do 302, k = 5, 6
            tyma(k) = ipyra5
            nbno2(k) = 5
302          continue
!
!        -- DEFINITION DES POINT_I :
            nterm1(1)=1
            call eclan1(1, mxnbpi, nsomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(1, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(2)=1
            call eclan1(2, mxnbpi, nsomm1, nterm1, 2,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(2, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(3)=1
            call eclan1(3, mxnbpi, nsomm1, nterm1, 3,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(3, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(4)=1
            call eclan1(4, mxnbpi, nsomm1, nterm1, 4,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(4, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
!
!
            nterm1(5)=2
            call eclan1(5, mxnbpi, nsomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(5, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(6)=2
            call eclan1(6, mxnbpi, nsomm1, nterm1, 2,&
                        3, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(6, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(7)=2
            call eclan1(7, mxnbpi, nsomm1, nterm1, 3,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(7, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(8)=2
            call eclan1(8, mxnbpi, nsomm1, nterm1, 1,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(8, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(9)=2
            call eclan1(9, mxnbpi, nsomm1, nterm1, 2,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(9, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(10)=2
            call eclan1(10, mxnbpi, nsomm1, nterm1, 3,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(10, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
!
!        -- CONNECTIVITE DES SOUS-ELEMENTS :
            call eclaco(1, mxnbn2, connx, nbno2, 1,&
                        5, 7, 8, 0, 0,&
                        0, 0)
            call eclaco(2, mxnbn2, connx, nbno2, 2,&
                        5, 6, 9, 0, 0,&
                        0, 0)
            call eclaco(3, mxnbn2, connx, nbno2, 3,&
                        7, 6, 10, 0, 0,&
                        0, 0)
            call eclaco(4, mxnbn2, connx, nbno2, 4,&
                        8, 10, 9, 0, 0,&
                        0, 0)
            call eclaco(5, mxnbn2, connx, nbno2, 5,&
                        7, 8, 9, 6, 0,&
                        0, 0)
            call eclaco(6, mxnbn2, connx, nbno2, 10,&
                        7, 8, 9, 6, 0,&
                        0, 0)
!
!
        else if (fapg .eq. 'FPG15') then
!               -----------------
            npoini = 32
            tyma(1) = itetr4
            nbno2(1) = 4
            do 42, k = 2, 5
            tyma(k) = ipent6
            nbno2(k) = 6
42          continue
            do 40, k = 6, 15
            tyma(k) = ihexa8
            nbno2(k) = 8
40          continue
!
!        -- DEFINITION DES POINT_I :
            nterm1(1)=1
            call eclan1(1, mxnbpi, nsomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(1, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(2)=1
            call eclan1(2, mxnbpi, nsomm1, nterm1, 2,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(2, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(3)=1
            call eclan1(3, mxnbpi, nsomm1, nterm1, 3,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(3, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(4)=1
            call eclan1(4, mxnbpi, nsomm1, nterm1, 4,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(4, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(5)=2
            call eclan1(5, mxnbpi, nsomm1, nterm1, 3,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(5, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(6)=2
            call eclan1(6, mxnbpi, nsomm1, nterm1, 3,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(6, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(7)=2
            call eclan1(7, mxnbpi, nsomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(7, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(8)=2
            call eclan1(8, mxnbpi, nsomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(8, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(9)=2
            call eclan1(9, mxnbpi, nsomm1, nterm1, 2,&
                        3, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(9, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(10)=2
            call eclan1(10, mxnbpi, nsomm1, nterm1, 2,&
                        3, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(10, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(11)=3
            call eclan1(11, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(11, mxnbpi, csomm1, nterm1, 1,&
                        1, 2, 0, 0, 0,&
                        0, 0)
            nterm1(12)=3
            call eclan1(12, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(12, mxnbpi, csomm1, nterm1, 2,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(13)=3
            call eclan1(13, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(13, mxnbpi, csomm1, nterm1, 1,&
                        2, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(14)=2
            call eclan1(14, mxnbpi, nsomm1, nterm1, 3,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(14, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(15)=2
            call eclan1(15, mxnbpi, nsomm1, nterm1, 3,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(15, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(16)=2
            call eclan1(16, mxnbpi, nsomm1, nterm1, 2,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(16, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(17)=2
            call eclan1(17, mxnbpi, nsomm1, nterm1, 2,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(17, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(18)=3
            call eclan1(18, mxnbpi, nsomm1, nterm1, 4,&
                        2, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(18, mxnbpi, csomm1, nterm1, 1,&
                        1, 2, 0, 0, 0,&
                        0, 0)
            nterm1(19)=3
            call eclan1(19, mxnbpi, nsomm1, nterm1, 4,&
                        2, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(19, mxnbpi, csomm1, nterm1, 2,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(20)=3
            call eclan1(20, mxnbpi, nsomm1, nterm1, 4,&
                        2, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(20, mxnbpi, csomm1, nterm1, 1,&
                        2, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(21)=2
            call eclan1(21, mxnbpi, nsomm1, nterm1, 1,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(21, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(22)=2
            call eclan1(22, mxnbpi, nsomm1, nterm1, 1,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(22, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(23)=3
            call eclan1(23, mxnbpi, nsomm1, nterm1, 1,&
                        4, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(23, mxnbpi, csomm1, nterm1, 1,&
                        1, 2, 0, 0, 0,&
                        0, 0)
            nterm1(24)=3
            call eclan1(24, mxnbpi, nsomm1, nterm1, 1,&
                        4, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(24, mxnbpi, csomm1, nterm1, 2,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(25)=3
            call eclan1(25, mxnbpi, nsomm1, nterm1, 1,&
                        4, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(25, mxnbpi, csomm1, nterm1, 1,&
                        2, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(26)=3
            call eclan1(26, mxnbpi, nsomm1, nterm1, 1,&
                        2, 4, 0, 0, 0,&
                        0, 0)
            call eclac1(26, mxnbpi, csomm1, nterm1, 1,&
                        1, 2, 0, 0, 0,&
                        0, 0)
            nterm1(27)=3
            call eclan1(27, mxnbpi, nsomm1, nterm1, 1,&
                        2, 4, 0, 0, 0,&
                        0, 0)
            call eclac1(27, mxnbpi, csomm1, nterm1, 2,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(28)=3
            call eclan1(28, mxnbpi, nsomm1, nterm1, 1,&
                        2, 4, 0, 0, 0,&
                        0, 0)
            call eclac1(28, mxnbpi, csomm1, nterm1, 1,&
                        2, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(29)=4
            call eclan1(29, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 0, 0,&
                        0, 0)
            call eclac1(29, mxnbpi, csomm1, nterm1, 2,&
                        1, 1, 1, 0, 0,&
                        0, 0)
            nterm1(30)=4
            call eclan1(30, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 0, 0,&
                        0, 0)
            call eclac1(30, mxnbpi, csomm1, nterm1, 1,&
                        2, 1, 1, 0, 0,&
                        0, 0)
            nterm1(31)=4
            call eclan1(31, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 0, 0,&
                        0, 0)
            call eclac1(31, mxnbpi, csomm1, nterm1, 1,&
                        1, 2, 1, 0, 0,&
                        0, 0)
            nterm1(32)=4
            call eclan1(32, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 0, 0,&
                        0, 0)
            call eclac1(32, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 2, 0, 0,&
                        0, 0)
!
!        -- CONNECTIVITE DES SOUS-ELEMENTS :
            call eclaco(1, mxnbn2, connx, nbno2, 29,&
                        30, 31, 32, 0, 0,&
                        0, 0)
            call eclaco(2, mxnbn2, connx, nbno2, 26,&
                        27, 28, 32, 29, 30,&
                        0, 0)
            call eclaco(3, mxnbn2, connx, nbno2, 23,&
                        25, 24, 31, 32, 29,&
                        0, 0)
            call eclaco(4, mxnbn2, connx, nbno2, 18,&
                        20, 19, 31, 30, 32,&
                        0, 0)
            call eclaco(5, mxnbn2, connx, nbno2, 11,&
                        12, 13, 31, 29, 30,&
                        0, 0)
            call eclaco(6, mxnbn2, connx, nbno2, 3,&
                        5, 11, 10, 14, 23,&
                        31, 18)
            call eclaco(7, mxnbn2, connx, nbno2, 20,&
                        30, 13, 9, 17, 28,&
                        8, 2)
            call eclaco(8, mxnbn2, connx, nbno2, 6,&
                        1, 7, 12, 24, 21,&
                        27, 29)
            call eclaco(9, mxnbn2, connx, nbno2, 15,&
                        25, 32, 19, 4, 22,&
                        26, 16)
            call eclaco(10, mxnbn2, connx, nbno2, 18,&
                        31, 11, 10, 20, 30,&
                        13, 9)
            call eclaco(11, mxnbn2, connx, nbno2, 5,&
                        6, 12, 11, 23, 24,&
                        29, 31)
            call eclaco(12, mxnbn2, connx, nbno2, 14,&
                        23, 31, 18, 15, 25,&
                        32, 19)
            call eclaco(13, mxnbn2, connx, nbno2, 7,&
                        8, 28, 27, 12, 13,&
                        30, 29)
            call eclaco(14, mxnbn2, connx, nbno2, 16,&
                        26, 28, 17, 19, 32,&
                        30, 20)
            call eclaco(15, mxnbn2, connx, nbno2, 22,&
                        21, 27, 26, 25, 24,&
                        29, 32)
!
!
        else
            valk (1) = nomte
            valk (2) = elrefa
            valk (3) = fapg
            call u2mesk('F', 'CALCULEL5_76', 3, valk)
        endif
!
!     -----------------------------------------------------------------
!     PENTAEDRES
!     -----------------------------------------------------------------
        elseif ( elrefa .eq. 'PE6' .or. elrefa .eq. 'P15' .or. elrefa&
    .eq. 'P18') then
!
        if (fapg .eq. 'FPG1') then
!           -----------------
            npoini = 6
            tyma(1) = ipent6
            nbno2(1) = 6
!
!        -- DEFINITION DES POINT_I :
            nterm1(1)=1
            call eclan1(1, mxnbpi, nsomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(1, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(2)=1
            call eclan1(2, mxnbpi, nsomm1, nterm1, 2,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(2, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(3)=1
            call eclan1(3, mxnbpi, nsomm1, nterm1, 3,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(3, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(4)=1
            call eclan1(4, mxnbpi, nsomm1, nterm1, 4,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(4, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(5)=1
            call eclan1(5, mxnbpi, nsomm1, nterm1, 5,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(5, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(6)=1
            call eclan1(6, mxnbpi, nsomm1, nterm1, 6,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(6, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
!
!        -- CONNECTIVITE DES SOUS-ELEMENTS :
            call eclaco(1, mxnbn2, connx, nbno2, 1,&
                        2, 3, 4, 5, 6,&
                        0, 0)
!
        else if (fapg .eq. 'FPG6') then
!           -----------------
            npoini = 12
            do 50, k = 1, npg
            tyma(k) = ipent6
            nbno2(k) = 6
50          continue
!
!        -- DEFINITION DES POINT_I :
            nterm1(1)=1
            call eclan1(1, mxnbpi, nsomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(1, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(2)=1
            call eclan1(2, mxnbpi, nsomm1, nterm1, 2,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(2, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(3)=1
            call eclan1(3, mxnbpi, nsomm1, nterm1, 3,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(3, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(4)=1
            call eclan1(4, mxnbpi, nsomm1, nterm1, 4,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(4, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(5)=1
            call eclan1(5, mxnbpi, nsomm1, nterm1, 5,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(5, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(6)=1
            call eclan1(6, mxnbpi, nsomm1, nterm1, 6,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(6, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(7)=2
            call eclan1(7, mxnbpi, nsomm1, nterm1, 1,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(7, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(8)=2
            call eclan1(8, mxnbpi, nsomm1, nterm1, 2,&
                        5, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(8, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(9)=2
            call eclan1(9, mxnbpi, nsomm1, nterm1, 3,&
                        6, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(9, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(10)=3
            call eclan1(10, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(10, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(12)=3
            call eclan1(12, mxnbpi, nsomm1, nterm1, 4,&
                        5, 6, 0, 0, 0,&
                        0, 0)
            call eclac1(12, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(11)=6
            call eclan1(11, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 5, 6,&
                        0, 0)
            call eclac1(11, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 1, 1, 1,&
                        0, 0)
!
!        -- CONNECTIVITE DES SOUS-ELEMENTS :
            call eclaco(1, mxnbn2, connx, nbno2, 1,&
                        2, 10, 7, 8, 11,&
                        0, 0)
            call eclaco(2, mxnbn2, connx, nbno2, 2,&
                        3, 10, 8, 9, 11,&
                        0, 0)
            call eclaco(3, mxnbn2, connx, nbno2, 3,&
                        1, 10, 9, 7, 11,&
                        0, 0)
            call eclaco(4, mxnbn2, connx, nbno2, 7,&
                        8, 11, 4, 5, 12,&
                        0, 0)
            call eclaco(5, mxnbn2, connx, nbno2, 8,&
                        9, 11, 5, 6, 12,&
                        0, 0)
            call eclaco(6, mxnbn2, connx, nbno2, 9,&
                        7, 11, 6, 4, 12,&
                        0, 0)
!
!
        else if (fapg .eq. 'FPG21') then
!               ------------------
            npoini = 48
!
!        LES SOUS-ELEMENTS SONT DES H8 SAUF 3 P6 :
            do 60, k = 1, 21
            tyma(k) = ihexa8
            nbno2(k) = 8
60          continue
            tyma(1) = ipent6
            tyma(8) = ipent6
            tyma(15) = ipent6
            nbno2(1) = 6
            nbno2(8) = 6
            nbno2(15) = 6
!
!        -- DEFINITION DES POINT_I :
            nterm1(1)=1
            call eclan1(1, mxnbpi, nsomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(1, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(2)=1
            call eclan1(2, mxnbpi, nsomm1, nterm1, 2,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(2, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(3)=1
            call eclan1(3, mxnbpi, nsomm1, nterm1, 3,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(3, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(4)=1
            call eclan1(4, mxnbpi, nsomm1, nterm1, 4,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(4, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(5)=1
            call eclan1(5, mxnbpi, nsomm1, nterm1, 5,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(5, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(6)=1
            call eclan1(6, mxnbpi, nsomm1, nterm1, 6,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(6, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(7)=2
            call eclan1(7, mxnbpi, nsomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(7, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(8)=2
            call eclan1(8, mxnbpi, nsomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(8, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(9)=2
            call eclan1(9, mxnbpi, nsomm1, nterm1, 2,&
                        3, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(9, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(10)=2
            call eclan1(10, mxnbpi, nsomm1, nterm1, 2,&
                        3, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(10, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(11)=2
            call eclan1(11, mxnbpi, nsomm1, nterm1, 3,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(11, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(12)=2
            call eclan1(12, mxnbpi, nsomm1, nterm1, 3,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(12, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(13)=3
            call eclan1(13, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(13, mxnbpi, csomm1, nterm1, 2,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(14)=3
            call eclan1(14, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(14, mxnbpi, csomm1, nterm1, 1,&
                        2, 1, 0, 0, 0,&
                        0, 0)
            nterm1(15)=3
            call eclan1(15, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 0, 0, 0,&
                        0, 0)
            call eclac1(15, mxnbpi, csomm1, nterm1, 1,&
                        1, 2, 0, 0, 0,&
                        0, 0)
!
            nterm1(16)=2
            call eclan1(16, mxnbpi, nsomm1, nterm1, 4,&
                        5, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(16, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(17)=2
            call eclan1(17, mxnbpi, nsomm1, nterm1, 4,&
                        5, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(17, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(18)=2
            call eclan1(18, mxnbpi, nsomm1, nterm1, 5,&
                        6, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(18, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(19)=2
            call eclan1(19, mxnbpi, nsomm1, nterm1, 5,&
                        6, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(19, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(20)=2
            call eclan1(20, mxnbpi, nsomm1, nterm1, 6,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(20, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(21)=2
            call eclan1(21, mxnbpi, nsomm1, nterm1, 6,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(21, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(22)=3
            call eclan1(22, mxnbpi, nsomm1, nterm1, 4,&
                        5, 6, 0, 0, 0,&
                        0, 0)
            call eclac1(22, mxnbpi, csomm1, nterm1, 2,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(23)=3
            call eclan1(23, mxnbpi, nsomm1, nterm1, 4,&
                        5, 6, 0, 0, 0,&
                        0, 0)
            call eclac1(23, mxnbpi, csomm1, nterm1, 1,&
                        2, 1, 0, 0, 0,&
                        0, 0)
            nterm1(24)=3
            call eclan1(24, mxnbpi, nsomm1, nterm1, 4,&
                        5, 6, 0, 0, 0,&
                        0, 0)
            call eclac1(24, mxnbpi, csomm1, nterm1, 1,&
                        1, 2, 0, 0, 0,&
                        0, 0)
!
            nterm1(25)=2
            call eclan1(25, mxnbpi, nsomm1, nterm1, 1,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(25, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(26)=2
            call eclan1(26, mxnbpi, nsomm1, nterm1, 2,&
                        5, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(26, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(27)=2
            call eclan1(27, mxnbpi, nsomm1, nterm1, 3,&
                        6, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(27, mxnbpi, csomm1, nterm1, 2,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(28)=4
            call eclan1(28, mxnbpi, nsomm1, nterm1, 1,&
                        2, 4, 5, 0, 0,&
                        0, 0)
            call eclac1(28, mxnbpi, csomm1, nterm1, 4,&
                        2, 2, 1, 0, 0,&
                        0, 0)
            nterm1(29)=4
            call eclan1(29, mxnbpi, nsomm1, nterm1, 1,&
                        2, 4, 5, 0, 0,&
                        0, 0)
            call eclac1(29, mxnbpi, csomm1, nterm1, 2,&
                        4, 1, 2, 0, 0,&
                        0, 0)
            nterm1(30)=4
            call eclan1(30, mxnbpi, nsomm1, nterm1, 2,&
                        3, 5, 6, 0, 0,&
                        0, 0)
            call eclac1(30, mxnbpi, csomm1, nterm1, 4,&
                        2, 2, 1, 0, 0,&
                        0, 0)
            nterm1(31)=4
            call eclan1(31, mxnbpi, nsomm1, nterm1, 2,&
                        3, 5, 6, 0, 0,&
                        0, 0)
            call eclac1(31, mxnbpi, csomm1, nterm1, 2,&
                        4, 1, 2, 0, 0,&
                        0, 0)
            nterm1(32)=4
            call eclan1(32, mxnbpi, nsomm1, nterm1, 3,&
                        1, 6, 4, 0, 0,&
                        0, 0)
            call eclac1(32, mxnbpi, csomm1, nterm1, 4,&
                        2, 2, 1, 0, 0,&
                        0, 0)
            nterm1(33)=4
            call eclan1(33, mxnbpi, nsomm1, nterm1, 3,&
                        1, 6, 4, 0, 0,&
                        0, 0)
            call eclac1(33, mxnbpi, csomm1, nterm1, 2,&
                        4, 1, 2, 0, 0,&
                        0, 0)
            nterm1(34)=6
            call eclan1(34, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 5, 6,&
                        0, 0)
            call eclac1(34, mxnbpi, csomm1, nterm1, 4,&
                        2, 2, 2, 1, 1,&
                        0, 0)
            nterm1(35)=6
            call eclan1(35, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 5, 6,&
                        0, 0)
            call eclac1(35, mxnbpi, csomm1, nterm1, 2,&
                        4, 2, 1, 2, 1,&
                        0, 0)
            nterm1(36)=6
            call eclan1(36, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 5, 6,&
                        0, 0)
            call eclac1(36, mxnbpi, csomm1, nterm1, 2,&
                        2, 4, 1, 1, 2,&
                        0, 0)
!
            nterm1(37)=2
            call eclan1(37, mxnbpi, nsomm1, nterm1, 1,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(37, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(38)=2
            call eclan1(38, mxnbpi, nsomm1, nterm1, 2,&
                        5, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(38, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(39)=2
            call eclan1(39, mxnbpi, nsomm1, nterm1, 3,&
                        6, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(39, mxnbpi, csomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            nterm1(40)=4
            call eclan1(40, mxnbpi, nsomm1, nterm1, 1,&
                        2, 4, 5, 0, 0,&
                        0, 0)
            call eclac1(40, mxnbpi, csomm1, nterm1, 2,&
                        1, 4, 2, 0, 0,&
                        0, 0)
            nterm1(41)=4
            call eclan1(41, mxnbpi, nsomm1, nterm1, 1,&
                        2, 4, 5, 0, 0,&
                        0, 0)
            call eclac1(41, mxnbpi, csomm1, nterm1, 1,&
                        2, 2, 4, 0, 0,&
                        0, 0)
            nterm1(42)=4
            call eclan1(42, mxnbpi, nsomm1, nterm1, 2,&
                        3, 5, 6, 0, 0,&
                        0, 0)
            call eclac1(42, mxnbpi, csomm1, nterm1, 2,&
                        1, 4, 2, 0, 0,&
                        0, 0)
            nterm1(43)=4
            call eclan1(43, mxnbpi, nsomm1, nterm1, 2,&
                        3, 5, 6, 0, 0,&
                        0, 0)
            call eclac1(43, mxnbpi, csomm1, nterm1, 1,&
                        2, 2, 4, 0, 0,&
                        0, 0)
            nterm1(44)=4
            call eclan1(44, mxnbpi, nsomm1, nterm1, 3,&
                        1, 6, 4, 0, 0,&
                        0, 0)
            call eclac1(44, mxnbpi, csomm1, nterm1, 2,&
                        1, 4, 2, 0, 0,&
                        0, 0)
            nterm1(45)=4
            call eclan1(45, mxnbpi, nsomm1, nterm1, 3,&
                        1, 6, 4, 0, 0,&
                        0, 0)
            call eclac1(45, mxnbpi, csomm1, nterm1, 1,&
                        2, 2, 4, 0, 0,&
                        0, 0)
            nterm1(46)=6
            call eclan1(46, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 5, 6,&
                        0, 0)
            call eclac1(46, mxnbpi, csomm1, nterm1, 2,&
                        1, 1, 4, 2, 2,&
                        0, 0)
            nterm1(47)=6
            call eclan1(47, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 5, 6,&
                        0, 0)
            call eclac1(47, mxnbpi, csomm1, nterm1, 1,&
                        2, 1, 2, 4, 2,&
                        0, 0)
            nterm1(48)=6
            call eclan1(48, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 5, 6,&
                        0, 0)
            call eclac1(48, mxnbpi, csomm1, nterm1, 1,&
                        1, 2, 2, 2, 4,&
                        0, 0)
!
!        -- CONNECTIVITE DES SOUS-ELEMENTS :
            call eclaco(7, mxnbn2, connx, nbno2, 2,&
                        9, 14, 8, 26, 30,&
                        35, 29)
            call eclaco(3, mxnbn2, connx, nbno2, 9,&
                        10, 15, 14, 30, 31,&
                        36, 35)
            call eclaco(5, mxnbn2, connx, nbno2, 10,&
                        3, 11, 15, 31, 27,&
                        32, 36)
            call eclaco(4, mxnbn2, connx, nbno2, 15,&
                        11, 12, 13, 36, 32,&
                        33, 34)
            call eclaco(6, mxnbn2, connx, nbno2, 13,&
                        12, 1, 7, 34, 33,&
                        25, 28)
            call eclaco(2, mxnbn2, connx, nbno2, 13,&
                        7, 8, 14, 34, 28,&
                        29, 35)
            call eclaco(1, mxnbn2, connx, nbno2, 13,&
                        14, 15, 34, 35, 36,&
                        0, 0)
            call eclaco(14, mxnbn2, connx, nbno2, 26,&
                        30, 35, 29, 38, 42,&
                        47, 41)
            call eclaco(10, mxnbn2, connx, nbno2, 30,&
                        31, 36, 35, 42, 43,&
                        48, 47)
            call eclaco(12, mxnbn2, connx, nbno2, 31,&
                        27, 32, 36, 43, 39,&
                        44, 48)
            call eclaco(11, mxnbn2, connx, nbno2, 36,&
                        32, 33, 34, 48, 44,&
                        45, 46)
            call eclaco(13, mxnbn2, connx, nbno2, 34,&
                        33, 25, 28, 46, 45,&
                        37, 40)
            call eclaco(9, mxnbn2, connx, nbno2, 34,&
                        28, 29, 35, 46, 40,&
                        41, 47)
            call eclaco(8, mxnbn2, connx, nbno2, 34,&
                        35, 36, 46, 47, 48,&
                        0, 0)
            call eclaco(21, mxnbn2, connx, nbno2, 38,&
                        42, 47, 41, 5, 18,&
                        23, 17)
            call eclaco(17, mxnbn2, connx, nbno2, 42,&
                        43, 48, 47, 18, 19,&
                        24, 23)
            call eclaco(19, mxnbn2, connx, nbno2, 43,&
                        39, 44, 48, 19, 6,&
                        20, 24)
            call eclaco(18, mxnbn2, connx, nbno2, 48,&
                        44, 45, 46, 24, 20,&
                        21, 22)
            call eclaco(20, mxnbn2, connx, nbno2, 46,&
                        45, 37, 40, 22, 21,&
                        4, 16)
            call eclaco(16, mxnbn2, connx, nbno2, 46,&
                        40, 41, 47, 22, 16,&
                        17, 23)
            call eclaco(15, mxnbn2, connx, nbno2, 46,&
                        47, 48, 22, 23, 24,&
                        0, 0)
!
        else
            valk (1) = nomte
            valk (2) = elrefa
            valk (3) = fapg
            call u2mesk('F', 'CALCULEL5_76', 3, valk)
        endif
!
!     -----------------------------------------------------------------
!     PYRAMIDES
!     -----------------------------------------------------------------
    else if (elrefa .eq. 'PY5' .or. elrefa .eq. 'P13') then
        lpyram=.true.
!
        if (fapg .eq. 'FPG1') then
!           -----------------
            npoini = 5
            tyma(1) = ipyra5
            nbno2(1) = 5
!
!        -- DEFINITION DES POINT_I :
            nterm1(1)=1
            call eclan1(1, mxnbpi, nsomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(1, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(2)=1
            call eclan1(2, mxnbpi, nsomm1, nterm1, 2,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(2, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(3)=1
            call eclan1(3, mxnbpi, nsomm1, nterm1, 3,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(3, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(4)=1
            call eclan1(4, mxnbpi, nsomm1, nterm1, 4,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(4, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(5)=1
            call eclan1(5, mxnbpi, nsomm1, nterm1, 5,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(5, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
!
!        -- CONNECTIVITE DES SOUS-ELEMENTS :
            call eclaco(1, mxnbn2, connx, nbno2, 1,&
                        2, 3, 4, 5, 0,&
                        0, 0)
!
!
        else if (fapg .eq. 'FPG5' .or. fapg .eq. 'FPG27') then
!           -----------------------------------------------
            npoini = 19
            do 710, k = 1, 4
            tyma(k) = ihexa8
            nbno2(k) = 8
710          continue
            do 711, k = 5, 8
            tyma(k) = ipyra5
            nbno2(k) = 5
711          continue
!
!        -- DEFINITION DES POINT_I :
            nterm1(1)=1
            call eclan1(1, mxnbpi, nsomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(1, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(2)=1
            call eclan1(2, mxnbpi, nsomm1, nterm1, 2,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(2, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(3)=1
            call eclan1(3, mxnbpi, nsomm1, nterm1, 3,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(3, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(4)=1
            call eclan1(4, mxnbpi, nsomm1, nterm1, 4,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(4, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            nterm1(5)=1
            call eclan1(5, mxnbpi, nsomm1, nterm1, 5,&
                        0, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(5, mxnbpi, csomm1, nterm1, 1,&
                        0, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(6)=2
            call eclan1(6, mxnbpi, nsomm1, nterm1, 1,&
                        2, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(6, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(7)=2
            call eclan1(7, mxnbpi, nsomm1, nterm1, 2,&
                        3, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(7, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(8)=2
            call eclan1(8, mxnbpi, nsomm1, nterm1, 3,&
                        4, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(8, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(9)=2
            call eclan1(9, mxnbpi, nsomm1, nterm1, 4,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(9, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(10)=2
            call eclan1(10, mxnbpi, nsomm1, nterm1, 1,&
                        5, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(10, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(11)=2
            call eclan1(11, mxnbpi, nsomm1, nterm1, 2,&
                        5, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(11, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(12)=2
            call eclan1(12, mxnbpi, nsomm1, nterm1, 3,&
                        5, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(12, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
            nterm1(13)=2
            call eclan1(13, mxnbpi, nsomm1, nterm1, 4,&
                        5, 0, 0, 0, 0,&
                        0, 0)
            call eclac1(13, mxnbpi, csomm1, nterm1, 1,&
                        1, 0, 0, 0, 0,&
                        0, 0)
!
            nterm1(14)=3
            call eclan1(14, mxnbpi, nsomm1, nterm1, 1,&
                        2, 5, 0, 0, 0,&
                        0, 0)
            call eclac1(14, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(15)=3
            call eclan1(15, mxnbpi, nsomm1, nterm1, 2,&
                        3, 5, 0, 0, 0,&
                        0, 0)
            call eclac1(15, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(16)=3
            call eclan1(16, mxnbpi, nsomm1, nterm1, 3,&
                        4, 5, 0, 0, 0,&
                        0, 0)
            call eclac1(16, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
            nterm1(17)=3
            call eclan1(17, mxnbpi, nsomm1, nterm1, 4,&
                        1, 5, 0, 0, 0,&
                        0, 0)
            call eclac1(17, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 0, 0, 0,&
                        0, 0)
!
            nterm1(18)=4
            call eclan1(18, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 0, 0,&
                        0, 0)
            call eclac1(18, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 1, 0, 0,&
                        0, 0)
!
            nterm1(19)=5
            call eclan1(19, mxnbpi, nsomm1, nterm1, 1,&
                        2, 3, 4, 5, 0,&
                        0, 0)
            call eclac1(19, mxnbpi, csomm1, nterm1, 1,&
                        1, 1, 1, 4, 0,&
                        0, 0)
!
!
!
!        -- CONNECTIVITE DES SOUS-ELEMENTS :
            call eclaco(1, mxnbn2, connx, nbno2, 1,&
                        6, 14, 10, 9, 18,&
                        19, 17)
            call eclaco(2, mxnbn2, connx, nbno2, 2,&
                        7, 15, 11, 6, 18,&
                        19, 14)
            call eclaco(3, mxnbn2, connx, nbno2, 3,&
                        8, 16, 12, 7, 18,&
                        19, 15)
            call eclaco(4, mxnbn2, connx, nbno2, 4,&
                        9, 17, 13, 8, 18,&
                        19, 16)
!
            call eclaco(5, mxnbn2, connx, nbno2, 14,&
                        11, 15, 19, 5, 0,&
                        0, 0)
            call eclaco(6, mxnbn2, connx, nbno2, 15,&
                        12, 16, 19, 5, 0,&
                        0, 0)
            call eclaco(7, mxnbn2, connx, nbno2, 16,&
                        13, 17, 19, 5, 0,&
                        0, 0)
            call eclaco(8, mxnbn2, connx, nbno2, 17,&
                        10, 14, 19, 5, 0,&
                        0, 0)
!
        else
            valk (1) = nomte
            valk (2) = elrefa
            valk (3) = fapg
            call u2mesk('F', 'CALCULEL5_76', 3, valk)
        endif
!
    else
        valk (1) = nomte
        valk (2) = elrefa
        call u2mesk('F', 'CALCULEL5_78', 2, valk)
    endif
!
!
!     -- POUR PRESQUE TOUS LES SCHEMAS 3D, IL Y A IDENTITE: KSE -> KPG:
    nbsel=npg
    do 5, k=1,npg
    corsel(k)=k
    5 end do
!
!     -- EXCEPTION 1
    if (ltetra .and. fapg .eq. 'FPG5') then
        nbsel=6
        corsel(1)=4
        corsel(2)=3
        corsel(3)=2
        corsel(4)=5
        corsel(5)=1
        corsel(6)=1
    endif
!
!     -- EXCEPTION 2
    if (lpyram .and. fapg .eq. 'FPG5') then
        nbsel=8
        corsel(1)=1
        corsel(2)=2
        corsel(3)=3
        corsel(4)=4
!
        corsel(5)=5
        corsel(6)=5
        corsel(7)=5
        corsel(8)=5
    endif
!
!     -- EXCEPTION 3
    if (lpyram .and. fapg .eq. 'FPG27') then
        nbsel=8
        corsel(1)=8
        corsel(2)=9
        corsel(3)=10
        corsel(4)=11
!
        corsel(5)=13
        corsel(6)=14
        corsel(7)=15
        corsel(8)=12
    endif
!
    call jedema()
end subroutine
