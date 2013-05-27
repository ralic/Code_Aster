subroutine eclaty(nomte, elrefa, fapg, npg, npoini,&
                  nterm1, nsomm1, csomm1, tyma, nbno2,&
                  connx, mxnbn2, mxnbpi, mxnbte, mxnbse,&
                  nbsel, corsel)
    implicit   none
    include 'asterc/indik8.h'
    include 'asterfort/assert.h'
    include 'asterfort/ecla2d.h'
    include 'asterfort/ecla3d.h'
    include 'asterfort/elraca.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    integer :: mxnbn2, mxnbpi, mxnbte, mxnbse
    integer :: ndim, npg, connx(mxnbn2, mxnbse), nsomm1(mxnbpi, mxnbte)
    integer :: nterm1(mxnbpi), nbno2(mxnbse), npoini, tyma(mxnbse)
    integer :: nbsel, corsel(mxnbse)
    real(kind=8) :: csomm1(mxnbpi, mxnbte)
    character(len=8) :: elrefa, fapg
    character(len=16) :: nomte
!
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
! --------------------------------------------------------------------
! BUT : DECOMPOSER LES TYPE_ELEM EN AUTANT DE SOUS-ELEMENTS QUE
!       DE POINTS DE GAUSS.
! ---------------------------------------------------------------------
! IN : NOMTE  : NOM D'UN TYPE_ELEM
! IN : ELREFA : NOM DE L'ELREFA
! IN : FAPG   : FAMILLE DE POINTS DE GAUSS
!      MXNBN2 : MAX DU NOMBRE DE NOEUDS D'UN SOUS-ELEMENT (HEXA8)
!      MXNBPI : MAX DU NOMBRE DE POINT_I (HEXA A 27 POINTS DE GAUSS)
!      MXNBTE : MAX DU NOMBRE DE TERMES DE LA C.L. DEFINISSANT 1 POINT_I
!      MXNBSE : MAX DU NOMBRE DE SOUS-ELEMENTS
!
! OUT : NPG    : NOMBRE DE POINTS DE GAUSS (PG)
! OUT : NBSEL  : NOMBRE DE SOUS-ELEMENTS (SE)
! OUT : CORSEL : CORRESPONDANCE SE -> KPG
!       EN GENERAL : NBSE=NBPG ET CORSEL(KSE)=KPG
!       MAIS PARFOIS :
!           NBSEL > NBPG : PLUSIEURS SE POUR 1 SEUL PG
!           NBSEL < NBPG : IL Y A DES PG SANS SE (DEHORS)
!
! ---------------------------------------------------------------------
! DESCRIPTION DES POINTS INTERMEDIAIRES (POINT_I) :
! ------------------------------------------------
! UN POINT_I EST DEFINI COMME UNE COMBINAISON LINEAIRE DES NOEUDS
! DE LA MAILLE SOUS-JACENTE AU TYPE_ELEM :
! POINT_I = SOMME COEF(K)*NOEUD(K)  (1<= K <=NTERMES)
!           NTERMES <= 27 (HEXA27)
!
! OUT : NPOINI : NOMBRE DE POINT_I POUR LE TYPE_ELEM/FAPG
!                    (NPOINI <= 64 : (HEXA20 A 27 POINTS DE GAUSS))
! OUT : NTERM1 : NTERM1(IPOINI) : NOMBRE DE TERMES DE LA COMBINAISON
!                POUR LE POINT_I IPOINI
! OUT : NSOMM1 : NSOMM1(IPOINI,K) NUMERO DU NOEUD DU KEME TERME.
! OUT : CSOMM1 : CSOMM1(IPOINI,K) COEF. DU NOEUD DU KEME TERME.
!
! CONNECTIVITE DES SOUS-ELEMENTS :
!-------------------------------
! OUT : TYMA (I)  : TYMA(KSE)-> TYPE_MAILLE ASSOCIE AU SOUS-ELEMENT KSE
! OUT : NBNO2 (I) : NBNO2(KSE) -> NNO2
!                    NNO2 : NOMBRE DE NOEUDS DU SOUS-ELEMENT KSE
! OUT : CONNX (I) : CONNX(INO2,KSE) -> IPOINI
!                    IPOINI= NUMERO DU POINT_I ASSOCIE AU
!                    SOMMET INO2 DU SOUS-ELEMENT KSE
!                    (1<= INO2 <= NNO2(KSE))
!                    (1<= IPOINI <= NPOINI)
!
! ---------------------------------------------------------------------
    integer :: nbnomx, nbfamx
    parameter    ( nbnomx=27, nbfamx=20)
    integer :: nno, nnos, nbfpg, nbpg(nbfamx), nufpg
    real(kind=8) :: vol, x(3*nbnomx)
    character(len=8) :: famg(nbfamx)
! ---------------------------------------------------------------------
    call jemarq()
!
    npg = 0
    npoini = 0
    nbsel = 0
!
    call elraca(elrefa, ndim, nno, nnos, nbfpg,&
                famg, nbpg, x, vol)
!
    nufpg = indik8( famg, fapg, 1, nbfpg )
    call assert(nufpg.gt.0)
    npg = nbpg(nufpg)
!
    if (ndim .eq. 2) then
        call ecla2d(nomte, elrefa, fapg, npg, npoini,&
                    nterm1, nsomm1, csomm1, tyma, nbno2,&
                    connx, mxnbn2, mxnbpi, mxnbte, mxnbse,&
                    nbsel, corsel)
!
    else if (ndim .eq. 3) then
        call ecla3d(nomte, elrefa, fapg, npg, npoini,&
                    nterm1, nsomm1, csomm1, tyma, nbno2,&
                    connx, mxnbn2, mxnbpi, mxnbte, mxnbse,&
                    nbsel, corsel)
!
    endif
!
    call jedema()
!
end subroutine
