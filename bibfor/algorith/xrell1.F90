subroutine xrell1(tabnoz, ndim, nar, pickno, nbpino,&
                  nliseq)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! TOLE CRS_1404
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/wkvect.h'
    integer :: ndim, nar, nbpino
    integer :: tabnoz(3, nar), pickno(nbpino)
    real(kind=8) :: tabco(ndim, nar)
    character(len=19) :: nliseq
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (PREPARATION)
!
! CHOIX DE L'ESPACE DES LAGRANGES POUR LE CONTACT - V1:
!                    (VOIR BOOK VI 15/07/05)
!    - CREATION DES RELATIONS DE LIAISONS ENTRE LAGRANGE
!
! ----------------------------------------------------------------------
!
!
! IN  TABNOZ : TABLEAU DES NOEUDS EXTREMITES ET NOEUD MILIEU
! IN  NAR    : NOMBRE D'ARETES COUPEES
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  TABCO  : TABLEAU DES COORDONNEES DE NOEUDS MILIEU
! IN  PICKNO : NUMEROS DES NOEUDS SELECTIONNE
! IN  NBPINO : NOMBRE DE NOEUDS SELECTIONNE
! OUT NLISRL : LISTE REL. LIN. POUR V1 ET V2
! OUT NLISCO : LISTE REL. LIN. POUR V1 ET V2
! OUT NLISEQ : LISTE REL. LIN. POUR V2 SEULEMENT
!
!
!
!
    integer :: i, j, in, dimeq, ia, ext, libre, k, eq(100), tabno(nar, 3), ie
    integer :: liseqt(nar, 2), nreleq, jlis1, picked
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    do 10 i = 1, nar
        do 11 j = 1, 3
            tabno(i,j)=tabnoz(j,i)
11      continue
10  end do
    nreleq = 0
!
! --- CREATION DU TABLEAU TEMPORAIRE DES RELATION D'EGALITE : LISEQT
!
    do 100 i = 1, nbpino
        in = pickno(i)
        dimeq = 0
        do 110 ia = 1, nar
            do 111 j = 1, 2
!           ON CHERCHE LES ARETES EMANANTES
                if (tabno(ia,j) .eq. in) then
                    ext=tabno(ia,3-j)
!             ON REGARDE SI L'AUTRE EXTREMITE EST LIBRE
                    libre=1
                    do 112 k = 1, nbpino
                        if (ext .eq. pickno(k)) libre=0
112                  continue
                    if (libre .eq. 1) then
                        dimeq=dimeq+1
                        eq(dimeq)=ia
                    endif
                endif
111          continue
110      continue
        do 121 ie = 1, dimeq
            nreleq=nreleq+1
            liseqt(nreleq,1)=tabno(eq(ie) , 1)
            liseqt(nreleq,2)=tabno(eq(ie) , 2)
121      continue
100  end do
!
! --- STOCKAGE DE LISEQT
!
    if (nreleq .gt. 0) then
        call wkvect(nliseq, 'G V I', nreleq*2, jlis1)
        do 130 ie = 1, nreleq
            zi(jlis1-1+2*(ie-1)+1)=liseqt(ie,1)
            zi(jlis1-1+2*(ie-1)+2)=liseqt(ie,2)
130      continue
    endif
!
    call jedema()
end subroutine
