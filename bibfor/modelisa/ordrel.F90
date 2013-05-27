subroutine ordrel(numnoe, nomnoe, ddl, coef, coefc,&
                  nbocno, nbterm, nomcmp, nddla)
    implicit none
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
    include 'asterc/indik8.h'
    include 'asterfort/ordre1.h'
    integer :: numnoe(nbterm), nbocno(nbterm)
    real(kind=8) :: coef(nbterm)
    complex(kind=8) :: coefc(nbterm)
    character(len=8) :: nomnoe(nbterm), ddl(nbterm), nomcmp(nddla)
!
! ------------------------------------------------------------------
!     REARRANGEMENT DES TABLEAUX D'UNE RELATION LINEAIRE PAR ORDRE
!     DE NOEUD CROISSANT ET DE NUMERO DE DDL CROISSANT POUR UN
!     NOEUD DONNE
! ------------------------------------------------------------------
!  NUMNOE(NBTERM) - VAR    - I    - : NUMEROS DES NOEUDS DE LA
!                 -        -      -   RELATION EN ENTREE
!                 -        -      -   CONTIENT PAR LA SUITE  LES
!                 -        -      -   NUMEROS DES DDLS
!                 -        -      -   SERT DE CLE POUR LE TRI
! -----------------------------------------------------------------
!  NOMNOE(NBTERM) - VAR    - K8   - : NOMS DES NOEUDS DE LA
!                 -        -      -   RELATION
! -----------------------------------------------------------------
!  DDL(NBTERM)    - VAR    - K8   - : NOMS DES DDLS DE LA
!                 -        -      -   RELATION
! -----------------------------------------------------------------
!  COEF(NBTERM)   - VAR    - R    - : COEFFICIENTS REELS DES TERMES
!                 -        -      -   DE LA RELATION
! -----------------------------------------------------------------
!  COEFC(NBTERM)  - VAR    - C    - : COEFFICIENTS COMPLEXES DES
!                 -        -      -   TERMES DE LA RELATION
! -----------------------------------------------------------------
!  NBOCNO(NBTERM) - VAR    - I    - : NOMBRE D'OCCURENCES DE CHAQUE
!                 -        -      -   TERME DANS LA RELATION
! -----------------------------------------------------------------
!  NBTERM         - IN     - I    - : NOMBRE DE TERMES DE LA
!                 -        -      -   RELATION
! -----------------------------------------------------------------
!  NOMCMP(NDDLA)  - IN     - K8   - : NOMS DES COMPOSANTES POSSIBLES
!                 -        -      -   AUX NOEUDS DU MAILLAGE
! -----------------------------------------------------------------
!  NDDLA          - IN     - I    - : NOMBRE MAX DE COMPOSANTES
!                 -        -      -   POSSIBLES EN UN NOEUD
! -----------------------------------------------------------------
!
! --- REARRANGEMENT DES TABLEAUX DES NOEUDS, DES DDLS ET DES ---
! --- COEFFICIENTS DE LA RELATION SELON L'ORDRE CROISSANT    ---
! --- DES NOEUDS                                             ---
!
!-----------------------------------------------------------------------
    integer :: i, ind, j, k, nbterm, nddla
!-----------------------------------------------------------------------
    call ordre1(numnoe, nomnoe, ddl, coef, coefc,&
                nbterm)
!
! --- DETERMINATION DU TABLEAU NBOCNO DONNANT LE NOMBRE ---
! --- D'OCCURENCES DES NOEUDS DANS LA LISTE_RELA
!
    do 10 i = 1, nbterm
        nbocno(i) = 1
10  end do
    do 20 i = 1, nbterm - 1
        do 30 j = i+1, nbterm
            if (numnoe(i) .eq. numnoe(j)) then
                nbocno(i) = nbocno(i) + 1
            endif
30      continue
20  end do
!
! --- REARRANGEMENT DES TABLEAUX DES NOEUDS, DES DDLS ET DES ---
! --- COEFFICIENTS DE LA RELATION SELON L'ORDRE CROISSANT    ---
! --- DES DDLS POUR UN NOEUD DONNE                           ---
!
    k = 0
!
! --- CAS DU PREMIER NOEUD ---
!
    if (nbocno(1) .gt. 1) then
        do 40 j = 1, nbocno(1)
            k = k+1
! --- ICI NUMNOE CONTIENT LES NUMEROS DES DDLS POUR UN NOEUD DONNE
            numnoe(k) = indik8(nomcmp,ddl(k),1,nddla)
40      continue
        call ordre1(numnoe, nomnoe, ddl, coef, coefc,&
                    nbocno(1))
    else
        k = k+1
    endif
!
! --- CAS DES AUTRES NOEUDS  ---
!
    do 50 i = 2, nbterm
!
! --- ON UTILISE LE FAIT QUE SI DES NOEUDS SONT EGAUX, ILS  ---
! --- SONT CONSECUTIFS                                      ---
!
        if (nomnoe(i) .eq. nomnoe(i-1)) goto 50
        if (nbocno(i) .gt. 1) then
            do 60 j = 1, nbocno(i)
                k = k + 1
! --- ICI NUMNOE CONTIENT LES NUMEROS DES DDLS POUR UN NOEUD DONNE
                numnoe(k) = indik8(nomcmp,ddl(k),1,nddla)
60          continue
            ind = k - nbocno(i) + 1
            call ordre1(numnoe(ind), nomnoe(ind), ddl(ind), coef(ind), coefc(ind),&
                        nbocno(i))
        else
            k = k+1
        endif
50  end do
end subroutine
