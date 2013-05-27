subroutine caladj(col, diag, xadj, adjncy, n,&
                  nnz, deb, tab, suiv, lmat,&
                  ladjn, nrl)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! person_in_charge: olivier.boiteau at edf.fr
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
!------------------------------------------------------------
    implicit none
!      CALCUL DES VOISINS DE TOUS LES NOEUDS ( VERSION ASTER )
!      DONNEES
!      -------
!             DIAG(1:N)        : POINTEURS DE LA MATRICE RANGEE
!             COL(1:LMAT )         SYMETRIQUE INFERIEURE PAR LIGNES
!      RESULTATS
!      ---------
!             XADJ(1:N+1)     POINTEURS ,LES VOISINS DE I SONT ENTRE
!             ADJNCY(1:NADJ)     LES ADRESSES XADJ(I) ET XADJ(I+1)-1
!                                PAR CONTINUITE ON XADJ(N+1) = NADJ+1
!      TAB DE TRAVAIL
!      --------------
!             TAB(1:LT),SUIV(1:LT)
!             NNZ(1:N)
! ATTENTION : XADJ SERT DE TAB DE TRAVAIL DANS LA 1ERE PARTIE (DO 1 )
!-----------
    include 'asterfort/u2mesg.h'
    integer :: lmat, n, col(lmat), diag(0:n), adjncy(*)
    integer :: xadj(n+1), nnz(n), suiv(*), tab(*)
    integer :: nrl
    integer :: deb(n)
!--------------------------------------------------------------
!     VAR. LOCALES
    integer :: i, j, k, ii, it, ladjn, iad
    integer :: vali(2)
    if (nrl .eq. 0) then
!     PAS DE RELATION LINEAIRE ENTRE PLUSIEURS DDL
        do 110 j = 1, n
!
            nnz(j) = diag(j) - diag(j-1 ) - 1
110      continue
        do 120 k = 1, diag(n)
!     PARTIE TRIANGULAIRE SUPERIEURE
            nnz(col(k)) = nnz(col(k)) + 1
120      continue
!
        xadj(1) = 1
        do 130 j = 1, n
!     ON DIMINUE DE 1 CAR ON NE VEUT PAS LE TERME
!     DIAGONAL DANS ADJNCY
            xadj(j+1) = xadj(j) + nnz(j) - 1
            nnz(j) = 0
130      continue
!
        do 150 j = 1, n
            do 140 ii = diag(j-1)+1, diag(j)-1
                i = col(ii)
                adjncy(xadj(j)+nnz(j)) = i
                nnz(j) = nnz(j) + 1
                adjncy(xadj (i)+nnz(i)) = j
                nnz(i) = nnz(i) + 1
140          continue
150      continue
!
!     ---------------------------
    else
!     AVEC RELATION LINEAIRE
!     CALCUL DES LISTES DE NOEUDS A AJOUTER ( FAIT DANS PREMLA)
!
        do 190 i = 1, n
!            DEB(I) =0
            nnz(i) =0
190      continue
!     INITIALISATION DE NNZ : NBRE DE TERMES A AJOUTER
!     POUR CHAQUE LIGNE
        do 220 j = 1, n
            it = deb(j)
219          continue
            if (it .gt. 0) then
                nnz(j) = nnz(j) + 1
                it = suiv(it)
                goto 219
            endif
220      continue
!     VERIFICATION
        do 310 j = 1, n
!     TERMES A AJOUTER PARTIE INFERIEURE
            nnz(j) = nnz(j) + diag(j) - diag(j-1 ) - 1
310      continue
        do 320 k = 1, diag(n)
!     PARTIE TRIANGULAIRE SUPERIEURE
            nnz(col(k)) = nnz(col(k)) + 1
320      continue
        do 325 j = 1, n
!     TERMES A AJOUTER PARTIE SUPERIEURE
            it = deb(j)
324          continue
            if (it .gt. 0) then
                nnz(tab(it)) = nnz(tab(it)) + 1
                it = suiv(it)
                goto 324
            endif
325      continue
!
        xadj(1) = 1
        do 330 j = 1, n
!     ON DIMINUE DE 1 CAR ON NE VEUT PAS LE TERME
!     DIAGONAL DANS ADJNCY
            xadj(j+1) = xadj(j) + nnz(j) - 1
            nnz(j) = 0
330      continue
        if ((xadj(n+1)-1) .gt. ladjn) then
!       TEST D'ESPACE SUFFISANT DANS ADJNCY
            vali (1) = ladjn
            vali (2) = xadj(n+1)-1
            call u2mesg('F', 'ALGELINE4_4', 0, ' ', 2,&
                        vali, 0, 0.d0)
        endif
!
        iad=0
        do 350 j = 1, n
            do 340 ii = diag(j-1)+1, diag(j)-1
                i = col(ii)
                adjncy(xadj(j)+nnz(j)) = i
                nnz(j) = nnz(j) + 1
                adjncy(xadj (i)+nnz(i)) = j
                nnz(i) = nnz(i) + 1
                iad=max(iad,(xadj (i)+nnz(i)))
                iad=max(iad,(xadj (j)+nnz(j)))
340          continue
            it = deb(j)
344          continue
            if (it .gt. 0) then
                adjncy(xadj(j)+nnz(j)) = tab(it)
                nnz(j) = nnz(j) + 1
                adjncy(xadj(tab(it))+nnz(tab(it))) = j
                nnz(tab(it)) = nnz(tab(it)) + 1
                it = suiv(it)
                goto 344
            endif
350      continue
!
    endif
end subroutine
