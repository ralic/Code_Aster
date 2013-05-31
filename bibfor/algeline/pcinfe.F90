subroutine pcinfe(n, icpl, icpc, icpd, icplp,&
                  icpcp, ind, lca, ier)
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
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!       S.P. PCINFE IDEM S-P PCFULL
!                    SAUF NON CREATION COEFS SUR U
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! BUT : CE SP CALCULE LES POINTEURS ICPLP,ICPCP
! ----  CORRESPONDANTS AU 'REMPLISSAGE' DE LA
!       MATRICE EN COURS DE FACTORISATION
!
!       VERSION GENERALE : MATRICE A STRUCTURE QUELCONQUE
!
!   PARAMETRES D'ENTREE:
!   -------------------
!
!   ICPL,ICPD,ICPC : LES POINTEURS ASSOCIES A LA MATRICE A FACTORISER
!
!   * ICPL(I) = ADRESSE DANS LE RANGEMENT DES COEFFICIENTS A(I,J)
!               DU DERNIER COEFFICIENT DE LA LIGNE I
!   * ICPC(K) = POUR K = ICPL(I-1)+1...ICPL(I) NUMEROS DES INDICES DE
!               COLONNE J, DES COEFFICIENTS A(I,J) DE LA LIGNE I
!               ( RANGES PAR ORDRE DE J CROISSANT)
!   * ICPD(I) = ADRESSE DANS LE RANGEMENT DES COEFFICIENTS A(I,J)
!               DU DERNIER COEFFICIENT DE LA LIGNE I AVEC A(I,J), J < I
!
!   IND         : TABLEAU UTILITAIRE
!   LCA         : TAILLE MAXIMALE ADMISE POUR LA MATRICE FACTORISEE
!
!   PARAMETRE DE SORTIE:
!   -------------------
!
!   ICPLP,ICPCP : LES POINTEURS ASSOCIES AU REMPLISSAGE
!
!   * ICPLP(I) = ADRESSE DANS LE RANGEMENT DES COEFFICIENTS A(I,J)
!               DU DERNIER COEFFICIENT DE REMPLISSAGE DE LA LIGNE I
!   * ICPCP(K) = POUR K = ICPL(I-1)+1...ICPL(I) NUMEROS DES INDICES DE
!               COLONNE J, DES COEFFICIENTS DE REMPLISSAGE DE LA LIGNE I
!               ( RANGES PAR ORDRE DE J CROISSANT)
!
!   ICPL,ICPC  : LES POINTEURS ASSOCIES A LA MATRICE FACTORISEE
!                ( REUNION DE ICPL ET ICPLP, ICPC ET ICPCP )
!   NZA        : NOMBRE DE COEFFICIENTS DE LA MATRICE FACTORISEE
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! aslint: disable=W1304
    implicit none
    include 'asterfort/pctrii.h'
    integer(kind=4) :: icpc(*)
    integer :: icpl(0:n), icpd(n)
    integer :: icplp(0:n), icpcp(*), ind(n)
!
!     INITIALISATION DU TABLEAU INDIC
!     -------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ic1, ic2, ier, istop, j, jj
    integer :: k, k1, k2, kp1, kp2, l, lca
    integer :: n, nzero
!-----------------------------------------------------------------------
    do 10 i = 1, n
        ind(i) = 0
10  end do
    ic1 = 0
    ic2 = 0
    k1 = 1
!
!     FACTORISATION LOGIQUE : LIGNE PAR LIGNE
!     ---------------------------------------
!
    do 50 i = 1, n
        k2 = icpl(i)
!
!     MISE A JOUR DU TABLEAU INDIC
!
        do 20 k = k1, k2
            j = icpc(k)
            ind(j) = i
20      continue
        ind(i) = i
!
!     RECHERCHE DANS LA LIGNE I DES L(I,J) NON NULS
!
        do 40 k = k1, icpd(i)
            j = icpc(k)
!
!     RECHERCHE DANS LA LIGNE J DES U(J,JJ) NON NULS
!
            do 30 l = icpd(j) + 1, icpl(j)
                jj = icpc(l)
!
!     LE COEFFICIENT L(I,JJ) EXISTE-T-IL ?
!                         ARRET AU PREMIER  U(JJ,I)
!
                if (jj .ge. i) goto 30
!
                if (ind(jj) .ne. i) then
!
!     NON ==> CREATION D'UN COEFFICIENT DE REMPLISSAGE
!
                    ic1 = ic1 + 1
!
!     TEST DE DEPASSEMENT DE DIMENSION (PROTECTION DES TABLEAUX)
!
                    if (ic1 .gt. lca) then
!               WRITE (6,107) NIV,LCA,I
                        istop = i
                        goto 100
                    endif
!
!     STOCKAGE DE L'INDICE DE COLONNE DU COEFFICIENT LU(I,JJ)
!
                    icpcp(ic1) = jj
!
!     MISE A JOUR DU TABLEAU INDIC
!
                    ind(jj) = i
                endif
30          continue
40      continue
!
!     RECLASSEMENT DES INDICES DE COLONNE PAR ORDRE CROISSANT
!
        call pctrii(icpcp(ic2+1), ic1-ic2)
!
!     MISE A JOUR DU POINTEUR ICPLP
!
        icplp(i) = ic1
        ic2 = ic1
        k1 = k2 + 1
50  end do
    icplp(0) = 0
!
!     AVANT FUSION DE ICPC ET ICPCP
!     TEST DE DEPASSEMENT DE DIMENSION (PROTECTION DES TABLEAUX)
!
    k1 = icpl(n)
    kp1 = icplp(n)
    nzero = k1 + kp1
    if (nzero .gt. lca) then
!       WRITE (6,200) NIV,LCA,NZERO
        ier = nzero
        goto 150
    endif
!
!     CREATION DES TABLEAUX ICPL ET ICPC
!     POUR LA MATRICE FACTORISEE : REUNION DES TABLEAUX ICPC ET ICPCP
!     ---------------------------------------------------------------
!
    k = nzero
    do 90 i = n, 1, -1
        icpl(i) = k
        kp2 = icplp(i-1)
        k2 = icpl(i-1)
60      continue
        if (k1 .gt. k2) then
            if (kp1 .gt. kp2) then
!       -------------------
                if (icpc(k1) .lt. icpcp(kp1)) then
                    icpc(k) = icpcp(kp1)
                    k = k - 1
                    kp1 = kp1 - 1
                else
                    icpc(k) = icpc(k1)
                    k = k - 1
                    k1 = k1 - 1
                endif
            else
                icpc(k) = icpc(k1)
                k = k - 1
                k1 = k1 - 1
            endif
        else
!     ---- LIGNE DE L EPUISEE ------
70          continue
            if (kp1 .gt. kp2) then
                icpc(k) = icpcp(kp1)
                k = k - 1
                kp1 = kp1 - 1
            else
                goto 80
            endif
            goto 70
        endif
!     ------
        goto 60
80      continue
90  end do
!
!     LE NOMBRE DE COEFFICIENTS DE LA MATRICE FACTORISEE
!
!     NZCA = NZERO
!     WRITE (6,*) ' FIN DU S-P PCINFE  TAILLE FACTORISEE= ',NZCA
!
    goto 150
!
!  DEPASSEMENT DE DIMENSION ON CALCULE IC1= PLACE A AJOUTER
!
100  continue
    do 140 i = istop, n
        k2 = icpl(i)
        do 110 k = k1, k2
            j = icpc(k)
            ind(j) = i
110      continue
        ind(i) = i
        do 130 k = k1, icpd(i)
            j = icpc(k)
            do 120 l = icpd(j) + 1, icpl(j)
                jj = icpc(l)
                if (jj .ge. i) goto 120
!
                if (ind(jj) .ne. i) then
!     NON ==> CREATION D'UN COEFFICIENT DE REMPLISSAGE
                    ic1 = ic1 + 1
                    ind(jj) = i
                endif
120          continue
130      continue
        k1 = k2 + 1
140  end do
! NZERO=TAILLE MAT INI.+TAILLE MAT REMPLIE
    nzero = icpl(n) + ic1
!     WRITE (6,200) NIV,LCA,NZERO
    ier = nzero
150  continue
!
end subroutine
