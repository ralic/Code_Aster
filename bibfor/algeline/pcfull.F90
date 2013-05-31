subroutine pcfull(n, icpl, icpc, icpd, icplp,&
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
!                    S.P. PCFULL
!                    -----------
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
!=======================================================================
!
!     ON EFFECTUE UNE FACTORISATION LOGIQUE DE LA MATRICE A, LIGNE PAR
!     LIGNE, SANS ACTUALISATION DU REMPLISSAGE : ON NE CALCULE PAS LES
!      COEFFICIENTS L(I,J) ET U(I,J), MAIS LEUR POSITION (I,J).
!
!     LES FORMULES DE FACTORISATION UTILISEES :
!
! (1) L(I,JJ) = [ A(I,JJ) - SOM[J=1,JJ-1] L(I,J).U(J,JJ) ] / U(JJ,JJ)
!      POUR I=1,2,.....N ET JJ=1,2,....I
!
! (2) U(JJ,I) =   A(JJ,I) - SOM[J=1,JJ-1] L(JJ,J).U(JJ,J)
!      POUR I=1,2,.....N ET JJ=1,2,....I
!
!     ON PROCEDE AU CALCUL DES L(I,JJ) ET U(JJ,I) : IL Y A REMPLISSAGE
!     SI UN AU MOINS DES PRODUITS DE LA SOMME EST NON NUL
!
!     => POUR CHACUN DES L(I,J) NON NUL DE LA LIGNE I (BOUCLE 3: J < I)
!        ON RECHERCHE S'IL EXISTE UN U(J,JJ) NON NUL  (BOUCLE 4: J < JJ)
!        SI ON EN TROUVE UN, ON TESTE L'EXISTENCE DU COEFFICIENT A(I,JJ)
!        A L'AIDE DU TABLEAU INDIC. ON CREE UN COEFFICIENT DE REMPLISSAG
! E
!        SI NECESSAIRE.
!
!     => POUR CHACUN DES U(J,I) NON NUL DE LA LIGNE I (BOUCLE 3: J < I)
!        ON RECHERCHE S'IL EXISTE UN L(JJ,J) NON NUL  (BOUCLE 4: J < JJ)
!        SI ON EN TROUVE UN, ON TESTE L'EXISTENCE DU COEFFICIENT A(I,JJ)
!        A L'AIDE DU TABLEAU INDIC. ON CREE UN COEFFICIENT DE REMPLISSAG
! E
!        SI NECESSAIRE.
!
!    REMARQUE :  DANS LES FORMULES (1) ET (2) LES SOMMES SONT CALCULEES
!    --------    AVEC A(K,L) A LA PLACE DE L(K,L) OU U(K,L) CE QUI EST
!                COHERENT PUISQU'ON N'ACTUALISE PAS LE REMPLISSAGE
!
!=======================================================================
!
!     INITIALISATION DU TABLEAU INDIC
!     -------------------------------
!-----------------------------------------------------------------------
    integer :: i, ic1, ic2, ier, istop, j, jj
    integer :: k, k1, k2, kp1, kp2, l, lca
    integer :: n, ncremx, nzero
!-----------------------------------------------------------------------
    do 10 i = 1, n
        ind(i)=0
10  end do
    ic1=0
    ic2=0
    k1=1
!
!
!     FACTORISATION LOGIQUE : LIGNE PAR LIGNE
!     ---------------------------------------
    do 50 i = 1, n
!
!       TEST DE DEPASSEMENT DE DIMENSION (PROTECTION DES TABLEAUX)
!       REMARQUE JP : LA FICHE 12292 MONTRE QUE CE TEST EST INSUFFISANT
!       POUR NE PAS DEBORDER DU TABLEAU ICPCP, J'AI DU AJOUTER UN AUTRE
!       TEST AVANT ICPCP(IC1)=JJ (VOIR CI-DESSOUS)
        ncremx=i-icpc(k1)
        if (ic1+ncremx .gt. lca) then
            istop=i
            goto 90
        endif
!
!       MISE A JOUR DU TABLEAU INDIC
        k2=icpl(i)
        do 20 k = k1, k2
            j=icpc(k)
            ind(j)=i
20      continue
        ind(i)=i
!
!       RECHERCHE DANS LA LIGNE I DES L(I,J) NON NULS
        do 40 k = k1, icpd(i)
            j=icpc(k)
!         RECHERCHE DANS LA LIGNE J DES U(J,JJ) NON NULS
            do 30 l = icpd(j)+1, icpl(j)
                jj=icpc(l)
!           LE COEFFICIENT L(I,JJ) OU U(JJ,I) EXISTE-T-IL ?
                if (ind(jj) .ne. i) then
!             NON ==> CREATION D'UN COEFFICIENT DE REMPLISSAGE
                    ic1=ic1+1
!             STOCKAGE DE L'INDICE DE COLONNE DU COEFFICIENT LU(I,JJ)
                    if (ic1 .gt. lca) then
                        istop=i
                        goto 90
                    endif
                    icpcp(ic1)=jj
!             MISE A JOUR DU TABLEAU INDIC
                    ind(jj)=i
                endif
30          continue
40      continue
!
!       RECLASSEMENT DES INDICES DE COLONNE PAR ORDRE CROISSANT
        call pctrii(icpcp(ic2+1), ic1-ic2)
!
!       MISE A JOUR DU POINTEUR ICPLP
        icplp(i)=ic1
        ic2=ic1
        k1=k2+1
50  end do
    icplp(0)=0
!
!
!     TEST DE DEPASSEMENT DE DIMENSION (PROTECTION DES TABLEAUX)
    k1=icpl(n)
    kp1=icplp(n)
    nzero=k1+kp1
    if (nzero .gt. lca) then
        ier=nzero
        goto 140
    endif
!
!
!     CREATION DES TABLEAUX ICPL ET ICPC
!     POUR LA MATRICE FACTORISEE : REUNION DES TABLEAUX ICPC ET ICPCP
!     ---------------------------------------------------------------
    k=nzero
    do 80 i = n, 1, -1
        icpl(i)=k
        kp2=icplp(i-1)
        k2=icpl(i-1)
60      continue
!
        if (k1 .gt. k2) then
!          LIGNE DE L EN COURS
            if (kp1 .gt. kp2) then
!           LIGNE DE COEF EN COURS
                if (icpc(k1) .lt. icpcp(kp1)) then
                    icpc(k)=icpcp(kp1)
                    kp1=kp1-1
                else
                    icpc(k)=icpc(k1)
                    k1=k1-1
                endif
!
            else
!           LIGNE DE COEF FINIE
                icpc(k)=icpc(k1)
                k1=k1-1
            endif
!
        else
!         LIGNE DE L FINIE
            if (kp1 .gt. kp2) then
!           LIGNE DE COEF EN COURS
                icpc(k)=icpcp(kp1)
                kp1=kp1-1
            else
                goto 70
            endif
        endif
!
        k=k-1
        goto 60
!
70      continue
80  end do
!
!     LE NOMBRE DE COEFFICIENTS DE LA MATRICE FACTORISEE
!     NZCA = NZERO
!     WRITE (6,*) ' FIN DU S-P FILLIN  TAILLE FACTORISEE= ',NZCA
    goto 140
!
!
90  continue
!     DEPASSEMENT DE DIMENSION ON CALCULE IC1= PLACE A AJOUTER
    do 130 i = istop, n
        k2=icpl(i)
        do 100 k = k1, k2
            j=icpc(k)
            ind(j)=i
100      continue
        ind(i)=i
        do 120 k = k1, icpd(i)
            j=icpc(k)
            do 110 l = icpd(j)+1, icpl(j)
                jj=icpc(l)
                if (jj .ge. i) goto 110
!
                if (ind(jj) .ne. i) then
!             NON ==> CREATION D'UN COEFFICIENT DE REMPLISSAGE
                    ic1=ic1+1
                    ind(jj)=i
                endif
110          continue
120      continue
        k1=k2+1
130  end do
!
!
!     NZERO=TAILLE MAT INI. + TAILLE MAT REMPLIE
    nzero=icpl(n)+ic1
    ier=nzero
140  continue
!
end subroutine
