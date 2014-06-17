subroutine tlduc8(nommat, hcol, adia, ablo, npivot,&
                  neq, nbbloc, ildeb, ilfin, eps)
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nommat
!  ------------------------------------------------------------------
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
!  ------------------------------------------------------------------
! BUT : DECOMPOSTION D'UNE MATRICE NON_SYMETRIQUE A COEFFICIENTS
!       COMPLEXES SOUS LA FORME DE CROUT LDU
!       ELLE CORRESPOND AU SOLVEUR 'LDLT' + COMPLEXE + NON SYM
!       (CETTE ROUTINE EST LA SOEUR JUMELLE DE TLDUR8)
!  ------------------------------------------------------------------
!
!  IN  NOMMAT  :    : NOM UTILISATEUR DE LA MATRICE A FACTORISER
!  IN  HCOL    : IS : HCOL DE LA MATRICE
!  HCOL(I) RENVOIE LA HAUTEUR DE LA I-EME COLONNE
!  IN  ADIA    : IS : ADRESSE DU TERME DIAGONALE DANS SON BLOC
!  ADIA(I) RENVOIE L'ADRESSE DE LA I-EME LIGNE DANS SON BLOC
!  IN  ABLO    :  :   POINTEUR DE BLOC
!  ABLO(I+1) RENVOIE LE NO DE LA DERNIERE LIGNE DU I-EME BLOC
!
!  LES TABLEAUX RELATIFS AUX BLOCS ONT ETE CONSTRUITS A PARTIR
!  DES NBBLOCS PREMIERS BLOCS (I.E LES BLOCS SUP)
!  MAIS ILS RESTENT VALABLES POUR LES NBBLOCS BLOCS SUIVANTS
!  (I.E LES BLOCS INF)
!  PUISQUE LES POFILS SUP ET INF SONT IDENTIQUES
!
!  VAR PIVOT   : IS :
!  : EN SORTIE : NPIVOT  = 0 ==> R.A.S.
!  :             NPIVOT  > 0 ==> MATRICE SINGULIERE
!                                POUR L'EQUATION DE NUMERO NPIVOT
!  :             NPIVOT  < 0 ==> -NPIVOT TERMES DIAGONAUX < 0
!
!  IN  NEQ     : IS : NOMBRE TOTAL D'EQUATION
!  IN  NBBLOC  : IS : NOMBRE DE BLOC DE LA MATRICE
!  IN  ILDEB   : IS : NUMERO DE LA LIGNE DE DEPART DE LA FACTORISATION
!  IN  ILFIN   : IS : NUMERO DE LA LIGNE DE FIN DE FACTORISITION
!  ------------------------------------------------------------------
!
!  CREATION DE DEUX OBJETS DE TRAVAIL (SUR LA VOLATILE)
!  1)  UN TABLEAU POUR LA DIAGONALE
!  2)  UN TABLEAU POUR LA COLONNE COURANTE
!
!
!  --- RAPPEL SOMMAIRE DE L'ALGORITHME ------------------------------
!
!  POUR S = 2,3, ... ,N
!  !  POUR I = 1,2, ... ,S-1
!  !  !  POUR M = 1,2, ... ,I-1
!  !  !  !  K(S,I) = K(S,I) - K(S,M)*K(M,I) % MODIFIE   LA LIGNE
!  !  !  !  K(I,S) = K(I,S) - K(I,M)*K(M,S) % MODIFIE   LA COLONNE
!  !  !  FIN_POUR
!  !  !  K(I,S) = K(I,S)/K(I,I)           % NORMALISATION DE LA COLONNE
!  !  FIN_POUR
!  !  POUR M = 1,2, ... ,S-1
!  !  !  K(S,S) = K(S,S) - K(S,M) * K(M,S)  % MODIFICATION DU PIVOT
!  !  FIN_POUR
!  FIN_POUR
!  ------------------------------------------------------------------
!  REFERENCE (HISTORIQUE) :
!  (1) P.D. CROUT,
!      A SHORT METHOD FOR EVALUATING DETERMINANTS AND SOLVING SYSTEMS
!      OF LINEAR EQUATIONS WITH REAL OR COMPLEX COEFFICIENTS.
!      AIEE TRANSACTION VOL 60, PP 1235-1240  (1941)
!  ------------------------------------------------------------------
!
!
!  ------------------------------------------------------------------
    integer :: hcol(*), adia(*), ablo(*)
    integer :: npivot, neq, nbbloc, ildeb, ilfin
    integer :: ldiag, ibloc, il1, il2, iaa, il
    integer :: iaas, iaai, kl1, imini, iequa, i, jblmin, jbloc, jl1, jl2
    integer :: iabs, iabi, ilong, iadiai, idei, iadias, ides, idl, jnmini
    integer ::  jequa, jlong, jadias, jdes, jadiai, jdei, jdl, ibcl1
    integer :: lm, icai, icas, icbi, icbs, icd
!
    real(kind=8) :: eps
    complex(kind=8) :: c8vali, c8vals
    character(len=24) :: nomdia, ualf
    character(len=19) :: noma19
    complex(kind=8), pointer :: digs(:) => null()
    character(len=24), pointer :: refa(:) => null()
!  ------------------------------------------------------------------
    data ualf/'                   .UALF'/
    data nomdia/'                   .&VDI'/
!  ------------------------------------------------------------------
!
    call jemarq()
!
    noma19 = nommat
    ualf(1:19) = nommat
    nomdia(1:19) = nommat
!
!  --- CREATION D'UN TABLEAU POUR STOCKER LA DIAGONALE
    call wkvect(nomdia, 'V V C', neq, ldiag)
    call jeveuo(noma19//'.DIGS', 'E', vc=digs)
!
!  --- CREATION/RAPPEL D'UN TABLEAU INTERMEDIAIRE (D'UNE COLONNE)---
!
!  --- INITIALISATIONS ET ALLOCATION ---
!
    npivot = 0
!
!  ------- BOUCLE SUR LES BLOCS A TRANSFORMER -----------------------
    do 150 ibloc = 1, nbbloc
!
        il1 = ablo(ibloc) + 1
        il2 = ablo(ibloc+1)
!
!
        if (il2 .lt. ildeb) then
!        --- C'EST TROP TOT : MAIS ON REMPLIT LA DIAGONALE ---
            call jeveuo(jexnum(ualf, ibloc), 'L', iaa)
            do 10 il = il1, il2
                zc(ldiag+il-1) = zc(iaa+adia(il)-1)
10          continue
            call jelibe(jexnum(ualf, ibloc))
            goto 150
        else if (il1.gt.ilfin) then
!        --- C'EST FINI ---
            goto 160
        else
!
!- RECUPERATION DES BLOCS SUP ET INF COURANTS
!
            call jeveuo(jexnum(ualf, ibloc), 'E', iaas)
            call jeveuo(jexnum(ualf, ibloc+nbbloc), 'E', iaai)
            if (il1 .lt. ildeb) then
                kl1 = ildeb
                do 20 il = il1, kl1 - 1
                    zc(ldiag+il-1) = zc(iaai+adia(il)-1)
20              continue
            else
                kl1 = il1
            endif
            if (il2 .gt. ilfin) il2 = ilfin
        endif
!
!
!     --- RECHERCHE DE LA PLUS PETITE EQUATION EN RELATION AVEC UNE
!     --- EQUATION DES LIGNES EFFECTIVES DU BLOC COURANT
        imini = il2 - 1
        do 30 iequa = kl1, il2
            imini = min(iequa-hcol(iequa),imini)
30      continue
        imini = imini + 1
!
!     --- RECHERCHE DU BLOC D'APPARTENANCE DE L'EQUATION IMINI ---
        do 40 i = 1, ibloc
            jblmin = i
            if (ablo(1+i) .ge. imini) goto 50
40      continue
50      continue
!
!     --- BOUCLE  SUR  LES  BLOCS  DEJA  TRANSFORMES ---
        do 90 jbloc = jblmin, ibloc - 1
!
            jl1 = max(imini,ablo(jbloc)+1)
            jl2 = ablo(jbloc+1)
            call jeveuo(jexnum(ualf, jbloc), 'L', iabs)
            call jeveuo(jexnum(ualf, jbloc+nbbloc), 'L', iabi)
!
            do 80 iequa = kl1, il2
!
!           --- RECUPERATION DE L'ADRESSE ET LA LONGUEUR DE LA LIGNE
                ilong = hcol(iequa) - 1
!-   ADRESSE DU TERME DIAGONAL DE LA LIGNE IEQUA DANS LE BLOC INF
                iadiai = iaai + adia(iequa) - 1
!-   ADRESSE DU DEBUT DE LA LIGNE IEQUA DANS LE BLOC INF
                idei = iadiai - ilong
!-   ADRESSE DU TERME DIAGONAL DE LA COLONNE IEQUA DANS LE BLOC SUP
                iadias = iaas + adia(iequa) - 1
!-   ADRESSE DU DEBUT DE LA COLONNE IEQUA DANS LE BLOC SUP
                ides = iadias - ilong
!-   INDICE DU DEBUT DE LA LIGNE (COLONNE) IEQUA
                idl = iequa - ilong
!
!           --- UTILISATION DES LIGNES (IDL+1) A (JL2) ---
!
!- MODIFICATION DES LIGNES   IDLI+1 A JL2
!-          ET  DES COLONNES IDLS+1 A JL2
!- POUR LES BLOCS SUP ET INF COURANTS AUXQUELS ON A ACCEDE EN ECRITURE
!
                jnmini = max(idl,jl1)
                do 70 jequa = jnmini, jl2
                    jlong = hcol(jequa) - 1
!-    ADRESSE DU TERME DIAGONAL KII DANS LE BLOC SUP
                    jadias = iabs + adia(jequa) - 1
!-    ADRESSE DANS LE BLOC SUP DU PREMIER TERME NON NUL DE LA COLONNE I
                    jdes = jadias - jlong
!-    ADRESSE DU TERME DIAGONAL KII DANS LE BLOC INF
                    jadiai = iabi + adia(jequa) - 1
!-    ADRESSE DANS LE BLOC INF DU PREMIER TERME NON NUL DE LA LIGNE I
                    jdei = jadiai - jlong
!-    INDICE DU PREMIER TERME NON NUL DE LA COLONNE (LIGNE) I
                    jdl = jequa - jlong
!-    INDICE DU PREMIER TERME A PARTIR DUQUEL ON VA FAIRE LE PRODUIT
!-    SCALAIRE K(S,M)*K(M,I) DES TERMES LIGNE-COLONNE
                    ibcl1 = max(idl,jdl)
!-    LONGUEUR SUR LAQUELLE ON FAIT LE PRODUIT SCALAIRE LIGNE-COLONNE
                    lm = jequa - ibcl1
!-    ADRESSE DANS LE BLOC INF DU PREMIER TERME A MODIFIER SUR LA LIGNE
!-    IEQUA
                    icai = idei + ibcl1 - idl
!-    ADRESSE DANS LE BLOC SUP DU PREMIER TERME A MODIFIER SUR LA
!-    COLONNE IEQUA
                    icas = ides + ibcl1 - idl
!-    ADRESSE DANS LE BLOC INF DU PREMIER TERME DE LA LIGNE
!-    A PARTIR DUQUEL ON FAIT LE PRODUIT SCALAIRE POUR MODIFIER LA
!-    COLONNE = K(I,M)
                    icbi = jdei + ibcl1 - jdl
!-    ADRESSE DANS LE BLOC SUP DU PREMIER TERME DE LA COLONNE
!-    A PARTIR DUQUEL ON FAIT LE PRODUIT SCALAIRE POUR MODIFIER LA
!-    LIGNE = K(M,I)
                    icbs = jdes + ibcl1 - jdl
!-    TERME COURANT DE LA LIGNE S=IEQUA A MODIFIER ( = K(S,I))
                    c8vali = zc(icai+lm)
!-    TERME COURANT DE LA COLONNE S=IEQUA A MODIFIER ( = K(I,S))
                    c8vals = zc(icas+lm)
                    do 60 i = 0, lm - 1
!-                   K(S,I) = K(S,I) -  K(S,M)   * K(M,I)
                        c8vali = c8vali - zc(icai+i)*zc(icbs+i)
!-                   K(I,S) = K(I,S) -  K(M,S)   * K(I,M)
                        c8vals = c8vals - zc(icas+i)*zc(icbi+i)
60                  continue
                    zc(icai+lm) = c8vali
                    zc(icas+lm) = c8vals
                    icd = ldiag + jequa - 1
!                    ZC(ICAI+LM) = ZC(ICAI+LM) / ZC(ICD)
                    zc(icas+lm) = zc(icas+lm)/zc(icd)
!
70              continue
80          continue
!
!-  DEVEROUILLAGE DES BLOCS SUP ET INF DEJA TRANSFORMES
!
            call jelibe(jexnum(ualf, jbloc))
            call jelibe(jexnum(ualf, jbloc+nbbloc))
90      continue
!
!     --- UTILISATION DU BLOC EN COURS DE TRANSFORMATION ---
        jl1 = max(imini,il1)
        do 140 iequa = kl1, il2
!
!        --- RECUPERATION DE L ADRESSE ET LA LONGUEUR DE LA LIGNE
!            (COLONNE) ---
!           IADIAI : ADRESSE DU TERME DIAGONAL COURANT DS LE BLOC INF
!           IADIAS : ADRESSE DU TERME DIAGONAL COURANT DS LE BLOC SUP
!           IDEI   : ADRESSE DU DEBUT DE LA LIGNE COURANTE
!           IDES   : ADRESSE DU DEBUT DE LA COLONNE COURANTE
!           IDL   : 1-ER DDL A VALEUR NON NULLE DANS LA LIGNE (COLONNE)
            ilong = hcol(iequa) - 1
!-  ADRESSE DE K(S,S) DANS LE BLOC INF
            iadiai = iaai + adia(iequa) - 1
!-  ADRESSE DU PREMIER TERME NON NUL SUR LA LIGNE S (IEQUA) DANS
!-  LE BLOC INF
            idei = iadiai - ilong
!-  ADRESSE DE K(S,S) DANS LE BLOC SUP
            iadias = iaas + adia(iequa) - 1
!-  ADRESSE DU PREMIER TERME NON NUL SUR LA COLONNE S (IEQUA)
!-  DANS LE BLOC SUP
            ides = iadias - ilong
!-  INDICE DU PREMIER TERME NON NUL SUR LA LIGNE (COLONNE) S (IEQUA)
            idl = iequa - ilong
!
!        --- UTILISATION DES LIGNES (IDL+1) A (IEQUA-1) ---
            jnmini = max(iequa-ilong,jl1)
            do 120 jequa = jnmini, iequa - 1
                jlong = hcol(jequa) - 1
!-      ADRESSE DE K(I,I) DANS LE BLOC INF
                jadiai = iaai + adia(jequa) - 1
!-      ADRESSE DU PREMIER TERME NON NUL SUR LA LIGNE I (JEQUA)
!-      DANS LE BLOC INF
                jdei = jadiai - jlong
!-      ADRESSE DE K(I,I) DANS LE BLOC SUP
                jadias = iaas + adia(jequa) - 1
!-      ADRESSE DU PREMIER TERME NON NUL SUR LA COLONNE I (JEQUA)
!-      DANS LE BLOC SUP
                jdes = jadias - jlong
!-      INDICE DU PREMIER TERME NON NUL SUR LA LIGNE (COLONNE) I
!-      (JEQUA)
                jdl = jequa - jlong
!-      INDICE DU PREMIER TERME A PARTIR DUQUEL ON VA FAIRE LES
!-      PRODUITS SCALAIRES (LIGNE I X COLONNE S) ET
!-      (LIGNE S X COLONNE I)
                ibcl1 = max(idl,jdl)
!-      LONGUEUR SUR LAQUELLE ON FAIT LES PRODUITS SCALAIRES
!-      VECTEUR LIGNE X VECTEUR COLONNE
                lm = jequa - ibcl1
!-      ADRESSE DANS LE BLOC INF DU PREMIER TERME A PARTIR DUQUEL ON
!-      FAIT LE PRODUIT SCALAIRE POUR CALCULER LE TERME LIGNE K(S,I)
                icai = idei + ibcl1 - idl
!-      ADRESSE DANS LE BLOC SUP DU PREMIER TERME A PARTIR DUQUEL ON
!-      FAIT LE PRODUIT SCALAIRE POUR CALCULER LE TERME LIGNE K(S,I)
                icbs = jdes + ibcl1 - jdl
!-      ADRESSE DANS LE BLOC SUP DU PREMIER TERME A PARTIR DUQUEL ON
!-      FAIT LE PRODUIT SCALAIRE POUR CALCULER LE TERME COLONNE
                icas = ides + ibcl1 - idl
!-      ADRESSE DANS LE BLOC INF DU PREMIER TERME A PARTIR DUQUEL ON
!-      FAIT LE PRODUIT SCALAIRE POUR CALCULER LE TERME COLONNE
                icbi = jdei + ibcl1 - jdl
!-      TERME COURANT DE LA LIGNE S (IEQUA) = K(S,I)
                c8vali = zc(icai+lm)
!-      TERME COURANT DE LA COLONNE S (IEQUA) = K(I,S)
                c8vals = zc(icas+lm)
                do 100 i = 0, lm - 1
!-                K(S,I) = K(S,I) -  K(S,M)   * K(M,I)
                    c8vali = c8vali - zc(icai+i)*zc(icbs+i)
!-                K(I,S) = K(I,S) -  K(M,S)   * K(I,M)
                    c8vals = c8vals - zc(icas+i)*zc(icbi+i)
100              continue
                zc(icai+lm) = c8vali
                zc(icas+lm) = c8vals
!
!        --- UTILISATION DE LA LIGNE IEQUA (CALCUL DU PIVOT) ---
                icd = ldiag + jequa - 1
                zc(icas+lm) = zc(icas+lm)/zc(icd)
120          continue
!
!
!        --- CALCUL DU TERME DIAGONAL ---
            lm = ilong - 1
!-      INDICE DU PREMIER TERME NON NUL DE LA LIGNE COURANTE IEQUA
!-      DS LE BLOC
            icai = iadiai - ilong
            icas = iadias - ilong
!
!        --- SAUVEGARDE DE LA COLONNE ---
!        --- NORMALISATION DE LA LIGNE ---
!-      INDICE DU PREMIER TERME DIAGONAL DANS LE VECTEUR DES TERMES
!-      DIAGONAUX NON ENCORE DECOMPOSES CORRESPONDANT AU PREMIER
!-      TERME DE LA LIGNE COURANTE A NORMALISER
            icd = ldiag + iequa - ilong - 1
            c8vali = zc(iadiai)
            do 130 i = 0, lm
                c8vali = c8vali - zc(icai+i)*zc(icas+i)
130          continue
            zc(iadiai) = c8vali
            digs(neq+iequa) = zc(iadiai)
            zc(ldiag+iequa-1) = c8vali
!
!
!           --- LE PIVOT EST-IL NUL ? ----------------------------------
            if (abs(c8vali) .le. eps) then
                npivot = iequa
                goto 9999
            endif
!
140      continue
        call jelibe(jexnum(ualf, ibloc))
        call jelibe(jexnum(ualf, ibloc+nbbloc))
150  end do
!
160  continue
9999  continue
!
!
    call jeveuo(noma19//'.REFA', 'E', vk24=refa)
    if (ilfin .eq. neq) then
        refa(8)='DECT'
    else
        refa(8)='DECP'
    endif
    call jedetr(nomdia)
!
    call jedema()
end subroutine
