subroutine tldlr8(nommat, hcol, adia, ablo, npivot,&
                  neq, nbbloc, ildeb, ilfin, eps)
    implicit none
! multi-threading optimization for MULT_FRONT
! aslint: disable=C1513
!
#include "jeveux.h"
!
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nommat
    integer :: hcol(*), adia(*), ablo(*)
    integer :: npivot, neq, nbbloc, ildeb, ilfin
    real(kind=8) :: eps
!
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! COMPIL PARAL
!     FACTORISATION DE GAUSS PAR LA VARIANTE DE CROUT SOUS FORME L*D*LT
!     D'UNE MATRICE SYMETRIQUE A COEFFICIENTS COMPLEXES
!                     LA FACTORISATION EST EN PLACE
!     ------------------------------------------------------------------
!
!     IN  NOMMAT  :    : NOM UTILISATEUR DE LA MATRICE A FACTORISER
!     IN  HCOL    : IS : HCOL DE LA MATRICE
!                        HCOL(I) RENVOIE LA HAUTEUR DE LA I-EME COLONNE
!     IN  ADIA    : IS : ADRESSE DU TERME DIAGONALE DANS SON BLOC
!                        ADIA(I) RENVOIE L'ADRESSE DE LA I-EME LIGNE
!                        DANS SON BLOC
!     IN  ABLO    :    : POINTEUR DE BLOC
!                        ABLO(I+1) RENVOIE LE NO DE LA DERNIERE
!                        LIGNE DU I-EME BLOC
!
!     VAR PIVOT   : IS :
!                 : SORTIE : NPIVOT = 0 ==> R.A.S.
!                 :  NPIVOT > 0 ==> MATRICE SINGULIERE
!                                   POUR L'EQUATION DE NUMERO NPIVOT
!                 :  NPIVOT < 0 ==> -NPIVOT TERMES DIAGONAUX < 0
!
!     IN  NEQ     : IS : NOMBRE TOTAL D'EQUATION
!     IN  NBBLOC  : IS : NOMBRE DE BLOC DE LA MATRICE
!     IN  ILDEB   : IS : NUMERO DE LA LIGNE DE DEPART DE FACTORISATION
!     IN  ILFIN   : IS : NUMERO DE LA LIGNE DE FIN    DE FACTORISITION
!
!     ------------------------------------------------------------------
!
!     CREATION DE DEUX OBJETS DE TRAVAIL (SUR LA VOLATILE)
!        1)  UN TABLEAU POUR LA DIAGONALE
!        2)  UN TABLEAU POUR LA COLONNE COURANTE
!
!     --- RAPPEL SOMMAIRE DE L'ALGORITHME ------------------------------
!
!     POUR S = 2,3, ... ,N
!     !  POUR I = 1,2, ... ,S-1
!     !  !  POUR M = 1,2, ... ,I-1
!     !  !  !  K(S,I) = K(S,I) - K(S,M)*K(M,I) % MODIFIE   LA LIGNE
!     !  !  !  K(I,S) = K(I,S) - K(I,M)*K(M,S) % MODIFIE   LA COLONNE
!     !  !  FIN_POUR
!     !  !  K(S,I) = K(S,I)/K(I,I)           % NORMALISATION DE LA LIGNE
!     !  FIN_POUR
!     !  POUR M = 1,2, ... ,S-1
!     !  !  K(S,S) = K(S,S) - K(S,M) * K(M,S)  % MODIFICATION DU PIVOT
!     !  FIN_POUR
!     FIN_POUR
!
!     ------------------------------------------------------------------
!
!     REFERENCE (HISTORIQUE) :
!     (1) P.D. CROUT,
!         A SHORT METHOD FOR EVALUATING DETERMINANTS AND SOLVING SYSTEMS
!         OF LINEAR EQUATIONS WITH REAL OR COMPLEX COEFFICIENTS.
!         AIEE TRANSACTION VOL 60, PP 1235-1240  (1941)
!
!     ------------------------------------------------------------------
!
    integer :: ifm, niv, ldiag, ltrav
    integer :: ibloc, il1, il2, iaa, il, kl1
    integer :: imini, iequa, i, jblmin, jbloc, jl1, jl2
    integer :: iab, ilong, iadia, ide, idl
    integer :: jnmini, jequa, jlong, jadia, jde, jdl, ibcl1
    integer :: lm, ica, icb, icd
!
!     ----- CNJ --------------------------------------------------------
!     ----- CNJ --------------------------------------------------------
!
    real(kind=8) :: val
    character(len=19) :: noma19
    character(len=24) :: nomdia, ualf, nomtra
    character(len=24), pointer :: refa(:) => null()
    real(kind=8), pointer :: digs(:) => null()
!
!     ------------------------------------------------------------------
!
    data ualf/'                   .UALF'/
    data nomdia/'                   .&VDI'/
    data nomtra/'                   .&TRA'/
!
!     ------------------------------------------------------------------
!
    call infniv(ifm, niv)
    call jemarq()
!
!     --- INITIALISATIONS ----------------------------------------------
!
    noma19 = nommat
!
    ualf(1:19) = nommat
    nomdia(1:19) = nommat
    nomtra(1:19) = nommat
!
    npivot = 0
    call jeveuo(noma19//'.DIGS', 'E', vr=digs)
!
!     --- CREATION D'UN TABLEAU POUR STOCKER LA DIAGONALE
!
    call wkvect(nomdia, 'V V R', neq, ldiag)
!
!     --- CREATION D'UN TABLEAU INTERMEDIAIRE (D'UNE COLONNE)
!
    call wkvect(nomtra, 'V V R', neq, ltrav)
!
!     ------- BOUCLE SUR LES BLOCS A TRANSFORMER -----------------------
    do 160 ibloc = 1, nbbloc
!
        il1 = ablo(ibloc) + 1
        il2 = ablo(ibloc+1)
!
        if (il2 .lt. ildeb) then
!        --- C'EST TROP TOT : MAIS ON REMPLIT LA DIAGONALE -------------
            call jeveuo(jexnum(ualf, ibloc), 'L', iaa)
            do 10 il = il1, il2
                zr(ldiag+il-1) = zr(iaa+adia(il)-1)
10          continue
            call jelibe(jexnum(ualf, ibloc))
            goto 160
        else if (il1.gt.ilfin) then
!        --- C'EST FINI ------------------------------------------------
            goto 170
        else
            call jeveuo(jexnum(ualf, ibloc), 'E', iaa)
            if (il1 .lt. ildeb) then
                kl1 = ildeb
                do 20 il = il1, kl1 - 1
                    zr(ldiag+il-1) = zr(iaa+adia(il)-1)
20              continue
            else
                kl1 = il1
            endif
            if (il2 .gt. ilfin) il2 = ilfin
        endif
!
!     --- RECHERCHE DE LA PLUS PETITE EQUATION EN RELATION AVEC --------
!     --- UNE EQUATION DES LIGNES EFFECTIVES DU BLOC COURANT -----------
!
        imini = il2 - 1
        do 30 iequa = kl1, il2
            imini = min(iequa-hcol(iequa),imini)
30      continue
        imini = imini + 1
!
!     --- RECHERCHE DU BLOC D'APPARTENANCE DE L'EQUATION IMINI ---------
!
        do 40 i = 1, ibloc
            jblmin = i
            if (ablo(1+i) .ge. imini) goto 50
40      continue
50      continue
!
!     --- BOUCLE  SUR  LES  BLOCS  DEJA  TRANSFORMES -------------------
!
        do 90 jbloc = jblmin, ibloc - 1
!
            jl1 = max(imini,ablo(jbloc)+1)
            jl2 = ablo(jbloc+1)
            call jeveuo(jexnum(ualf, jbloc), 'L', iab)
!
!MIC$ DO ALL SHARED (ADIA, HCOL, IAA, IAB, IL2, JL1, JL2, KL1)
!MIC$*       SHARED (ZR)
!MIC$*       PRIVATE (I, IADIA, IBCL1, ICA, ICB, IDE, IDL, IEQUA)
!MIC$*       PRIVATE (ILONG, JADIA, JDE, JDL, JEQUA, JLONG, JNMINI)
!MIC$*       PRIVATE (LM, VAL)
!
            do 80 iequa = kl1, il2
!
!           --- RECUPERATION DE L'ADRESSE ET LA LONGUEUR DE LA LIGNE ---
!
                ilong = hcol(iequa) - 1
                iadia = iaa + adia(iequa) - 1
                ide = iadia - ilong
                idl = iequa - ilong
!
!           --- UTILISATION DES LIGNES (IDL+1) A (JL2) -----------------
!
                jnmini = max(idl,jl1)
                do 70 jequa = jnmini, jl2
                    jlong = hcol(jequa) - 1
                    jadia = iab + adia(jequa) - 1
                    jde = jadia - jlong
                    jdl = jequa - jlong
                    ibcl1 = max(idl,jdl)
                    lm = jequa - ibcl1
                    ica = ide + ibcl1 - idl
                    icb = jde + ibcl1 - jdl
                    val = zr(ica+lm)
                    do 60 i = 0, lm - 1
                        val = val - zr(ica+i)*zr(icb+i)
60                  continue
                    zr(ica+lm) = val
70              continue
80          continue
            call jelibe(jexnum(ualf, jbloc))
90      continue
!
!     --- UTILISATION DU BLOC EN COURS DE TRANSFORMATION ---------------
!
        jl1 = max(imini,il1)
        do 150 iequa = kl1, il2
!
!        --- RECUPERATION DE L ADRESSE ET LA LONGUEUR DE LA LIGNE ------
!           IADIA : ADRESSE DU TERME DIAGONAL COURANT
!           IDE   : ADRESSE DU DEBUT DE LA LIGNE COURANTE
!           IDL   : 1-ER DDL A VALEUR NON NULLE DANS LA LIGNE
!
            ilong = hcol(iequa) - 1
            iadia = iaa + adia(iequa) - 1
            ide = iadia - ilong
            idl = iequa - ilong
!
!        --- UTILISATION DES LIGNES (IDL+1) A (IEQUA-1) ---
!
            jnmini = max(iequa-ilong,jl1)
            do 110 jequa = jnmini, iequa - 1
                jlong = hcol(jequa) - 1
                jadia = iaa + adia(jequa) - 1
                jde = jadia - jlong
                jdl = jequa - jlong
                ibcl1 = max(idl,jdl)
                lm = jequa - ibcl1
                ica = ide + ibcl1 - idl
                icb = jde + ibcl1 - jdl
                val = zr(ica+lm)
                do 100 i = 0, lm - 1
                    val = val - zr(ica+i)*zr(icb+i)
100              continue
                zr(ica+lm) = val
110          continue
!
!        --- UTILISATION DE LA LIGNE IEQUA (CALCUL DU PIVOT) -----------
!
            lm = ilong - 1
            ica = iadia - ilong
!
!        --- SAUVEGARDE DE LA COLONNE ET NORMALISATION DE LA LIGNE -----
!
            icd = ldiag + iequa - ilong - 1
            do 130 i = 0, lm
                zr(ltrav+i) = zr(ica+i)
                zr(ica+i) = zr(ica+i)/zr(icd+i)
130          continue
!
!
!        --- CALCUL DU TERME DIAGONAL ----------------------------------
!
            val = zr(iadia)
            do 140 i = 0, lm
                val = val - zr(ica+i)*zr(ltrav+i)
140          continue
            zr(iadia) = val
            digs(neq+iequa) = zr(iadia)
            zr(ldiag+iequa-1) = val
!
!           --- LE PIVOT EST-IL NUL ? ----------------------------------
            if (abs(val) .le. eps) then
                npivot = iequa
                goto 9999
            endif
!
!           --- ON COMPTE LES PIVOTS NEGATIFS --------------------------
            if (val .lt. 0.d0) then
                npivot = npivot - 1
            endif
!
150      continue
        call jelibe(jexnum(ualf, ibloc))
        if (niv .eq. 2) then
            write (ifm,*) '>>> FACTORISATION (LDLT_R8):',' FIN DU BLOC ',&
     &      ibloc,' SUR ',nbbloc,' SOIT DU DDL ',il1,' AU  DDL ',il2,&
     &      ' INCLUS'
        endif
160  end do
170  continue
9999  continue
!
    call jeveuo(noma19//'.REFA', 'E', vk24=refa)
    if (ilfin .eq. neq) then
        refa(8)='DECT'
    else
        refa(8)='DECP'
    endif
    call jedetr(nomdia)
    call jedetr(nomtra)
!
    call jedema()
end subroutine
