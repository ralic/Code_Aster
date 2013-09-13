subroutine rldur8(nommat, hcol, adia, ablo, neq,&
                  nbbloc, xsol, nbsol)
    implicit none
!
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
    integer :: neq
    integer :: hcol(*), adia(*), ablo(*)
    real(kind=8) :: xsol (neq, *)
!
!     ------------------------------------------------------------------
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
!     RESOLUTION DU SYSTEME A COEFFICIENTS REELS:  A * X = B
!     LA MATRICE EST NON SYMETRIQUE ET A ETE FACTORISEE SOUS FORME L*D*U
!     LA RESOLUTION EST EN PLACE
!
!     ON PEUT RESOUDRE SUR UNE SOUS-MATRICE DE A :
!     ON PREND LES NEQ PREMIERES LIGNES ET COLONNES (NEQ PEUT ETRE
!     INFERIEUR A LA DIMENSION DE LA MATRICE).
!
!     ON PEUT RESOUDRE NBSOL SYSTEMES D'UN COUP A CONDITION
!     QUE LES VECETURS SOIENT CONSECUTIFS EN MEMOIRE :
!     XSOL EST UN VECTEUR DE NBSOL*NEQ REELS
!     ------------------------------------------------------------------
!
! IN  NOMMAT  :    : NOM UTILISATEUR DE LA MATRICE A FACTORISER
! IN  HCOL    : IS : HCOL DE LA MATRICE
!             HCOL(I) RENVOIE LA HAUTEUR DE LA I-EME COLONNE
! IN  ADIA    : IS : ADRESSE DU TERME DIAGONALE DANS SON BLOC
!             ADIA(I) RENVOIE L'ADRESSE DE LA I-EME LIGNE DANS SON BLOC
! IN  ABLO    :  :   POINTEUR DE BLOC
!             ABLO(I+1) RENVOIE LE NO DE LA DERNIERE LIGNE DU I-EME BLOC
!
! IN  NEQ     : IS : NOMBRE D'EQUATIONS PRISES EN COMPTE
!                    C'EST AUSSI LA DIMENSION DES VECTEURS XSOL.
! IN  NBBLOC  : IS : NOMBRE DE BLOC DE LA MATRICE
! VAR XSOL    : R8 : EN ENTREE LES SECONDS MEMBRES
!                    EN ENTREE LES SOLUTIONS
! IN  NBSOL   : IS : NOMBRE DE SOLUTIONS / SECONDS MEMBRES
!     ------------------------------------------------------------------
!  LES TABLEAUX RELATIFS AUX BLOCS ONT ETE CONSTRUITS A PARTIR
!  DES NBBLOCS PREMIERS BLOCS (I.E LES BLOCS SUP)
!  MAIS ILS RESTENT VALABLES POUR LES NBBLOCS BLOCS SUIVANTS
!   (I.E LES BLOCS INF) PUISQUE LES POFILS SUP ET INF SONT IDENTIQUES
!     ------------------------------------------------------------------
!
!     --- RAPPEL SOMMAIRE DE L'ALGORITHME ------------------------------
!
!     POUR I = 1,2, ... ,N
!     !  ACC = 0
!     !  POUR K = 1,2, ... ,I-1
!     !  !  ACC = ACC +  K(I,K)* X(K)
!     !  FIN_POUR
!     !  X(I) = X(I)-ACC/K(I,I)
!     FIN_POUR
!
!     POUR I = N,N-1,... ,1
!     !  ACC = 0
!     !  POUR K = I+1, ... ,N
!     !  !  ACC = ACC +  K(K,I)* X(K)
!     !  FIN_POUR
!     !  X(I) = X(I) - ACC
!     FIN_POUR
!     ------------------------------------------------------------------
!
!     REFERENCE (HISTORIQUE) :
!     (1) P.D. CROUT,
!         A SHORT METHOD FOR EVALUATING DETERMINANTS AND SOLVING SYSTEMS
!         OF LINEAR EQUATIONS WITH REAL OR COMPLEX COEFFICIENTS.
!         AIEE TRANSACTION VOL 60, PP 1235-1240  (1941)
!     ------------------------------------------------------------------
!
!
!
!     ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    real(kind=8) :: r8val
    character(len=24) :: nomdia, ualf
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, iadia, ibloc, ide, iderbl, iequa, ilong
    integer :: isol, ixx, ldiag, lmat, nbbloc, nbsol
!
!-----------------------------------------------------------------------
    data  ualf  /'                   .UALF'/
    data  nomdia/'                   .&VDI'/
!     ------------------------------------------------------------------
!
    call jemarq()
    ualf(1:19) = nommat
    nomdia(1:19) = nommat
!
!     --- CREATION/RAPPEL D'UN TABLEAU POUR STOCKER LA DIAGONALE -------
    call wkvect(nomdia, 'V V R', neq, ldiag)
!
!     ------------------------------------------------------------------
!     --- PREMIERE  PARTIE : RESOLUTION DESCENDANTE ---
!     --- ET REMPLI LE TABLEAU DIAGONAL POUR L'ETAPE SUIVANTE
!     --- DANS L'ETAPE DE RESOLUTION DESCENDANTE, SEULE EST
!     --- CONCERNEE LA PARTIE INFERIEURE DE LA MATRICE
!
    do 100 ibloc = 1, nbbloc
        if (ablo(ibloc) .ge. neq) goto 101
!
!        -- IDERBL EST LE NUMERO DU DERNIER BLOC A TRAITER:
!
!-     ACCES EN LECTURE AU BLOC INF COURANT
!
        iderbl=ibloc+nbbloc
        call jeveuo(jexnum(ualf, ibloc+nbbloc), 'L', lmat)
        do 110 iequa = ablo(ibloc)+1, ablo(ibloc+1)
            if (iequa .gt. neq) goto 111
            ilong = hcol (iequa)
!-  ADRESSE DANS LE BLOC INF DU TERME DIAGONAL CORRESPONDANT A
!-  L'EQUATION IEQUA
            iadia = lmat + adia(iequa) - 1
!-  ADRESSE DANS LE BLOC INF DU PREMIER TERME NON NUL SUR LA
!-  LIGNE IEQUA
            ide = iadia - ilong + 1
!-  INDICE DU PREMIER TERME NON NUL SUR LA LIGNE IEQUA
            ixx = iequa - ilong + 1
            zr(ldiag+iequa-1) = zr(iadia)
            do 120 isol = 1, nbsol
                r8val = 0
                do 130 i = 0, ilong - 2
                    r8val = r8val + xsol(ixx+i,isol) * zr(ide+i)
130              continue
                xsol(iequa,isol) = xsol(iequa,isol) - r8val
                xsol(iequa,isol) = xsol(iequa,isol)/zr(ldiag+iequa-1)
120          continue
110      continue
111      continue
        call jelibe(jexnum(ualf, ibloc+nbbloc))
100  end do
101  continue
!
!
!     --- DEUXIEME  PARTIE : RESOLUTION REMONTANTE ---
!     --- UTILISATION DES BLOCS SUP DE LA MATRICE   ---
    iderbl = nbbloc + nbbloc
    do 300 ibloc = iderbl, nbbloc+1, -1
        call jeveuo(jexnum(ualf, ibloc-nbbloc), 'L', lmat)
        do 310 iequa = ablo(ibloc-nbbloc+1), ablo(ibloc-nbbloc)+1, -1
            if (iequa .gt. neq) goto 310
            ilong = hcol(iequa)
!-  ADRESSE DANS LE BLOC SUP DU TERME DIAGONAL CORRESPONDANT A
!-  L'EQUATION IEQUA
            iadia = lmat + adia(iequa) - 1
!-  ADRESSE DANS LE BLOC SUP DU PREMIER TERME NON NUL SUR LA
!-  COLONNE IEQUA
            ide = iadia - ilong + 1
!-  INDICE DU PREMIER TERME NON NUL SUR LA COLONNE IEQUA
            ixx = iequa - ilong + 1
            do 320 isol = 1, nbsol
                r8val = - xsol(iequa,isol)
                if (r8val .ne. 0) then
                    do 330 i = 0, ilong - 2
                        xsol(ixx+i,isol)=xsol(ixx+i,isol)+r8val*zr(&
                        ide+i)
!                    XSOL(IXX+I,ISOL)=XSOL(IXX+I,ISOL)+R8VAL*ZR(IDE+I)/
!     1              ZR(LDIAG+IXX+I-1)
330                  continue
                endif
320          continue
310      continue
        call jelibe(jexnum(ualf, ibloc-nbbloc))
300  end do
!
    call jedetr(nomdia)
!
    call jedema()
end subroutine
