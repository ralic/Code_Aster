subroutine exchem(modloc, tcmp, nbc, nbsp, tvale,&
                  valcmp, taberr)
    implicit none
!
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
!
#include "jeveux.h"
!
#include "asterc/r8vide.h"
#include "asterfort/iposdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/ncpact.h"
#include "asterfort/wkvect.h"
    integer :: modloc(*), tcmp(*), nbc, taberr(*), nbsp
    real(kind=8) :: tvale(*), valcmp(*)
!
!*********************************************************************
!
!     OPERATION REALISEE
!     ------------------
!
!       EXTRACTION DES VALEURS D' UN ENSEMBLE DE COMPOSANTES SUR LES
!       NOEUDS D' UNE MAILLE DANS UN CHAM_ELEM
!
!     ARGUMENTS EN ENTREES
!     --------------------
!
!       MODLOC : VECTEUR MODE LOCALE DU TYPE D' ELEMENT DEFINI
!                SUR LA MAILLE
!
!                  (1) --> CODE (ICI TJS 3 : CAS DU CHAM_ELEM)
!
!                  (2) --> NUMERO DE LA GRANDEUR
!
!                  (3) --> NBR DE SCALAIRES UTILISES POUR DECRIRE
!                          LE CHAM_ELEM DE LA GRANDEUR SUR LA MAILLE
!
!                  (4) --> ACCES AU NBR DE POINTS UTILISES POUR LA
!                          DESCRIPTION
!
!                  (5) --> DEBUT DES DESCRIPTEURS PAR ENTIERS CODES
!
!
!       TCMP   : TABLE DES NUMEROS DES CMP ACTIVES POUR L' EXTRACTION
!       NBC    : NBR DE CMP ACTIVES
!       NBSP   : NBR DE SOUS-POINTS
!       TAVLE  : TABLE DES VALEURS DU CHAM_ELEM SUR LA MAILLE
!
!     ARGUMENTS EN SORTIE
!     -------------------
!
!       VALCMP : TABLE DES VALEURS DES CMP EXTRAITES
!
!*********************************************************************
!
    integer :: gd, nbpt, nbnmai, acpact, nbec, adesgd
    integer :: nbrcpa, adrnd, asgtnd, aposcp, poscmp, i, j, k
!
!   FONCTIONS EXTERNES
!   ------------------
!
!
!   -------------------------
!
!
!======================================================================
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call jemarq()
    gd = modloc(2)
!
    call jeveuo(jexnum('&CATA.GD.DESCRIGD', gd), 'L', adesgd)
!
    nbec = zi(adesgd + 3-1)
    nbpt = modloc(4)
!
!     /* PAR HYP. D' APPEL : LE CHAMP EST REPRESENTE AUX NOEUDS */
!     /* DONC, MODLOC(4) < 0                                    */
!
    call ncpact(modloc(5), nbec, nbrcpa)
!
    if (nbpt .gt. 10000) then
!
!        /* CAS D' UNE REPRESENTATION VARIANT AVEC  LES NOEUDS */
!
        nbnmai = nbpt - 10000
!
        call wkvect('&&EXCHEM.NBRCMPACTIVE', 'V V I', nbnmai, acpact)
        call wkvect('&&EXCHEM.ADRSGTNOEUD', 'V V I', nbnmai, asgtnd)
        call wkvect('&&EXCHEM.POSCMP', 'V V I', nbc*nbnmai, aposcp)
!
        zi(asgtnd + 1-1) = 1
        zi(acpact + 1-1) = nbrcpa
!
        do 5, k = 1, nbc, 1
        i=1
        zi(aposcp+k-1) = iposdg(modloc(5+(i-1)*nbec),tcmp(k))
!
 5      continue
!
        do 10, i = 2, nbnmai, 1
!
        call ncpact(modloc(5 + nbec*(i-1)), nbec, nbrcpa)
!
        zi(asgtnd + i-1) = zi(asgtnd + i-1-1) + nbrcpa*nbsp
        zi(acpact + i-1) = nbrcpa
!
        adrnd = (i-1)*nbc
!
        do 11, k = 1, nbc, 1
!
        zi(aposcp+adrnd+k-1)=iposdg(modloc(5+(i-1)*nbec),tcmp(&
                k))
!
11      continue
!
10      continue
!
    else
!
!        /* CAS D' UNE REPRESENTATION CONSTANTES SUR LES NOEUDS */
!
        nbnmai = nbpt
!
        call wkvect('&&EXCHEM.NBRCMPACTIVE', 'V V I', nbnmai, acpact)
        call wkvect('&&EXCHEM.ADRSGTNOEUD', 'V V I', nbnmai, asgtnd)
        call wkvect('&&EXCHEM.POSCMP', 'V V I', nbc*nbnmai, aposcp)
!
        do 20, i = 1, nbnmai, 1
!
        zi(asgtnd + i-1) = (i-1)*nbrcpa*nbsp + 1
        zi(acpact + i-1) = nbrcpa
!
        adrnd = (i-1)*nbc
!
        do 21, k = 1, nbc, 1
!
        zi(aposcp + adrnd + k-1) = iposdg(modloc(5),tcmp(k))
!
21      continue
!
20      continue
!
    endif
!
    do 100, i = 1, nbnmai, 1
!
    adrnd = zi(asgtnd + i-1)
    nbrcpa = zi(acpact + i-1)
!
    do 110, j = 1, nbsp, 1
!
    do 120, k = 1, nbc, 1
!
    poscmp = zi(aposcp + (i-1)*nbc + k-1)
!
    if (poscmp .gt. 0) then
!
        valcmp(((i-1)*nbsp + j-1)*nbc+k) = tvale( adrnd + ( j-1)*nbrcpa + poscmp-1)
!
        taberr(k) = 1
!
    else
!
        valcmp(((i-1)*nbsp + j-1)*nbc+k) = r8vide()
!
        taberr(k) = 0
!
    endif
!
120  continue
!
110  continue
!
    100 end do
!
    call jedetr('&&EXCHEM.NBRCMPACTIVE')
    call jedetr('&&EXCHEM.ADRSGTNOEUD')
    call jedetr('&&EXCHEM.POSCMP')
!
    call jedema()
end subroutine
