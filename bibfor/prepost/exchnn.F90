subroutine exchnn(descn, numn, tcmp, nbc, tvale,&
                  tnueq, b, valcmp, taber)
    implicit none
!
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
#include "asterc/r8vide.h"
#include "asterfort/iposdg.h"
    integer :: descn(*), tcmp(*), nbc, taber(*), numn, tnueq(*)
    real(kind=8) :: tvale(*), valcmp(*)
    logical(kind=1) :: b
!
!**********************************************************************
!
!     OPERATION REALISEE
!     ------------------
!
!       EXTRACTION DES VALEURS D' UN ENSEMBLE DE COMPOSANTES SUR UN
!       NOEUDS DANS UN CHAMP_NO
!
!     ARGUMENTS EN ENTREE
!     -------------------
!
!       DESCN : PARTIE DU PRNO ASSOCIE AU NOEUD TRAITE
!
!                  (1) --> ADRESSE DANS TVALE DE LA PARTIE
!                          ASSOCIEE AU NOEUD
!
!                  (2) --> NBR DE CMP SUR CE NOEUD
!
!                  (3),(4),.. --> LES ENTIERS CODES
!
!       NUMN  : NUMERO DU NOEUD A TRAITER
!               QUAND LE CHAMP EST A REPRESENTATION NON CONSTANTE
!               CET INFORMATION EST REDONDANTE AVEC DESCN, DANS CE
!               CAS NUMN VAUT ZERO
!
!       TCMP  : TABLE DES NUMERO DE COMPOSANTES MISE EN JEU
!
!       NBC   : NBR DE COMPOSANTES MISES EN JEU
!
!       TVALE : TABLE DES VALEURS DES CMP DE TOUT LE CHAMP_NO
!
!       TNUEQ : TABLE D'INDIRECTION (JACOT) '.NUEQ'
!
!       B     : .TRUE. LE CHAMP EST PROF_CHNO (FALSE SINON).
!
!     ARGUMENTS EN SORTIE
!     -------------------
!
!       VALCMP : TABLE DES VALEURS DES CMP MISE EN JEU SUR LE NOEUD
!
!**********************************************************************
!
!
    integer :: adr, i, poscmp, nbcn
!
!-----------------------------------------------------------------------
    integer :: iiad
!-----------------------------------------------------------------------
    adr = descn(1)
    nbcn = -descn(2)
!
    if (numn .gt. 0) then
!
        adr = 1 + nbcn* (numn-1)
!
    endif
!
    do 10,i = 1,nbc,1
!
    poscmp = iposdg(descn(3),tcmp(i))
!
    if (poscmp .gt. 0) then
!
        if (b) then
!
            iiad = tnueq(adr+poscmp-1)
!
        else
!
            iiad = adr + poscmp - 1
!
        endif
!
        valcmp(i) = tvale(iiad)
!
        taber(i) = 1
!
    else
!
        valcmp(i) = r8vide()
!
        taber(i) = 0
!
    endif
!
    10 end do
!
end subroutine
