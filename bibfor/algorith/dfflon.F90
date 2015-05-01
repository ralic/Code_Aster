subroutine dfflon(geom, nonoff, nomnoe, inoff, nbnoff,&
                  typfon, d)
!
    implicit none
!
#include "jeveux.h"
!
#include "asterfort/dis2no.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jexnom.h"
    real(kind=8) :: geom(*), d
    integer :: inoff, nbnoff
    character(len=8) :: nonoff(*), typfon
    character(len=24) :: nomnoe
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! FONCTION REALISEE (OPERATEURS DEFI_FOND_FISS) :
!
!      RETOURNE UNE ESTIMATION DE LA LONGUEUR DES SEGMENTS DU FOND DE
!      FISSURE EN UN NOEUD DU FOND
!      (UNIQUEMENT EN 3D)
!
! IN
!   GEOM    : VALEURS DES COORDONNEES DU MAILLAGE
!   NONOFO  : NOMS DES NOEUDS DU FOND
!   NOMNOE  : OBJET '.NOMNOE' DU MAILLAGE
!   INOFF   : INCIDE LOCAL DU NOEUD DE LA BASE DEMANDE
!   NBNOFF  : NOMBRE DE NOEUDS DU FOND DE FISSURE
!   TYPFON  : TYPE DE FOND (OUVERT/FERME/INF/SUP)
!
! OUT
!   D   : LONGUEUR CARACTERISTIQUES DES SEGMENTS DU FOND AUTOUT DU NOEUD
!
!-----------------------------------------------------------------------
!
!
    real(kind=8) :: dij, dih
    integer :: nunoi, nunoj, nunoh
!
!-----------------------------------------------------------------------
!
    call jemarq()
!
!     REMARQUE GENERALE : ON POURRAIT TROUVER UN ALGO PLUS ELEGANT,
!                         MAIS ON PERDRAIT EN LISIBILITE
!
!     NUNOI : NUMERO DU NOEUD COURANT
!     NUNOJ : NUMERO DU NOEUD APRES
!     NUNOH : NUMERO DU NOEUD AVANT
!
!     CAS PARTICULIER DU PREMIER NOEUD DU FOND DE FISSURE
    if (inoff .eq. 1) then
!
        call jenonu(jexnom(nomnoe, nonoff(inoff )), nunoi)
        call jenonu(jexnom(nomnoe, nonoff(inoff+1)), nunoj)
!
        dij = dis2no(geom,nunoi,nunoj)
        d = dij
!
        if (typfon .eq. 'FERME') then
!         ATTENTION, LE DERNIER NOEUD (EN POSITION NBNOFF) = LE 1ER
            call jenonu(jexnom(nomnoe, nonoff(nbnoff )), nunoi)
            call jenonu(jexnom(nomnoe, nonoff(nbnoff-1)), nunoh)
            dih = dis2no(geom,nunoi,nunoh)
            d = min(dij,dih)
        endif
!
!     CAS PARTICULIER DU DENIER NOEUD DU FOND DE FISSURE
    else if (inoff.eq.nbnoff) then
!
        call jenonu(jexnom(nomnoe, nonoff(inoff )), nunoi)
        call jenonu(jexnom(nomnoe, nonoff(inoff-1)), nunoh)
!
        dih = dis2no(geom,nunoi,nunoh)
        d = dih
!
        if (typfon .eq. 'FERME') then
!         ATTENTION, LE PREMIER NOEUD = LE DERNIER
            call jenonu(jexnom(nomnoe, nonoff(1)), nunoi)
            call jenonu(jexnom(nomnoe, nonoff(2)), nunoj)
            dij = dis2no(geom,nunoi,nunoj)
            d = min(dij,dih)
        endif
!
!     CAS GENERAL
    else
!
        call jenonu(jexnom(nomnoe, nonoff(inoff-1)), nunoh)
        call jenonu(jexnom(nomnoe, nonoff(inoff )), nunoi)
        call jenonu(jexnom(nomnoe, nonoff(inoff+1)), nunoj)
!
        dih = dis2no(geom,nunoi,nunoh)
        dij = dis2no(geom,nunoi,nunoj)
        d = min(dij,dih)
!
    endif
!
    call jedema()
end subroutine
