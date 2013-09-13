subroutine mefcen(caelem, iequiv, nbcyl, nbz, irot,&
                  numnog, nbnog, nummag, numgrp, coor,&
                  cent, req, xint, yint, zint,&
                  rint, nbgrp)
    implicit none
!
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
    integer :: iequiv, nbcyl, numnog(*), nbnog(*), nummag(*)
    integer :: numgrp(*), irot(3), nbgrp, nbz
    real(kind=8) :: xint(nbcyl), yint(nbcyl), zint(nbz, nbgrp), coor(*)
    real(kind=8) :: cent(2*nbcyl), req(nbgrp), rint(nbcyl)
    character(len=19) :: caelem
! ----------------------------------------------------------------------
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
! ----------------------------------------------------------------------
!     RECUPERATION DANS LES CARTES ELEMENTS DES RAYONS DANS LE CAS OU
!     IL N Y A PAS DE GROUPES D EQUIVALENCE ET DANS LES DONNEES DANS
!     LE CAS OU IL Y A DES GROUPES D EQUIVALENCE - RECUPERATION DES
!     COORDONNEES DES CENTRES DES CYLINDRES REELS DANS LES DONNEES
!     OPERATEUR APPELANT : OP0144 , FLUST3
! ----------------------------------------------------------------------
!     OPTION DE CALCUL   : CALC_FLUI_STRU , CALCUL DES PARAMETRES DE
!     COUPLAGE FLUIDE-STRUCTURE POUR UNE CONFIGURATION DE TYPE "FAISCEAU
!     DE TUBES SOUS ECOULEMENT AXIAL"
! ----------------------------------------------------------------------
! IN  : CAELEM : NOM DU CONCEPT DE TYPE CARA_ELEM
! IN  : IEQUIV : INDICE D EXISTANCE DES GROUPES D EQUIVALENCE
! IN  : NBCYL  : NOMBRE DE CYLINDRES REELS
! IN  : NBZ    : NOMBRE DE NOEUDS DE LA DISCRETISATION AXIALE
! IN  : IROT   : INDICE DE PERMUTATION CIRCULAIRE DU CHANGEMENT DE
!                REPERE
! IN  : NUMNOG : TABLEAU DES ADRESSES DES NUMEROS DES NOEUDS DES
!                CYLINDRES
! IN  : NBNOG  : TABLEAU DU NOMBRE DE NOEUDS DE CHAQUE CYLINDRE
! IN  : NUMMAG : TABLEAU DES ADRESSES DES NUMEROS DES MAILLES DES
!                CYLINDRES
! IN  : NUMGRP : INDICES DES GROUPES D EQUIVALENCE
! IN  : COOR   : COORDONNEES DES NOEUDS DU MAILLAGE
! IN  : CENT   : COORDONNEES DES CENTRES DONNES DANS LA COMMANDE
! IN  : REQ    : RAYONS DES CYLINDRES DONNES DANS LA COMMANDE
! OUT : XINT   : COORDONNEES 'X' DANS LE REPERE AXIAL DES CENTRES DES
!                CYLINDRES
! OUT : YINT   : COORDONNEES 'Y' DANS LE REPERE AXIAL DES CENTRES DES
!                CYLINDRES
! OUT : ZINT   : COORDONNEES 'Z' DANS LE REPERE AXIAL DES NOEUDS DES
!                CYLINDRES
! OUT : RINT   : RAYONS DES CYLINDRES
! ----------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: i, j, iret, rangr1
    character(len=19) :: carte, carsd
    character(len=3) :: note
!     ------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
    integer :: iad, icesc, icesd, icesl, icesv, icmp, idesc
    integer :: npmax, numma, numno1, numno2
    real(kind=8) :: epsit
!-----------------------------------------------------------------------
    call jemarq()
    epsit = 1.d-5
!
!
! --- COORDONNEES DES CENTRES DES CYLINDRES
! --- CAS OU IL N Y A PAS DE GROUPES D EQUIVALENCE
!
    if (iequiv .eq. 0) then
        do 20 i = 1, nbcyl
            numno1 = zi(numnog(i))
            xint(i) = coor((numno1-1)*3 + irot(1))
            yint(i) = coor((numno1-1)*3 + irot(2))
            zint(1,numgrp(i)) = coor((numno1-1)*3 + irot(3))
            do 10 j = 2, nbnog(i)
                numno2 = zi(numnog(i)+j-1)
                if (abs( coor( (numno1-1)*3 + irot(1)) - coor((numno2-1) *3 + irot(1) ) )&
                    .gt. epsit .or.&
                    abs( coor( (numno1-1)*3 + irot(2)) - coor((numno2-1)*3 + irot(2) ) )&
                    .gt. epsit) then
                    write(note(1:3),'(I3.3)') i
                    call utmess('F', 'ALGELINE_73', sk=note)
                endif
                zint(j,numgrp(i)) = coor((numno2-1)*3 + irot(3))
10          continue
20      continue
!
!
! --- COORDONNEES DES CENTRES DES CYLINDRES
! --- CAS OU IL Y A DES GROUPES D EQUIVALENCE
!
    else if (iequiv.eq.1) then
        do 40 i = 1, nbcyl
            xint(i) = cent(2*(i-1)+1)
            yint(i) = cent(2*(i-1)+2)
            do 30 j = 1, nbnog(numgrp(i))
                numno2 = zi(numnog(numgrp(i))+j-1)
                zint(j,numgrp(i)) = coor((numno2-1)*3 + irot(3))
30          continue
40      continue
    endif
!
!
! --- RAYONS DES CYLINDRES
! --- CAS OU IL Y A DES GROUPES D EQUIVALENCE
!
    if (iequiv .eq. 1) then
        do 50 i = 1, nbcyl
            rint(i) = req(numgrp(i))
50      continue
!
! --- RAYONS DES CYLINDRES
! --- CAS OU IL N Y A PAS DES GROUPES D EQUIVALENCE
!
    else if (iequiv.eq.0) then
!CC ON RECUPERE LA CARTE ET ON LA TRANSFORME EN CHAMELEM_S
        carte=caelem(1:8)//'.CARGEOPO'
        carsd='&&MEFCEN.CARGEOPO'
        call carces(carte, 'ELEM', ' ', 'G', carsd,&
                    'A', iret)
        ASSERT(iret.eq.0)
!
! --- RECUPERATION DE LA GRANDEUR (ICI R1)  ---
! --- REFERENCEE PAR LA CARTE CARGEOPO           ---
!
        call jeveuo(carsd//'.CESC', 'L', icesc)
        call jeveuo(carsd//'.CESD', 'L', icesd)
        call jeveuo(carsd//'.CESL', 'L', icesl)
        call jeveuo(carsd//'.CESV', 'L', icesv)
!
        call jeveuo(carte//'.DESC', 'L', idesc)
        call jeveuo(jexnom('&CATA.GD.NOMCMP', 'CAGEPO'), 'L', icmp)
!
        call jelira(jexnum('&CATA.GD.NOMCMP', zi(idesc)), 'LONMAX', npmax)
!
        rangr1= indik8(zk8(icmp),'R1      ',1,npmax)
!
! ---    DEBUT DE LA BOUCLE SUR LES CYLINDRES
! ---    ON RECHERCHE LA MAILLE ASSOCIEE AU PREMIER NOEUDS DE CHAQUE
! ---    CYLINDRE, ET ON LIT LA CARTE ELEMENT QUI LUI CORRESPOND
        do 160 i = 1, nbcyl
            numma = zi(nummag(i))
!
            call cesexi('C', icesd, icesl, numma, 1,&
                        1, rangr1, iad)
            if (iad .gt. 0) then
! ---       RECUPERATION DU RAYON DE LA PREMIERE MAILLE DE CHAQUE
! ---       CYLINDRE
!
                rint(i)=zr(icesv-1+abs(iad))
!
            else
                call utmess('F', 'ALGELINE_75')
            endif
160      continue
    endif
!
! --- MENAGE
    call detrsd('CHAM_ELEM_S', '&&MEFCEN.CARGEOPO')
!
    call jedema()
end subroutine
