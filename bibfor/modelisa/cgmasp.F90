subroutine cgmasp(mofaz, iocc, nomaz, lismaz, nbma)
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
!.======================================================================
    implicit none
!
!       CGMASP -- TRAITEMENT DE L'OPTION SPHERE
!                 DU MOT FACTEUR CREA_GROUP_MA DE
!                 LA COMMANDE DEFI_GROUP
!
!      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_MA CONSTITUE
!      DE TOUTES LES MAILLES DONT UN NOEUD AU MOINS APPARTIENT
!      A UNE SPHERE DE RAYON R ET DE CENTRE P0 (X0,Y0,Z0).
!      LE RAYON ET LE POINT P0 SONT DES DONNEES UTILISATEUR.
!
! -------------------------------------------------------
!  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_MA'
!  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
!  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
!  LISMAZ        - JXVAR - K24  - : NOM DE LA LISTE DE MAILLES DONT UN
!                                   NOEUD AU MOINS APPARTIENT A LA
!                                   SPHERE DEFINIE PAR L'UTILISATEUR
!  NBMA          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
! -------------------------------------------------------
!
!.========================= DEBUT DES DECLARATIONS ====================
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/utcono.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!
! -----  ARGUMENTS
    character(len=*) :: mofaz, nomaz, lismaz
!
! --------- VARIABLES LOCALES ---------------------------
    integer :: nbnod, nbno
    character(len=8) :: noma, k8bid, nomail
    character(len=16) :: motfac, mocle(3)
    character(len=16) :: selec
    character(len=24) :: lismai
!
    real(kind=8) :: x0(3), x(3)
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!-----------------------------------------------------------------------
    integer :: ibid,  idlima, idnoeu, ima, ino
    integer :: iocc, iret, nb, nbma, nbmai, ndim, nrayon
    integer :: numnoe
    real(kind=8) :: d2, rayon, zero
    real(kind=8), pointer :: vale(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    motfac = mofaz
    noma = nomaz
    lismai = lismaz
!
! --- RECUPERATION DU TYPE DE VERIFICATION A APPLIQUER :
!     --------------------------------------------------
    call getvtx(motfac, 'CRIT_NOEUD', iocc=iocc, scal=selec, nbret=ibid)
!
    zero = 0.0d0
!
    x0(1) = zero
    x0(2) = zero
    x0(3) = zero
!
    x(1) = zero
    x(2) = zero
    x(3) = zero
!
    rayon = zero
!
    nbma = 0
!
! --- RECUPERATION DE LA DIMENSION DU MAILLAGE :
!     ----------------------------------------
    call dismoi('Z_CST', noma, 'MAILLAGE', repk=k8bid)
    if (k8bid(1:3) .eq. 'OUI') then
        ndim = 2
    else
        ndim = 3
    endif
!
! --- RECUPERATION DES COORDONNES DES NOEUDS DU MAILLAGE :
!     --------------------------------------------------
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
!
! --- RECUPERATION DU CENTRE DE LA SPHERE (OU DU CERCLE) :
!     --------------------------------------------------
    mocle(1) = 'POINT'
    mocle(2) = 'NOEUD_CENTRE'
    mocle(3) = 'GROUP_NO_CENTRE'
    call utcono(motfac, mocle, iocc, noma, ndim,&
                x0, iret)
!
! --- RECUPERATION DU RAYON DE LA SPHERE :
!     ----------------------------------
    call getvr8(motfac, 'RAYON', iocc=iocc, nbval=0, nbret=nrayon)
    if (nrayon .eq. 0) then
        call utmess('F', 'MODELISA3_82')
    else
        call getvr8(motfac, 'RAYON', iocc=iocc, scal=rayon, nbret=nb)
        if (rayon .le. zero) then
            call utmess('F', 'MODELISA3_83')
        endif
    endif
!
! --- RECUPERATION DU NOMBRE DE MAILLES DU MAILLAGE :
!     ---------------------------------------------
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbmai)
!
! --- ALLOCATION DU VECTEUR DES NOMS DES MAILLES  APPARTENANT
! --- A LA SPHERE :
!     -----------
    call wkvect(lismai, 'V V I', nbmai, idlima)
!
! --- PARCOURS DES MAILLES DU MAILLAGE :
!     --------------------------------
    do ima = 1, nbmai
!
! ---     RECUPERATION DU NOM DE LA MAILLE à partrir du numero d'ordre:
!         --------------------------------
        call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
!
! ---     RECUPERATION DES CONNECTIVITES DE LA MAILLE :
!         -------------------------------------------
        call jenonu(jexnom(noma//'.NOMMAI', nomail), ibid)
        call jeveuo(jexnum(noma//'.CONNEX', ibid), 'L', idnoeu)
!
! ---     RECUPERATION DU NOMBRE DE CONNECTIVITES DE LA MAILLE :
!         ----------------------------------------------------
        call jenonu(jexnom(noma//'.NOMMAI', nomail), ibid)
        call jelira(jexnum(noma//'.CONNEX', ibid), 'LONMAX', nbno)
!
! ---      COMPTE NOMBRE DES NOEUDS D'UN MAILLE DANS LE SPHERE :
!          ----------------------------------------------------
        nbnod = 0
!
! ---     BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
!         -----------------------------------------
        do ino = 1, nbno
!
! ---        NUMERO DU NOEUD :
!            ---------------
            numnoe = zi(idnoeu+ino-1)
!
! ---        COORDONNEES DU NOEUD :
!            --------------------
            x(1) = vale(3*(numnoe-1)+1)
            x(2) = vale(3*(numnoe-1)+2)
            if (ndim .eq. 3) then
                x(3) = vale(3*(numnoe-1)+3)
            endif
!
! ---        DISTANCE DU NOEUD COURANT AU CENTRE DE LA SPHERE :
!            ------------------------------------------------
            d2 = (x(1)-x0(1))*(x(1)-x0(1)) + (x(2)-x0(2))*(x(2)-x0(2)) + (x(3)-x0(3))*(x(3)-x0(3)&
                 )
!
! ---      SI LE MOT CLE SIMPLE CRIT_NOEUD EST EGAL A AU MOINS UN NOEUD
!          -------------------------------------------------------------
            if (selec .eq. 'AU_MOINS_UN') then
!
! ---             SI LE NOEUD COURANT EST DANS LA SPHERE, ON AFFECTE
! ---             LA MAILLE COURANTE A LA LISTE DE MAILLES QUI SERA
! ---             AFFECTEE AU GROUP_MA :
!                 --------------------
                if (d2 .le. rayon*rayon) then
                    nbma = nbma + 1
                    zi(idlima+nbma-1) = ima
                    call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
                    goto 10
                endif
! ---            SI LE MOT CLE SIMPLE CRIT_NOEUD EST EGAL A TOUT OU
! ---            MAJORITE , COMPTER LE NOMBRE DES NOEUDS D'UNE MAILLE
! ---            DANS LE SPHERE :
!                ----------------------------------------------------
            else if ((selec.eq.'TOUS').or.(selec.eq.'MAJORITE')) then
                if (d2 .le. rayon*rayon) then
                    nbnod=nbnod+1
                endif
            endif
!
        end do
!
        if (selec .eq. 'TOUS') then
            if (nbnod .eq. nbno) then
                nbma = nbma + 1
                zi(idlima+nbma-1) = ima
                call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
                goto 10
            endif
        endif
        if (selec .eq. 'MAJORITE') then
            if (nbnod .ge. (nbno+1)/2) then
                nbma = nbma + 1
                zi(idlima+nbma-1) = ima
                call jenuno(jexnum(noma//'.NOMMAI', ima), nomail)
                goto 10
            endif
        endif
!
 10     continue
    end do
!
    call jedema()
!.============================ FIN DE LA ROUTINE ======================
end subroutine
