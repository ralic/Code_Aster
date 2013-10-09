subroutine cgmaba(mofaz, iocc, nomaz, lismaz, nbma)
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
!.======================================================================
    implicit none
!
!       CGMABA -- TRAITEMENT DE L'OPTION BANDE
!                 DU MOT FACTEUR CREA_GROUP_MA DE
!                 LA COMMANDE DEFI_GROUP
!
!      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_MA CONSTITUE
!      DE TOUTES LES MAILLES DONT UN NOEUD AU MOINS APPARTIENT
!      A UNE BANDE DEFINIE PAR SON PLAN MILIEU ET LA DISTANCE
!      DES PLANS SUPERIEUR ET INFERIEUR A CE PLAN MILIEU.
!      LE PLAN MILIEU EST DEFINI PAR UN POINT APPARTENANT A CE
!      PLAN ET UN VECTEUR QUI LUI EST PERPENDICULAIRE.
!
! -------------------------------------------------------
!  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_MA'
!  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
!  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
!  LISMAZ        - JXVAR - K24  - : NOM DE LA LISTE DE MAILLES DONT UN
!                                   NOEUD AU MOINS APPARTIENT A LA
!                                   BANDE DEFINIE PAR L'UTILISATEUR
!  NBMA          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
! -------------------------------------------------------
!
!.========================= DEBUT DES DECLARATIONS ====================
#include "jeveux.h"
#include "asterc/r8dgrd.h"
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
    integer :: vali(2)
    character(len=8) :: noma, k8bid, nomail
    character(len=16) :: motfac, mocle(3)
    character(len=24) :: lismai
    character(len=24) :: valk
    character(len=16) :: selec
!
    real(kind=8) :: x0(3), x(3), xx0(3), vecnor(3), angle(2)
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!-----------------------------------------------------------------------
    integer :: ibid, idcoor, idlima, idnoeu, ima, ino
    integer :: iocc, iret, nangle, nb, nbma, nbmai, nbno
    integer :: nbnod, ndim, ndim1, ndist, numnoe, nv, nvect
!
    real(kind=8) :: d, dist, xnorm, xnorm2, zero
!
!-----------------------------------------------------------------------
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    motfac = mofaz
    noma = nomaz
    lismai = lismaz
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
    xx0(1) = zero
    xx0(2) = zero
    xx0(3) = zero
!
    vecnor(1) = zero
    vecnor(2) = zero
    vecnor(3) = zero
!
!
    nbma = 0
!
!
! --- RECUPERATION DU TYPE DE VERIFICATION A APPLIQUER :
!     --------------------------------------------------
    call getvtx(motfac, 'CRIT_NOEUD', iocc=iocc, scal=selec, nbret=ibid)
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
    call jeveuo(noma//'.COORDO    .VALE', 'L', idcoor)
!
! --- RECUPERATION DU POINT SITUE SUR LE PLAN MILIEU :
!     ----------------------------------------------
    mocle(1) = 'POINT'
    mocle(2) = 'NOEUD_CENTRE'
    mocle(3) = 'GROUP_NO_CENTRE'
    call utcono(motfac, mocle, iocc, noma, ndim,&
                x0, iret)
!
! --- RECUPERATION DE LA DEMI-LARGEUR DE LA BANDE :
!     -------------------------------------------
    call getvr8(motfac, 'DIST', iocc=iocc, nbval=0, nbret=ndist)
    if (ndist .eq. 0) then
        call utmess('F', 'MODELISA3_67')
    else
        call getvr8(motfac, 'DIST', iocc=iocc, scal=dist, nbret=nb)
        if (dist .le. zero) then
            call utmess('F', 'MODELISA3_68')
        endif
    endif
!
! --- RECUPERATION DE LA DIRECTION PERPENDICULAIRE AU PLAN MILIEU
! --- DE LA BANDE :
!     -----------
    call getvr8(motfac, 'ANGL_NAUT', iocc=iocc, nbval=0, nbret=nangle)
    if (nangle .eq. 0) then
        call getvr8(motfac, 'VECT_NORMALE', iocc=iocc, nbval=0, nbret=nvect)
        if (nvect .eq. 0) then
            call utmess('F', 'MODELISA3_69')
        else
            nvect = -nvect
            if (ndim .eq. 3 .and. nvect .ne. 3) then
                call utmess('F', 'MODELISA3_70')
            else if (ndim.eq.2.and.nvect.ne.2) then
                call utmess('F', 'MODELISA3_71')
            else
                call getvr8(motfac, 'VECT_NORMALE', iocc=iocc, nbval=nvect, vect=vecnor,&
                            nbret=nv)
            endif
        endif
    else
        nangle = -nangle
        ndim1 = ndim - 1
        nangle = min (nangle,ndim1)
        call getvr8(motfac, 'ANGL_NAUT', iocc=iocc, nbval=nangle, vect=angle,&
                    nbret=nv)
        if (abs(nv) .ne. ndim1) then
            valk = motfac
            vali (1) = iocc
            call utmess('F+', 'MODELISA9_32', sk=valk, si=vali(1))
            if (ndim .eq. 2) then
                call utmess('F+', 'MODELISA9_24')
            else
                call utmess('F+', 'MODELISA9_25')
            endif
            vali (1) = abs(nv)
            vali (2) = ndim1
            call utmess('F', 'MODELISA9_35', ni=2, vali=vali)
        endif
!
        if (ndim .eq. 2) then
            angle(1) = angle(1)*r8dgrd()
!
            vecnor(1) = cos(angle(1))
            vecnor(2) = sin(angle(1))
            vecnor(3) = zero
        else if (ndim.eq.3) then
            angle(1) = angle(1)*r8dgrd()
            angle(2) = angle(2)*r8dgrd()
!
            vecnor(1) = cos(angle(1))*cos(angle(2))
            vecnor(2) = sin(angle(1))*cos(angle(2))
            vecnor(3) = -sin(angle(2))
        endif
    endif
!
    xnorm2 = vecnor(1)*vecnor(1) + vecnor(2)*vecnor(2) + vecnor(3)*vecnor(3)
!
    if (xnorm2 .eq. zero) then
        call utmess('F', 'MODELISA3_72')
    endif
!
    xnorm = sqrt(xnorm2)
!
    vecnor(1) = vecnor(1)/xnorm
    vecnor(2) = vecnor(2)/xnorm
    vecnor(3) = vecnor(3)/xnorm
!
! --- RECUPERATION DU NOMBRE DE MAILLES DU MAILLAGE :
!     ---------------------------------------------
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbmai)
!
! --- ALLOCATION DU VECTEUR DES NOMS DES MAILLES APPARTENANT AU
! --- CYLINDRE :
!     --------
    call wkvect(lismai, 'V V I', nbmai, idlima)
!
! --- PARCOURS DES MAILLES DU MAILLAGE :
!     --------------------------------
    do 10 ima = 1, nbmai
!
! ---     RECUPERATION DU NOM DE LA MAILLE :
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
!
! ---      COMPTE NOMBRE DES NOEUDS D'UN MAILLE DANS LE SPHERE :
!          ----------------------------------------------------
        nbnod = 0
!
! ---     BOUCLE SUR LES CONNECTIVITES DE LA MAILLE :
!         -----------------------------------------
        do 20 ino = 1, nbno
!
! ---        NUMERO DU NOEUD :
!            ---------------
            numnoe = zi(idnoeu+ino-1)
!
! ---        COORDONNEES DU NOEUD :
!            --------------------
            x(1) = zr(idcoor-1+3*(numnoe-1)+1)
            x(2) = zr(idcoor-1+3*(numnoe-1)+2)
            x(3) = zr(idcoor-1+3*(numnoe-1)+3)
!
            xx0(1) = x(1) - x0(1)
            xx0(2) = x(2) - x0(2)
            xx0(3) = x(3) - x0(3)
!
! ---        CALCUL DE LA DISTANCE DU NOEUD COURANT AU PLAN MILIEU :
!            -----------------------------------------------------
            d = xx0(1)*vecnor(1) + xx0(2)*vecnor(2) + xx0(3)*vecnor(3)
!
! ---      SI LE MOT CLE SIMPLE CRIT_NOEUD EST EGAL A AU MOINS UN NOEUD
!          -------------------------------------------------------------
            if (selec .eq. 'AU_MOINS_UN') then
!
! ---            SI LE NOEUD COURANT EST DANS LA BANDE, ON AFFECTE
! ---            LA MAILLE COURANTE A LA LISTE DE MAILLES QUI SERA
! ---            AFFECTEE AU GROUP_MA :
!                --------------------
                if (abs(d) .le. dist) then
                    nbma = nbma + 1
                    zi(idlima+nbma-1) = ima
                    goto 10
                endif
!
            else if ((selec.eq.'TOUS').or.(selec.eq.'MAJORITE')) then
! ---            SI LE MOT CLE SIMPLE CRIT_NOEUD EST EGAL A TOUT OU
! ---            MAJORITE, COMPTER LES NOMBRES DES NOEUDS D'UNE MAILLE
! ---            DANS LA BANDE:
!                -------------------------------------------------
                if (abs(d) .le. dist) then
                    nbnod=nbnod+1
                endif
            endif
!
 20     continue
!
        if (selec .eq. 'TOUS') then
            if (nbnod .eq. nbno) then
                nbma = nbma + 1
                zi(idlima+nbma-1) = ima
                goto 10
            endif
        endif
!
        if (selec .eq. 'MAJORITE') then
            if (nbnod .ge. (nbno+1)/2) then
                nbma = nbma + 1
                zi(idlima+nbma-1) = ima
                goto 10
            endif
        endif
!
 10 end do
!
    call jedema()
!.============================ FIN DE LA ROUTINE ======================
end subroutine
