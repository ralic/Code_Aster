subroutine cgnoes(mofaz, iocc, nomaz, lisnoz, nbno)
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
!       CGNOES -- TRAITEMENT DE L'OPTION ENV_SPHERE
!                 DU MOT FACTEUR CREA_GROUP_NO DE
!                 LA COMMANDE DEFI_GROUP
!
!      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_NO CONSTITUE
!      DE TOUS LES NOEUDS APPARTENANT A L'ENVELOPPE D'UNE SPHERE
!      DEFINIE PAR L'UTILISATEUR.
!
! -------------------------------------------------------
!  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_NO'
!  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
!  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
!  LISNOZ        - JXVAR - K24  - : NOM DE LA LISTE DE NOEUDS
!                                   APPARTENANT A L'ENVELOPPE
!                                   DE LA SPHERE.
!  NBNO          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
! -------------------------------------------------------
!
!.========================= DEBUT DES DECLARATIONS ====================
!
! -----  ARGUMENTS
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utcono.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=*) :: mofaz, nomaz, lisnoz
!
! --------- VARIABLES LOCALES ---------------------------
    character(len=8) :: noma, k8bid
    character(len=16) :: motfac, mocle(3)
    character(len=24) :: lisnoe
!
    real(kind=8) :: x0(3), x(3)
!.========================= DEBUT DU CODE EXECUTABLE ==================
!
!-----------------------------------------------------------------------
    integer ::  idlino, ino, iocc, iret, nb
    integer :: nbno, nbnoe, ndim, nprec, nrayon
    real(kind=8) :: d2, dist, prec, rayon, zero
    real(kind=8), pointer :: vale(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
!
! --- INITIALISATIONS :
!     ---------------
    motfac = mofaz
    noma = nomaz
    lisnoe = lisnoz
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
    nbno = 0
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
! --- RECUPERATION DE LA DEMI-EPAISSEUR DE L'ENVELOPPE :
!     ------------------------------------------------
    call getvr8(motfac, 'PRECISION', iocc=iocc, nbval=0, nbret=nprec)
    if (nprec .eq. 0) then
        call utmess('F', 'MODELISA3_90')
    else
        call getvr8(motfac, 'PRECISION', iocc=iocc, scal=prec, nbret=nb)
        if (prec .le. zero) then
            call utmess('F', 'MODELISA3_91')
        endif
    endif
!
! --- RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE :
!     ---------------------------------------------
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnoe)
!
! --- ALLOCATION DU VECTEUR DES NOMS DES NOEUDS  APPARTENANT
! --- A L'ENVELOPPE DE LA SPHERE :
!     --------------------------
    call wkvect(lisnoe, 'V V I', nbnoe, idlino)
!
! --- PARCOURS DES NOEUDS DU MAILLAGE :
!     --------------------------------
    nbno = 0
    do ino = 1, nbnoe
!
! ---     COORDONNEES DU NOEUD :
!         --------------------
        x(1) = vale(3*(ino-1)+1)
        x(2) = vale(3*(ino-1)+2)
        if (ndim .eq. 3) then
            x(3) = vale(3*(ino-1)+3)
        endif
!
! ---     DISTANCE DU NOEUD COURANT AU CENTRE DE LA SPHERE :
!         ------------------------------------------------
        d2 = ( x(1)-x0(1))*(x(1)-x0(1)) + (x(2)-x0(2))*(x(2)-x0(2)) + (x(3)-x0(3))*(x(3)-x0(3) )
!
! ---     SI LE NOEUD COURANT APPARTIENT A L'ENVELOPPE DE LA SPHERE
! ---     ON L'AFFECTE A LA LISTE DE NOEUDS QUI SERA AFFECTEE
! ---     AU GROUP_NO :
!         -----------
        dist = sqrt(d2)
        if (abs(dist-rayon) .le. prec) then
            nbno = nbno + 1
            zi(idlino+nbno-1) = ino
        endif
!
    end do
!
    call jedema()
!.============================ FIN DE LA ROUTINE ======================
end subroutine
