subroutine cgnofu(mofaz, iocc, nomaz, lisnoz, nbno)
! aslint: disable=
    implicit none
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterfort/cgnoor.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvr8.h"
#include "asterfort/gmgnre.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ornofd.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
#include "blas/ddot.h"
    integer :: iocc, nbno
    character(len=*) :: mofaz, nomaz, lisnoz
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
! -------------------------------------------------------
!
!       CGNOFU -- TRAITEMENT DE L'OPTION "TUNNEL"
!                 DU MOT FACTEUR CREA_GROUP_NO DE
!                 LA COMMANDE DEFI_GROUP
!
!      CETTE FONCTIONNALITE PERMET DE CREER UN GROUP_NO CONSTITUE
!      DE TOUS LES NOEUDS APPARTENANT A UN CYLINDRE CENTRE SUR
!      UN AXE DEFINI PAR L'UTILISATEUR.
!
! ----------------------------------------------------------------------
!  MOFAZ         - IN    - K16  - : MOT FACTEUR 'CREA_GROUP_NO'
!  IOCC          - IN    - I    - : NUMERO D'OCCURENCE DU MOT-FACTEUR
!  NOMAZ         - IN    - K8   - : NOM DU MAILLAGE
!  LISNOZ        - JXVAR - K24  - : NOM DE LA LISTE DE NOEUDS
!                                   APPARTENANT A L'ENVELOPPE
!                                   DU CYLINDRE.
!  NBNO          - OUT   -  I   - : LONGUEUR DE CETTE LISTE
! ----------------------------------------------------------------------
    real(kind=8) :: vecori(3)
!
    integer :: nrf, nlf, nbnot, nbmat, nbmb, nbnb, nbnc, i, j
    integer :: jmail,  idlino,   idnono, ino1, ino2, ino
    integer ::  nbnor, irest, nbma
    real(kind=8) :: c1(3), c2(3), nb(3), c1nb(3), c1c2(3), lc1c2, psca, zero
    real(kind=8) :: rfut, rfut2, lfut, lcumul, xc1h, xc2h, r, c2nb(3), lc1nb, x
    real(kind=8) :: y, z, xmin, xmax, lc2nb, c2h(3), ymin, ymax, zmin, zmax
    real(kind=8) :: hnb(3), c1h(3), l12
    character(len=8) :: noma, prefix, typm, ndorig, ndextr
    character(len=16) :: motfac, motcle(3), typmcl(3)
    character(len=24) :: lisnoe, mesmai, lisnom, mafour
    integer, pointer :: noeud_beton(:) => null()
    integer, pointer :: noeuds_cube(:) => null()
    integer, pointer :: noeuds_trouves(:) => null()
    integer, pointer :: travail(:) => null()
    real(kind=8), pointer :: vale(:) => null()
!     ------------------------------------------------------------------
!
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
! --- RECUPERATION DES COORDONNES DES NOEUDS DU MAILLAGE :
!     --------------------------------------------------
    call jeveuo(noma//'.COORDO    .VALE', 'L', vr=vale)
!
! --- RECUPERATION DU NOMBRE DE NOEUDS DU MAILLAGE :
!     ---------------------------------------------
    call dismoi('NB_NO_MAILLA', noma, 'MAILLAGE', repi=nbnot)
!
! --- RECUPERATION DU NOMBRE DE MAILLES DU MAILLAGE :
!     ---------------------------------------------
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbmat)
!
! --- RECUPERATION DU GROUPE DE MAILLES BETON :
!     ---------------------------------------
    mesmai = '&&CGNOFU.MES_MAILLES'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    motcle(3) = 'TOUT'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
    typmcl(3) = 'TOUT'
    call reliem(' ', noma, 'NU_MAILLE', motfac, iocc,&
                3, motcle, typmcl, mesmai, nbmb)
    call jeveuo(mesmai, 'L', jmail)
!
! --- TRANSFORMATION EN LISTE DE NOEUDS
!
    AS_ALLOCATE(vi=travail, size=nbnot)
    AS_ALLOCATE(vi=noeud_beton, size=nbnot)
    call gmgnre(noma, nbnot, travail, zi(jmail), nbmb,&
                noeud_beton, nbnb, 'TOUS')
!
! --- RECUPERATION DES NOEUDS AXE :
!     ---------------------------
    motcle(1) = 'GROUP_MA_AXE'
    motcle(2) = 'MAILLE_AXE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
    prefix = '&&CGNOFU'
    mafour = '&&CGNOFU.MALIGNE'
    call cgnoor(mafour, noma, motfac, iocc, 2,&
                motcle, typmcl, ' ', nbma, ndorig,&
                ndextr, typm, vecori)
    lisnom = prefix//'.NOEUD'
    call ornofd(mafour, noma, nbma, lisnom, ndorig,&
                ndextr, 'V', vecori)
    call jedetr(mafour)
    call jelira(lisnom, 'LONMAX', nbnc)
    call jeveuo(lisnom, 'L', idnono)
!
! --- RECUPERATION DU RAYON DU TUNNEL :
!     -------------------------------
    call getvr8(motfac, 'RAYON', iocc=iocc, scal=rfut, nbret=nrf)
    rfut2 = rfut * rfut
!
! --- RECUPERATION DE LA LONGUEUR TUNNEL A TRAITER :
!     --------------------------------------------
    lfut = r8maem( )
    call getvr8(motfac, 'LONGUEUR', iocc=iocc, scal=lfut, nbret=nlf)
!
    AS_ALLOCATE(vi=noeuds_cube, size=nbnot)
    AS_ALLOCATE(vi=noeuds_trouves, size=nbnot)
!
    lcumul = zero
!
!     C1    : NOEUD 1 D'UN SEGMENT DE L'AXE
!     C2    : NOEUD 2 D'UN SEGMENT DE L'AXE
!     LC1C2 : LONGUEUR D'UN SEGMENT DE L'AXE
!     NB    : NOEUD DE LA BOITE A PERCER
!
! --- BOUCLE SUR LE NOMBRE DE SEGMENTS DE L'AXE :
!     -----------------------------------------
!
    irest = 0
    do i = 1, nbnc-1
!
! ------ RECUPERATION DE LA DIRECTION DEFINISSANT L'AXE DU SEGMENT :
!        ---------------------------------------------------------
        ino1 = zi(idnono+i-1)
        ino2 = zi(idnono+i )
!
        c1(1) = vale(3*(ino1-1)+1)
        c1(2) = vale(3*(ino1-1)+2)
        c1(3) = vale(3*(ino1-1)+3)
        xmax = c1(1) + rfut
        xmin = c1(1) - rfut
        ymax = c1(2) + rfut
        ymin = c1(2) - rfut
        zmax = c1(3) + rfut
        zmin = c1(3) - rfut
!
        c2(1) = vale(3*(ino2-1)+1)
        c2(2) = vale(3*(ino2-1)+2)
        c2(3) = vale(3*(ino2-1)+3)
        xmax = max ( xmax, (c2(1) + rfut) )
        xmin = min ( xmin, (c2(1) - rfut) )
        ymax = max ( ymax, (c2(2) + rfut) )
        ymin = min ( ymin, (c2(2) - rfut) )
        zmax = max ( zmax, (c2(3) + rfut) )
        zmin = min ( zmin, (c2(3) - rfut) )
!
        c1c2(1) = c2(1) - c1(1)
        c1c2(2) = c2(2) - c1(2)
        c1c2(3) = c2(3) - c1(3)
!
        lc1c2 = c1c2(1)*c1c2(1) + c1c2(2)*c1c2(2) + c1c2(3)*c1c2(3)
        if (lc1c2 .eq. zero) then
            call utmess('F', 'MODELISA3_92')
        endif
        l12 = sqrt(lc1c2)
        if ((lcumul+l12) .ge. lfut) then
            if (irest .ne. 0) goto 999
            irest = irest + 1
            c1c2(1) = c1c2(1) / l12
            c1c2(2) = c1c2(2) / l12
            c1c2(3) = c1c2(3) / l12
            y = lfut - lcumul
            c2(1) = c1(1) + y*c1c2(1)
            c2(2) = c1(2) + y*c1c2(2)
            c2(3) = c1(3) + y*c1c2(3)
            c1c2(1) = c2(1) - c1(1)
            c1c2(2) = c2(2) - c1(2)
            c1c2(3) = c2(3) - c1(3)
            lc1c2 = c1c2(1)*c1c2(1) + c1c2(2)*c1c2(2) + c1c2(3)*c1c2( 3)
            lcumul = lfut
        else
            lcumul = lcumul + l12
        endif
!
! ------ ON LIMITE LA RECHECHE AUX NOEUDS SITUES DANS UNE BOITE
!        DONT LES DIMENSIONS SONT XMAX,XMIN, YMAX,YMIN, ZMAX,ZMIN :
!        --------------------------------------------------------
        nbnor = 0
        do j = 1, nbnb
            ino = noeud_beton(j)
            x = vale(3*(ino-1)+1)
            y = vale(3*(ino-1)+2)
            z = vale(3*(ino-1)+3)
            if ((x.le.xmax .and. x.ge.xmin) .and. (y.le.ymax .and. y.ge.ymin) .and.&
                (z.le.zmax .and. z.ge.zmin)) then
                nbnor = nbnor + 1
                noeuds_cube(nbnor) = ino
            endif
        end do
!
! ------ PARCOURS DES NOEUDS DE LA BOITE A INTERSECTER :
!        ---------------------------------------------
        do j = 1, nbnor
            ino = noeuds_cube(j)
!
            nb(1) = vale(3*(ino-1)+1)
            nb(2) = vale(3*(ino-1)+2)
            nb(3) = vale(3*(ino-1)+3)
!
            c1nb(1) = nb(1) - c1(1)
            c1nb(2) = nb(2) - c1(2)
            c1nb(3) = nb(3) - c1(3)
!
            c2nb(1) = nb(1) - c2(1)
            c2nb(2) = nb(2) - c2(2)
            c2nb(3) = nb(3) - c2(3)
!
            lc1nb = c1nb(1)*c1nb(1) + c1nb(2)*c1nb(2) + c1nb(3)*c1nb( 3)
            lc2nb = c2nb(1)*c2nb(1) + c2nb(2)*c2nb(2) + c2nb(3)*c2nb( 3)
!
            psca=ddot(3,c1nb,1,c1c2,1)
            c1h(1) = psca * c1c2(1) / lc1c2
            c1h(2) = psca * c1c2(2) / lc1c2
            c1h(3) = psca * c1c2(3) / lc1c2
            xc1h = c1h(1)*c1h(1) + c1h(2)*c1h(2) + c1h(3)*c1h(3)
            psca=ddot(3,c2nb,1,c1c2,1)
            c2h(1) = psca * c1c2(1) / lc1c2
            c2h(2) = psca * c1c2(2) / lc1c2
            c2h(3) = psca * c1c2(3) / lc1c2
            xc2h = c2h(1)*c2h(1) + c2h(2)*c2h(2) + c2h(3)*c2h(3)
!
            hnb(1) = c1nb(1) - c1h(1)
            hnb(2) = c1nb(2) - c1h(2)
            hnb(3) = c1nb(3) - c1h(3)
            r = hnb(1)*hnb(1) + hnb(2)*hnb(2) + hnb(3)*hnb(3)
!
! ---       SI LE NOEUD COURANT APPARTIENT AU TUNNEL, ON L'AFFECTE
! ---       A LA LISTE DE NOEUDS QUI SERA AFFECTEE AU GROUP_NO :
!           --------------------------------------------------
            if (r .le. rfut2) then
                if (xc1h .le. lc1c2 .and. xc2h .le. lc1c2) then
                    noeuds_trouves(ino) = 1
                endif
!
! ---          ON TRAITE LES EXTREMITES COMME DES ROTULES :
!              ------------------------------------------
                if ((lcumul+sqrt(xc2h)) .le. lfut) then
                    if (lc2nb .le. rfut2) noeuds_trouves(ino) = 1
                endif
                if ((lcumul-l12+sqrt(xc1h)) .le. lfut) then
                    if (lc1nb .le. rfut2) noeuds_trouves(ino) = 1
                endif
            endif
!
        end do
!
    end do
!
999 continue
!
! --- ON COMPTE LES NOEUDS ET ON LES AFFECTE A LISNOE
!
    call wkvect(lisnoe, 'V V I', nbnot, idlino)
!
    nbno = 0
    do i = 1, nbnot
        if (noeuds_trouves(i) .eq. 1) then
            nbno = nbno + 1
            zi(idlino+nbno-1) = i
        endif
    end do
!
    call jedetr(mesmai)
    AS_DEALLOCATE(vi=travail)
    AS_DEALLOCATE(vi=noeuds_cube)
    AS_DEALLOCATE(vi=noeuds_trouves)
    AS_DEALLOCATE(vi=noeud_beton)
    call jedetr(lisnom)
!
    call jedema()
!
end subroutine
