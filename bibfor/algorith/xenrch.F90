subroutine xenrch(nomo, noma, cnslt, cnsln, cnslj,&
                  cnsen, cnsenr, ndim, fiss, goinop,&
                  lismae, lisnoe)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cncinv.h"
#include "asterfort/cnscre.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jerazo.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xlmail.h"
#include "asterfort/xmafis.h"
#include "asterfort/xoriff.h"
#include "asterfort/xptfon.h"
#include "asterfort/xstama.h"
#include "asterfort/xstami.h"
#include "asterfort/xstano.h"
#include "asterfort/xtabff.h"
    integer :: ndim
    character(len=8) :: noma, fiss, nomo
    character(len=19) :: cnslt, cnsln, cnslj
    character(len=19) :: cnsen, cnsenr
    character(len=24) :: lismae, lisnoe
    logical :: goinop
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (PREPARATION)
!
! CALCUL DE L'ENRICHISSEMENT ET DES POINTS DU FOND DE FISSURE - 2D/3D
!
! ----------------------------------------------------------------------
!
!
! I/O FISS   : NOM DE LA FISSURE
! IN  NOMO   : NOM DU MODELE
! IN  NOMA   : NOM DU MAILLAGE
! IN  GOINOP : .TRUE.  SI  OPOO10 AVEC UPWIND-SIMPLEXE/GRILLE/3D
!              .FALSE. SINON
! IN  LISMAE : NOM DE LA LISTE DES MAILLES ENRICHIES
! IN  LISNOE : NOM DE LA LISTE DES NOEUDS DE GROUP_ENRI
! IN  CNSLT  : LEVEL-SET TANGENTE (TRACE DE LA FISSURE)
! IN  CNSLN  : LEVEL-SET NORMALE  (PLAN DE LA FISSURE)
! IN  CNSLJ  : LEVEL-SET JONCTION
! OUT CNSEN  : CHAM_NO SIMPLE POUR LE STATUT DES NOEUDS
! OUT CNSENR : CHAM_NO SIMPLE REEL POUR VISUALISATION
!
!
    integer :: nxmafi, nxptff
!
    integer :: iret, nbno, ino, imae, nmafon, jfon, jtail, nfon
    integer :: jfono, jbaso, jtailo
    integer :: jcoor, jstano, jfonmu
    integer :: jensv, jensl, nbma
    integer :: jensvr, jenslr, jcaraf
    integer :: i, nmafis, ibid
    integer :: jmafis, jmafon, k, jbas, jmaen1, jmaen2, jmaen3
    integer :: nbfond, numfon
    integer :: ifm, niv
    integer :: nmaen1, nmaen2, nmaen3, ncouch, nfono
    character(len=8) :: k8bid
    character(len=16) :: typdis
    character(len=19) :: cnxinv, info, listpt
    character(len=24) :: mafis, stano, xcarfo, fonmul
    real(kind=8) :: q(4)
    real(kind=8) :: rayon
    logical :: orient
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
!
! --- ACCES AU MAILLAGE
!
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call dismoi('F', 'NB_NO_MAILLA', noma, 'MAILLAGE', nbno,&
                k8bid, iret)
    call dismoi('F', 'NB_MA_MAILLA', noma, 'MAILLAGE', nbma,&
                k8bid, iret)
!
!     NOMBRE MAX DE MAILLES TRAVERSEES PAR LA FISSURE
    nxmafi = nbma
!
!     CONNECTIVITE INVERSEE
    cnxinv = '&&XENRCH.CNCINV'
    call cncinv(noma, ibid, 0, 'V', cnxinv)
!
    call dismoi('F', 'TYPE_DISCONTINUITE', fiss, 'FISS_XFEM', ibid,&
                typdis, iret)
!
! --- RECUPERATION INFORMATIONS SUR LE FOND DE FISSURE
    rayon = 0.d0
    ncouch = 0
    if (typdis .eq. 'FISSURE') then
        xcarfo = fiss(1:8)//'.CARAFOND'
        call jeveuo(xcarfo, 'L', jcaraf)
        rayon = zr(jcaraf)
        ncouch = nint(zr(jcaraf+1))
    endif
!
!     VOIR ALGORITHME DÉTAILLÉ DANS BOOK II (16/12/03)
!
!-------------------------------------------------------------------
!    1) ON RESTREINT LA ZONE D'ENRICHISSEMENT AUTOUR DE LA FISSURE
!-------------------------------------------------------------------
!
    if (niv .ge. 3) write(ifm,*)'1) RESTRICTION DE LA ZONE D ENRICHISSEMENT'
!
    mafis='&&XENRCH.MAFIS'
    call wkvect(mafis, 'V V I', nxmafi, jmafis)
!     ATTENTION, MAFIS EST LIMITÉ À NXMAFI MAILLES
    call xmafis(noma, cnsln, nxmafi, mafis, nmafis,&
                lismae)
!
!     FISSURE OU INTERFACE EN DEHORS DE LA STRUCTURE OU
!     COINCIDANT AVEC UN BORD DE LA STRUCTURE
    if (nmafis .eq. 0) then
        call utmess('F', 'XFEM_57')
    endif
!
    if (niv .ge. 2) then
        call utmess('I', 'XFEM_19', si=nmafis)
    endif
    if (niv .ge. 3) then
        call utmess('I', 'XFEM_26')
        do 110 imae = 1, nmafis
            write(ifm,*)' ',zi(jmafis-1+imae)
110      continue
    endif
!
!--------------------------------------------------------------------
!    2°) ON ATTRIBUE LE STATUT DES NOEUDS DE GROUP_ENRI
!--------------------------------------------------------------------
!
    if (niv .ge. 3) write(ifm,*)'2 ) ATTRIBUTION DU STATUT DES NOEUDS DE GROUPENRI'
!
!     CREATION DU VECTEUR STATUT DES NOEUDS
    stano='&&XENRCH.STANO'
    call wkvect(stano, 'V V I', nbno, jstano)
!
!     ON INITIALISE POUR TOUS LES NOEUDS DU MAILLAGE ENR À 0
    call jerazo(stano, nbno, 1)
!
!     CALCUL DU STATUT DES NOEUDS
    call xstano(noma, lisnoe, nmafis, jmafis, cnslt,&
                cnsln, cnslj, rayon, cnxinv, stano)
!
!--------------------------------------------------------------------
!    3°) ON ATTRIBUE LE STATUT DES MAILLES DU MAILLAGE
!        (MAILLES PRINCIPALES ET MAILLES DE BORD)
!        ET ON CONSTRUIT LES MAILLES DE MAFOND (NB MAX = NMAFIS)
!        + MAJ DU STANO SI ENRICHISSEMENT A NB COUCHES
!--------------------------------------------------------------------
!
    if (niv .ge. 3) write(ifm,*)'3) ATTRIBUTION DU STATUT DES MAILLES'
!
    call wkvect('&&XENRCH.MAFOND', 'V V I', nmafis, jmafon)
    call wkvect('&&XENRCH.MAENR1', 'V V I', nbma, jmaen1)
    call wkvect('&&XENRCH.MAENR2', 'V V I', nbma, jmaen2)
    call wkvect('&&XENRCH.MAENR3', 'V V I', nbma, jmaen3)
!
!     CALCUL EFFECTIF DU STATUT DES MAILLES (+MAJ STANO)
    call xstama(nomo, noma, nbma, nmafis, jmafis,&
                ncouch, lisnoe, zi(jstano), cnslt, cnsln,&
                jmafon, jmaen1, jmaen2, jmaen3, nmafon,&
                nmaen1, nmaen2, nmaen3)
!
!
!     IMPRESSION DES MAILLES ENRICHIES
    call xstami(noma, nmafon, nmaen1, nmaen2, nmaen3,&
                jmafon, jmaen1, jmaen2, jmaen3)
!
!--------------------------------------------------------------------
!     3.5°) ENREGISTREMENT DES STATUT DES NOEUDS
!--------------------------------------------------------------------
!
!     RQ : ON NE PEUT PAS FAIRE CA AVANT CAR STANO EST MODIFIE
!     SI ON DEFINIT UN ENRICHISSEMENT GEOM A NB_COUCHES
!
!     ENREGISTREMENT DU CHAM_NO SIMPLE : STATUT DES NOEUDS
    call cnscre(noma, 'NEUT_I', 1, 'X1', 'V',&
                cnsen)
    call jeveuo(cnsen//'.CNSV', 'E', jensv)
    call jeveuo(cnsen//'.CNSL', 'E', jensl)
    do 210 ino = 1, nbno
        zi(jensv-1+(ino-1)+1)=zi(jstano-1+(ino-1)+1)
        zl(jensl-1+(ino-1)+1)=.true.
210  end do
!
!     ENREGISTREMENT DU CHAM_NO SIMPLE REEL (POUR VISUALISATION)
    call cnscre(noma, 'NEUT_R', 1, 'X1', 'V',&
                cnsenr)
    call jeveuo(cnsenr//'.CNSV', 'E', jensvr)
    call jeveuo(cnsenr//'.CNSL', 'E', jenslr)
    do 211 ino = 1, nbno
        zr(jensvr-1+(ino-1)+1)=zi(jstano-1+(ino-1)+1)
        zl(jenslr-1+(ino-1)+1)=.true.
211  end do
!
!     POUR UNE INTERFACE, ON PASSE DIRECTEMENT A LA CREATION DE LA SD
    if (typdis .eq. 'INTERFACE') then
        ASSERT(nmaen2+nmaen3.eq.0)
        nfon = 0
        nbfond = 0
        goto 800
!     DE MEME POUR UNE FISSURE DONT LE FOND SE SITUE EN DEHORS DE LA
!     MATIERE (EX: FISSURE QUI DEBOUCHE EN FIN DE PROPAGATION)
    else if (nmafon.eq.0) then
        call utmess('A', 'XFEM_58')
        if (rayon .gt. 0.d0) then
            call utmess('A', 'XFEM_59')
        endif
        ASSERT(nmaen2+nmaen3.eq.0)
        nfon = 0
        nbfond = 0
        goto 800
    endif
!
!--------------------------------------------------------------------
!    4°) RECHERCHES DES POINTS DE FONFIS (ALGO BOOK I 18/12/03)
!        ET REPERAGE DES POINTS DE BORD
!--------------------------------------------------------------------
!
    if (niv .ge. 3) write(ifm,*)'4) RECHERCHE DES POINTS DE FONFIS'
!
!     ON RAJOUTE +1 POUR LES CAS PARTICULIER OU TOUS LES ELTS
!     CONTIENNENT LE FOND DE FISSURE
    nxptff = nmaen1 + nmaen2 + nmaen3 +1
!
    call wkvect('&&XENRCH.FONFIS', 'V V R', 11*nxptff, jfono)
    call wkvect('&&XENRCH.BASFON', 'V V R', 2*ndim*nxptff, jbaso)
    call wkvect('&&XENRCH.FOND_TAIL_R', 'V V R', nxptff, jtailo)
!
!     VECTEUR CONTENANT LES INDICES DES POINTS DU FOND PAR MAILLE
    listpt = '&&XENRCH.LISTPT'
!
    call xptfon(noma, ndim, nmafon, cnslt, cnsln,&
                cnxinv, jmafon, nxptff, jfono, nfon,&
                jbaso, jtailo, fiss, goinop, listpt,&
                orient)
    ASSERT(nfon.gt.0)
!
    if (.not.goinop) then
        call utmess('I', 'XFEM_33', si=nfon)
    else
        call utmess('I', 'XFEM_74', si=nfon)
    endif
!
    if (.not.orient) then
        nfon = 0
        nbfond = 0
        goto 800
    endif
!
!--------------------------------------------------------------------
!    5°) ORIENTATION DES POINTS DE FONFIS (ALGO BOOK I 19/12/03)
!        ET DETECTION DES FONDS MULTIPLES
!--------------------------------------------------------------------
!     VECTEURS TEMPORAIRES DIMENSIONNES A NFON+1 AU CAS OU ON A UN
!     FOND FERME
    call wkvect('&&XENRCH.FONFI', 'V V R', 4*(nfon+1), jfon)
    call wkvect('&&XENRCH.BASFO', 'V V R', 2*ndim*(nfon+1), jbas)
    call wkvect('&&XENRCH.TAILR', 'V V R', nfon+1, jtail)
!
    fonmul = '&&XENRCH.FONDMULT'
    call wkvect(fonmul, 'V V I', nfon, jfonmu)
    nfono = nfon
!
!     SEULEMENT EN 3D
    if (ndim .eq. 3) then
!
        if (niv .ge. 3) write(ifm,*)'5) ORIENTATION DU FOND DE FISSURE'
!
        info = fiss//'.INFO'
!
        call xoriff(info, nfon, jfono, jbaso, jtailo,&
                    nmafon, listpt, goinop, jfon, jbas,&
                    jtail, fonmul, nbfond)
!
    endif
!   SI LE FOND EST FERME
    if (nfono .eq. (nfon-1)) call utmess('I','XFEM_60')
!
!     REMPLISSAGE DE FONDFISS ET DE BASEFOND
!     STOCKAGE DES FONDS MULTIPLES EN 2D
!       EN 2D, CHAQUE POINT DE FOND DE FISSURE EST UN FOND A LUI SEUL
!       IL Y A DONC AUTANT DE FONDS MULTIPLES QUE DE POINTS (1 OU 2)
!       LES POINTS DE DEPART ET D'ARRIVEES SONT LES MEMES
    if (ndim .eq. 2) then
        if (nfon .gt. 2) then
            call utmess('F', 'XFEM_11')
        endif
        nbfond = nfon
        do 999 i = 1, nfon
            do 11 k = 1, 2
                zr(jfon-1+4*(i-1)+k) = zr(jfono-1+11*(i-1)+k)
                zr(jbas-1+4*(i-1)+k) = zr(jbaso-1+4*(i-1)+k)
                zr(jbas-1+4*(i-1)+k+2) = zr(jbaso-1+4*(i-1)+k+2)
11          continue
            zr(jtail-1+i) = zr(jtailo-1+i)
            zi(jfonmu-1+2*(i-1)+1)=i
            zi(jfonmu-1+2*(i-1)+2)=i
999      continue
    endif
!
!     IMPRESSION DES POINTS DE FOND DE FISSURE (2D/3D)
    if (.not.goinop) then
        call utmess('I', 'XFEM_35')
    else
        call utmess('I', 'XFEM_75')
    endif
!
    numfon=1
!
    do 799 i = 1, nfon
        q(1)=zr(jfon-1+4*(i-1)+1)
        q(2)=zr(jfon-1+4*(i-1)+2)
        if (ndim .eq. 3) then
            q(3)=zr(jfon-1+4*(i-1)+3)
            q(4)=zr(jfon-1+4*(i-1)+4)
        endif
        if (zi(jfonmu-1+2*(numfon-1)+1) .eq. i) then
            call utmess('I', 'XFEM_36', si=numfon)
            if (ndim .eq. 3) write(ifm,797)
            if (ndim .eq. 2) write(ifm,7970)
        endif
        if (ndim .eq. 2) write(ifm,798)(q(k),k=1,2)
        if (ndim .eq. 3) write(ifm,798)(q(k),k=1,4)
        if (zi(jfonmu-1+2*(numfon-1)+2) .eq. i) numfon=numfon+1
799  end do
!
    797 format(7x,'X',13x,'Y',13x,'Z',13x,'S')
!
    7970 format(7x,'X',13x,'Y')
!
    798 format(2x,4(e12.5,2x))
!
!
800  continue
!
! --- CREATION DE LA SD
!
    call xlmail(fiss, nmaen1, nmaen2, nmaen3, nmafon,&
                jmaen1, jmaen2, jmaen3, jmafon, nfon,&
                jfon, nbfond, jbas, jtail, jfonmu,&
                ndim, goinop)
!
    if (.not.goinop) then
!
!     CONSTRUCTION DES TABLES SUR LES FONDS DE FISSURES
!
        call xtabff(nbfond, nfon, ndim, fiss)
!
    endif
! --- MENAGE
!
    call jedetr(cnxinv)
    call jedetr('&&XENRCH.FONFIS')
    call jedetr('&&XENRCH.MAFOND')
    call jedetr('&&XENRCH.MAENR1')
    call jedetr('&&XENRCH.MAENR2')
    call jedetr('&&XENRCH.MAENR3')
    call jedetr('&&XENRCH.BASFON')
    call jedetr('&&XENRCH.FONDMULT')
    call jedetr('&&XENRCH.FOND_TAIL_R')
    call jedetr('&&XENRCH.FONFI')
    call jedetr('&&XENRCH.BASFO')
    call jedetr('&&XENRCH.TAILR')
    if (goinop) then
        call jedetr('&&XENRCH.MAFIS')
        call jedetr('&&XENRCH.STANO')
        call jedetr('&&XENRCH.LISTPT')
    endif
!
    if (niv .ge. 3) write(ifm,*)'7) FIN DE XENRCH'
!
    call jedema()
end subroutine
