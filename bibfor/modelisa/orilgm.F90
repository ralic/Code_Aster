subroutine orilgm(noma)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/orilma.h"
#include "asterfort/ornorm.h"
#include "asterfort/orvlma.h"
#include "asterfort/orvlse.h"
#include "asterfort/utmess.h"
#include "asterfort/utnono.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: noma
! ======================================================================
!
!     ORILGM  --  LE BUT EST DE REORIENTER, SI C'EST NECESSAIRE,
!                 LES MAILLES DE PEAU DE GROUPES DE MAILLES
!                 DONNES SOUS LES MOTS CLES :
!                 'ORIE_PEAU_2D' EN 2D
!                 'ORIE_PEAU_3D' ET 'ORIE_NORM_COQUE' EN 3D
!                 DE TELLE FACON A CE QUE LA NORMALE A LA MAILLE DE
!                 PEAU SOIT EXTERIEURE AU VOLUME.
!
!   ARGUMENT        E/S  TYPE         ROLE
!    MODELZ         IN    K*      NOM DU MODELE
!
! ========================= DEBUT DES DECLARATIONS ====================
! -----  VARIABLES LOCALES
    integer :: ifm, niv, nbf1, nbf2, nbf3, jjj, jgro, n1, n2
    integer :: n3, noeud, iocc, ngv, ier, ndim, igr, ng, nbmail, norit, norien
    integer :: ntrait, jjv, nbmavo, jmavo, nbmato, ima, nbmavi, jmavi, k, jgv
    integer :: ncf3, ngs, jgs, nbmasu, jmasu
    real(kind=8) :: vect(3)
    logical :: reorie, orivec
    character(len=8) :: k8b
    character(len=16) :: mofa2d, mofa3d, mofb3d, mofc3d
    character(len=24) :: nomnoe, grmama, nnoeud, gmat
    character(len=24) :: valk(2)
    integer :: iarg
!
! ========================= DEBUT DU CODE EXECUTABLE ==================
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- INITIALISATIONS :
!     ---------------
!
    norit = 0
    reorie = .true.
!
    mofa2d = 'ORIE_PEAU_2D'
    mofa3d = 'ORIE_PEAU_3D'
    mofb3d = 'ORIE_NORM_COQUE'
    mofc3d = 'ORIE_LIGNE'
!
    call getfac(mofa2d, nbf1)
    call getfac(mofa3d, nbf2)
    call getfac(mofb3d, nbf3)
    call getfac(mofc3d, ncf3)
!
! --- RECUPERATION DU MAILLAGE ASSOCIE AU MODELE :
!     ------------------------------------------
    nomnoe = noma//'.NOMNOE'
    grmama = noma//'.GROUPEMA'
!
! --- RECUPERATION DE LA DIMENSION (2 OU 3) DU PROBLEME :
!     -------------------------------------------------
    call dismoi('Z_CST', noma, 'MAILLAGE', repk=k8b)
    if (k8b(1:3) .eq. 'OUI') then
        ndim = 2
    else
        ndim = 3
    endif
!
! --- COMPATIBILITE DU PROBLEME AVEC LES MOTS CLES FACTEUR :
!     ----------------------------------------------------
    if (( nbf1 .gt. 0 ) .and. ( ndim .eq. 3 )) then
        call utmess('F', 'MODELISA5_95')
    endif
    if (( nbf2 .gt. 0 ) .and. ( ndim .eq. 2 )) then
        call utmess('F', 'MODELISA5_96')
    endif
!
! --- TRAITEMENT DE 'ORIE_PEAU_2D' :
!     ----------------------------
!
    do iocc = 1, nbf1
        call getvem(noma, 'GROUP_MA', mofa2d, 'GROUP_MA', iocc,&
                    iarg, 0, k8b, ng)
        ng = -ng
        call wkvect('&&ORILGM.WORK', 'V V K24', ng, jjj)
        call getvem(noma, 'GROUP_MA', mofa2d, 'GROUP_MA', iocc,&
                    iarg, ng, zk24(jjj), ng)
!        PRESENCE DE GROUP_MA_SURF ?
!        ---------------------------
        call getvtx(mofa2d, 'GROUP_MA_SURF', iocc=iocc, nbval=0, nbret=ngs)
        if (ngs .ne. 0) then
            ngs = -ngs
            call wkvect('&&ORILGM.WORK2', 'V V K24', ngs, jgs)
            call getvem(noma, 'GROUP_MA', mofa2d, 'GROUP_MA_SURF', iocc,&
                        iarg, ngs, zk24(jgs), ngs)
            call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbmato)
            call wkvect('&&ORILGM.WORK3', 'V V I', nbmato, jjv)
            do ima = 1, nbmato
                zi(jjv+ima-1)=0
            end do
            do igr = 1, ngs
                gmat = zk24(jgs+igr-1)
                call jelira(jexnom(grmama, gmat), 'LONMAX', nbmavi)
                call jeveuo(jexnom(grmama, gmat), 'L', jmavi)
                do ima = 1, nbmavi
                    zi(jjv+zi(jmavi+ima-1)-1)=1
                end do
            end do
!          NOMBRE DE MAILLES 'VOLUMIQUES' (SANS DOUBLON) : NBMASU
            nbmasu=0
            do ima = 1, nbmato
                nbmasu=nbmasu+zi(jjv+ima-1)
            end do
!          LISTE DES MAILLES 'VOLUMIQUES' (SANS DOUBLON) : ZI(JMASU)
            call wkvect('&&ORILGM.GROUP_MA_SURF', 'V V I', nbmasu, jmasu)
            k=0
            do ima = 1, nbmato
                if (zi(jjv+ima-1) .eq. 1) then
                    k=k+1
                    zi(jmasu+k-1)=ima
                endif
            end do
            call jedetr('&&ORILGM.WORK3')
            call jedetr('&&ORILGM.WORK2')
        else
            nbmasu=0
            call wkvect('&&ORILGM.GROUP_MA_SURF', 'V V I', 1, jmasu)
        endif
!
        do igr = 1, ng
            gmat = zk24(jjj+igr-1)
            call jelira(jexnom(grmama, gmat), 'LONUTI', nbmail)
            call jeveuo(jexnom(grmama, gmat), 'L', jgro)
            write(ifm,1000) gmat, nbmail
            norien=0
            call orilma(noma, ndim, zi(jgro), nbmail, norien,&
                        ntrait, reorie, nbmasu, zi(jmasu))
            norit = norit + norien
            write(ifm,1100) norien
            if (ntrait .ne. 0) write(ifm,1110) ntrait
        end do
        call jedetr('&&ORILGM.WORK')
        call jedetr('&&ORILGM.GROUP_MA_SURF')
    end do
!
! --- TRAITEMENT DE 'ORIE_PEAU_3D' :
!     ----------------------------
!
    do iocc = 1, nbf2
        call getvem(noma, 'GROUP_MA', mofa3d, 'GROUP_MA', iocc,&
                    iarg, 0, k8b, ng)
        ng = -ng
        call wkvect('&&ORILGM.WORK', 'V V K24', ng, jjj)
        call getvem(noma, 'GROUP_MA', mofa3d, 'GROUP_MA', iocc,&
                    iarg, ng, zk24(jjj), ng)
!
!        PRESENCE DE GROUP_MA_VOLU ?
!        ---------------------------
        call getvtx(mofa3d, 'GROUP_MA_VOLU', iocc=iocc, nbval=0, nbret=ngv)
        if (ngv .ne. 0) then
            ngv = -ngv
            call wkvect('&&ORILGM.WORK2', 'V V K24', ngv, jgv)
            call getvem(noma, 'GROUP_MA', mofa3d, 'GROUP_MA_VOLU', iocc,&
                        iarg, ngv, zk24(jgv), ngv)
            call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbmato)
            call wkvect('&&ORILGM.WORK3', 'V V I', nbmato, jjv)
            do ima = 1, nbmato
                zi(jjv+ima-1)=0
            end do
            do igr = 1, ngv
                gmat = zk24(jgv+igr-1)
                call jelira(jexnom(grmama, gmat), 'LONMAX', nbmavi)
                call jeveuo(jexnom(grmama, gmat), 'L', jmavi)
                do ima = 1, nbmavi
                    zi(jjv+zi(jmavi+ima-1)-1)=1
                end do
            end do
!          NOMBRE DE MAILLES 'VOLUMIQUES' (SANS DOUBLON) : NBMAVO
            nbmavo=0
            do ima = 1, nbmato
                nbmavo=nbmavo+zi(jjv+ima-1)
            end do
!          LISTE DES MAILLES 'VOLUMIQUES' (SANS DOUBLON) : ZI(JMAVO)
            call wkvect('&&ORILGM.GROUP_MA_VOLU', 'V V I', nbmavo, jmavo)
            k=0
            do ima = 1, nbmato
                if (zi(jjv+ima-1) .eq. 1) then
                    k=k+1
                    zi(jmavo+k-1)=ima
                endif
            end do
            call jedetr('&&ORILGM.WORK3')
            call jedetr('&&ORILGM.WORK2')
        else
            nbmavo=0
            call wkvect('&&ORILGM.GROUP_MA_VOLU', 'V V I', 1, jmavo)
        endif
!
!
        do igr = 1, ng
            gmat = zk24(jjj+igr-1)
            call jelira(jexnom(grmama, gmat), 'LONUTI', nbmail)
            call jeveuo(jexnom(grmama, gmat), 'L', jgro)
            write(ifm,1000) gmat, nbmail
            norien=0
            call orilma(noma, ndim, zi(jgro), nbmail, norien,&
                        ntrait, reorie, nbmavo, zi(jmavo))
            norit = norit + norien
            write(ifm,1100) norien
            if (ntrait .ne. 0) write(ifm,1110) ntrait
        end do
        call jedetr('&&ORILGM.WORK')
        call jedetr('&&ORILGM.GROUP_MA_VOLU')
    end do
!
! --- TRAITEMENT DE 'ORIE_NORM_COQUE':
!     -------------------------------
!
    do iocc = 1, nbf3
        orivec = .false.
        call getvr8(mofb3d, 'VECT_NORM', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            orivec = .true.
            call getvr8(mofb3d, 'VECT_NORM', iocc=iocc, nbval=-n1, vect=vect,&
                        nbret=n1)
            call getvtx(mofb3d, 'NOEUD', iocc=iocc, nbval=0, nbret=n2)
            if (n2 .ne. 0) then
                call getvtx(mofb3d, 'NOEUD', iocc=iocc, scal=nnoeud, nbret=n2)
                call jenonu(jexnom(nomnoe, nnoeud), noeud)
                if (noeud .eq. 0) then
                    call utmess('F', 'MODELISA5_97', sk=nnoeud)
                endif
            else
                call getvtx(mofb3d, 'GROUP_NO', iocc=iocc, scal=nnoeud, nbret=n3)
                call utnono(' ', noma, 'NOEUD', nnoeud, k8b,&
                            ier)
                if (ier .eq. 10) then
                    call utmess('F', 'MODELISA8_75', sk=nnoeud)
                else if (ier .eq. 1) then
                    valk(1) = nnoeud
                    valk(2) = k8b
                    call utmess('A', 'SOUSTRUC_87', nk=2, valk=valk)
                endif
                call jenonu(jexnom(nomnoe, k8b), noeud)
            endif
        endif
        call getvem(noma, 'GROUP_MA', mofb3d, 'GROUP_MA', iocc,&
                    iarg, 0, k8b, ng)
        ng = -ng
        call wkvect('&&ORILGM.WORK', 'V V K24', ng, jjj)
        call getvem(noma, 'GROUP_MA', mofb3d, 'GROUP_MA', iocc,&
                    iarg, ng, zk24(jjj), ng)
        if (orivec) then
            do igr = 1, ng
                gmat = zk24(jjj+igr-1)
                call jelira(jexnom(grmama, gmat), 'LONUTI', nbmail)
                call jeveuo(jexnom(grmama, gmat), 'L', jgro)
                write(ifm,1000) gmat, nbmail
                norien=0
                call orvlma(noma, zi(jgro), nbmail, norien, vect,&
                            noeud)
                norit = norit + norien
                write(ifm,1100) norien
            end do
        else
            do igr = 1, ng
                gmat = zk24(jjj+igr-1)
                call jelira(jexnom(grmama, gmat), 'LONUTI', nbmail)
                call jeveuo(jexnom(grmama, gmat), 'L', jgro)
                write(ifm,1000) gmat, nbmail
                norien=0
                call ornorm(noma, zi(jgro), nbmail, reorie, norien)
                norit = norit + norien
                write(ifm,1100) norien
            end do
        endif
        call jedetr('&&ORILGM.WORK')
    end do
!
! --- TRAITEMENT DE 'ORIE_LIGNE':
!     ------------------------------
!
    do iocc = 1, ncf3
        orivec = .false.
        call getvr8(mofc3d, 'VECT_TANG', iocc=iocc, nbval=0, nbret=n1)
        if (n1 .ne. 0) then
            orivec = .true.
            call getvr8(mofc3d, 'VECT_TANG', iocc=iocc, nbval=-n1, vect=vect,&
                        nbret=n1)
            call getvtx(mofc3d, 'NOEUD', iocc=iocc, nbval=0, nbret=n2)
            if (n2 .ne. 0) then
                call getvtx(mofc3d, 'NOEUD', iocc=iocc, scal=nnoeud, nbret=n2)
                call jenonu(jexnom(nomnoe, nnoeud), noeud)
                if (noeud .eq. 0) then
                    call utmess('F', 'MODELISA5_97', sk=nnoeud)
                endif
            else
                call getvtx(mofc3d, 'GROUP_NO', iocc=iocc, scal=nnoeud, nbret=n3)
                call utnono(' ', noma, 'NOEUD', nnoeud, k8b,&
                            ier)
                if (ier .eq. 10) then
                    call utmess('F', 'MODELISA8_75', sk=nnoeud)
                else if (ier .eq. 1) then
                    valk(1) = nnoeud
                    valk(2) = k8b
                    call utmess('A', 'SOUSTRUC_87', nk=2, valk=valk)
                endif
                call jenonu(jexnom(nomnoe, k8b), noeud)
            endif
        endif
        call getvem(noma, 'GROUP_MA', mofc3d, 'GROUP_MA', iocc,&
                    iarg, 0, k8b, ng)
        ng = -ng
        call wkvect('&&ORILGM.WORK', 'V V K24', ng, jjj)
        call getvem(noma, 'GROUP_MA', mofc3d, 'GROUP_MA', iocc,&
                    iarg, ng, zk24(jjj), ng)
        if (orivec) then
            do igr = 1, ng
                gmat = zk24(jjj+igr-1)
                call jelira(jexnom(grmama, gmat), 'LONUTI', nbmail)
                call jeveuo(jexnom(grmama, gmat), 'L', jgro)
                write(ifm,1000) gmat, nbmail
                norien=0
                call orvlse(noma, zi(jgro), nbmail, norien, vect,&
                            noeud)
                norit = norit + norien
                write(ifm,1100) norien
            end do
        else
            do igr = 1, ng
                gmat = zk24(jjj+igr-1)
                call jelira(jexnom(grmama, gmat), 'LONUTI', nbmail)
                call jeveuo(jexnom(grmama, gmat), 'L', jgro)
                write(ifm,1000) gmat, nbmail
                norien=0
                call ornorm(noma, zi(jgro), nbmail, reorie, norien)
                norit = norit + norien
                write(ifm,1100) norien
            end do
        endif
        call jedetr('&&ORILGM.WORK')
    end do
!
    if (norit .ne. 0) write(ifm,1010) norit
!
    1000 format('TRAITEMENT DU GROUP_MA: ',a24,' DE ',i7,' MAILLES')
    1100 format(24x,i7,' MAILLE(S) ONT ETE ORIENTEE(S)')
    1110 format(24x,i7,' MAILLE(S) N''ONT PAS ETE TRAITEE(S) ')
    1010 format('AU TOTAL ', i7, ' MAILLE(S) ORIENTEE(S) ')
!
    call jedema()
end subroutine
