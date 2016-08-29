subroutine xdetfo(cnsdet, cnsln, cnslt, ndim,&
                  nmafon, noma, nomfis, resuco)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: patrick.massin at edf.fr
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cescns.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnsred.h"
#include "asterfort/codent.h"
#include "asterfort/conare.h"
#include "asterfort/copisd.h"
#include "blas/ddot.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterc/r8maem.h"
#include "asterc/r8prem.h"
#include "asterfort/rsexch.h"
#include "asterfort/rslesd.h"
#include "asterfort/rsorac.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/xmafis.h"
#include "asterfort/rs_getfirst.h"
#include "asterfort/rs_getlast.h"
!
! Propagation de fissures cohésives avec XFEM
! Calculer la fonction pour détecter le nouveau front
!   et repérer les éléments intersectés par l'iso-zéro
!   de cette fonction
!
! In/out cnsdet => cham_no_s pour fonction de détection
! In cnsln => cham_no_s level-set normale
! In cnslt => cham_no_s level-set tangente
! Out '&&XDETFO.MAFOND' => mailles intersectées par l'iso-zéro
! In ndim => dimension
! Out nmafon => nombre de mailles intersectées
! In noma => nom du maillage
! In nomfis => nom de la fissure d'où l'ancien front
!              est déduit
! In resuco => nom du résultat à post-traiter
!
    real(kind=8) :: a(3), ab(3), ac(3)
    integer :: ar(12, 3)
    real(kind=8) :: b(3), c(3)
    character(len=19) :: carmat
    character(len=19) :: cesco, cnsco, cnsdet, cnsln, cnslt, cnsto
    character(len=24) :: cohee
    real(kind=8) :: deta, detb, detc, gc
    integer :: i, ia, ibid, ils, ima, imafon, ino, nume_last, nume_first
    integer :: iret, itypma, j, jcesd, jcesl, jcesv, jcnsd
    integer :: jcnsl, jcnto, jconx1, jconx2, jcoor
    integer :: jlisno, jlnsv, jltsv, jma, jmaco, jmafis
    integer :: jmafon, jmaifo, jnscov, jnsdl, jnsdv, jvale
    integer :: jvalk, jvalm, k
    character(len=24) :: lismae
    character(len=19) :: lisno
    real(kind=8) :: lsna, lsnb, lsta, lstb
    character(len=24) :: mafis
    character(len=19) :: mai
    character(len=24) :: mater
    real(kind=8) :: maxdet, mindet
    integer :: na, nb, nbar, nbls, nbma, nbno, ncmpa, ndim
    integer :: nmaco, nmafis, nmafon, nmaifo
    character(len=8) :: noma, nomfis, nommat
    integer :: nunoa, nunob
    real(kind=8) :: d1, crilst
    character(len=8) :: resuco
    real(kind=8) :: sc, rr
    character(len=8) :: typma
    character(len=6) :: k6
    character(len=32) :: nomrc
    integer :: irc, nbrc, ianorc
! ------------------------------------------------------------
!
    call jemarq()
    crilst = 1.d-2
!
!   RECUP INFOS CONNECTIVITE MAILLAGE
!
    call jeveuo(noma//'.CONNEX', 'L', jconx1)
    call jeveuo(noma//'.COORDO    .VALE', 'L', jcoor)
    call jeveuo(jexatr(noma//'.CONNEX', 'LONCUM'), 'L', jconx2)
    mai = noma//'.TYPMAIL'
    call jeveuo(mai, 'L', jma)
!
!   RECUP LEVEL-SET NORMALES ET TANGENTES
!
    call jeveuo(cnsln//'.CNSV', 'L', jlnsv)
    call jeveuo(cnslt//'.CNSV', 'L', jltsv)
!
!   RECUP STATUT DES NOEUDS
!
    cnsto = '&&XDETFO.CNSTO'
    call cnocns(nomfis(1:8)//'.STNO', 'V', cnsto)
    call jeveuo(cnsto//'.CNSV', 'L', jcnto)
!
!   INFOS ANCIENNE FISSURE
!
    call jelira(nomfis(1:8)//'.MAILFISS.CONT', 'LONMAX', nmaco)
    call jeveuo(nomfis(1:8)//'.MAILFISS.CONT', 'L', jmaco)
    call jelira(nomfis(1:8)//'.MAILFISS.MAFOND', 'LONMAX', nmaifo)
    call jeveuo(nomfis(1:8)//'.MAILFISS.MAFOND', 'L', jmaifo)
!
!   RECUP DERNIER NUMERO D ORDRE
!   POUR L INSTANT LA DETECTION SE FAIT A PARTIR DU DERNIER NUMERO D ORDRE
!
    call rs_getlast(resuco, nume_last)
! 
! --- RECUP CHAMP VARIABLES INTERNES COHESIVES
! 
    call rsexch('F', resuco, 'COHE_ELEM', nume_last, cohee,&
                iret)
!
!   TRANSFO CHAM_ELNO --> CHAM_ELEM_S (EXEMPLE XGRALS)
    cesco = '&&XDETFO.CESCO'
    call celces(cohee, 'V', cesco)
    call jeveuo(cesco(1:19)//'.CESD', 'L', jcesd)
    call jeveuo(cesco(1:19)//'.CESL', 'L', jcesl)
    call jeveuo(cesco(1:19)//'.CESV', 'L', jcesv)
!
!   TRANSFO CHAM_ELEM_S --> CHAM_NO
!   POUR CHAQUE NOEUD MOYENNE DES VALEURS QUI EXISTENT
!   NORMALEMENT CE SONT LES MEMES
    cnsco = '&&XDETFO.CNSCO'
    call cescns(cesco, ' ', 'V', cnsco, ' ',&
                ibid)
!
!   RECUP PREMIER NUMERO ORDRE
!
    call rs_getfirst(resuco, nume_first)
!
!   RECUP DU MATERIAU 
    call rslesd(resuco, nume_first, materi_ = mater)
!
!   RECUP CONTRAINTE CRITIQUE ET TENACITE
!
    carmat = mater(1:8)//'.CHAMP_MAT'
    call jeveuo(carmat(1:19)//'.VALE', 'L', jvalm)
    nommat = zk8(jvalm)
!
    call jeveuo(nommat//'.MATERIAU.NOMRC', 'L', ianorc)
    call jelira(nommat//'.MATERIAU.NOMRC', 'LONMAX', nbrc)
    do irc = 1,nbrc
        nomrc=zk32(ianorc-1+irc)
        if(nomrc.eq.'RUPT_FRAG') then
            call codent(irc,'D0',k6)
        else
            goto 1
        endif
1   continue
    end do
    call jeveuo(nommat//'.CPT.'//k6//'.VALK', 'L', jvalk)
    call jelira(nommat//'.CPT.'//k6//'.VALK', 'LONMAX', ncmpa)
    ncmpa = ncmpa/2
    call jeveuo(nommat//'.CPT.'//k6//'.VALR', 'L', jvale)
    do j = 1, ncmpa
        if (zk16(jvalk-1+j) .eq. 'GC') gc = zr(jvale-1+j)
        if (zk16(jvalk-1+j) .eq. 'SIGM_C') sc = zr(jvale-1+j)
        if (zk16(jvalk-1+j) .eq. 'PENA_LAGR') rr = zr(jvale-1+j)
    end do
    rr = rr*sc*sc/gc
!
!   REDUCTION CHAMP VARIABLES INTERNES
    call cnsred(cnsco, 0, [0], 1, 'X1',&
                'V', cnsco)
!   
!   ON ENLEVE LES NOEUDS QUI N ONT PAS DE VARIABLE INTERNE
    call jeveuo(cnsco(1:19)//'.CNSD', 'L', jcnsd)
    call jeveuo(cnsco(1:19)//'.CNSL', 'L', jcnsl)
    nbno = zi(jcnsd)
    lisno = '&&XDETFO.LISNO'
    call wkvect(lisno, 'V V I', nbno, jlisno)
    ils = 0
    do ino = 1, nbno
        if (zl(jcnsl-1+ino)) then
            ils = ils + 1
            zi(jlisno-1+ils)=ino
        endif
    end do
    nbls = ils
!
!   REDUCTION SUR LES NOEUDS
!   call cnsred(cnsco,nbls,zi(jlisno),0,kbid,'V',cnsco)
!
    call jeveuo(cnsco(1:19)//'.CNSV', 'L', jnscov)
!
! --- CREATION CHAMP INDICATEUR
! --- SUR LES NOEUDS QUI PORTENT UNE VARIABLE INTERNE
!
    call copisd('CHAM_NO_S', 'V', cnsco, cnsdet)
    call jeveuo(cnsdet(1:19)//'.CNSL', 'E', jnsdl)
    call jeveuo(cnsdet(1:19)//'.CNSV', 'E', jnsdv)
!
    do i = 1, nbls
        ino = zi(jlisno-1+i)
        zl(jnsdl-1+ino) = .true.
        zr(jnsdv-1+ino) =&
        min(zr(jnscov-1+ino)-sc,0.d0)+(sc*sc/gc)/rr*max(zr(jnscov-1+ino)-sc,0.d0)
    end do
!
!   ON REPREND UNE PARTIE DE LA STRUCTURE DE XENRCH
!   POUR TROUVER LE FRONT
    call dismoi('NB_MA_MAILLA', noma, 'MAILLAGE', repi=nbma)
    mafis = '&&XDETFO.MAFIS'
    call wkvect(mafis, 'V V I', nbma, jmafis)
    lismae = nomfis//'.GROUP_MA_ENRI'
    call xmafis(noma, cnsln, nbma, mafis, nmafis,&
                lismae)
!
    call wkvect('&&XDETFO.MAFOND', 'V V I', nmaco, jmafon)
    imafon = 0
!
!   BOUCLE SUR LES MAILLES DE CONTACT
!   (TOUTES LES VARIABLES INTERNES Y SONT DEFINIES)
!
    do j = 1, nmaco
!
        mindet = r8maem()
        maxdet = -1*r8maem()
!
!       RECUP NUMERO DE MAILLE + SA CONNECTIVITE
        ima = zi(jmaco-1+j)
        itypma=zi(jma-1+ima)
        call jenuno(jexnum('&CATA.TM.NOMTM', itypma), typma)
        call conare(typma, ar, nbar)
!
!       BOUCLE SUR LES ARÊTES : MIN ET MAX DES LST
!       COPIE XSTANO
        do 153 ia = 1, nbar
!
            na=ar(ia,1)
            nb=ar(ia,2)
!
            nunoa=zi(jconx1-1+zi(jconx2+ima-1)+na-1)
            nunob=zi(jconx1-1+zi(jconx2+ima-1)+nb-1)
!
            lsna=zr(jlnsv-1+(nunoa-1)+1)
            lsnb=zr(jlnsv-1+(nunob-1)+1)
            lsta=zr(jltsv-1+(nunoa-1)+1)
            lstb=zr(jltsv-1+(nunob-1)+1)
!
!           SI FONCTION DETECTION PAS ATTRIBUEE, ON SORT
!           NE DOIT PAS ARRIVER NORMALEMENT (BOUCLE ELMTS CONTACT)
            if (.not.zl(jnsdl-1+nunoa) .or. (.not.zl(jnsdl-1+nunob))) then
!               Il faudrait un message à l'utilisateur ici
                goto 153
            endif
            deta=zr(jnsdv-1+(nunoa-1)+1)
            detb=zr(jnsdv-1+(nunob-1)+1)
!           SI LST TROP GRANDES, ON CORRIGE
            if (lsta .ge. 0.d0) deta=-1.e-4*sc
            if (lstb .ge. 0.d0) detb=-1.e-4*sc
!
!           SI NOEUD PAS ENRICHI, ON CORRIGE
!           SINON, LA VALEUR VA NOUS INDUIRE EN ERREUR
            if (zi(jcnto-1+nunoa) .eq. 0) deta=-1.e-4*sc
            if (zi(jcnto-1+nunob) .eq. 0) detb=-1.e-4*sc
!
! --- REAJUSTEMENT FONCTION DETECTION SI NECESSAIRE (VOIR XAJULS)
!
            if (abs(deta-detb) .gt. r8prem()) then
                d1=deta/(deta-detb)
                if (abs(d1) .le. crilst) then
                    zr(jnsdv-1+(nunoa-1)+1)=0.d0
                    deta=0.d0
                endif
                if (abs(d1-1.d0) .le. (crilst)) then
                    zr(jnsdv-1+(nunob-1)+1)=0.d0
                    detb = 0.d0
                endif
            endif
!
!           MIN ET MAX DE LA FONCTION DE DETECTION
!
!           CAS ARETE CONFORME
            if (lsna .eq. 0.d0 .and. lsnb .eq. 0.d0) then
                if (deta .lt. mindet) mindet=deta
                if (deta .gt. maxdet) maxdet=deta
                if (detb .lt. mindet) mindet=detb
                if (detb .gt. maxdet) maxdet=detb
!           CAS ARETE NON CONFORME
            else if ((lsna*lsnb).le.0.d0) then
                do k = 1, ndim
                    a(k)=zr(jcoor-1+3*(nunoa-1)+k)
                    b(k)=zr(jcoor-1+3*(nunob-1)+k)
                    ab(k)=b(k)-a(k)
!                   COORDONNEE PT INTERSECTION
                    c(k)=a(k)-lsna/(lsnb-lsna)*ab(k)
                    ac(k)=c(k)-a(k)
                end do
                ASSERT(ddot(ndim, ab, 1, ab, 1).gt.0.d0)
!
!               FONCTION DE DETECTION AU POINT D INTERSECTION
                detc = deta + (detb-deta) * ddot(ndim,ab,1,ac,1) / ddot(ndim,ab,1,ab,1)
!
!               ACTUALISATION MIN ET MAX DE LA FONCTION DE DETECTION
                if (detc .lt. mindet) mindet=detc
                if (detc .gt. maxdet) maxdet=detc
!
!               VERIF STANO A REPORTER?
            endif
!
153     continue
!
        if (mindet*maxdet .le. r8prem()) then
!           ALARME UTILISATEUR SI MAILLE DE FOND
            do i = 1, nmaifo
                if (zi(jmaco-1+j) .eq. zi(jmaifo-1+i)) then
!
                    call utmess('A', 'XFEM_94')
                endif
            end do
            imafon = imafon + 1
            zi(jmafon-1+imafon) = ima
        endif
!
    end do
    nmafon = imafon
    ASSERT(nmafon.le.nmaco)
    call jedema()
end subroutine
