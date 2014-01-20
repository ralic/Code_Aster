subroutine irgmce(chamsy, partie, ifi, nomcon, ordr,&
                  nbordr, coord, connx, point, nobj,&
                  nbel, nbcmpi, nomcmp, lresu, para,&
                  nomaou, nomain, versio, tycha)
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/irgme2.h"
#include "asterfort/irgmec.h"
#include "asterfort/irgmor.h"
#include "asterfort/irgmpv.h"
#include "asterfort/irgmtb.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/rsexch.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=*) :: nomcon, chamsy, nomcmp(*), partie
    character(len=8) :: nomaou, nomain, tycha
    real(kind=8) :: coord(*), para(*)
    logical :: lresu
    integer :: nbcmpi, ifi, nbordr, versio
    integer :: ordr(*), connx(*), point(*)
!     NBRE, NOM D'OBJET POUR CHAQUE TYPE D'ELEMENT
    integer :: neletr
    parameter (neletr =  8)
    integer :: ntyele, maxel, maxno
    parameter (ntyele = 28)
    parameter (maxel  = 48)
    parameter (maxno  =  8)
    integer :: tdec(ntyele, maxel, maxno)
    integer :: typd(ntyele, 3)
    integer :: tord(neletr)
    integer :: nbel(ntyele), nbel2(ntyele), jel(ntyele)
    character(len=24) :: nobj(ntyele)
!     ------------------------------------------------------------------
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
!        IMPRESSION D'UN CHAM_ELEM AU FORMAT GMSH
!
!        CHAMSY : NOM DU CHAM_ELEM A ECRIRE
!        IFI    : NUMERO D'UNITE LOGIQUE DU FICHIER DE SORTIE GMSH
!        NOMCON : NOM DU CONCEPT A IMPRIMER
!        PARTIE : IMPRESSION DE LA PARTIE COMPLEXE OU REELLE DU CHAMP
!        NBORDR : NOMBRE DE NUMEROS D'ORDRE DANS LE TABLEAU ORDR
!        ORDR   : LISTE DES NUMEROS D'ORDRE A IMPRIMER
!        COORD  : VECTEUR COORDONNEES DES NOEUDS DU MAILLAGE
!        CONNX  : VECTEUR CONNECTIVITES DES NOEUDS DU MAILLAGE
!        POINT  : VECTEUR DU NOMBRE DE NOEUDS DES MAILLES DU MAILLAGE
!        NOBJ(i): NOM JEVEUX DEFINISSANT LES ELEMENTS DU MAILLAGE
!        NBEL(i): NOMBRE D'ELEMENTS DU MAILLAGE DE TYPE i
!        NBCMPI : NOMBRE DE COMPOSANTES DEMANDEES A IMPRIMER
!        NOMCMP : NOMS DES COMPOSANTES DEMANDEES A IMPRIMER
!        LRESU  : LOGIQUE INDIQUANT SI NOMCON EST UNE SD RESULTAT
!        PARA   : VALEURS DES VARIABLES D'ACCES (INST, FREQ)
!        NOMAOU : NOM DU MAILLAGE REDECOUPE
!        VERSIO : NUMERO DE LA VERSION GMSH UTILISEE (1 OU 2)
!
!     ------------------------------------------------------------------
!
    integer :: ior, i, j, k, ine, inoe, ima, listno(8), ix, nbno
    integer :: iq, ifm, niv, jtype,  ncmpme
    integer :: nbcmp, ipoin, iret, jcesc, jcesl
    integer ::     jcesk, jcesd
    integer :: icmp,  ipt, isp, nbpt, nbsp, jnumol
    integer :: nbma, ncmpu, iad, nbcmpd, nbord2, iadmax, iadmm
    parameter(ncmpme=12)
    logical :: iwri, tens, scal, vect, lcmp
    character(len=1) :: tsca
    character(len=8) :: k8b, nomgd, type, nocmp
    character(len=19) :: noch19, champs
    character(len=24) :: numold, connex
    integer, pointer :: cesc(:) => null()
    integer, pointer :: cesd(:) => null()
    integer, pointer :: cesl(:) => null()
    integer, pointer :: cesv(:) => null()
    character(len=8), pointer :: vnocmp(:) => null()
    character(len=8), pointer :: ordre_cmp(:) => null()
!     ------------------------------------------------------------------
!
    call infniv(ifm, niv)
    call jemarq()
!
! --- TABLEAU DES INFOS DE DECOUPAGE
    call irgmtb(tdec, typd, versio)
!
! --- ORDRE D'IMPRESSION DES VALEURS
    call irgmor(tord, versio)
!
    nbord2 = max(1,nbordr)
    numold = nomaou//'.NUMOLD         '
    connex = nomain//'.CONNEX         '
!
    AS_ALLOCATE(vi=cesd, size=nbord2)
    AS_ALLOCATE(vi=cesc, size=nbord2)
    AS_ALLOCATE(vi=cesv, size=nbord2)
    AS_ALLOCATE(vi=cesl, size=nbord2)
    call wkvect('&&IRGMCG.TYPE', 'V V K8', nbord2, jtype)
!
    nbcmp = 0
    do ior = 1, nbord2
        if (lresu) then
            call rsexch(' ', nomcon, chamsy, ordr(ior), noch19,&
                        iret)
            if (iret .ne. 0) goto 60
        else
            noch19 = nomcon
        endif
        call codent(ior, 'D0', k8b)
        champs = '&&IRGMCE.CH'//k8b
        call celces(noch19, 'V', champs)
        call jeveuo(champs//'.CESK', 'L', jcesk)
        call jeveuo(champs//'.CESD', 'L', cesd(ior))
        call jeveuo(champs//'.CESC', 'L', cesc(ior))
        call jeveuo(champs//'.CESV', 'L', cesv(ior))
        call jeveuo(champs//'.CESL', 'L', cesl(ior))
        call jelira(champs//'.CESV', 'TYPE', cval=zk8(jtype+ior-1))
!
        nomgd = zk8(jcesk-1+2)
        call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
        if (tsca .ne. 'R') then
            call utmess('F', 'ALGORITH2_63')
        endif
!
        type = zk8(jcesk-1+3)
        if (type(1:4) .ne. 'ELNO') then
            call utmess('F', 'PREPOST2_52')
        endif
!
        if (ior .eq. 1) then
            jcesc = cesc(ior)
            jcesd = cesd(ior)
            jcesl = cesl(ior)
            nbma = zi(jcesd-1+1)
            nbcmp = zi(jcesd-1+2)
            ncmpu = 0
            AS_ALLOCATE(vk8=vnocmp, size=nbcmp)
            do icmp = 1, nbcmp
                do ima = 1, nbma
                    nbpt = zi(jcesd-1+5+4* (ima-1)+1)
                    nbsp = zi(jcesd-1+5+4* (ima-1)+2)
                    do ipt = 1, nbpt
                        do isp = 1, nbsp
                            call cesexi('C', jcesd, jcesl, ima, ipt,&
                                        isp, icmp, iad)
                            if (iad .gt. 0) goto 40
                        end do
                    end do
                end do
                goto 50
 40             continue
                ncmpu = ncmpu + 1
                vnocmp(ncmpu) = zk8(jcesc-1+icmp)
 50             continue
            end do
        else
            if (zi(cesd(ior)-1+2) .ne. nbcmp) then
                call utmess('F', 'PREPOST2_53')
            endif
        endif
!
 60     continue
    end do
!
! --- RECUPERATION DU TABLEAU DE CORRESPONDANCE ENTRE NUMERO DES
!     NOUVELLES MAILLES ET NUMERO DE LA MAILLE INITIALE
!     CREE PAR IRGMMA
!
    call jeveuo(numold, 'L', jnumol)
    do i = 1, ntyele
        nbel2(i)=0
    end do
!
    tens = .false.
    vect = .false.
    scal = .false.
!
! --- BOUCLE SUR LE NOMBRE DE COMPOSANTES DU CHAM_ELEM
!     *************************************************
    if (nbcmpi .eq. 0) then
        nbcmpd = nbcmp
    else
        nbcmpd = nbcmpi
    endif
!
    if (versio .eq. 1) then
        tens = .false.
        scal = .true.
        vect = .false.
    else if (versio.eq.2) then
        if (tycha(1:4) .eq. 'SCAL') then
            scal=.true.
        else if (tycha(1:4).eq.'TENS') then
            tens=.true.
        else if (tycha(1:4).eq.'VECT') then
            vect=.true.
        endif
    endif
!
    if (versio .eq. 2 .and. tens) then
        lcmp=.false.
        AS_ALLOCATE(vk8=ordre_cmp, size=ncmpme)
        do k = 1, ncmpme
            ordre_cmp(k)=' '
        end do
        do k = 1, nbcmpd
            ordre_cmp(k)=nomcmp(k)
            ordre_cmp(1+ncmpme/2+k-1)=vnocmp(k)
            do ix = 1, nbcmp
                if (vnocmp(ix) .eq. nomcmp(k)) then
                    icmp = ix
                    goto 62
                endif
            end do
            k8b = nomcmp(k)
            call utmess('F', 'PREPOST2_54', sk=k8b)
 62         continue
            if (k .ne. ix) lcmp=.true.
        end do
        if (lcmp) then
            call utmess('A', 'PREPOST2_55', nk=ncmpme, valk=ordre_cmp)
        endif
        AS_DEALLOCATE(vk8=ordre_cmp)
    endif
!
    do k = 1, nbcmpd
        if (nbcmpi .ne. 0) then
            do ix = 1, nbcmp
                if (vnocmp(ix) .eq. nomcmp(k)) then
                    icmp = ix
                    goto 80
                endif
            end do
            k8b = nomcmp(k)
            call utmess('F', 'PREPOST2_54', sk=k8b)
 80         continue
        else
            icmp = k
        endif
        nocmp = vnocmp(icmp)
!
! ----- PREMIER PASSAGE POUR DETERMINER SI LE CHAMP A ECRIRE EXISTE
!       SUR LES POI1, SEG2, TRIA3, TETR4...
!       DONC ON  N'ECRIT RIEN
        iwri = .false.
!
! ----- BOUCLE SUR LES ELEMENTS DANS L'ORDRE DONNE PAR IRGMOR
!
        do ine = 1, neletr
!         I=NUM DE L'ELEMENT DANS LE CATALOGUE
            i=tord(ine)
            if (nbel(i) .ne. 0) then
                iadmm = 0
!           NBNO=NBRE DE NOEUDS DE CET ELEMENT
                nbno = typd(i,3)
                call jeveuo(nobj(i), 'L', jel(i))
                do iq = 1, nbel(i)
                    ima = zi(jel(i)-1+iq)
                    ipoin = point(ima)
                    do inoe = 1, nbno
                        listno(inoe) = connx(ipoin-1+inoe)
                    end do
                    call irgmec(zi(jnumol), ima, connex, nbord2, cesd,&
                                cesl, cesv, partie, jtype, nbno,&
                                listno, icmp, ifi, iwri, iadmax,&
                                ordr, chamsy, nomcon, lresu)
                    iadmm = max(iadmax,iadmm)
                end do
                if (iadmm .gt. 0) nbel2(i) = nbel(i)
            endif
        end do
!
!
        if (.not.tens) then
!
! ----- ECRITURE DE L'ENTETE DE View
!       ****************************
            call irgmpv(ifi, lresu, nomcon, chamsy, nbord2,&
                        para, nocmp, nbel2, scal, vect,&
                        tens, versio)
!
            iwri = .true.
!
! ----- BOUCLE SUR LES ELEMENTS DANS L'ORDRE DONNE PAR IRGMOR
!
            do ine = 1, neletr
!         I=NUM DE L'ELEMENT DANS LE CATALOGUE
                i=tord(ine)
                if (nbel2(i) .ne. 0) then
!           NBNO=NBRE DE NOEUDS DE CET ELEMENT
                    nbno = typd(i,3)
                    call jeveuo(nobj(i), 'L', jel(i))
                    do iq = 1, nbel(i)
                        ima = zi(jel(i)-1+iq)
                        ipoin = point(ima)
                        do inoe = 1, nbno
                            listno(inoe) = connx(ipoin-1+inoe)
                        end do
                        do j = 1, 3
                            write(ifi,1000) (coord(3*(listno(inoe)-1)+&
                            j),inoe=1,nbno)
                        end do
                        call irgmec(zi(jnumol), ima, connex, nbord2, cesd,&
                                    cesl, cesv, partie, jtype, nbno,&
                                    listno, icmp, ifi, iwri, iadmax,&
                                    ordr, chamsy, nomcon, lresu)
                    end do
                endif
            end do
!
! ----- FIN D'ECRITURE DE View
!       **********************
            write (ifi,1010) '$EndView'
!
        endif
!
    end do
!
!
    if (tens) then
!
! ----- VERIFICATION SUR LES COMPOSANTES FOURNIES PAR L'UTILISATEUR:
        do k = 1, nbcmpi
            icmp=indik8(vnocmp,nomcmp(k),1,nbcmp)
            if (icmp .eq. 0) then
                call utmess('F', 'PREPOST6_34', sk=nomcmp(k))
            endif
        end do
!
! ----- ECRITURE DE L'ENTETE DE View
!       ****************************
        nocmp = 'TENSEUR'
        call irgmpv(ifi, lresu, nomcon, chamsy, nbord2,&
                    para, nocmp, nbel2, scal, vect,&
                    tens, versio)
!
        iwri = .true.
!
! ----- BOUCLE SUR LES ELEMENTS DANS L'ORDRE DONNE PAR IRGMOR
!
        do ine = 1, neletr
!         I=NUM DE L'ELEMENT DANS LE CATALOGUE
            i=tord(ine)
            if (nbel2(i) .ne. 0) then
!           NBNO=NBRE DE NOEUDS DE CET ELEMENT
                nbno = typd(i,3)
                call jeveuo(nobj(i), 'L', jel(i))
                do iq = 1, nbel(i)
                    ima = zi(jel(i)-1+iq)
                    ipoin = point(ima)
                    do inoe = 1, nbno
                        listno(inoe) = connx(ipoin-1+inoe)
                    end do
                    do j = 1, 3
                        write(ifi,1000) (coord(3*(listno(inoe)-1)+j),&
                        inoe=1,nbno)
                    end do
                    call irgme2(zi(jnumol), ima, connex, nbord2, cesd,&
                                cesl, cesv, partie, jtype, nbno,&
                                listno, nbcmp, ifi, iadmax)
                end do
            endif
        end do
!
! ----- FIN D'ECRITURE DE View
!       **********************
        write (ifi,1010) '$EndView'
!
    endif
!
    AS_DEALLOCATE(vi=cesc)
    AS_DEALLOCATE(vi=cesd)
    AS_DEALLOCATE(vi=cesv)
    AS_DEALLOCATE(vi=cesl)
    AS_DEALLOCATE(vk8=vnocmp)
    call jedetr('&&IRGMCG.TYPE')
    call jedema()
!
    1000 format (1p,4(e15.8,1x))
    1010 format (a8)
!
end subroutine
