subroutine acearp(noma, nomo, lmax, noemaf, nbocc, ivr, ifm)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
    integer :: ifm, lmax, noemaf, nbocc, ivr(*)
    character(len=8) :: noma, nomo
!
! --------------------------------------------------------------------------------------------------
!
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR LES ELEMENTS DISCRET PAR
!     RAIDEUR REPARTIE
!
! --------------------------------------------------------------------------------------------------
!
! IN  : NOMA   : NOM DU MAILLAGE
!       NOMO   : NOM DU MODELE
!       LMAX   : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
!       NBOCC  : NOMBRE D'OCCURRENCES DU MOT CLE RIGI_PARASOL
!       IVR    : TABLEAU DES INDICES DE VERIFICATION
!
! --------------------------------------------------------------------------------------------------
! person_in_charge: jean-luc.flejou at edf.fr
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/affdis.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvem.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infdis.h"
#include "asterfort/infniv.h"
#include "asterfort/iunifi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/r8inir.h"
#include "asterfort/rairep.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
! --------------------------------------------------------------------------------------------------
    integer :: nbcar, nbval, nrd
    parameter    ( nbcar = 100 , nbval = 12 , nrd = 2 )
    integer :: jdc(3), jdv(3), ibid, niv, ir, ia, iunite
    integer :: jdcinf, jdvinf
    integer :: i, iamto, ier, ii, in, inbn, ino, inoe, ioc, irep
    integer :: irgno, irgto, isym, itbmp, itbno, iv
    integer :: irepn, irepv, iaepn, iaepv
    integer :: ixci, ixckma, j, jd, jdls, jj, jn
    integer :: l, ldgm, ldnm, lokm, lorep, nbnma
    integer :: nbno, nbnoeu, nborm, nc, ncar, ncmp, nbomp
    integer :: ndim, ng, ngp, nma, nrep, nval, dimcar
    integer :: vali(2)
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: val(nbval), eta, vale(nbval), rirot(3), r8bid
    character(len=1) :: kma(3)
    character(len=8) :: nomnoe, nommai, k8bid, nomu, car(nbcar), lamass
    character(len=16) :: rep, repdis(nrd), concep, cmd
    character(len=19) :: cart(3), ligmo, cartdi
    character(len=19) :: vrepxv, vrepxn, vaepxv, vaepxn
    character(len=24) :: tmpnd(3), tmpvd(3), nogp
    character(len=24) :: tmpdis, mlgnno, mlgnma, tmcinf, tmvinf
! --------------------------------------------------------------------------------------------------
    aster_logical :: transl, trarot, eurplx, lbid
    integer :: iarg
    data repdis  /'GLOBAL          ','LOCAL           '/
    data kma     /'K','M','A'/
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call getres(nomu, concep, cmd)
!
    tmpdis = nomu//'.DISCRET'
    mlgnno = noma//'.NOMNOE'
    mlgnma = noma//'.NOMMAI'
    ligmo = nomo//'.MODELE    '
!
    call wkvect('&&TMPDISCRET', 'V V K24', lmax, jdls)
    call wkvect('&&TMPTABNO', 'V V K8', lmax, itbno)
    call wkvect('&&TMPRIGNO', 'V V R', 6*lmax, irgno)
    call wkvect('&&TMPRIGTO', 'V V R', 6*noemaf, irgto)
    call wkvect('&&TMPAMOTO', 'V V R', 6*noemaf, iamto)
    call wkvect('&&TMPTABMP', 'V V K8', lmax, itbmp)
!
!   Pour EUROPLEXUS
!       si europlexus alors toutes les occurrences de rigi_parasol doivent avoir europlexus='oui'.
!       Test sur la 1ere occurence du mot clef, puis dans la boucle sur les occurrences pour
!       vérifier que l'option ne change pas
    eurplx = .false.
    call getvtx('RIGI_PARASOL', 'EUROPLEXUS', iocc=1, scal=k8bid, nbret=ibid)
    if (ibid .ne. 0) then
        eurplx = ( k8bid(1:3) .eq. 'OUI' )
    endif
    if (eurplx) then
!       numcar = 12
        vrepxv = nomu//'.CARRIGXV'
        vrepxn = nomu//'.CARRIGXN'
        vaepxv = nomu//'.CARAMOXV'
        vaepxn = nomu//'.CARAMOXN'
!       les structures sont utilisees seulement en python
        call wkvect(vrepxv, 'G V R', 6*lmax, irepv)
        call wkvect(vrepxn, 'G V K8', lmax, irepn)
        call wkvect(vaepxv, 'G V R', 6*lmax, iaepv)
        call wkvect(vaepxn, 'G V K8', lmax, iaepn)
    endif
!
    ifm = iunifi('MESSAGE')
!   Récupération de la dimension géométrique du modèle
    call dismoi('DIM_GEOM', nomo, 'MODELE', repi=ibid)
    ndim=ibid
    if (ibid .ge. 100) then
        ibid = ibid - 100
        ndim=1
    endif
    if (ibid .ge. 20) then
        ibid = ibid - 20
        ndim=2
    endif
    if (ibid .eq. 3) ndim=3
!   Pour les discrets c'est obligatoirement du 2d ou 3d
    ASSERT((ndim.eq.2).or.(ndim.eq.3))
!
!   Construction des cartes et allocation
    cartdi = nomu//'.CARDINFO'
    tmcinf = cartdi//'.NCMP'
    tmvinf = cartdi//'.VALV'
!   Si la carte n'existe pas on la cree
    call jeexin(tmcinf, ixci)
    if (ixci .eq. 0) call alcart('G', cartdi, noma, 'CINFDI')
    call jeveuo(tmcinf, 'E', jdcinf)
    call jeveuo(tmvinf, 'E', jdvinf)
!   Par défaut pour M, A, K : repère global, matrice symétrique, pas affectée
    call infdis('DIMC', dimcar, r8bid, k8bid)
    do i = 1, 3
        zk8(jdcinf+i-1) = 'REP'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i-1), zk8(jdcinf+i-1))
        zk8(jdcinf+i+2) = 'SYM'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i+2), zk8(jdcinf+i+2))
        zk8(jdcinf+i+5) = 'DIS'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i+5), zk8(jdcinf+i+5))
    enddo
    zk8(jdcinf+9) = 'ETAK    '
    call infdis('INIT', ibid, zr(jdvinf+9), zk8(jdcinf+9))
    zk8(jdcinf+10) = 'TYDI    '
    call infdis('INIT', ibid, zr(jdvinf+10), zk8(jdcinf+10))
!   Création des cartes
    do i = 1, 3
        cart(i) = nomu//'.CARDISC'//kma(i)
        tmpnd(i) = cart(i)//'.NCMP'
        tmpvd(i) = cart(i)//'.VALV'
!       si les cartes n'existent pas on les crées
        call jeexin(tmpnd(i), ixckma)
        if (ixckma .eq. 0) then
            call alcart('G', cart(i), noma, 'CADIS'//kma(i))
        endif
        call jeveuo(tmpnd(i), 'E', jdc(i))
        call jeveuo(tmpvd(i), 'E', jdv(i))
    enddo
!
!   Récupération du niveau d'impression
    call infniv(ibid, niv)
!   Raideur et amortissement pour EUROPLEXUS
    ir = 0
    ia = 0
!   Boucle sur les occurrences de rigi_parasol
    do ioc = 1, nbocc
        eta = 0.0d0
!       Par défaut on est dans le repère global, matrices symétriques
        irep = 1
        isym = 1
        rep = repdis(1)
!
        call getvem(noma, 'GROUP_MA', 'RIGI_PARASOL', 'GROUP_MA', ioc, iarg, lmax, zk24(jdls), ng)
        call getvtx('RIGI_PARASOL', 'CARA', iocc=ioc, nbval=nbcar, vect=car, nbret=ncar)
        call getvr8('RIGI_PARASOL', 'VALE', iocc=ioc, nbval=nbval, vect=val, nbret=nval)
        call getvtx('RIGI_PARASOL', 'REPERE', iocc=ioc, scal=rep, nbret=nrep)
        call getvtx('RIGI_PARASOL', 'GROUP_MA_POI1', iocc=ioc, scal=nogp, nbret=ngp)
        if (ngp .eq. 0) then
            call getvtx('RIGI_PARASOL', 'GROUP_MA_SEG2', iocc=ioc, scal=nogp, nbret=ngp)
            if (eurplx) call utmess('F', 'MODELISA9_92')
        endif
        ASSERT( ngp  .ne. 0 )
        ASSERT( ncar .ge. 1 )
!
        if (nrep .ne. 0) then
            do i = 1, nrd
                if (rep .eq. repdis(i)) irep = i
            enddo
        endif
!       Pour EUROPLEXUS
        lbid = .false.
        call getvtx('RIGI_PARASOL', 'EUROPLEXUS', iocc=1, scal=k8bid, nbret=ibid)
        if (ibid .ne. 0) then
            lbid = ( k8bid(1:3) .eq. 'OUI' )
        endif
        if (lbid .neqv. eurplx) then
            call utmess('F', 'MODELISA9_93', si=ioc)
        endif
!       Unité pour imprimer les valeur des discrets
        call getvis('RIGI_PARASOL', 'UNITE', iocc=ioc, scal=ibid, nbret=ier)
        iunite = -1
        if (ier .ne. 0) then
            iunite = ibid
        endif
        if (iunite .gt. 0) then
            write(iunite,100) rep,ioc
        endif
!       GROUP_MA = toutes les mailles de tous les groupes de mailles
        if (ng .le. 0) goto 30
        ii = 0
        do nc = 1, ncar
            if ((nc.eq.2) .and. (car(1)(1:1).eq.car(2)(1:1))) then
                call utmess('F', 'MODELISA_16')
            endif
!           Discrets seulement en translation
            transl = (car(nc)(1:7) .eq. 'K_T_D_N') .or. (car(nc)(1:7) .eq. 'K_T_D_L') .or.&
                     (car(nc)(1:7) .eq. 'A_T_D_N') .or. (car(nc)(1:7) .eq. 'A_T_D_L')
!           Discrets en translation et rotation
            trarot = (car(nc)(1:8) .eq. 'K_TR_D_N') .or. (car(nc)(1:8) .eq. 'K_TR_D_L') .or.&
                     (car(nc)(1:8) .eq. 'A_TR_D_N') .or. (car(nc)(1:8) .eq. 'A_TR_D_L')
!
            if (transl .eqv. trarot) then
                call utmess('F', 'MODELISA_17', sk=car(nc))
            endif
!
            if (transl) then
                lamass = 'M'//car(nc)(2:7)
                if (ii+3 .gt. nval) then
                    call utmess('F', 'DISCRETS_21')
                endif
                do j = 1, 3
                    vale(j) = val(ii+j)
                enddo
                call rairep(noma, ioc, car(nc), vale, ng,&
                            zk24(jdls), nbno, zk8(itbno), zr(irgno), zr(irgto),&
                            zr(iamto), rirot, ndim)
                ii = ii + 3
            else if (trarot) then
                lamass = 'M'//car(nc)(2:8)
                if (ii+6 .gt. nval) then
                    call utmess('F', 'DISCRETS_21')
                endif
                do j = 1, 6
                    vale(j) = val(ii+j)
                enddo
                call rairep(noma, ioc, car(nc), vale, ng,&
                            zk24(jdls), nbno, zk8(itbno), zr(irgno), zr(irgto),&
                            zr(iamto), rirot, ndim)
                ii = ii + 6
            else
                ASSERT(.false.)
            endif
!
            do ino = 1, nbno
                zk8(itbmp + ino - 1) = ' '
            enddo
!
            nbnoeu = 0
            lokm = 0
            if (transl) lokm = 7
            if (trarot) lokm = 8
            if (car(nc)(lokm:lokm) .eq. 'N') nbnoeu = 1
            if (car(nc)(lokm:lokm) .eq. 'L') nbnoeu = 2
            ASSERT((nbnoeu.gt.0).and.(lokm.gt.0))
!
            call jelira(jexnom(noma//'.GROUPEMA', nogp), 'LONMAX', nma)
            call jeveuo(jexnom(noma//'.GROUPEMA', nogp), 'L', ldgm)
!
            if (nma .ne. nbno) then
                vali(1) = nbno
                vali(2) = nma
                call utmess('F', 'MODELISA2_10', sk=nogp, ni=2, vali=vali)
            endif
!
            do in = 0, nma-1
!               Récupère le nombre de noeud de la maille
                call jelira(jexnum(noma//'.CONNEX', zi(ldgm+in)), 'LONMAX', nbnma)
                call jeveuo(jexnum(noma//'.CONNEX', zi(ldgm+in)), 'L', ldnm)
                call jenuno(jexnum(mlgnma, zi(ldgm+in)), nommai)
!               Boucle sur le nb de noeud de la maille
                if (nbnma .ne. nbnoeu) then
                    call utmess('F', 'MODELISA_20', sk=nommai)
                endif
                do inbn = 1, nbnma
                    inoe = zi(ldnm+inbn-1)
                    call jenuno(jexnum(mlgnno, inoe), nomnoe)
                    do ino = 1, nbno
                        if (zk8(itbno+ino-1) .eq. nomnoe) then
                            zk8(itbmp+ino-1) = nommai
                            goto 22
                        endif
                    enddo
                enddo
!               Si on passe ici aucun des noeuds du discret appartient à la surface.
!               Ce n'est pas normal
                write(ifm,*)'GROUP_MA :', (' '//zk24(jdls+ii-1), ii=1,ng)
                call utmess('F', 'MODELISA_21', sk=nomnoe)
22              continue
            enddo
!           Préparation des impressions dans le fichier message
            lorep = 5
            if (irep .eq. 1) lorep = 6
            if (iunite .gt. 0) then
                if (transl) then
                    write(iunite,105) car(nc)(1:lokm)
                else
                    write(iunite,106) car(nc)(1:lokm), rirot(1),&
                    rirot(2),rirot(3)
                endif
            endif
!           Vérif qu'un discret est fixé à chacun des noeuds du radier
!           (une seule fois par occurrence de rigi_parasol)
            if (nc .eq. 1) then
                do ino = 1, nbno
                    if (zk8(itbmp + ino - 1) .eq. ' ') then
                        call jenuno(jexnum(mlgnno, ino), nomnoe)
                        call utmess('F', 'MODELISA2_8', sk=nomnoe)
                    endif
                enddo
            endif
!
            if (iunite .gt. 0) then
                do i = 1, nbno
                    iv = 1
                    jd = itbmp + i - 1
                    jn = itbno + i - 1
                    if (nbnoeu .eq. 1) then
                        if (transl) then
                            write(iunite,110) 'NOEUD',zk8(jn),&
                            car(nc)(1:lokm), (zr(irgno+6*i-6+jj),&
                            jj=0,2), repdis(irep)(1:lorep)
                        else
                            write(iunite,111) 'NOEUD',zk8(jn),&
                            car(nc)(1:lokm), (zr(irgno+6*i-6+jj),&
                            jj=0,5), repdis(irep)(1:lorep)
                        endif
                    else
                        if (transl) then
                            write(iunite,110) 'MAILLE',zk8(jd),&
                            car(nc)(1:lokm), (zr(irgno+6*i-6+jj),&
                            jj=0,2), repdis(irep)(1:lorep)
                        else
                            write(iunite,111) 'MAILLE',zk8(jd),&
                            car(nc)(1:lokm), (zr(irgno+6*i-6+jj),&
                            jj=0,5), repdis(irep)(1:lorep)
                        endif
                    endif
                enddo
            endif
!
            do i = 1, nbno
                iv = 1
                jd = itbmp + i - 1
                jn = itbno + i - 1
!               Pour EUROPLEXUS préparation de l'attribut python
                if (eurplx) then
                    if (nbnoeu .eq. 1) then
                        if (car(nc)(1:3) .eq. 'K_T') then
                            if (transl) then
                                do jj = 0, 2
                                    zr(irepv+6*ir+jj)=zr(irgno+6*i-6+jj)
                                    zr(irepv+6*ir+3+jj)=0.d0
                                enddo
                            else
                                do jj = 0, 5
                                    zr(irepv+6*ir+jj)=zr(irgno+6*i-6+jj)
                                enddo
                            endif
                            zk8(irepn+ir) = zk8(jd)
                            ir = ir + 1
                        else if (car(nc)(1:3) .eq. 'A_T') then
                            if (transl) then
                                do jj = 0, 2
                                    zr(iaepv+6*ia+jj)=zr(irgno+6*i-6+jj)
                                    zr(iaepv+6*ia+3+jj)=0.d0
                                enddo
                            else
                                do jj = 0, 5
                                    zr(iaepv+6*ia+jj)=zr(irgno+6*i-6+jj)
                                enddo
                            endif
                            zk8(iaepn+ia) = zk8(jd)
                            ia = ia + 1
                        endif
                    else
                        call utmess('A', 'MODELISA9_96', sk=zk8(jd))
                    endif
                endif
!               Affectation des valeurs réparties
                call affdis(ndim, irep, eta, car(nc), zr(irgno+6*i-6),&
                            jdc, jdv, ivr, iv, kma,&
                            ncmp, l, jdcinf, jdvinf, isym, ifm)
                call nocart(cartdi, 3, dimcar, mode='NOM', nma=1, limano=[zk8(jd)])
                call nocart(cart(l), 3, ncmp, mode='NOM', nma=1, limano=[zk8(jd)])
!               affectation de matrice masse nulle
                iv = 1
                call r8inir(nbval, 0.0d0, vale, 1)
                call affdis(ndim, irep, eta, lamass, vale,&
                            jdc, jdv, ivr, iv, kma,&
                            ncmp, l, jdcinf, jdvinf, isym, ifm)
                call nocart(cartdi, 3, dimcar, mode='NOM', nma=1, limano=[zk8(jd)])
                call nocart(cart(l), 3, ncmp, mode='NOM', nma=1, limano=[zk8(jd)])
            enddo
        enddo
        if (ii .ne. nval) then
            call utmess('F', 'DISCRETS_21')
        endif
30      continue
    enddo
!
    call jedetr('&&TMPDISCRET')
    call jedetr('&&TMPTABNO')
    call jedetr('&&TMPRIGNO')
    call jedetr('&&TMPRIGTO')
    call jedetr('&&TMPAMOTO')
    call jedetr('&&TMPTABMP')
    call getfac('RIGI_MISS_3D', nborm)
    call getfac('MASS_AJOU', nbomp)
    if (nborm .eq. 0 .and. nbomp .eq. 0) then
        do i = 1, 3
            call jedetr(tmpnd(i))
            call jedetr(tmpvd(i))
        enddo
        call jedetr(tmcinf)
        call jedetr(tmvinf)
    endif
!
    call jedema()
!
100 format(/, ' <DISCRET> MATRICES AFFECTEES AUX ELEMENTS DISCRET ',&
               '(REPERE ',a6,'), OCCURRENCE ',i4)
105 format(/,' PAS DE REPARTITION EN ROTATION POUR DES ',a,/)
106 format(/,' RAIDEURS DE ROTATION A REPARTIR POUR DES ',a,/&
            ,'  RX: ',1pe12.5,' RY: ',1pe12.5,' RZ: ',1pe12.5,/)
110 format(' _F(',a,'=''',a8,''', CARA=''',a,''',',/,&
            '    VALE=(',3(1x,1pe12.5,','),'),',/,&
            '    REPERE=''',a,'''),')
111 format(' _F(',a,'=''',a8,''', CARA=''',a,''',',/,&
            '    VALE=(',3(1x,1pe12.5,','),/,&
            '          ',3(1x,1pe12.5,','),'),',/,&
            '    REPERE=''',a,'''),')
end subroutine
