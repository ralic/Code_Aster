subroutine acearp(noma, nomo, lmax, noemaf, nbocc,&
                  ivr, ifm)
    implicit      none
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/affdis.h'
    include 'asterfort/alcart.h'
    include 'asterfort/assert.h'
    include 'asterfort/crlinu.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/getvem.h'
    include 'asterfort/infdis.h'
    include 'asterfort/infniv.h'
    include 'asterfort/iunifi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnom.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/nocart.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rairep.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesi.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: ifm, lmax, noemaf, nbocc, ivr(*)
    character(len=8) :: noma, nomo
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! TOLE CRP_20
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
!     AFFE_CARA_ELEM
!     AFFECTATION DES CARACTERISTIQUES POUR LES ELEMENTS DISCRET PAR
!     RAIDEUR REPARTIE
! ----------------------------------------------------------------------
! IN  : NOMA   : NOM DU MAILLAGE
! IN  : NOMO   : NOM DU MODELE
! IN  : LMAX   : NOMBRE MAX DE MAILLE OU GROUPE DE MAILLE
! IN  : NBOCC  : NOMBRE D'OCCURRENCES DU MOT CLE RIGI_PARASOL
! IN  : IVR    : TABLEAU DES INDICES DE VERIFICATION
! ----------------------------------------------------------------------
!
    integer :: nbcar, nbval, nrd
    parameter    ( nbcar = 100 , nbval = 12 , nrd = 2 )
    integer :: jdc(3), jdv(3), ibid, niv, ir, ia, iunite
    integer :: jdcinf, jdvinf
    integer :: i, iamto, ier, ii, in, inbn, ino, inoe, ioc, irep
    integer :: irgno, irgto, isym, itbmp, itbno, itrou, iv
    integer :: irepn, irepv, iaepn, iaepv
    integer :: ixci, ixckma, ixnw, j, jd, jddi, jdls, jdnw, jj, jn, k, kk
    integer :: l, ldgm, ldnm, lokm, lorep, nbmtrd, nbnma
    integer :: nbno, nbnoeu, nborm, nc, ncar, ncarac, ncmp
    integer :: ndim, ng, ngp, nma, nrep, numnoe, nval, dimcar
    integer :: vali(2)
!
    real(kind=8) :: val(nbval), eta, vale(nbval), rirot(3), r8bid
    character(len=1) :: kma(3), k1bid
    character(len=8) :: nomnoe, nommai, k8bid, nomu, car(nbcar), lamass
    character(len=16) :: rep, repdis(nrd), concep, cmd
    character(len=19) :: cart(3), ligmo, cartdi
    character(len=19) :: vrepxv, vrepxn, vaepxv, vaepxn
    character(len=24) :: tmpnd(3), tmpvd(3), nogp
    character(len=24) :: tmpdis, mlgnno, mlgnma, tmcinf, tmvinf, modnem
!
!
    logical :: transl, trarot, eurplx, lbid
    integer :: iarg
    data repdis  /'GLOBAL          ','LOCAL           '/
    data kma     /'K','M','A'/
!     ------------------------------------------------------------------
!
    call jemarq()
    call getres(nomu, concep, cmd)
!
    tmpdis = nomu//'.DISCRET'
    mlgnno = noma//'.NOMNOE'
    mlgnma = noma//'.NOMMAI'
    ligmo = nomo//'.MODELE    '
    modnem = nomo//'.MODELE    .NEMA'
    call jeexin(modnem, ixnw)
    nbmtrd = 0
    if (ixnw .ne. 0) then
        call jelira(modnem, 'NMAXOC', nbmtrd, k1bid)
        call jeveuo(modnem, 'L', jdnw)
        call wkvect(tmpdis, 'V V I', nbmtrd, jddi)
    endif
    call wkvect('&&TMPDISCRET', 'V V K24', lmax, jdls)
    call wkvect('&&TMPTABNO', 'V V K8', lmax, itbno)
    call wkvect('&&TMPRIGNO', 'V V R', 6*lmax, irgno)
    call wkvect('&&TMPRIGTO', 'V V R', 6*noemaf, irgto)
    call wkvect('&&TMPAMOTO', 'V V R', 6*noemaf, iamto)
    call wkvect('&&TMPTABMP', 'V V K8', lmax, itbmp)
!
!     POUR EUROPLEXUS
!     SI EUROPLEXUS ALORS TOUTES LES OCCURRENCES DE RIGI_PARASOL DOIVENT
!     AVOIR EUROPLEXUS='OUI'. TEST SUR LA 1ERE OCCURENCE DU MOT CLEF,
!     PUIS DANS LA BOUCLE SUR LES OCCURRENCES QUE L'OPTION NE CHANGE PAS
    eurplx = .false.
    call getvtx('RIGI_PARASOL', 'EUROPLEXUS', 1, iarg, 1,&
                k8bid, ibid)
    if (ibid .ne. 0) then
        eurplx = ( k8bid(1:3) .eq. 'OUI' )
    endif
    if (eurplx) then
!         NUMCAR = 12
        vrepxv = nomu//'.CARRIGXV'
        vrepxn = nomu//'.CARRIGXN'
        vaepxv = nomu//'.CARAMOXV'
        vaepxn = nomu//'.CARAMOXN'
!        LES STRUCTURES SONT UTILISEES SEULEMENT EN PYTHON
        call wkvect(vrepxv, 'G V R', 6*lmax, irepv)
        call wkvect(vrepxn, 'G V K8', lmax, irepn)
        call wkvect(vaepxv, 'G V R', 6*lmax, iaepv)
        call wkvect(vaepxn, 'G V K8', lmax, iaepn)
    endif
!
    ifm = iunifi('MESSAGE')
!
! --- RECUPERATION DE LA DIMENSION GEOMETRIQUE DU MODELE
    call dismoi('F', 'DIM_GEOM', nomo, 'MODELE', ibid,&
                k8bid, ier)
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
!     POUR LES DISCRETS C'EST OBLIGATOIREMENT DU 2D OU 3D
    call assert((ndim.eq.2).or.(ndim.eq.3))
!
! --- CONSTRUCTION DES CARTES ET ALLOCATION
    cartdi = nomu//'.CARDINFO'
    tmcinf = cartdi//'.NCMP'
    tmvinf = cartdi//'.VALV'
!     SI LA CARTE N'EXISTE PAS ON LA CREE
    call jeexin(tmcinf, ixci)
    if (ixci .eq. 0) call alcart('G', cartdi, noma, 'CINFDI')
    call jeveuo(tmcinf, 'E', jdcinf)
    call jeveuo(tmvinf, 'E', jdvinf)
!     PAR DEFAUT POUR M, A, K :
!        REPERE GLOBAL, MATRICE SYMETRIQUE, PAS AFFECTEE
    call infdis('DIMC', dimcar, r8bid, k8bid)
    do 200 i = 1, 3
        zk8(jdcinf+i-1) = 'REP'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i-1), zk8(jdcinf+i-1))
        zk8(jdcinf+i+2) = 'SYM'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i+2), zk8(jdcinf+i+2))
        zk8(jdcinf+i+5) = 'DIS'//kma(i)//'    '
        call infdis('INIT', ibid, zr(jdvinf+i+5), zk8(jdcinf+i+5))
200  end do
    zk8(jdcinf+9) = 'ETAK    '
    call infdis('INIT', ibid, zr(jdvinf+9), zk8(jdcinf+9))
    zk8(jdcinf+10) = 'TYDI    '
    call infdis('INIT', ibid, zr(jdvinf+10), zk8(jdcinf+10))
! --- CREATION DES CARTES
    do 220 i = 1, 3
        cart(i) = nomu//'.CARDISC'//kma(i)
        tmpnd(i) = cart(i)//'.NCMP'
        tmpvd(i) = cart(i)//'.VALV'
!        SI LES CARTES N'EXISTENT PAS ON LES CREES
        call jeexin(tmpnd(i), ixckma)
        if (ixckma .eq. 0) then
            call alcart('G', cart(i), noma, 'CADIS'//kma(i))
        endif
        call jeveuo(tmpnd(i), 'E', jdc(i))
        call jeveuo(tmpvd(i), 'E', jdv(i))
220  end do
!
!     RECUPERATION DU NIVEAU D'IMPRESSION
    call infniv(ibid, niv)
!  I RAIDEUR (POUR EPX)
    ir = 0
! I AMOR (POUR EPX)
    ia = 0
! --- BOUCLE SUR LES OCCURRENCES DE RIGI_PARASOL
    do 30 ioc = 1, nbocc
        eta = 0.0d0
!        PAR DEFAUT ON EST DANS LE REPERE GLOBAL, MATRICES SYMETRIQUES
        irep = 1
        isym = 1
        rep = repdis(1)
!
        call getvem(noma, 'GROUP_MA', 'RIGI_PARASOL', 'GROUP_MA', ioc,&
                    iarg, lmax, zk24(jdls), ng)
        call getvtx('RIGI_PARASOL', 'CARA', ioc, iarg, nbcar,&
                    car, ncar)
        call getvr8('RIGI_PARASOL', 'VALE', ioc, iarg, nbval,&
                    val, nval)
        call getvtx('RIGI_PARASOL', 'REPERE', ioc, iarg, 1,&
                    rep, nrep)
        call getvtx('RIGI_PARASOL', 'GROUP_MA_POI1', ioc, iarg, 1,&
                    nogp, ngp)
        if (ngp .eq. 0) then
            call getvtx('RIGI_PARASOL', 'GROUP_MA_SEG2', ioc, iarg, 1,&
                        nogp, ngp)
        endif
        if (nrep .ne. 0) then
            do 32 i = 1, nrd
                if (rep .eq. repdis(i)) irep = i
32          continue
        endif
!        POUR EUROPLEXUS
        lbid = .false.
        call getvtx('RIGI_PARASOL', 'EUROPLEXUS', 1, iarg, 1,&
                    k8bid, ibid)
        if (ibid .ne. 0) then
            lbid = ( k8bid(1:3) .eq. 'OUI' )
        endif
        if (lbid .neqv. eurplx) then
            call u2mesi('F', 'MODELISA9_93', 1, ioc)
        endif
!        UNITE POUR IMPRIMER LES VALEUR DES DISCRETS
        call getvis('RIGI_PARASOL', 'UNITE', ioc, iarg, 1,&
                    ibid, ier)
        iunite = -1
        if (ier .ne. 0) then
            iunite = ibid
        endif
!
        if (ncar .gt. 0) ncarac = ncar
        if (iunite .gt. 0) then
            write(iunite,1000) rep,ioc
        endif
!
! ---    "GROUP_MA" = TOUTES LES MAILLES DE TOUS LES GROUPES DE MAILLES
        if (ng .le. 0) goto 30
!
        ii = 0
        do 34 nc = 1, ncarac
            if ((nc.eq.2) .and. (car(1)(1:1).eq.car(2)(1:1))) call u2mess('F', 'MODELISA_16')
!           DISCRETS SEULEMENT EN TRANSLATION
            transl = (car(nc)(1:7) .eq. 'K_T_D_N') .or. (car(nc)(1:7) .eq. 'K_T_D_L') .or.&
                     (car(nc)(1:7) .eq. 'A_T_D_N') .or. (car(nc)(1:7) .eq. 'A_T_D_L')
!           DISCRETS EN TRANSLATION ET ROTATION
            trarot = (car(nc)(1:8) .eq. 'K_TR_D_N') .or. (car(nc)(1:8) .eq. 'K_TR_D_L') .or.&
                     (car(nc)(1:8) .eq. 'A_TR_D_N') .or. (car(nc)(1:8) .eq. 'A_TR_D_L')
!
            if (transl .eqv. trarot) call u2mesk('F', 'MODELISA_17', 1, car(nc))
!
            if (transl) then
                lamass = 'M'//car(nc)(2:7)
                if (ii+3 .gt. nval) call u2mess('F', 'DISCRETS_21')
                do 132 j = 1, 3
                    vale(j) = val(ii+j)
132              continue
                call rairep(noma, ioc, car(nc), vale, ng,&
                            zk24(jdls), nbno, zk8(itbno), zr(irgno), zr(irgto),&
                            zr(iamto), rirot, ndim)
                ii = ii + 3
            else if (trarot) then
                lamass = 'M'//car(nc)(2:8)
                if (ii+6 .gt. nval) call u2mess('F', 'DISCRETS_21')
                do 131 j = 1, 6
                    vale(j) = val(ii+j)
131              continue
                call rairep(noma, ioc, car(nc), vale, ng,&
                            zk24(jdls), nbno, zk8(itbno), zr(irgno), zr(irgto),&
                            zr(iamto), rirot, ndim)
                ii = ii + 6
            else
                call assert(.false.)
            endif
!
            do 255 ino = 1, nbno
                zk8(itbmp + ino - 1) = ' '
255          continue
!
!
            if (ixnw .ne. 0 .and. ngp .eq. 0) then
                do 39 i = 1, nbno
                    itrou = 0
                    do 100 k = 1, nbmtrd
                        numnoe = zi(jdnw+k*2-2)
                        call jenuno(jexnum(mlgnno, numnoe), nomnoe)
                        if (zk8(itbno+i-1) .eq. nomnoe) then
                            itrou = 1
                            goto 101
                        endif
100                  continue
101                  continue
                    if (itrou .eq. 0) then
                        call u2mesk('F', 'MODELISA_18', 1, zk8(itbno+i-1))
                    endif
39              continue
            else if (ixnw.eq.0.and.ngp.eq.0) then
                call u2mess('F', 'MODELISA_19')
            endif
            if (ngp .ne. 0) then
                nbnoeu = 0
                lokm = 0
                if (transl) lokm = 7
                if (trarot) lokm = 8
                if (car(nc)(lokm:lokm) .eq. 'N') nbnoeu = 1
                if (car(nc)(lokm:lokm) .eq. 'L') nbnoeu = 2
                call assert((nbnoeu.gt.0).and.(lokm.gt.0))
!
                call jelira(jexnom(noma//'.GROUPEMA', nogp), 'LONMAX', nma, k8bid)
                call jeveuo(jexnom(noma//'.GROUPEMA', nogp), 'L', ldgm)
!
                if (nma .ne. nbno) then
                    vali(1) = nbno
                    vali(2) = nma
                    call u2mesg('F', 'MODELISA2_10', 1, nogp, 2,&
                                vali, 0, r8bid)
                endif
!
!
                do 22 in = 0, nma-1
!                 RECUPERE LE NOMBRE DE NOEUD DE LA MAILLE
                    call jelira(jexnum(noma//'.CONNEX', zi(ldgm+in)), 'LONMAX', nbnma, k8bid)
                    call jeveuo(jexnum(noma//'.CONNEX', zi(ldgm+in)), 'L', ldnm)
                    call jenuno(jexnum(mlgnma, zi(ldgm+in)), nommai)
!                 BOUCLE SUR LE NB DE NOEUD DE LA MAILLE
                    if (nbnma .ne. nbnoeu) then
                        call u2mesk('F', 'MODELISA_20', 1, nommai)
                    endif
                    do 25 inbn = 1, nbnma
                        inoe = zi(ldnm+inbn-1)
                        call jenuno(jexnum(mlgnno, inoe), nomnoe)
                        do 24 ino = 1, nbno
                            if (zk8(itbno+ino-1) .eq. nomnoe) then
                                zk8(itbmp+ino-1) = nommai
!
                                goto 22
                            endif
24                      continue
25                  continue
!                 SI ON PASSE ICI AUCUN DES NOEUDS DU DISCRET APPARTIENT
!                 A LA SURFACE, ET CE N'EST PAS NORMAL
                    write(ifm,*)'GROUP_MA :', (' '//zk24(jdls+ii-1),&
                    ii=1,ng)
                    call u2mesk('F', 'MODELISA_21', 1, nomnoe)
22              continue
!              PREPARATION DES IMPRESSIONS DANS LE FICHIER MESSAGE
!              IFR = IUNIFI('RESULTAT')
                lorep = 5
                if (irep .eq. 1) lorep = 6
                if (iunite .gt. 0) then
                    if (transl) then
                        write(iunite,1005) car(nc)(1:lokm)
                    else
                        write(iunite,1006) car(nc)(1:lokm), rirot(1),&
                        rirot(2),rirot(3)
                    endif
                endif
!
!            VERIF QU'UN DISCRET EST FIXE A CHACUN DES NOEUDS DU RADIER
!            (UNE SEULE FOIS PAR OCCURRENCE DE RIGI_PARASOL)
                if (nc .eq. 1) then
                    do 227 ino = 1, nbno
                        if (zk8(itbmp + ino - 1) .eq. ' ') then
                            call jenuno(jexnum(mlgnno, ino), nomnoe)
                            call u2mesk('F', 'MODELISA2_8', 1, nomnoe)
                        endif
227                  continue
                endif
!
                if (iunite .gt. 0) then
                    do 27 i = 1, nbno
                        iv = 1
                        jd = itbmp + i - 1
                        jn = itbno + i - 1
                        if (nbnoeu .eq. 1) then
                            if (transl) then
                                write(iunite,1010) 'NOEUD',zk8(jn),&
                                car(nc)(1:lokm), (zr(irgno+6*i-6+jj),&
                                jj=0,2), repdis(irep)(1:lorep)
                            else
                                write(iunite,1011) 'NOEUD',zk8(jn),&
                                car(nc)(1:lokm), (zr(irgno+6*i-6+jj),&
                                jj=0,5), repdis(irep)(1:lorep)
                            endif
                        else
                            if (transl) then
                                write(iunite,1010) 'MAILLE',zk8(jd),&
                                car(nc)(1:lokm), (zr(irgno+6*i-6+jj),&
                                jj=0,2), repdis(irep)(1:lorep)
                            else
                                write(iunite,1011) 'MAILLE',zk8(jd),&
                                car(nc)(1:lokm), (zr(irgno+6*i-6+jj),&
                                jj=0,5), repdis(irep)(1:lorep)
                            endif
                        endif
27                  continue
                endif
!
                do 28 i = 1, nbno
                    iv = 1
                    jd = itbmp + i - 1
                    jn = itbno + i - 1
!                 POUR EUROPLEXUS PREPARATION DE L'ATTRIBUT PYTHON
                    if (eurplx) then
                        if (nbnoeu .eq. 1) then
!
                            if (car(nc)(1:3) .eq. 'K_T') then
                                if (transl) then
                                    do 666 jj = 0, 2
                                        zr(irepv+6*ir+jj)=zr(irgno+6*&
                                        i-6+jj)
                                        zr(irepv+6*ir+3+jj)=0.d0
666                                  continue
                                else
                                    do 667 jj = 0, 5
                                        zr(irepv+6*ir+jj)=zr(irgno+6*&
                                        i-6+jj)
667                                  continue
                                endif
                                zk8(irepn+ir) = zk8(jn)
                                ir = ir + 1
                            else if (car(nc)(1:3) .eq. 'A_T') then
                                if (transl) then
                                    do 668 jj = 0, 2
                                        zr(iaepv+6*ia+jj)=zr(irgno+6*&
                                        i-6+jj)
                                        zr(iaepv+6*ia+3+jj)=0.d0
668                                  continue
                                else
                                    do 669 jj = 0, 5
                                        zr(iaepv+6*ia+jj)=zr(irgno+6*&
                                        i-6+jj)
669                                  continue
                                endif
                                zk8(iaepn+ia) = zk8(jn)
                                ia = ia + 1
                            endif
!
                        else
                            call u2mesk('A', 'MODELISA9_96', 1, zk8(jd))
                        endif
                    endif
!                 AFFECTATION DES VALEURS REPARTIES
                    call affdis(ndim, irep, eta, car(nc), zr(irgno+6*i-6),&
                                jdc, jdv, ivr, iv, kma,&
                                ncmp, l, jdcinf, jdvinf, isym,&
                                ifm)
                    call nocart(cartdi, 3, ' ', 'NOM', 1,&
                                zk8(jd), 0, ' ', dimcar)
                    call nocart(cart(l), 3, ' ', 'NOM', 1,&
                                zk8(jd), 0, ' ', ncmp)
!                 AFFECTATION DE MATRICE MASSE NULLE
                    iv = 1
!
                    call r8inir(nbval, 0.0d0, vale, 1)
                    call affdis(ndim, irep, eta, lamass, vale,&
                                jdc, jdv, ivr, iv, kma,&
                                ncmp, l, jdcinf, jdvinf, isym,&
                                ifm)
                    call nocart(cartdi, 3, ' ', 'NOM', 1,&
                                zk8(jd), 0, ' ', dimcar)
                    call nocart(cart(l), 3, ' ', 'NOM', 1,&
                                zk8(jd), 0, ' ', ncmp)
28              continue
            else
                lokm = 0
                if (transl) lokm = 7
                if (trarot) lokm = 8
                lorep = 5
                if (irep .eq. 1) lorep = 6
                if (iunite .gt. 0) then
                    if (transl) then
                        write(iunite,1005) car(nc)(1:lokm)
                    else
                        write(iunite,1006) car(nc)(1:lokm), rirot(1),&
                        rirot(2),rirot(3)
                    endif
                    do 35 i = 1, nbno
                        iv = 1
                        jd = itbno + i - 1
                        if (transl) then
                            write(iunite,1010) 'NOEUD',zk8(jd),&
                            car(nc)(1:lokm), (zr(irgno+6*i-6+jj),jj=0,&
                            2), repdis(irep)(1:lorep)
                        else
                            write(iunite,1011) 'NOEUD',zk8(jd),&
                            car(nc)(1:lokm), (zr(irgno+6*i-6+jj),jj=0,&
                            5), repdis(irep)(1:lorep)
                        endif
35                  continue
                endif
                do 36 i = 1, nbno
                    iv = 1
                    jd = itbno + i - 1
                    call crlinu('NOM', mlgnno, 1, ibid, zk8(jd),&
                                nbmtrd, zi(jdnw), zi(jddi), kk)
                    call affdis(ndim, irep, eta, car(nc), zr(irgno+6*i-6),&
                                jdc, jdv, ivr, iv, kma,&
                                ncmp, l, jdcinf, jdvinf, isym,&
                                ifm)
                    call nocart(cartdi, -3, ' ', 'NUM', kk,&
                                ' ', zi(jddi), ligmo, dimcar)
                    call nocart(cart(l), -3, ' ', 'NUM', kk,&
                                ' ', zi(jddi), ligmo, ncmp)
!                 AFFECTATION DE MATRICE MASSE NULLE
                    iv = 1
                    call r8inir(nbval, 0.0d0, vale, 1)
                    call affdis(ndim, irep, eta, lamass, vale,&
                                jdc, jdv, ivr, iv, kma,&
                                ncmp, l, jdcinf, jdvinf, isym,&
                                ifm)
                    call nocart(cartdi, -3, ' ', 'NUM', kk,&
                                ' ', zi(jddi), ligmo, dimcar)
                    call nocart(cart(l), -3, ' ', 'NUM', kk,&
                                ' ', zi(jddi), ligmo, ncmp)
36              continue
            endif
34      continue
        if (ii .ne. nval) call u2mess('F', 'DISCRETS_21')
30  end do
!
    if (ixnw .ne. 0) call jedetr(tmpdis)
    call jedetr('&&TMPDISCRET')
    call jedetr('&&TMPTABNO')
    call jedetr('&&TMPRIGNO')
    call jedetr('&&TMPRIGTO')
    call jedetr('&&TMPAMOTO')
    call jedetr('&&TMPTABMP')
    call getfac('RIGI_MISS_3D', nborm)
    if (nborm .eq. 0) then
        do 240 i = 1, 3
            call jedetr(tmpnd(i))
            call jedetr(tmpvd(i))
240      continue
        call jedetr(tmcinf)
        call jedetr(tmvinf)
    endif
!
    call jedema()
!
    1000 format(/,&
     &    ' <DISCRET> MATRICES AFFECTEES AUX ELEMENTS DISCRET ',&
     &    '(REPERE ',a6,'), OCCURRENCE ',i4)
    1005 format(/,' PAS DE REPARTITION EN ROTATION POUR DES ',a,/)
    1006 format(/,' RAIDEURS DE ROTATION A REPARTIR POUR DES ',a,/&
     &        ,'  RX: ',1pe12.5,' RY: ',1pe12.5,' RZ: ',1pe12.5,/)
    1010 format(' _F(',a,'=''',A8,''', CARA=''',A,''',',/,&
     &       '    VALE=(',3(1x,1pe12.5,','),'),',/,&
     &       '    REPERE=''',A,'''),')
    1011 format(' _F(',a,'=''',A8,''', CARA=''',A,''',',/,&
     &       '    VALE=(',3(1x,1pe12.5,','),/,&
     &       '          ',3(1x,1pe12.5,','),'),',/,&
     &       '    REPERE=''',A,'''),')
end subroutine
