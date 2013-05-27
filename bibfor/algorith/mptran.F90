subroutine mptran(nombas, nommes, nbmesu, nbmode, basepr,&
                  vnoeud, vrange, vcham)
!
!     ------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!
!     PROJ_MESU_MODAL : CALCUL DES CONTRIBUTIONS MODALES ET CONSTRUCTION
!                       DU TRAN_GENE OU HARM_GENE
!
!     IN  : NOMBAS : NOM DE LA BASE DE PROJECTION
!     IN  : NOMMES : NOM DE LA SD MESURE
!     IN  : NBMESU : NOMBRE DE DDL DE MESURE
!     IN  : NBMODE : NOMBRE DE VECTEURS DE BASE
!     IN  : BASEPR : NOM BASE PROJETEE SUIVANT DIRECTION MESURE
!     IN  : VNOEUD : NOM RANGEMENT NOEUD MESURE
!     IN  : VRANGE : NOM CORRESPONDANCE ORIENTATION SUIVANT LNOEUD
!
    implicit none
!
!
!
!
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/gettco.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/cnocns.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/mdallo.h'
    include 'asterfort/mdallr.h'
    include 'asterfort/mpinv2.h'
    include 'asterfort/mpinvc.h'
    include 'asterfort/mpinvr.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rscrsd.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/scalai.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    character(len=8) :: nomres, nombas, nommes
    character(len=24) :: vrange, vnoeud, basepr, vcham
    integer :: nbmesu, nbmode, jpara, iexi, nbsym
!
    character(len=1) :: typval
    character(len=4) :: k4bid(3), nomsym(3)
    character(len=8) :: k8bid, k8b, scal, kcmp, kreg
    character(len=8) :: modele, chmat, carael
    character(len=16) :: nomcmd, typres, k16bid, nomcha, kcham
    character(len=19) :: chs, chamno, sd2
!
    character(len=24) :: vabs, vmes, typba
!
    logical :: lfonct, zcmplx
!
    integer :: i, j, jabs
    integer :: jdep, jvit, jacc, jpass, jordr, lord, imes, iret, gd
    integer :: labs, lmesu, lcoef, lred, jcnsd, jcnsc, jcnsv, n1
    integer :: ncoef, nfonc, lfonc, null, ibid, jcnsl, nbcmp
    integer :: lvale, lonmax, iocc, numord, ino, icmp, indice, jraid
    integer :: idesc, jcnsk, lrange, lnoeud, nbabs, jord, nbord
    integer :: jbasm, lcham, nbcham, ich, lch, jpames
!
    real(kind=8) :: r8bid, dt, pas, diff, rbid
!
    complex(kind=8) :: cbid
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    data nomcmd/'&PROJ_MESU_MODAL'/, k8b/'        '/, null/0/
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! RECUPERATION DU NOM DU CONCEPT RESULTAT
    call getres(nomres, typres, k16bid)
! RECUPERATION DU CHAMP MESURE : NOMMES
    call getvtx('MODELE_MESURE', 'NOM_CHAM', 1, iarg, 0,&
                nomcha, nbcham)
    if (nbcham .ne. 0) then
        nbcham = -nbcham
    else
        call u2mess('A', 'ALGORITH10_93')
    endif
!
    call wkvect('&&LISTE_CH', 'V V K16', nbcham, lch)
    call getvtx('MODELE_MESURE', 'NOM_CHAM', 1, iarg, nbcham,&
                zk16(lch), ibid)
!
    call getvtx('RESOLUTION', 'REGUL', 1, iarg, 1,&
                kreg, n1)
!
! RECUPERATION DU NOMBRE D ABSCISSES : NBABS
    call rsorac(nommes, 'LONUTI', ibid, r8bid, k8bid,&
                cbid, r8bid, 'ABSOLU', nbabs, 1,&
                ibid)
!
    vabs = '&&ABSCISSES'
    call wkvect(vabs, 'V V R', nbabs, labs)
!
    vmes = '&&MESURE'
!
    call jeveuo(vrange, 'L', lrange)
    call jeveuo(vnoeud, 'L', lnoeud)
    call jeveuo(vcham, 'L', lcham)
!
! RECUPERATION ADRESSE DES NUMEROS D'ORDRE ET DU NOM SYMBOLIQUE
!
    call jeveuo(nommes//'           .ORDR', 'L', lord)
!
    chs = '&&MESURE.CHS'
!
! BOUCLE SUR LES CHAMPS
    do 140 ich = 1, nbcham
        nomcha = zk16(lch-1 +ich)
!
! BOUCLE SUR LES NUMEROS ORDRE
!
        do 110 numord = 1, nbabs
!        -> EXISTENCE DES CHAMPS DANS LA STRUCTURE DE DONNEES MESURE
            call rsexch(' ', nommes, nomcha, zi(lord-1+numord), chamno,&
                        iret)
            if ((numord .le. 1) .and. (ich .eq. 1)) then
                call jeveuo(chamno//'.DESC', 'L', idesc)
                gd = zi(idesc-1 +1)
                scal = scalai(gd)
                typval = scal(1:1)
                if (typval .eq. 'C') then
                    zcmplx = .true.
                    call wkvect(vmes, 'V V C', nbmesu*nbabs, lmesu)
                else
                    zcmplx = .false.
                    call wkvect(vmes, 'V V R', nbmesu*nbabs, lmesu)
                endif
            endif
!
! RECUPERATION DE L ABSCISSE
            if ((typres(1:9).eq.'MODE_GENE') .or. (typres(1:9) .eq.'HARM_GENE')) then
                call rsadpa(nommes, 'L', 1, 'FREQ', numord,&
                            0, jabs, k8bid)
            else if (typres(1:9).eq.'TRAN_GENE') then
                call rsadpa(nommes, 'L', 1, 'INST', numord,&
                            0, jabs, k8bid)
            endif
            zr(labs-1 + numord) = zr(jabs)
!
! TRANSFORMATION DE CHAMNO EN CHAM_NO_S : CHS
            call detrsd('CHAM_NO_S', chs)
            call cnocns(chamno, 'V', chs)
            call jeveuo(chs//'.CNSK', 'L', jcnsk)
            call jeveuo(chs//'.CNSD', 'L', jcnsd)
            call jeveuo(chs//'.CNSC', 'L', jcnsc)
            call jeveuo(chs//'.CNSV', 'L', jcnsv)
            call jeveuo(chs//'.CNSL', 'L', jcnsl)
!
            nbcmp = zi(jcnsd-1 + 2)
!
            do 120 imes = 1, nbmesu
                ino = zi(lnoeud-1 +imes)
                kcmp = zk8(lrange-1 +imes)
                kcham = zk16(lcham-1 +imes)
                do 130 icmp = 1, nbcmp
                    indice = (ino-1)*nbcmp+icmp
                    if ((zk8(jcnsc-1 +icmp) .eq. kcmp) .and. (nomcha .eq. kcham)) then
                        if (zcmplx) then
                            zc(lmesu-1 +(numord-1)*nbmesu+imes) =&
                            zc(jcnsv-1 +indice)
                        else
                            zr(lmesu-1 +(numord-1)*nbmesu+imes) =&
                            zr(jcnsv-1 +indice)
                        endif
                    endif
130              continue
120          continue
!
! FIN BOUCLE SUR NUMERO ORDRE
110      continue
!
! FIN BOUCLE SUR LES CHAMPS
140  continue
!
! GESTION PARAMETRES DE REGULARISATION
    call getvr8('RESOLUTION', 'COEF_PONDER', 1, iarg, 0,&
                r8bid, ncoef)
    call getvid('RESOLUTION', 'COEF_PONDER_F', 1, iarg, 0,&
                k8bid, nfonc)
    iocc = abs(ncoef) + abs(nfonc)
    if ((ncoef .eq. 0) .and. (nfonc .eq. 0)) iocc = 0
!
    if ((iocc .eq. 0) .or. (kreg .eq. 'NON')) then
! CAS SANS REGULARISATION : PAR DEFAUT
        lfonct = .false.
        call wkvect(nomcmd//'.PONDER', 'V V R', nbmode, lcoef)
        do 5 i = 1, nbmode
            zr(lcoef-1 + i) = 0.d0
 5      continue
    else
        call getvr8('RESOLUTION', 'COEF_PONDER', 1, iarg, 0,&
                    r8bid, ncoef)
        if (-ncoef .gt. 0) then
! CAS DE REGULARISATION SOUS FORME DE LISTE DE REELS
            lfonct = .false.
            if (-ncoef .gt. nbmode) then
                call u2mess('F', 'ALGORITH6_27')
            endif
            if (-ncoef .gt. 0) then
                call wkvect(nomcmd//'.PONDER', 'V V R', nbmode, lcoef)
                call getvr8('RESOLUTION', 'COEF_PONDER', 1, iarg, - ncoef,&
                            zr(lcoef), ncoef)
            endif
            if (ncoef .lt. nbmode) then
                call u2mess('I', 'ALGORITH6_28')
                do 10 i = ncoef + 1, nbmode
                    zr(lcoef-1 + i) = zr(lcoef-1 + ncoef)
10              continue
            endif
        else
! CAS DE REGULARISATION SOUS FORME DE LISTE DE FONCTIONS
            lfonct = .true.
            call getvid('RESOLUTION', 'COEF_PONDER_F', 1, iarg, 0,&
                        k8bid, nfonc)
            if (-nfonc .gt. nbmode) call u2mess('F', 'ALGORITH6_29')
            if (-nfonc .gt. 0) then
                call wkvect(nomcmd//'.FONC', 'V V K8', nbmode, lfonc)
                call getvid('RESOLUTION', 'COEF_PONDER_F', 1, iarg, - nfonc,&
                            zk8(lfonc), nfonc)
            endif
            if (nfonc .gt. 0 .and. nfonc .lt. nbmode) then
                call u2mess('I', 'ALGORITH6_30')
                do 200 i = nfonc + 1, nbmode
                    zk8(lfonc-1 + i) = zk8(lfonc-1 + nfonc)
200              continue
            endif
            call wkvect(nomcmd//'.PONDER', 'V V R', nbmode*nbabs, lcoef)
            do 210 i = 1, nbmode
                call jelira(zk8(lfonc-1 + i)//'           .VALE', 'LONMAX', lonmax, k8bid)
                if (lonmax .ne. 2*nbabs) call u2mess('F', 'ALGORITH6_31')
!
                call jeveuo(zk8(lfonc-1 + i)//'           .VALE', 'L', lvale)
                do 220 j = 1, nbabs
                    diff = zr(lvale-1 + j) - zr(labs-1 + j)
                    if (j .eq. 1) then
                        pas = zr(labs + 1) - zr(labs)
                    else
                        pas = zr(labs-1 + j) - zr(labs-1 + j - 1)
                    endif
                    if (abs(diff) .gt. pas*1.d-4) call u2mess('F', 'ALGORITH6_32')
!
                    zr(lcoef-1 + (j - 1)*nbmode + i) = zr( lvale-1 + (lonmax/2) + j )
220              continue
210          continue
        endif
! FIN TEST SUR TYPE DE PONDERATION : REELS / LISTE DE FONCTIONS
    endif
! FIN GESTION PARAMETRES DE REGULARISATION
!
!
! INITIALISATION POUR ALLOCATION DU TRAN_GENE
!
    if (typres(1:9) .eq. 'TRAN_GENE') then
        dt = (zr(labs-1 +nbabs) - zr(labs))/nbabs
    endif
!
! RECUPERATION DE LA MATRICE MODALE PROJETEE
!
    call jeveuo(basepr, 'L', lred)
!
! ALLOCATION DE TRAN_GENE OU HARM_GENE ET RESOLUTION DU SYSTEME
!
    if (.not. zcmplx) then
! SECOND MEMBRE REEL
        if (typres(1:9) .eq. 'HARM_GENE') call u2mess('F', 'ALGORITH6_33')
        if (typres(1:9) .eq. 'TRAN_GENE') then
! ALLOCATION
            call mdallo(nomres, nombas, k8b, k8b, k8b,&
                        nbmode, dt, nbabs, null, k8bid,&
                        k8bid, null, k8bid, null, k8bid,&
                        jdep, jvit, jacc, jpass, jordr,&
                        jabs, ibid, ibid, ibid, ibid,&
                        ibid, ibid, ibid, ibid, k16bid,&
                        ibid, k4bid, 'TRAN', 'GLOB')
! RESOLUTION
            call mpinv2(nbmesu, nbmode, nbabs, zr(lred), zr(lmesu),&
                        zr(lcoef), zr(labs), lfonct, zr(jdep), zr(jvit),&
                        zr(jacc))
!
        else if (typres(1:9).eq.'MODE_GENE') then
            call wkvect(nomcmd//'.RETA', 'V V R', nbmode*nbabs, jdep)
            call mpinvr(nbmesu, nbmode, nbabs, zr(lred), zr(lmesu),&
                        zr(lcoef), zr(labs), lfonct, zr(jdep))
!
            call rscrsd('G', nomres, 'MODE_GENE', nbabs)
            call mdallr(nommes, nomres, nombas, nbmode, nbabs,&
                        zr(jdep), cbid, zcmplx)
        endif
!
    else
! SECOND MEMBRE COMPLEXE
        if (typres(1:9) .eq. 'HARM_GENE') then
!
! ALLOCATION
!         -- DANS PROJ_MESU_MODAL ON REMPLIT TOUJOURS LES TROIS
!            CHAMPS, PEU IMPORTE LE TYPE DE MESURE FOURNI
            nbsym = 3
            nomsym(1) = 'DEPL'
            nomsym(2) = 'VITE'
            nomsym(3) = 'ACCE'
            call mdallo(nomres, nombas, k8b, k8b, k8b,&
                        nbmode, r8bid, nbabs, null, k8bid,&
                        k8bid, null, k8bid, null, k8bid,&
                        jdep, jvit, jacc, ibid, jordr,&
                        jabs, ibid, ibid, ibid, ibid,&
                        ibid, ibid, ibid, ibid, k16bid,&
                        nbsym, nomsym, 'HARM', 'GLOB')
! RESOLUTION
            call mpinvc(nbmesu, nbmode, nbabs, zr(lred), zc(lmesu),&
                        zr(lcoef), zr(labs), lfonct, zc(jdep), zc(jvit),&
                        zc(jacc))
!
!
        else if (typres(1:9).eq.'MODE_GENE') then
! ALLOCATION
            call wkvect(nomcmd//'.RETA', 'V V C', nbmode*nbabs, jdep)
            call wkvect(nomcmd//'.RET1', 'V V C', nbmode*nbabs, jvit)
            call wkvect(nomcmd//'.RET2', 'V V C', nbmode*nbabs, jacc)
! RESOLUTION
            call mpinvc(nbmesu, nbmode, nbabs, zr(lred), zc(lmesu),&
                        zr(lcoef), zr(labs), lfonct, zc(jdep), zc(jvit),&
                        zc(jacc))
!
            call rscrsd('G', nomres, 'MODE_GENE', nbabs)
            call mdallr(nommes, nomres, nombas, nbmode, nbabs,&
                        rbid, zc(jdep), zcmplx)
        else
            call u2mess('F', 'ALGORITH6_33')
        endif
!
    endif
!
!     -- REMPLISSAGE DE L'OBJET .ORDR :
!
    call jeveuo(nomres//'           .ORDR', 'E', jordr)
    do 400 i = 1, nbabs
        if (typres(1:9) .eq. 'MODE_GENE') then
            zi(jordr-1 + i) = i
        else if (typres(1:9).eq.'HARM_GENE') then
            zi(jordr-1 + i) = i
        else
            zi(jordr-1 + i) = i - 1
        endif
400  end do
!     -- REMPLISSAGE DE L'OBJET .PTEM :
    if (typres(1:9) .eq. 'TRAN_GENE') then
        call jeexin(nommes//'           .PTEM', iexi)
        if (iexi .gt. 0) then
            call jeveuo(nommes//'           .PTEM', 'E', jpames)
            do 500 i = 1, nbabs
                zr(jpass-1+i)=zr(jpames-1+i)
500          continue
        endif
    endif
!
!
!     -- REMPLISSAGE DE L'OBJET .NUMO :
    if (typres(1:9) .eq. 'MODE_GENE') then
        do 401 i = 1, nbabs
            call rsadpa(nomres, 'E', 1, 'NUME_MODE', zi(jordr-1+i),&
                        0, jpara, k8b)
            zi(jpara) = i
401      continue
    endif
!     -- REMPLISSAGE DE "FREQ/DISC" :
    if (typres(1:9) .eq. 'TRAN_GENE' .or. typres(1:9) .eq. 'HARM_GENE') then
        call jeveuo(nomres//'           .DISC', 'E', jabs)
    else
        call jeexin(nomres//'           .FREQ', iexi)
        if (iexi .gt. 0) then
            call jeveuo(nomres//'           .FREQ', 'E', jabs)
        endif
    endif
    do 402 i = 1, nbabs
        if (typres(1:9) .eq. 'TRAN_GENE' .or. typres(1:9) .eq. 'HARM_GENE') then
            zr(jabs-1+i) = zr(labs-1+i)
        else
            if (iexi .gt. 0) then
                zr(jabs-1+i) = zr(labs-1+i)
            else
                call rsadpa(nomres, 'E', 1, 'FREQ', zi(jordr-1+i),&
                            0, jpara, k8b)
                zr(jpara) = zr(labs-1 + i)
            endif
        endif
402  end do
!
!
! --- STOCKAGE
    if (typres(1:9) .eq. 'MODE_GENE') then
        call jeveuo(nomres//'           .ORDR', 'L', jord)
        call jelira(nomres//'           .ORDR', 'LONUTI', nbord, k8b)
        call gettco(nombas, typba)
        call jeveuo(nombas//'           .REFD', 'L', jraid)
        typba=zk24(jraid+6)
        if (typba(1:1) .ne. ' ') then
            if (zk24(jraid)(1:8) .eq. '        ') then
                call jeveuo(jexnum(nombas//'           .TACH', 1), 'L', jbasm)
                sd2=zk24(jbasm)(1:8)
                call rsadpa(sd2, 'L', 1, 'MODELE', 1,&
                            0, jpara, k8b)
                modele=zk8(jpara)
                call rsadpa(sd2, 'L', 1, 'CHAMPMAT', 1,&
                            0, jpara, k8b)
                chmat=zk8(jpara)
                call rsadpa(sd2, 'L', 1, 'CARAELEM', 1,&
                            0, jpara, k8b)
                carael=zk8(jpara)
                goto 44
            endif
        endif
!
!       -- POUR LES BASES TYPE MODE_MECA SANS REFERENCE
        if (zk24(jraid)(1:8) .eq. '        ') then
            modele='        '
            chmat ='        '
            carael='        '
            goto 44
        endif
!
        call dismoi('F', 'NOM_MODELE', zk24(jraid)(1:8), 'MATR_ASSE', ibid,&
                    modele, iret)
        call dismoi('F', 'CHAM_MATER', zk24(jraid)(1:8), 'MATR_ASSE', ibid,&
                    chmat, iret)
        call dismoi('F', 'CARA_ELEM', zk24(jraid)(1:8), 'MATR_ASSE', ibid,&
                    carael, iret)
44      continue
!
!
        do 43 i = 1, nbord
            call rsadpa(nomres, 'E', 1, 'MODELE', zi(jordr-1+i),&
                        0, jpara, k8b)
            zk8(jpara)=modele
            call rsadpa(nomres, 'E', 1, 'CHAMPMAT', zi(jordr-1+i),&
                        0, jpara, k8b)
            zk8(jpara)=chmat
            call rsadpa(nomres, 'E', 1, 'CARAELEM', zi(jordr-1+i),&
                        0, jpara, k8b)
            zk8(jpara)=carael
43      continue
    endif
!
    call jedetr(vabs)
    call jedetr(vmes)
    call jedetr('&&LISTE_CH')
    call detrsd('CHAM_NO_S', chs)
!
    call jedema()
!
end subroutine
