subroutine rvpost(mcf, iocc, dim, i1, i2,&
                  ncheff, xnomcp, nresu, nch19, nlsmac,&
                  nlsnac, nomtab, xnovar)
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
!     ------------------------------------------------------------------
!     PILOTAGE DU POST-TRAITEMENT
!     ------------------------------------------------------------------
! IN  IOCC   : I : INDICE DE L' OCCURENCE
! IN  DIM    : K : '2D' OU '3D'
! IN  I1, I2 : I : REPERAGE DU CHAMP DANS UNE SD RESULTAT_COMPOSE
! IN  XNOMCP : K : NOM DE LA COLLECTION DES NOMS DE CMP
! IN  NCH19  : K : NOM DU CHAMP A TRAITER
! IN  NLSMAC : K : NOM DU VECTEUR DES MAILLES ACTIVES
! IN  NLSNAC : K : NOM DU VECTEUR DES NOEUDS ACTIFS
!     ------------------------------------------------------------------
    implicit   none
!
    include 'jeveux.h'
!
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/extche.h'
    include 'asterfort/extchn.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rvaffe.h'
    include 'asterfort/rvaffm.h'
    include 'asterfort/rvaffs.h'
    include 'asterfort/rvcalq.h'
    include 'asterfort/rvchgr.h'
    include 'asterfort/rvcpnc.h'
    include 'asterfort/rvinfo.h'
    include 'asterfort/rvlieu.h'
    include 'asterfort/rvpste.h'
    include 'asterfort/rvpstm.h'
    include 'asterfort/rvpsts.h'
    include 'asterfort/tuesch.h'
    include 'asterfort/wkvect.h'
    integer :: iocc, i1, i2
    character(len=2) :: dim
    character(len=8) :: nresu
    character(len=16) :: ncheff
    character(len=19) :: nch19, nomtab
    character(len=24) :: xnomcp, nlsmac, nlsnac, xnovar
    character(len=*) :: mcf
!
    integer :: gd, i, idir, ie, niv, iret, isd, jcmpcd, jcmpnc, jdir, jlsmac
    integer :: jlsnac, jnomcp, jsdev, jsdli, jvec1, jvec2, n, n0, n1, n2, n3
    integer :: nbcac, nbcpn, nbcrb, nbmac, nbnac, nboper, nbsd, nr, ifm, ibid
    integer :: ny
    real(kind=8) :: vecty(3)
    logical :: tridim
    character(len=24) :: lscpnc, quant, sdlieu, sdeval, lscpcd
    character(len=24) :: sdev, sdli, sdmoye, sdmail
    character(len=19) :: sdpost, eval, lieu, sdnewr, ssch19
    character(len=16) :: option, oper, operat(2)
    character(len=8) :: k8b, typco, courbe, mailla, repere
    character(len=4) :: docu
    character(len=1) :: ca
    logical :: chok
    integer :: iarg
!
!==================== CORPS DE LA ROUTINE =============================
!
    call jemarq()
    chok = .true.
    call infniv(ifm, niv)
!
    call getvtx(mcf, 'OPERATION', iocc, iarg, 0,&
                k8b, nboper)
    nboper = -nboper
    call getvtx(mcf, 'OPERATION', iocc, iarg, nboper,&
                operat, n0)
!
    if (nch19(1:1) .eq. '&') then
        if (niv .gt. 1) call rvinfo(ifm, iocc, i1, i2, 'E',&
                                    ncheff)
    else
        lscpcd = '&&RVPOST.NOM.CMP.CAND.OC'
        lscpnc = '&&RVPOST.NOM.CMP.NCSR.OC'
        call jeexin(nch19//'.DESC', ibid)
        if (ibid .gt. 0) then
            call jelira(nch19//'.DESC', 'DOCU', n, docu)
            call jeveuo(nch19//'.DESC', 'L', n1)
        else
            call jelira(nch19//'.CELD', 'DOCU', n, docu)
            call jeveuo(nch19//'.CELD', 'L', n1)
        endif
        call jeveuo(jexnum(xnomcp, iocc), 'L', jnomcp)
        call jelira(jexnum(xnomcp, iocc), 'LONMAX', nbcac, k8b)
        call wkvect(lscpcd, 'V V K8', nbcac, jcmpcd)
        call wkvect('&&RVPOST.VAL.DIR', 'V V R', 3, jdir)
        do 10, i = 1, nbcac, 1
        zk8(jcmpcd + i-1) = zk8(jnomcp + i-1)
10      continue
        gd = zi(n1 + 1-1)
        if (niv .gt. 1) call rvinfo(ifm, iocc, i1, i2, 'B',&
                                    ncheff)
        if (nresu(1:1) .eq. ' ') nresu = nch19(1:8)
        call rvcpnc(mcf, iocc, nch19, gd, docu,&
                    nbcac, lscpcd, lscpnc, repere, option,&
                    quant, idir, zr(jdir), iret)
!        /* POSSIBILITE AGRANDISSEMENT DE LSCPCD => ON REFAIT JEVEUO */
        call jeveuo(lscpcd, 'L', jcmpcd)
!
        if (iret .ne. 0) then
            ssch19 = '&&RVPOST.SOUS.CH.GD'
            call jelira(lscpnc, 'LONMAX', nbcpn, k8b)
            call jeveuo(lscpnc, 'L', jcmpnc)
!
            if (docu .eq. 'CHNO') then
                call jelira(nlsnac, 'LONMAX', nbnac, k8b)
                call jeveuo(nlsnac, 'L', jlsnac)
                call extchn(nch19, k8b, zi(jlsnac), zk8(jcmpnc), nbnac,&
                            nbcpn, 'NUMERO', ssch19, mcf, iocc)
!
            else
                call jeexin(nlsnac, ibid)
                if (ibid .gt. 0) then
                    call jelira(nlsnac, 'LONMAX', nbnac, k8b)
                    call jeveuo(nlsnac, 'L', jlsnac)
                else
                    jlsnac = 1
                    nbnac = 0
                endif
                call jelira(nlsmac, 'LONMAX', nbmac, k8b)
                call jeveuo(nlsmac, 'L', jlsmac)
                call extche(nch19, k8b, zi(jlsmac), zk8(jcmpnc), nbmac,&
                            nbcpn, 'NUMERO', ssch19, mcf, iocc,&
                            nbnac, zi( jlsnac))
            endif
!
            call getvr8('ACTION', 'VECT_Y', iocc, iarg, 3,&
                        vecty, ny)
            tridim=ny.ne.0
!
            if (chok) then
                call dismoi('F', 'NOM_MAILLA', nch19, 'CHAMP', i,&
                            mailla, ie)
                call getvid(mcf, 'CHEMIN', iocc, iarg, 0,&
                            k8b, nbcrb)
                nbcrb = -nbcrb
                if (nbcrb .ne. 0) then
                    call getvid(mcf, 'CHEMIN', iocc, iarg, nbcrb,&
                                courbe, n1)
                    typco = 'CHEMIN'
                else
                    typco = 'AUTRE'
                    courbe = '&&YAPAS'
                endif
                sdlieu = '&&RVPOST.NOM.VECT.LIEU'
                sdeval = '&&RVPOST.NOM.VECT.EVAL'
!
                call getvtx(mcf, 'MOYE_NOEUD', iocc, iarg, 1,&
                            k8b, n)
                if (k8b(1:1) .eq. 'O') then
                    ca = 'N'
                else
                    ca = 'E'
                endif
!
                call rvlieu(mailla, typco, courbe, nlsnac, sdlieu)
                call rvpste(dim, sdlieu, ssch19, sdeval, ca)
                call jelira(sdlieu, 'LONMAX', nbsd, k8b)
                call jeveuo(sdlieu, 'L', jsdli)
                call jeveuo(sdeval, 'L', jsdev)
                call getvtx(mcf, 'RESULTANTE', iocc, iarg, 0,&
                            zk80, nr)
                sdnewr = '&&RVPOST.NEW.REPERE'
                if (repere(1:1) .ne. 'G' .and. .not.tridim) then
                    call rvchgr(mailla, courbe, nlsnac, repere, sdnewr,&
                                iret)
                else
                    iret = 1
                endif
!
                if (iret .ne. 0) then
                    sdpost = '&&RVPOST.FINAL.POST'
                    do 100, isd = 1, nbsd, 1
                    if (repere(1:1) .ne. 'G' .and. .not.tridim) then
                        call jeveuo(jexnum(sdnewr//'.VEC1', isd), 'L', jvec1)
                        call jeveuo(jexnum(sdnewr//'.VEC2', isd), 'L', jvec2)
                    else
                        jvec1 = 0
                        jvec2 = 0
                    endif
!
                    sdev = zk24(jsdev + isd-1)
                    sdli = zk24(jsdli + isd-1)
!
                    call rvcalq(iocc, sdev, zr(jvec1), zr(jvec2), repere,&
                                zk8(jcmpcd), nbcpn, nbcac, option, quant,&
                                sdli, idir, zr(jdir), sdpost, courbe)
!
                    if (nr .eq. 0) then
                        if (nboper .eq. 2) then
                            sdmail = sdev(1:19)//'.MAIL'
                            sdmoye = '&&RVPOST.MOYENNE'
                            call rvpstm(sdli, sdpost, sdmoye)
                            call rvaffe(mcf, iocc, sdli, sdpost, sdmail,&
                                        ca, quant, option, repere, nomtab,&
                                        xnovar, ncheff, i1, isd)
                            oper = 'MOYENNE'
                            call rvaffm(mcf, iocc, sdli, sdpost, sdmoye,&
                                        oper, quant, option, repere, nomtab,&
                                        xnovar, ncheff, i1, isd)
                            call jedetr(sdmoye)
!
                        else
                            oper = operat(1)
                            if (oper .eq. 'EXTRACTION') then
                                sdmail = sdev(1:19)//'.MAIL'
                                call rvaffe(mcf, iocc, sdli, sdpost, sdmail,&
                                            ca, quant, option, repere, nomtab,&
                                            xnovar, ncheff, i1, isd)
!
                            else
                                sdmoye = '&&RVPOST.MOYENNE'
                                call rvpstm(sdli, sdpost, sdmoye)
                                call rvaffm(mcf, iocc, sdli, sdpost, sdmoye,&
                                            oper, quant, option, repere, nomtab,&
                                            xnovar, ncheff, i1, isd)
                                call jedetr(sdmoye)
                            endif
                        endif
                    else
                        sdmoye = '&&RVPOST.SOMME'
                        call rvpsts(iocc, sdli, sdpost, sdmoye)
                        call rvaffs(mcf, iocc, sdli, sdpost, sdmoye,&
                                    quant, option, repere, nomtab, ncheff,&
                                    i1, isd)
                        sdmoye(20:24) = '.VALE'
                        call jedetr(sdmoye)
                        sdmoye(20:24) = '.NOCP'
                        call jedetr(sdmoye)
                    endif
!
                    call jedetr(sdpost//'.VALE')
                    call jedetr(sdpost//'.PADR')
                    call jedetr(sdpost//'.PNBN')
                    call jedetr(sdpost//'.NOCP')
                    call jedetr(sdpost//'.PNCO')
                    call jedetr(sdpost//'.PNSP')
100                  continue
!
                endif
                call jeexin(sdnewr//'.VEC1', n1)
                if (n1 .ne. 0) then
                    call jedetr(sdnewr//'.VEC1')
                    call jedetr(sdnewr//'.VEC2')
                endif
                call jelira(sdlieu, 'LONMAX', n, k8b)
                call jeveuo(sdlieu, 'L', n1)
                call jeveuo(sdeval, 'L', n2)
                do 20, i = 1, n, 1
                lieu = zk24(n1 + i-1)(1:19)
                eval = zk24(n2 + i-1)(1:19)
                call jedetr(lieu//'.ABSC')
                call jedetr(lieu//'.REFE')
                call jedetr(lieu//'.DESC')
                call jedetr(lieu//'.NUME')
                call jedetr(lieu//'.COOR')
                call tuesch(eval)
                call jeexin(eval//'.MAIL', n3)
                if (n3 .ne. 0) then
                    call jedetr(eval//'.MAIL')
                endif
20              continue
                call jedetr(sdlieu)
                call jedetr(sdeval)
            endif
            call jedetr(lscpnc)
            call tuesch(ssch19)
        endif
        call jedetr(lscpcd)
        call jedetr('&&RVPOST.VAL.DIR')
    endif
!
    call jedema()
end subroutine
