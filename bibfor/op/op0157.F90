subroutine op0157()
    implicit none
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
!     PROCEDURE IMPR_GENE
!     ------------------------------------------------------------------
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/gettco.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/irgene.h'
    include 'asterfort/irtitr.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/rsinfo.h'
    include 'asterfort/rstran.h'
    include 'asterfort/rsutnu.h'
    include 'asterfort/ulexis.h'
    include 'asterfort/ulopen.h'
    include 'asterfort/wkvect.h'
    character(len=3) :: toucha, toucmp, toupar, interp
    character(len=4) :: motfac
    character(len=8) :: k8b, form
    character(len=16) :: nomcmd, typcon, crit, k16bid, fich
    character(len=19) :: gene, knum, kdisc, krang
    character(len=80) :: titre
    logical :: lhist
    integer :: iarg
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ibid, ifi, iocc, ire2, iret, isy, jcmpg
    integer :: jdisc, jnosy, jordr, jpara, jrang, n, n01
    integer :: n10, n11, n21, n22, nbcmpg, nbdisc, nbnosy
    integer :: nbordr, nbpara, nc, nocc, np, nr
    real(kind=8) :: prec
!-----------------------------------------------------------------------
    call jemarq()
    call infmaj()
    call getres(k8b, k8b, nomcmd)
!
!     --- FORMAT ---
!
    call getvtx(' ', 'FORMAT', 1, iarg, 1,&
                form, n)
!
!     --- FICHIER ---
!
    ifi = 0
    fich = ' '
    call getvis(' ', 'UNITE', 1, iarg, 1,&
                ifi, n)
    if (.not. ulexis( ifi )) then
        call ulopen(ifi, ' ', fich, 'NEW', 'O')
    endif
!
    motfac = 'GENE'
    call getfac(motfac, nocc)
!
    do 10 iocc = 1, nocc
!
!        --- SEPARATION DES DIFFERENTES OCCURENCES---
!
        if (form .eq. 'RESULTAT') write(ifi,'(/,1X,80(''-''))')
!
        call getvid(motfac, 'RESU_GENE', iocc, iarg, 1,&
                    gene, nr)
        call gettco(gene, typcon)
!
!        --- ECRITURE DU TITRE ---
!
        k8b = '        '
        call irtitr(gene, k8b, form, ifi, titre)
!
!        --- IMPRESSION DE LA STRUCTURE DU RESU_GENE ---
!
        if (typcon .eq. 'MODE_GENE') then
            call getvtx(motfac, 'INFO_GENE', iocc, iarg, 1,&
                        k8b, n01)
            if (k8b(1:3) .eq. 'OUI') call rsinfo(gene, ifi)
        endif
!
!        --- QUELS SONT LES NOM_CHAMP A IMPRIMER ---
!
        toucha = 'OUI'
        call getvtx(motfac, 'TOUT_CHAM', iocc, iarg, 1,&
                    toucha, n21)
        call getvtx(motfac, 'NOM_CHAM', iocc, iarg, 0,&
                    k16bid, n22)
        if (n22 .lt. 0) then
            nbnosy = - n22
            call wkvect('&&OP0157.NOM_SYMB', 'V V K16', nbnosy, jnosy)
            call getvtx(motfac, 'NOM_CHAM', iocc, iarg, nbnosy,&
                        zk16( jnosy), n)
        else if (toucha .eq. 'OUI') then
            if (typcon .eq. 'MODE_GENE') then
                call jelira(gene//'.DESC', 'NOMUTI', nbnosy, k8b)
                call wkvect('&&OP0157.NOM_SYMB', 'V V K16', nbnosy, jnosy)
                do 20 isy = 1, nbnosy
                    call jenuno(jexnum(gene//'.DESC', isy), zk16(jnosy- 1+isy))
20              continue
            else
                nbnosy = 3
                call wkvect('&&OP0157.NOM_SYMB', 'V V K16', nbnosy, jnosy)
                zk16(jnosy+1-1) = 'DEPL'
                zk16(jnosy+2-1) = 'VITE'
                zk16(jnosy+3-1) = 'ACCE'
            endif
        else if (toucha .eq. 'NON') then
            nbnosy = 0
            jnosy = 1
        endif
!
!        --- QUELS SONT LES CMP_GENE A IMPRIMER ---
!
        nbcmpg = -1
        jcmpg = 1
        toucmp = '   '
        call getvtx(motfac, 'TOUT_CMP_GENE', iocc, iarg, 1,&
                    toucmp, n21)
        call getvis(motfac, 'NUME_CMP_GENE', iocc, iarg, 0,&
                    ibid, n22)
        if (toucmp .eq. 'NON') then
            nbcmpg = 0
        else if (n22 .lt. 0) then
            nbcmpg = -n22
            call wkvect('&&OP0157.NOM_CMPG', 'V V I', nbcmpg, jcmpg)
            call getvis(motfac, 'NUME_CMP_GENE', iocc, iarg, nbcmpg,&
                        zi(jcmpg), n)
        endif
!
!        --- ON RECHERCHE LES PARAMETRES A ECRIRE ---
!
        nbpara = -1
        jpara = 1
        toupar = '   '
        call getvtx(motfac, 'TOUT_PARA', iocc, iarg, 1,&
                    toupar, n11)
        call getvtx(motfac, 'NOM_PARA', iocc, iarg, 0,&
                    k8b, n10)
        if (toupar .eq. 'NON') then
            nbpara = 0
        else if (n10 .ne. 0) then
            nbpara = -n10
            call wkvect('&&OP0157.NOMUTI_PARA', 'V V K16', nbpara, jpara)
            call getvtx(motfac, 'NOM_PARA', iocc, iarg, nbpara,&
                        zk16( jpara), n)
        endif
!
!        --- LES ACCES ---
!
        nbordr = 0
        jordr = 1
        nbdisc = 0
        jdisc = 1
        jrang = 1
        if (typcon .eq. 'MODE_GENE') then
            knum = '&&OP0157.NUME_ORDRE'
            call getvr8(motfac, 'PRECISION', iocc, iarg, 1,&
                        prec, np)
            call getvtx(motfac, 'CRITERE', iocc, iarg, 1,&
                        crit, nc)
            call rsutnu(gene, motfac, iocc, knum, nbordr,&
                        prec, crit, iret)
            if (iret .ne. 0) goto 12
            call jeveuo(knum, 'L', jordr)
            elseif ((typcon.eq.'HARM_GENE') .or.(typcon.eq.'TRAN_GENE'))&
        then
            kdisc = '&&OP0157.DISCRET'
            krang = '&&OP0157.NUME_ORDRE'
            interp = 'NON'
            call rstran(interp, gene, motfac, iocc, kdisc,&
                        krang, nbdisc, iret)
            if (iret .ne. 0) goto 12
            call jeexin(kdisc, ire2)
            if (ire2 .gt. 0) then
                call jeveuo(kdisc, 'E', jdisc)
                call jeveuo(krang, 'E', jrang)
            endif
        endif
!
!        --- HISTOR ---
!
        lhist = .true.
        call getvtx(motfac, 'INFO_CMP_GENE', iocc, iarg, 1,&
                    k8b, n)
        if (k8b(1:3) .eq. 'NON') lhist = .false.
!
        call irgene(iocc, gene, form, ifi, nbnosy,&
                    zk16(jnosy), nbcmpg, zi(jcmpg), nbpara, zk16(jpara),&
                    nbordr, zi(jordr), nbdisc, zr(jdisc), zi(jrang),&
                    lhist)
!
12      continue
        call jedetr('&&OP0157.NOM_SYMB')
        call jedetr('&&OP0157.NOM_CMPG')
        call jedetr('&&OP0157.NOMUTI_PARA')
        call jedetr('&&OP0157.NUME_ORDRE')
        call jedetr('&&OP0157.DISCRET')
10  end do
!
    call jedema()
end subroutine
