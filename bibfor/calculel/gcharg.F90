subroutine gcharg(modele, nchar, lchar, chvolu, cf1d2d,&
                  cf2d3d, chpres, chepsi, chpesa, chrota,&
                  fonc, epsi, time, iord)
    implicit none
    include 'jeveux.h'
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterfort/codent.h'
    include 'asterfort/copisd.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/gcharf.h'
    include 'asterfort/gcharm.h'
    include 'asterfort/gverfo.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/mefor0.h'
    include 'asterfort/mepres.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/tecart.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: nchar, iord
    character(len=8) :: modele, lchar(*)
    character(len=19) :: chvolu, cf1d2d, cf2d3d, chpres, chepsi, chpesa, chrota
    logical :: fonc, epsi
    real(kind=8) :: time
! ----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! ----------------------------------------------------------------------
!
    integer :: ibid, i, if3d3d, if2d2d, if1d2d, if2d3d, ipress, iepsin, irota
    integer :: ipesa, iret, n1, nexci, jpara, jfcha, nres, nvolu, n1d2d, n2d3d
    integer :: npress, nepsi, nrota, npesa, iivolu, ii1d2d, ii2d3d, iipres
    integer :: jfonci, isvolu, is1d2d, is2d3d, ispres
    character(len=8) :: k8b, resu, nomf, nomchf
    character(len=16) :: type, oper
    character(len=19) :: charg, chtmp1, chtmp2
    character(len=24) :: nomfct
    character(len=24) :: excisd
    logical :: lchsd, fonci, fonc1, fonc2
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call getres(k8b, type, oper)
    chtmp1='&&CHARGE_INTERM1'
    chtmp2='&&CHARGE_INTERM2'
    nomchf='&FM00000'
!
    fonc = .false.
    epsi = .false.
    nvolu = 0
    n1d2d = 0
    n2d3d = 0
    npress = 0
    nepsi = 0
    nrota = 0
    npesa = 0
!
    iivolu=0
    ii1d2d=0
    ii2d3d=0
    iipres=0
!
    isvolu=0
    is1d2d=0
    is2d3d=0
    ispres=0
!
    call getfac('EXCIT', nexci)
    call getvid(' ', 'RESULTAT', 0, iarg, 1,&
                resu, nres)
    lchsd=.false.
    if (nres .ne. 0 .and. nexci .eq. 0) lchsd=.true.
    if (nchar .gt. 0) call wkvect('&&GCHARG.FONCI', 'V V L', nchar, jfonci)
!
!--- LECTURE DES INFORMATIONS CONTENUES DANS LA SD RESULTAT
!
    if (lchsd) then
        call rsadpa(resu, 'L', 1, 'EXCIT', iord,&
                    0, jpara, k8b)
        excisd = zk24(jpara)
        call jeexin(excisd(1:19)//'.FCHA', iret)
        if (iret .eq. 0) goto 99
        call jeveuo(excisd(1:19)//'.FCHA', 'L', jfcha)
    endif
!
!
    do 10 i = 1, nchar
!
        nomchf(7:8)='00'
!
        if (lchar(i) .ne. ' ') then
!
            call dismoi('F', 'TYPE_CHARGE', lchar(i), 'CHARGE', ibid,&
                        k8b, iret)
            if (k8b(5:7) .eq. '_FO') then
                fonc = .true.
                fonci = .true.
            else
                fonci = .false.
            endif
            zl(jfonci+i-1)=fonci
!
!  ---   CHVOLU  ---
!
            call jeexin(lchar(i)//'.CHME.F3D3D.DESC', if3d3d)
            call jeexin(lchar(i)//'.CHME.F2D2D.DESC', if2d2d)
            if (if3d3d .ne. 0) then
                charg= lchar(i)//'.CHME.F3D3D'
            else if (if2d2d .ne. 0) then
                charg= lchar(i)//'.CHME.F2D2D'
            endif
            if (if3d3d .ne. 0 .or. if2d2d .ne. 0) then
                if (lchsd) then
                    n1 = 1
                    nomfct = zk24(jfcha-1+i)
                    if (nomfct(1:2) .eq. '&&' .or. nomfct(1:1) .eq. ' ') n1 = 0
                else
                    call getvid('EXCIT', 'FONC_MULT', i, iarg, 1,&
                                nomfct, n1)
                endif
!
                if (n1 .ne. 0) then
                    if (fonci) then
                        call codent(i*10+1, 'D0', nomchf(6:7))
                        nomf=nomchf
                    else
                        nomf=' '
                    endif
                endif
!
                if (fonci) then
                    call gverfo(charg, iret)
                    if (iret .eq. 1) isvolu=isvolu+1
                endif
!
                if (nvolu .eq. 0) then
                    nvolu = nvolu + 1
                    call copisd('CHAMP_GD', 'V', charg, chvolu)
                    if (n1 .ne. 0) then
                        call gcharm(fonci, charg, nomfct, nomf, time,&
                                    iord, chvolu)
                    endif
                else
                    if (isvolu .ge. 1) call u2mess('F', 'CALCULEL5_57')
                    call copisd('CHAMP_GD', 'V', chvolu, chtmp1)
                    call copisd('CHAMP_GD', 'V', charg, chtmp2)
                    call detrsd('CHAMP_GD', chvolu)
                    if (n1 .ne. 0) then
                        call gcharm(fonci, charg, nomfct, nomf, time,&
                                    iord, chtmp2)
                    endif
                    fonc1=zl(jfonci+iivolu-1)
                    fonc2=zl(jfonci+i-1)
!
!            -- LA ROUTINE GCHS2F NE FONCTIONNE QUE SUR DES CARTES SUR
!               LESQUELLES ON A FAIT UN "CALL TECART" :
                    call tecart(chtmp1)
                    call tecart(chtmp2)
                    call gcharf(i, fonc1, chtmp1, fonc2, chtmp2,&
                                chvolu)
                    call detrsd('CHAMP_GD', chtmp1)
                    call detrsd('CHAMP_GD', chtmp2)
                endif
                iivolu=i
!
            endif
!
!  ---   CF1D2D  ---
!
            call jeexin(lchar(i)//'.CHME.F1D2D.DESC', if1d2d)
            if (if1d2d .ne. 0) then
!
                if (lchsd) then
                    n1 = 1
                    nomfct = zk24(jfcha-1+i)
                    if (nomfct(1:2) .eq. '&&' .or. nomfct(1:1) .eq. ' ') n1 = 0
                else
                    call getvid('EXCIT', 'FONC_MULT', i, iarg, 1,&
                                nomfct, n1)
                endif
!
                charg= lchar(i)//'.CHME.F1D2D'
                if (n1 .ne. 0) then
                    if (fonci) then
                        call codent(i*10+2, 'D0', nomchf(6:7))
                        nomf=nomchf
                    else
                        nomf=' '
                    endif
                endif
!
                if (fonci) then
                    call gverfo(charg, iret)
                    if (iret .eq. 1) is1d2d=is1d2d+1
                endif
!
                if (n1d2d .eq. 0) then
                    n1d2d = n1d2d + 1
                    call copisd('CHAMP_GD', 'V', charg, cf1d2d)
                    if (n1 .ne. 0) then
                        call gcharm(fonci, charg, nomfct, nomf, time,&
                                    iord, cf1d2d)
                    endif
                else
                    if (is1d2d .ge. 1) call u2mess('F', 'CALCULEL5_57')
                    call copisd('CHAMP_GD', 'V', cf1d2d, chtmp1)
                    call copisd('CHAMP_GD', 'V', charg, chtmp2)
                    call detrsd('CHAMP_GD', cf1d2d)
                    if (n1 .ne. 0) then
                        call gcharm(fonci, charg, nomfct, nomf, time,&
                                    iord, chtmp2)
                    endif
                    fonc1=zl(jfonci+ii1d2d-1)
                    fonc2=zl(jfonci+i-1)
!            -- LA ROUTINE GCHS2F NE FONCTIONNE QUE SUR DES CARTES SUR
!               LESQUELLES ON A FAIT UN "CALL TECART" :
                    call tecart(chtmp1)
                    call tecart(chtmp2)
                    call gcharf(i, fonc1, chtmp1, fonc2, chtmp2,&
                                cf1d2d)
                    call detrsd('CHAMP_GD', chtmp1)
                    call detrsd('CHAMP_GD', chtmp2)
                endif
                ii1d2d=i
!
            endif
!
!  ---   CF2D3D  ---
!
            call jeexin(lchar(i)//'.CHME.F2D3D.DESC', if2d3d)
            if (if2d3d .ne. 0) then
!
                if (lchsd) then
                    n1 = 1
                    nomfct = zk24(jfcha-1+i)
                    if (nomfct(1:2) .eq. '&&' .or. nomfct(1:1) .eq. ' ') n1 = 0
                else
                    call getvid('EXCIT', 'FONC_MULT', i, iarg, 1,&
                                nomfct, n1)
                endif
!
                charg= lchar(i)//'.CHME.F2D3D'
                if (n1 .ne. 0) then
                    if (fonci) then
                        call codent(i*10+3, 'D0', nomchf(6:7))
                        nomf=nomchf
                    else
                        nomf=' '
                    endif
                endif
!
                if (fonci) then
                    call gverfo(charg, iret)
                    if (iret .eq. 1) is2d3d=is2d3d+1
                endif
!
                if (n2d3d .eq. 0) then
                    n2d3d = n2d3d +1
                    call copisd('CHAMP_GD', 'V', charg, cf2d3d)
                    if (n1 .ne. 0) then
                        call gcharm(fonci, charg, nomfct, nomf, time,&
                                    iord, cf2d3d)
                    endif
                else
                    if (is2d3d .ge. 1) call u2mess('F', 'CALCULEL5_57')
                    call copisd('CHAMP_GD', 'V', cf2d3d, chtmp1)
                    call copisd('CHAMP_GD', 'V', charg, chtmp2)
                    call detrsd('CHAMP_GD', cf2d3d)
                    if (n1 .ne. 0) then
                        call gcharm(fonci, charg, nomfct, nomf, time,&
                                    iord, chtmp2)
                    endif
                    fonc1=zl(jfonci+ii2d3d-1)
                    fonc2=zl(jfonci+i-1)
!            -- LA ROUTINE GCHS2F NE FONCTIONNE QUE SUR DES CARTES SUR
!               LESQUELLES ON A FAIT UN "CALL TECART" :
                    call tecart(chtmp1)
                    call tecart(chtmp2)
                    call gcharf(i, fonc1, chtmp1, fonc2, chtmp2,&
                                cf2d3d)
                    call detrsd('CHAMP_GD', chtmp1)
                    call detrsd('CHAMP_GD', chtmp2)
                endif
                ii2d3d=i
!
            endif
!
!  ---   CHPRES  ---
!
            call jeexin(lchar(i)//'.CHME.PRESS.DESC', ipress)
            if (ipress .ne. 0) then
!
                if (lchsd) then
                    n1 = 1
                    nomfct = zk24(jfcha-1+i)
                    if (nomfct(1:2) .eq. '&&' .or. nomfct(1:1) .eq. ' ') n1 = 0
                else
                    call getvid('EXCIT', 'FONC_MULT', i, iarg, 1,&
                                nomfct, n1)
                endif
!
                charg= lchar(i)//'.CHME.PRESS'
                if (n1 .ne. 0) then
                    if (fonci) then
                        call codent(i*10+4, 'D0', nomchf(6:7))
                        nomf=nomchf
                    else
                        nomf=' '
                    endif
                endif
!
                if (fonci) then
                    call gverfo(charg, iret)
                    if (iret .eq. 1) ispres=ispres+1
                endif
!
                if (npress .eq. 0) then
                    npress = npress + 1
                    call copisd('CHAMP_GD', 'V', charg, chpres)
                    if (n1 .ne. 0) then
                        call gcharm(fonci, charg, nomfct, nomf, time,&
                                    iord, chpres)
                    endif
                else
                    if (ispres .ge. 1) call u2mess('F', 'CALCULEL5_57')
                    call copisd('CHAMP_GD', 'V', chpres, chtmp1)
                    call copisd('CHAMP_GD', 'V', charg, chtmp2)
                    call detrsd('CHAMP_GD', chpres)
                    if (n1 .ne. 0) then
                        call gcharm(fonci, charg, nomfct, nomf, time,&
                                    iord, chtmp2)
                    endif
                    fonc1=zl(jfonci+iipres-1)
                    fonc2=zl(jfonci+i-1)
!            -- LA ROUTINE GCHS2F NE FONCTIONNE QUE SUR DES CARTES SUR
!               LESQUELLES ON A FAIT UN "CALL TECART" :
                    call tecart(chtmp1)
                    call tecart(chtmp2)
                    call gcharf(i, fonc1, chtmp1, fonc2, chtmp2,&
                                chpres)
                    call detrsd('CHAMP_GD', chtmp1)
                    call detrsd('CHAMP_GD', chtmp2)
                endif
                iipres=i
!
!
            endif
!
!  ---   CHEPSI  ---
!
            call jeexin(lchar(i)//'.CHME.EPSIN.DESC', iepsin)
            if (iepsin .ne. 0) then
!
                if (lchsd) then
                    n1 = 1
                    nomfct = zk24(jfcha-1+i)
                    if (nomfct(1:2) .eq. '&&' .or. nomfct(1:1) .eq. ' ') n1 = 0
                else
                    call getvid('EXCIT', 'FONC_MULT', i, iarg, 1,&
                                nomfct, n1)
                endif
!
                charg= lchar(i)//'.CHME.EPSIN'
!
                if (n1 .ne. 0) then
                    if (fonci) then
                        call codent(i*10+5, 'D0', nomchf(6:7))
                        nomf=nomchf
                    else
                        nomf=' '
                    endif
                endif
!
                epsi = .true.
!
                if (nepsi .eq. 0) then
                    nepsi = nepsi + 1
                    call copisd('CHAMP_GD', 'V', charg, chepsi)
                    if (n1 .ne. 0) then
                        call gcharm(fonci, charg, nomfct, nomf, time,&
                                    iord, chepsi)
                    endif
                else
                    call u2mess('F', 'CALCULEL5_58')
                endif
!
            endif
!
!  ---   CHPESA  ---
!
            call jeexin(lchar(i)//'.CHME.PESAN.DESC', ipesa)
            if (ipesa .ne. 0) then
!
                if (lchsd) then
                    n1 = 1
                    nomfct = zk24(jfcha-1+i)
                    if (nomfct(1:2) .eq. '&&' .or. nomfct(1:1) .eq. ' ') n1 = 0
                else
                    call getvid('EXCIT', 'FONC_MULT', i, iarg, 1,&
                                nomfct, n1)
                endif
!
                charg= lchar(i)//'.CHME.PESAN'
!
                if (n1 .ne. 0) then
                    if (fonci) then
                        call codent(i*10+6, 'D0', nomchf(6:7))
                        nomf=nomchf
                    else
                        nomf=' '
                    endif
                endif
!
                if (npesa .eq. 0) then
                    npesa = npesa + 1
                    call copisd('CHAMP_GD', 'V', charg, chpesa)
                    if (n1 .ne. 0) then
                        call gcharm(fonci, charg, nomfct, nomf, time,&
                                    iord, chpesa)
                    endif
                else
                    call u2mess('F', 'CALCULEL5_59')
                endif
!
            endif
!
!
!  ---   CHROTA  ---
!
            call jeexin(lchar(i)//'.CHME.ROTAT.DESC', irota)
            if (irota .ne. 0) then
!
                if (lchsd) then
                    n1 = 1
                    nomfct = zk24(jfcha-1+i)
                    if (nomfct(1:2) .eq. '&&' .or. nomfct(1:1) .eq. ' ') n1 = 0
                else
                    call getvid('EXCIT', 'FONC_MULT', i, iarg, 1,&
                                nomfct, n1)
                endif
!
                charg= lchar(i)//'.CHME.ROTAT'
!
                if (n1 .ne. 0) then
                    if (fonci) then
                        call codent(i*10+7, 'D0', nomchf(6:7))
                        nomf=nomchf
                    else
                        nomf=' '
                    endif
                endif
!
                if (nrota .eq. 0) then
                    nrota = nrota + 1
                    call copisd('CHAMP_GD', 'V', charg, chrota)
                    if (n1 .ne. 0) then
                        call gcharm(fonci, charg, nomfct, nomf, time,&
                                    iord, chrota)
                    endif
                else
                    call u2mess('F', 'CALCULEL5_60')
                endif
!
            endif
        endif
!
10  end do
!
!  -  SI ABSENCE D'UN CHAMP DE FORCES, CREATION D'UN CHAMP NUL
!
99  continue
!
    if (nchar .gt. 0) call jedetr('&&GCHARG.FONCI')
!
    if (nvolu .eq. 0) then
        call mefor0(modele, chvolu, fonc)
    endif
    if (n1d2d .eq. 0) then
        call mefor0(modele, cf1d2d, fonc)
    endif
    if (n2d3d .eq. 0) then
        call mefor0(modele, cf2d3d, fonc)
    endif
    if (npress .eq. 0) then
        call mepres(modele, chpres, fonc)
    endif
!
    call jedema()
end subroutine
