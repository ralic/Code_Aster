subroutine op0107()
    implicit   none
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
!     OPERATEUR   POST_ELEM
!     ------------------------------------------------------------------
!
    include 'jeveux.h'
!
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/chpve2.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/medome.h'
    include 'asterfort/peaire.h'
    include 'asterfort/pecage.h'
    include 'asterfort/pecapo.h'
    include 'asterfort/pechli.h'
    include 'asterfort/peecin.h'
    include 'asterfort/peeint.h'
    include 'asterfort/peepot.h'
    include 'asterfort/peingl.h'
    include 'asterfort/pemain.h'
    include 'asterfort/pemima.h'
    include 'asterfort/penorm.h'
    include 'asterfort/peritr.h'
    include 'asterfort/pevolu.h'
    include 'asterfort/peweib.h'
    include 'asterfort/pewext.h'
    include 'asterfort/rsadpa.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsutnu.h'
    include 'asterfort/titre.h'
    include 'asterfort/u2mess.h'
    integer :: nh, iret, jcha, jordr, n1, n2, nbocc, nbordr, nc, nchar, np, nr
    integer :: jpara, ier
    real(kind=8) :: prec
    character(len=4) :: ctyp
    character(len=8) :: k8b, modele, cara, deform, resuco, crit
    character(len=16) :: concep, nomcmd
    character(len=19) :: resu, kcha, knum, tabtyp(3)
    character(len=24) :: mate, chdef
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    call getres(resu, concep, nomcmd)
!
    call getvid(' ', 'RESULTAT', 0, iarg, 1,&
                resuco, nr)
!
    if (nr .eq. 0) resuco='        '
!
    call infmaj()
!
    kcha = '&&OP0107.CHARGES'
!
    call getfac('TRAV_EXT', nbocc)
!                   ----------
    if (nbocc .ne. 0) then
!
        call pewext(resu)
!
    endif
!
!
    call getfac('CHAR_LIMITE', nbocc)
!                   -------------
    if (nbocc .ne. 0) then
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
!
        call pechli(resu, modele, mate)
!
    endif
!
!
    call getfac('AIRE_INTERNE', nbocc)
!                   --------------
    if (nbocc .ne. 0) then
        call getvid(' ', 'MODELE', 1, iarg, 1,&
                    modele, n1)
        if (n1 .eq. 0) then
            call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                        prec, np)
            call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                        crit, nc)
            call rsutnu(resuco, ' ', 0, knum, nbordr,&
                        prec, crit, iret)
            call jeveuo(knum, 'L', jordr)
            call rsadpa(resuco, 'L', 1, 'MODELE', zi(jordr),&
                        0, jpara, k8b)
            modele=zk16(jpara)
        endif
        call peaire(resu, modele, nbocc)
!
    endif
!
    call getfac('MASS_INER', nbocc)
!                   -----------
    if (nbocc .ne. 0) then
!
        nh = 0
        call getvis(' ', 'MODE_FOURIER', 1, iarg, 1,&
                    nh, n1)
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call jeveuo(kcha, 'L', jcha)
        chdef = ' '
        call getvtx(' ', 'GEOMETRIE', 1, iarg, 1,&
                    deform, n1)
        if (deform .eq. 'DEFORMEE') then
            call getvid(' ', 'CHAM_GD', 1, iarg, 1,&
                        chdef, n2)
            if (n2 .eq. 0) then
                tabtyp(1)='NOEU#DEPL_R'
                tabtyp(2)='NOEU#TEMP_R'
                tabtyp(3)='ELEM#ENER_R'
                knum = '&&OP0107.NUME_ORDRE'
                call getvid(' ', 'RESULTAT', 1, iarg, 1,&
                            resuco, nr)
                call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                            prec, np)
                call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                            crit, nc)
                call rsutnu(resuco, ' ', 0, knum, nbordr,&
                            prec, crit, iret)
                if (nbordr .ne. 1) then
                    call u2mess('F', 'UTILITAI2_80')
                endif
                if (iret .ne. 0) goto 9999
                call jeveuo(knum, 'L', jordr)
                call rsexch('F', resuco, 'DEPL', zi(jordr), chdef,&
                            iret)
                call chpve2(chdef, 3, tabtyp, ier)
            endif
        endif
!
        call pemain(resu, modele, mate, cara, nchar,&
                    zk8(jcha), nh, nbocc, chdef)
!
    endif
!
    call getfac('ENER_POT', nbocc)
!                   ----------
    if (nbocc .ne. 0) then
        nh = 0
        call getvis(' ', 'MODE_FOURIER', 1, iarg, 1,&
                    nh, n1)
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call jeveuo(kcha, 'L', jcha)
!
        call peepot(resu, modele, mate, cara, nchar,&
                    zk8(jcha), nh, nbocc)
!
    endif
!
    call getfac('ENER_CIN', nbocc)
!                   ----------
    if (nbocc .ne. 0) then
!
        nh = 0
        call getvis(' ', 'MODE_FOURIER', 1, iarg, 1,&
                    nh, n1)
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call jeveuo(kcha, 'L', jcha)
!
        call peecin(resu, modele, mate, cara, nchar,&
                    zk8(jcha), nh, nbocc)
!
    endif
!
    call getfac('INTEGRALE', nbocc)
!                   ----------
    if (nbocc .ne. 0) then
!
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call peeint(resu, modele, nbocc)
!
    endif
!
    call getfac('NORME', nbocc)
!                   ----------
    if (nbocc .ne. 0) then
!
!         --- ON RECUPERE LE MODELE
        call getvid('NORME', 'CHAM_GD', 1, iarg, 1,&
                    chdef, n1)
        if (n1 .ne. 0) then
            call getvid('NORME', 'MODELE', 1, iarg, 1,&
                        modele, n2)
        else
            call getvid('NORME', 'RESULTAT', 1, iarg, 1,&
                        resuco, nr)
            call medome(modele, mate, cara, kcha, nchar,&
                        ctyp, resuco)
        endif
!
        call penorm(resu, modele)
!
    endif
!
    call getfac('VOLUMOGRAMME', nbocc)
!                   --------------
    if (nbocc .ne. 0) then
!
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call pevolu(resu, modele, nbocc)
!
    endif
!
    call getfac('MINMAX', nbocc)
!                   ----------
    if (nbocc .ne. 0) then
        call getvid('MINMAX', 'CHAM_GD', 1, iarg, 1,&
                    chdef, n1)
        if (n1 .ne. 0) then
            call getvid('MINMAX', 'MODELE', 1, iarg, 1,&
                        modele, n2)
        else
            call getvid('MINMAX', 'RESULTAT', 1, iarg, 1,&
                        resuco, nr)
            call medome(modele, mate, cara, kcha, nchar,&
                        ctyp, resuco)
        endif
        call pemima(n1, chdef, resu, modele, nbocc)
    endif
!
    call getfac('WEIBULL', nbocc)
!                   ---------
    if (nbocc .ne. 0) then
!
        nh = 0
        call getvis(' ', 'MODE_FOURIER', 1, iarg, 1,&
                    nh, n1)
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call jeveuo(kcha, 'L', jcha)
!
        call peweib(resu, modele, mate, cara, k8b,&
                    nchar, zk8(jcha), nh, nbocc, 0,&
                    nomcmd)
!
!
    endif
!
    call getfac('RICE_TRACEY', nbocc)
!                   -------------
    if (nbocc .ne. 0) then
!
        nh = 0
        call getvis(' ', 'MODE_FOURIER', 1, iarg, 1,&
                    nh, n1)
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call jeveuo(kcha, 'L', jcha)
!
        call peritr(resu, modele, cara, nchar, zk8(jcha),&
                    nh, nbocc)
!
    endif
!
    call getfac('CARA_GEOM', nbocc)
!                   -----------
    if (nbocc .ne. 0) then
!
        nh = 0
        call getvis(' ', 'MODE_FOURIER', 1, iarg, 1,&
                    nh, n1)
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call jeveuo(kcha, 'L', jcha)
!
        call pecage(resu, modele, nbocc)
!
    endif
!
    call getfac('CARA_POUTRE', nbocc)
!                   -------------
    if (nbocc .ne. 0) then
!
        nh = 0
        call getvis(' ', 'MODE_FOURIER', 1, iarg, 1,&
                    nh, n1)
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call jeveuo(kcha, 'L', jcha)
!
        call pecapo(resu, modele, cara, nchar, zk8(jcha),&
                    nh)
!
    endif
!
    call getfac('INDIC_ENER', nbocc)
!                   ------------
    if (nbocc .ne. 0) then
!
        nh = 0
        call getvis(' ', 'MODE_FOURIER', 1, iarg, 1,&
                    nh, n1)
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call jeveuo(kcha, 'L', jcha)
!
        call peingl(resu, modele, mate, cara, nchar,&
                    zk8(jcha), nh, nbocc, 'INDIC_ENER')
!
    endif
!
    call getfac('INDIC_SEUIL', nbocc)
!                   -------------
    if (nbocc .ne. 0) then
!
        nh = 0
        call getvis(' ', 'MODE_FOURIER', 1, iarg, 1,&
                    nh, n1)
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call jeveuo(kcha, 'L', jcha)
!
        call peingl(resu, modele, mate, cara, nchar,&
                    zk8(jcha), nh, nbocc, 'INDIC_SEUIL')
!
    endif
!
    call getfac('ENER_ELAS', nbocc)
!                   -----------
    if (nbocc .ne. 0) then
!
        nh = 0
        call getvis(' ', 'MODE_FOURIER', 1, iarg, 1,&
                    nh, n1)
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call jeveuo(kcha, 'L', jcha)
!
        call peingl(resu, modele, mate, cara, nchar,&
                    zk8(jcha), nh, nbocc, 'ENER_ELAS')
!
    endif
!
    call getfac('ENER_TOTALE', nbocc)
!                   -------------
    if (nbocc .ne. 0) then
!
        nh = 0
        call getvis(' ', 'MODE_FOURIER', 1, iarg, 1,&
                    nh, n1)
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call jeveuo(kcha, 'L', jcha)
!
        call peingl(resu, modele, mate, cara, nchar,&
                    zk8(jcha), nh, nbocc, 'ENER_TOTALE')
!
    endif
!
!
    call getfac('ENER_DISS', nbocc)
!                   -----------
    if (nbocc .ne. 0) then
!
        nh = 0
        call getvis(' ', 'MODE_FOURIER', 1, iarg, 1,&
                    nh, n1)
        call medome(modele, mate, cara, kcha, nchar,&
                    ctyp, resuco)
        call jeveuo(kcha, 'L', jcha)
!
        call peingl(resu, modele, mate, cara, nchar,&
                    zk8(jcha), nh, nbocc, 'ENER_DISS')
!
    endif
9999  continue
    call titre()
!
    call jedema()
!
end subroutine
