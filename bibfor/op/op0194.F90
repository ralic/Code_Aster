subroutine op0194()
    implicit none
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
!
!      OPERATEUR :     CALC_META
!
! ----------------------------------------------------------------------
!
    include 'jeveux.h'
!
    include 'asterc/gettco.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/calcop.h'
    include 'asterfort/chpver.h'
    include 'asterfort/copisd.h'
    include 'asterfort/detrsd.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/medom1.h'
    include 'asterfort/modopt.h'
    include 'asterfort/mtdorc.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsorac.h'
    include 'asterfort/smevol.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/wkvect.h'
    character(len=6) :: nompro
    parameter(nompro='OP0194')
!
    integer :: ibid, iret, n1, n2, n3, num, numpha
    integer :: nbordt, nbtrou, ier, jopt, nbopt, nb, nchar, iopt
    integer :: nbordr, jordr, nuord, vali, iarg
!
    real(kind=8) :: inst, prec
    real(kind=8) :: valr, r8b
!
    complex(kind=8) :: c16b
    character(len=4) :: ctyp
    character(len=8) :: k8b, crit, temper, modele, cara
    character(len=16) :: tysd, option
    character(len=19) :: kordre, kcha
    character(len=24) :: compor, chmeta, phasin, mate, k24bid
    character(len=24) :: valk
    character(len=24) :: lesopt
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infmaj()
!
    lesopt='&&'//nompro//'.LES_OPTION'
    kordre='&&'//nompro//'.NUME_ORDRE'
    kcha = '&&'//nompro//'.CHARGES   '
!
    call getvid(' ', 'RESULTAT', 1, iarg, 1,&
                temper, n1)
    call gettco(temper, tysd)
!
    call rsorac(temper, 'LONUTI', ibid, r8b, k8b,&
                c16b, r8b, k8b, nbordr, 1,&
                ibid)
    call wkvect(kordre, 'V V I', nbordr, jordr)
    call rsorac(temper, 'TOUT_ORDRE', ibid, r8b, k8b,&
                c16b, r8b, k8b, zi(jordr), nbordr,&
                ibid)
    nuord = zi(jordr)
!
    call medom1(modele, mate, cara, kcha, nchar,&
                ctyp, temper, nuord)
!
    call getvtx(' ', 'OPTION', 1, iarg, 0,&
                k8b, nb)
    nbopt = -nb
    call wkvect(lesopt, 'V V K16', nbopt, jopt)
    call getvtx(' ', 'OPTION', 1, iarg, nbopt,&
                zk16(jopt), nb)
    call modopt(temper, modele, lesopt, nbopt)
    call jeveuo(lesopt, 'L', jopt)
!
    do 660 iopt = 1, nbopt
!
        option=zk16(jopt+iopt-1)
!
        if (option .eq. 'META_ELNO') then
!
            call mtdorc(modele, compor, k24bid)
!
! ----- ETAT INITIAL
            numpha = 0
            call getvid('ETAT_INIT', 'META_INIT_ELNO', 1, iarg, 1,&
                        chmeta, n3)
            if (n3 .gt. 0) then
                phasin = '&&SMEVOL_ZINIT'
                call chpver('F', chmeta(1:19), 'CART', 'VAR2_R', ier)
                call copisd('CHAMP_GD', 'V', chmeta, phasin(1:19))
            else
                call getvid('ETAT_INIT', 'EVOL_THER', 1, iarg, 1,&
                            temper, n1)
                call getvis('ETAT_INIT', 'NUME_INIT', 1, iarg, 1,&
                            num, n2)
                if (n2 .eq. 0) then
                    call getvr8('ETAT_INIT', 'INST_INIT', 1, iarg, 1,&
                                inst, n3)
                    call getvr8('ETAT_INIT', 'PRECISION', 1, iarg, 1,&
                                prec, n3)
                    call getvtx('ETAT_INIT', 'CRITERE', 1, iarg, 1,&
                                crit, n3)
                    nbordt = 1
                    call rsorac(temper, 'INST', ibid, inst, k8b,&
                                c16b, prec, crit, num, nbordt,&
                                nbtrou)
                    if (nbtrou .eq. 0) then
                        valk = temper
                        valr = inst
                        call u2mesg('F', 'UTILITAI6_51', 1, valk, 0,&
                                    0, 1, valr)
                    else if (nbtrou.gt.1) then
                        valk = temper
                        valr = inst
                        vali = nbtrou
                        call u2mesg('F', 'UTILITAI6_52', 1, valk, 1,&
                                    vali, 1, valr)
                    endif
                endif
                call rsexch('F', temper, 'META_ELNO', num, phasin,&
                            iret)
                numpha = num
            endif
!
            call smevol(temper(1:8), modele, mate, compor, option,&
                        phasin, numpha)
!
            call detrsd('CARTE', '&&NMDORC.COMPOR')
!
        else
!         PASSAGE CALC_CHAMP
            call calcop(option, lesopt, temper, temper, kordre,&
                        nbordr, kcha, nchar, ctyp, tysd,&
                        iret)
            if (iret .eq. 0) goto 660
!
        endif
!
660  end do
!
    call jedema()
end subroutine
