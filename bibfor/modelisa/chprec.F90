subroutine chprec(chou)
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
!     TRAITEMENT DE COMMANDE:   CREA_CHAMP / OPTION: 'EXTR'
!
!     ------------------------------------------------------------------
!
    implicit   none
!
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterc/gettco.h'
    include 'asterc/getvid.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/chmima.h'
    include 'asterfort/copisd.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/exisd.h'
    include 'asterfort/infniv.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/rsexch.h'
    include 'asterfort/rsinch.h'
    include 'asterfort/rsutnu.h'
    include 'asterfort/titre.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    character(len=*) :: chou
!
! 0.2. ==> COMMUNS
!
!
! 0.3. ==> VARIABLES LOCALES
!
    character(len=6) :: nompro
    parameter ( nompro = 'CHPREC' )
!
    integer :: ibid, icoret, iret, jordr, n1, n2, n3, n4, n5, nbordr, nc, np, ie
    integer :: ifm, niv, iexi
    real(kind=8) :: inst, epsi
    character(len=1) :: base
    character(len=8) :: resuco, interp, crit, proldr, prolga, typmax, cara
    character(len=8) :: nomgd, charme
    character(len=16) :: k16bid, nomcmd, nomch, acces, tysd, tychlu, tych
    character(len=19) :: chextr, noch19, knum
    character(len=24) :: valk(3)
    character(len=8) :: k8bid, ma, fis
    logical :: grille
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
!
    base = 'G'
    call getres(k8bid, k16bid, nomcmd)
    noch19 = chou
!
    call getvtx(' ', 'NOEUD_CMP', 0, iarg, 0,&
                k8bid, n1)
    if (n1 .ne. 0 .and. n1 .ne. -2) call u2mess('F', 'MODELISA4_16')
    nomch=' '
    call getvtx(' ', 'NOM_CHAM', 0, iarg, 1,&
                nomch, n2)
    tychlu=' '
    call getvtx(' ', 'TYPE_CHAM', 0, iarg, 1,&
                tychlu, n2)
!
!
!     1. CAS DE LA RECUPERATION DU CHAMP DE GEOMETRIE D'UN MAILLAGE
!     ==============================================================
    if (nomch .eq. 'GEOMETRIE') then
        call getvid(' ', 'MAILLAGE', 0, iarg, 1,&
                    ma, n1)
        if (n1 .eq. 0) call u2mess('F', 'MODELISA4_17')
!
!       ON VERIFIE QUE LE MOT-CLE TYPE_CHAMP EST COHERENT AVEC LE
!       TYPE DU CHAMP EXTRAIT.
        call dismoi('F', 'TYPE_CHAMP', ma//'.COORDO', 'CHAMP', ibid,&
                    tych, ie)
        call dismoi('F', 'NOM_GD', ma//'.COORDO', 'CHAMP', ibid,&
                    nomgd, ie)
!
        if ((tychlu(1:4).ne.tych) .or. (tychlu(6:12).ne.nomgd)) then
            valk(1) = tychlu
            valk(2) = tych(1:4)
            valk(3) = nomgd
            call u2mesk('F', 'MODELISA4_18', 3, valk)
        endif
        call copisd('CHAMP_GD', 'G', ma//'.COORDO', noch19)
        goto 20
    endif
!
!
!     2. CAS DE LA RECUPERATION D'UN CHAMP DANS UNE SD FISS_XFEM
!     ==============================================================
    call getvid(' ', 'FISSURE', 0, iarg, 1,&
                fis, n1)
    if (n1 .eq. 1) then
!
!       VERIFIE SI UNE GRILLE AUXILIAIRE EST DEFINIE POUR LA FISS
        call jeexin(fis//'.GRI.MODELE', ibid)
        if (ibid .eq. 0) then
            grille=.false.
        else
            grille=.true.
        endif
!
        if (nomch .eq. 'LTNO') then
            chextr = fis//'.LTNO'
        else if (nomch.eq.'LNNO') then
            chextr = fis//'.LNNO'
        else if (nomch.eq.'GRLNNO') then
            chextr = fis//'.GRLNNO'
        else if (nomch.eq.'GRLTNO') then
            chextr = fis//'.GRLTNO'
        else if (nomch.eq.'STNO') then
            chextr = fis//'.STNO'
        else if (nomch.eq.'STNOR') then
            chextr = fis//'.STNOR'
        else if (nomch.eq.'BASLOC') then
            chextr = fis//'.BASLOC'
        else
            if (grille) then
                if (nomch .eq. 'GRI.LTNO') then
                    chextr = fis//'.GRI.LTNO'
                else if (nomch.eq.'GRI.LNNO') then
                    chextr = fis//'.GRI.LNNO'
                else if (nomch.eq.'GRI.GRLNNO') then
                    chextr = fis//'.GRI.GRLNNO'
                else if (nomch.eq.'GRI.GRLTNO') then
                    chextr = fis//'.GRI.GRLTNO'
                endif
            else
                call u2mess('F', 'XFEM2_98')
            endif
        endif
!
!       ON VERIFIE QUE LE MOT-CLE TYPE_CHAMP EST COHERENT AVEC LE
!       TYPE DU CHAMP EXTRAIT.
        call dismoi('F', 'TYPE_CHAMP', chextr, 'CHAMP', ibid,&
                    tych, ie)
        call dismoi('F', 'NOM_GD', chextr, 'CHAMP', ibid,&
                    nomgd, ie)
!
        if ((tychlu(1:4).ne.tych) .or. (tychlu(6:12).ne.nomgd)) then
            valk(1) = tychlu
            valk(2) = tych(1:4)
            valk(3) = nomgd
            call u2mesk('F', 'MODELISA4_18', 3, valk)
        endif
        call copisd('CHAMP_GD', 'G', chextr, noch19)
        goto 20
    endif
!
!
!     3. CAS DE LA RECUPERATION D'UN CHAMP DANS UNE SD CARA_ELEM
!     ==============================================================
    call getvid(' ', 'CARA_ELEM', 0, iarg, 1,&
                cara, n1)
    if (n1 .eq. 1) then
!
        call assert(nomch(1:1).eq.'.')
        chextr = cara//nomch
        call exisd('CHAMP', chextr, iexi)
        if (iexi .eq. 0) call u2mesk('F', 'CALCULEL3_17', 1, chextr)
!
!       ON VERIFIE QUE LE MOT-CLE TYPE_CHAMP EST COHERENT AVEC LE
!       TYPE DU CHAMP EXTRAIT.
        call dismoi('F', 'TYPE_CHAMP', chextr, 'CHAMP', ibid,&
                    tych, ie)
        call dismoi('F', 'NOM_GD', chextr, 'CHAMP', ibid,&
                    nomgd, ie)
!
        if ((tychlu(1:4).ne.tych) .or. (tychlu(6:).ne.nomgd)) then
            valk(1) = tychlu
            valk(2) = tych(1:4)
            valk(3) = nomgd
            call u2mesk('F', 'MODELISA4_18', 3, valk)
        endif
        call copisd('CHAMP_GD', 'G', chextr, noch19)
        goto 20
    endif
!
!
!     4. CAS DE LA RECUPERATION D'UN CHAMP DANS UNE SD CHAR_MECA
!     ==============================================================
    call getvid(' ', 'CHARGE', 0, iarg, 1,&
                charme, n1)
    if (n1 .eq. 1) then
!
        call assert(nomch(1:1).eq.'.')
        chextr = charme//nomch
        call exisd('CHAMP', chextr, iexi)
        if (iexi .eq. 0) call u2mesk('F', 'CALCULEL3_17', 1, chextr)
!
!       ON VERIFIE QUE LE MOT-CLE TYPE_CHAMP EST COHERENT AVEC LE
!       TYPE DU CHAMP EXTRAIT.
        call dismoi('F', 'TYPE_CHAMP', chextr, 'CHAMP', ibid,&
                    tych, ie)
        call dismoi('F', 'NOM_GD', chextr, 'CHAMP', ibid,&
                    nomgd, ie)
!
        if ((tychlu(1:4).ne.tych) .or. (tychlu(6:).ne.nomgd)) then
            valk(1) = tychlu
            valk(2) = tych(1:4)
            valk(3) = nomgd
            call u2mesk('F', 'MODELISA4_18', 3, valk)
        endif
        call copisd('CHAMP_GD', 'G', chextr, noch19)
        goto 20
    endif
!
!
!     5. CAS DE LA RECUPERATION D'UN CHAMP D'UNE SD RESULTAT
!     ==============================================================
    call getvid(' ', 'RESULTAT', 0, iarg, 1,&
                resuco, n1)
    interp=' '
    call getvtx(' ', 'INTERPOL', 0, iarg, 1,&
                interp, n3)
    typmax=' '
    call getvtx(' ', 'TYPE_MAXI', 0, iarg, 1,&
                typmax, n5)
    call gettco(resuco, tysd)
!
!     --- ON PEUT FAIRE UNE INTERPOLATION ---
!         ===============================
    if (tysd .eq. 'EVOL_THER' .or. tysd .eq. 'EVOL_ELAS' .or. tysd .eq. 'EVOL_NOLI' .or.&
        tysd .eq. 'DYNA_TRANS' .or. tysd .eq. 'EVOL_VARC') then
!
        if (interp(1:3) .eq. 'LIN') then
            call getvr8(' ', 'INST', 0, iarg, 1,&
                        inst, n4)
            call assert(n4.eq.1)
            proldr = 'EXCLUS'
            prolga = 'EXCLUS'
            acces = 'INST'
            call rsinch(resuco, nomch, acces, inst, noch19,&
                        proldr, prolga, 2, base, icoret)
        else
            if (n5 .ne. 0) then
                call chmima(resuco, nomch, typmax, noch19)
            else
                knum = '&&'//nompro//'.NUME_ORDRE'
                call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                            epsi, np)
                call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                            crit, nc)
                call rsutnu(resuco, ' ', 0, knum, nbordr,&
                            epsi, crit, iret)
                if ((iret.ne.0) .or. (nbordr.gt.1)) goto 10
                if (nbordr .eq. 0) then
                    call u2mess('F', 'UTILITAI_23')
                endif
                call jeveuo(knum, 'L', jordr)
                call rsexch('F', resuco, nomch, zi(jordr), chextr,&
                            iret)
!
!
!           ON VERIFIE QUE LE MOT-CLE TYPE_CHAMP EST COHERENT AVEC LE
!           TYPE DU CHAMP EXTRAIT.
!
                call dismoi('F', 'TYPE_CHAMP', chextr, 'CHAMP', ibid,&
                            tych, ie)
                call dismoi('F', 'NOM_GD', chextr, 'CHAMP', ibid,&
                            nomgd, ie)
!
                if ((tychlu(1:4).ne.tych) .or. (tychlu(6:12).ne.nomgd)) then
                    valk(1) = tychlu
                    valk(2) = tych(1:4)
                    valk(3) = nomgd
                    call u2mesk('F', 'MODELISA4_18', 3, valk)
                endif
                call copisd('CHAMP_GD', 'G', chextr, noch19)
                call jedetr(knum)
            endif
        endif
!
!     --- ON NE FAIT QU'UNE EXTRACTION ---
!         ===========================
    else
        if (interp(1:3) .eq. 'LIN') then
            valk(1) = tysd
            call u2mesg('F', 'MODELISA8_55', 1, valk, 0,&
                        0, 0, 0.d0)
        else
            knum = '&&'//nompro//'.NUME_ORDRE'
            call getvr8(' ', 'PRECISION', 1, iarg, 1,&
                        epsi, np)
            call getvtx(' ', 'CRITERE', 1, iarg, 1,&
                        crit, nc)
            call rsutnu(resuco, ' ', 0, knum, nbordr,&
                        epsi, crit, iret)
            if ((iret.ne.0) .or. (nbordr.gt.1)) goto 10
            if (nbordr .eq. 0) then
                call u2mess('F', 'UTILITAI_23')
            endif
            call jeveuo(knum, 'L', jordr)
            call rsexch('F', resuco, nomch, zi(jordr), chextr,&
                        iret)
            call dismoi('F', 'TYPE_CHAMP', chextr, 'CHAMP', ibid,&
                        tych, ie)
            call dismoi('F', 'NOM_GD', chextr, 'CHAMP', ibid,&
                        nomgd, ie)
!
            if ((tychlu(1:4).ne.tych) .or. (tychlu(6:12).ne.nomgd)) then
                valk(1) = tychlu
                valk(2) = tych(1:4)
                valk(3) = nomgd
                call u2mesk('F', 'MODELISA4_18', 3, valk)
            endif
!
            call copisd('CHAMP_GD', 'G', chextr, noch19)
            call jedetr(knum)
        endif
    endif
!
!
    goto 20
10  continue
    call u2mess('F', 'MODELISA4_19')
!
20  continue
    call titre()
!
!
! --- MENAGE
!     ======
    call jedetr('&&'//nompro//'.NUME_ORDRE')
!
    call jedema()
end subroutine
