subroutine nmdoch(lischa, iexcit, excit)
!
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
! person_in_charge: mickael.abbas at edf.fr
! TOLE CRP_20
!
    implicit     none
    include 'jeveux.h'
    include 'asterc/getexm.h'
    include 'asterc/getfac.h'
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/codent.h'
    include 'asterfort/dismoi.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/liscad.h'
    include 'asterfort/lisccr.h'
    include 'asterfort/liscli.h'
    include 'asterfort/lislfc.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    integer :: iexcit
    character(len=19) :: lischa, excit
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE
!
! SAISIE ET VERIFICATION DE LA COHERENCE DES CHARGEMENTS
!
! ----------------------------------------------------------------------
!
!
! IN  IEXCIT : INDICE DEFINISSANT L'ORIGINE DU CHARGEMENT
!                      UTILISE LORS DES CALCLULS
!                      0 : LE CHARGEMENT EST ISSU DE LA SD RESULTAT
!                      1 : LE CHARGEMENT EST FOURNI PAR L'UTILISATEUR
! I/O LISCHA : IN - NOM DONNE A SD L_CHARGES
! IN  EXCIT  : NOM EXTRAIT DE LA SD RESULTAT SI IEXCIT=0
!
!
!
!
    integer :: itych
    integer :: n1, ibid, nocc, iexc, incha1, iret2
    integer :: npilo, nexci, nchar, nchar1, nchar2, nchar3
    integer :: infmax, indic, ich, iret, infc, j, ichar
    integer :: jinfch, jlcha, jinfc
    integer :: jlcha2, jinfc2
    integer :: jlisdb, ichd
    character(len=5) :: suffix
    character(len=8) :: k8bid, affcha, parcha, typcha
    character(len=8) :: fctcsr
    character(len=16) :: nomcmd, typesd
    character(len=8) :: nomcha, nomfct, nomch1, nomfc1
    character(len=24) :: infoc1, infoch
    character(len=19) :: lisch2, lisdbl
    character(len=24) :: ligrch, lchin, k24bid
    integer :: ival, ival1
    logical :: lfcplx, lacce
    integer :: nbinfo
! --- NOMBRE MAXIMUM DE TYPE_INFO
    integer :: nbinmx
    parameter   (nbinmx=99)
    character(len=24) :: lisinf(nbinmx)
! --- NOMBRE MAXIMUM DE RESUELEM POUR LES FORCES DE LAPLACE : NBCHMX
    integer :: nbchmx
    parameter   (nbchmx=99)
! --- NOMBRE MAXIMUM DE TYPESD DE CHARGE                    : NBTYCH
    integer :: nbtych
    parameter    (nbtych=18)
    character(len=6) :: nomlig(nbtych)
    integer :: iarg
!
    data nomlig  /'.FORNO','.F3D3D','.F2D3D','.F1D3D',&
     &              '.F2D2D','.F1D2D','.F1D1D','.PESAN',&
     &              '.ROTAT','.PRESS','.FELEC','.FCO3D',&
     &              '.FCO2D','.EPSIN','.FLUX' ,'.VEASS',&
     &              '.ONDPL','.SIINT'/
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call getres(k8bid, typesd, nomcmd)
!
! --- INITIALISATIONS
!
    nchar = 0
    infmax = 0
    fctcsr = '&&NMDOME'
    lisdbl = '&&NMDOME.LISDBL'
    lfcplx = .false.
    lacce = .false.
    infoch = 'RIEN'
    npilo = 0
    indic = 0
    nexci = 0
    ichar = 0
    k24bid = ' '
!
! --- NOMBRE DE CHARGES
!
    if (iexcit .eq. 1) then
        if (getexm('EXCIT','CHARGE') .eq. 1) then
            call getfac('EXCIT', nexci)
        else
            nexci=0
        endif
        if (nexci .gt. 0) then
            do 20 iexc = 1, nexci
                call getvid('EXCIT', 'CHARGE', iexc, iarg, 1,&
                            k24bid, nocc)
!
! --- GLUTE POUR LE CAS DEFI_CABLE_BP: MELANGE
! --- CINEMATIQUE/NEUMANN
!
                call jeexin(k24bid(1:8)//'.CHME.SIGIN.VALE', iret)
                if (iret .ne. 0) then
                    call jeexin(k24bid(1:8)//'.CHME.CIMPO.DESC', iret2)
                else
                    iret2 = 1
                endif
!
                if ((nocc.eq.1) .and. (iret2.ne.0)) then
                    nchar = nchar + 1
                endif
20          continue
        else
! --- CAS OU LE CHARGEMENT PEUT NE PAS ETRE OBLIGATOIRE (DYNA_NON_LINE)
!     ON CREE UNE SD CHARGE CONTENANT 1 CHARGE FICTIVE
            if (nomcmd .eq. 'DYNA_NON_LINE') then
                call lisccr(lischa, 1, 'V')
                call jeveuo(lischa(1:19)//'.INFC', 'E', jinfch)
                nchar=0
                zi(jinfch) = nchar
            else if (nomcmd.eq.'STAT_NON_LINE') then
                call u2mess('F', 'CHARGES_2')
            endif
        endif
    else
        call jeveuo(excit(1:19)//'.INFC', 'L', jinfc)
        nchar = zi(jinfc)
!
!
! --- POUR CALC_CHAMP : AFFE_CHAR_CINE EST ILLICITE: ON LES ENLEVE
!
        if (nomcmd .eq. 'CALC_CHAMP') then
            nchar1 = nchar
            nchar2 = 0
            call jeveuo(excit(1:19)//'.LCHA', 'L', jlcha)
            do 22 ich = 1, nchar1
                nomcha = zk24(jlcha-1+ich)(1:8)
!
                call jeexin(nomcha//'.CHME.SIGIN', iret)
                if (nomcha .ne. ' ') then
                    call dismoi('F', 'TYPE_CHARGE', nomcha, 'CHARGE', ibid,&
                                affcha, iret)
!
                    if (affcha(1:5) .ne. 'CIME_') then
                        nchar2 = nchar2 + 1
                    endif
                endif
22          continue
!
            if (nchar1 .ne. nchar2) then
                nchar3 = max(nchar2,1)
                lisch2 = '&&NMDOME.CHARGES'
                call lisccr(lisch2, nchar3, 'V')
                incha1 = 0
!
                do 24 ich = 1, nchar1
                    nbinfo = 1
                    call liscli(excit, ich, nomch1, nomfc1, nbinfo,&
                                infoc1, ival1)
                    if (infoc1(1:5) .ne. 'CINE_' .and. nomch1 .ne. ' ') then
                        incha1 = incha1 + 1
                        call liscad(lisch2, incha1, nomch1, nomfc1, nbinfo,&
                                    infoc1, ival1)
                    endif
!
24              continue
                nchar = nchar2
                excit = lisch2
            endif
        endif
!
        call jeveuo(excit(1:19)//'.INFC', 'L', jinfc2)
        call jeveuo(excit(1:19)//'.LCHA', 'L', jlcha2)
    endif
!
    if (nchar .ne. 0) then
!
! ----- CREATION LA SD L_CHARGES
!
        call lisccr(lischa, nchar, 'V')
!
! ----- LISTE DOUBLE
!
        call wkvect(lisdbl, 'V V K8', nchar, jlisdb)
!
! ----- BOUCLE SUR LES CHARGES
!
        do 130 ich = 1, nchar
            if (iexcit .eq. 1) then
                indic = indic + 1
30              continue
                call getvid('EXCIT', 'CHARGE', indic, iarg, 0,&
                            nomcha, n1)
                if (n1 .ne. 0) then
                    call getvid('EXCIT', 'CHARGE', indic, iarg, 1,&
                                nomcha, n1)
                    do 131,ichd = 1,nchar
                    if (nomcha .eq. zk8(jlisdb+ichd-1)) then
                        call u2mesk('E', 'CHARGES_1', 1, nomcha)
                    endif
131                  continue
                else
                    indic = indic + 1
                    goto 30
                endif
            else
                nomcha = zk24(jlcha2+ich-1)(1:8)
            endif
            zk8(jlisdb+ich-1) = nomcha(1:8)
!
! ------- LIGREL DE LA CHARGE
!
            ligrch = nomcha(1:8)//'.CHME.LIGRE'
!
! ------- TYPE DE LA CHARGE
!
            if (iexcit .eq. 1) then
                if (nomcmd .eq. 'DYNA_LINE_TRAN' .or. nomcmd .eq. 'DYNA_LINE_HARM') then
                    typcha='FIXE'
                else
                    call getvtx('EXCIT', 'TYPE_CHARGE', indic, iarg, 1,&
                                typcha, n1)
                endif
            else
                typcha = 'FIXE_CST'
                if (nomcmd .eq. 'CALC_CHAMP') then
                    if (zi(jinfc2+ich) .eq. 4 .or. zi(jinfc2+nchar+ich) .eq. 4) then
                        typcha = 'SUIV'
                        elseif (zi(jinfc2+ich).eq.5 .or. zi(jinfc2+nchar+&
                    ich).eq.5) then
                        typcha = 'FIXE_PIL'
                    else if (zi(jinfc2+3*nchar+2+ich).eq.1) then
                        typcha = 'DIDI'
                    endif
                endif
            endif
!
! ------- NOMBRE DE CHARGES PILOTEES
!
            if (typcha .eq. 'FIXE_PIL') then
                npilo = npilo + 1
            endif
!
! ------- CONTROLE DU CARACTERE MECANIQUE DE LA CHARGE
!
            call dismoi('F', 'TYPE_CHARGE', nomcha, 'CHARGE', ibid,&
                        affcha, iret)
            if ((affcha(1:5).ne.'MECA_') .and. (affcha(1:5) .ne.'CIME_')) then
                call u2mesk('F', 'CHARGES_22', 1, nomcha(1:8))
            endif
!
! ------- FONCTIONS MULTIPLICATIVES DES CHARGES
!
            lfcplx = (&
                     nomcmd .eq. 'DYNA_LINE_HARM' .or.&
                     ( nomcmd.eq.'LIRE_RESU' .and. typesd.eq.'DYNA_HARMO' )&
                     )
            lacce = (nomcmd.eq.'DYNA_NON_LINE'.or. nomcmd.eq.'LIRE_RESU')
            call lislfc(excit, ich, indic, iexcit, nexci,&
                        lfcplx, lacce, fctcsr, nomfct)
            if (nomfct .ne. fctcsr) then
                if (typcha .eq. 'FIXE_PIL') then
                    call u2mesk('F', 'CHARGES_38', 1, nomcha(1:8))
                endif
            endif
!
! ------- CHARGE DE TYPE DIRICHLET PROVENANT D'UN AFFE_CHAR_CINE
!
            nbinfo = 0
            infoch = 'RIEN'
            if (affcha(1:5) .eq. 'CIME_') then
                if (typcha(1:4) .eq. 'SUIV') then
                    call u2mesk('F', 'CHARGES_23', 1, nomcha(1:8))
                else if (typcha.eq.'FIXE_PIL') then
                    call u2mesk('F', 'CHARGES_27', 1, nomcha(1:8))
                else if (typcha(1:4).eq.'DIDI') then
                    call u2mesk('F', 'CHARGES_24', 1, nomcha(1:8))
                else
                    if (affcha(5:7) .eq. '_FT') then
                        infoch = 'CINE_FT'
                    else if (affcha(5:7).eq.'_FO') then
                        infoch = 'CINE_FO'
                    else
                        infoch = 'CINE_CSTE'
                    endif
                endif
            endif
            if (infoch .ne. 'RIEN') then
                nbinfo = nbinfo + 1
                call assert(nbinfo.lt.nbinmx)
                lisinf(nbinfo) = infoch
            endif
!
! -------- CHARGE DE TYPE DIRICHLET PROVENANT DE AFFE_CHAR_MECA
!
            infoch = 'RIEN'
            lchin = ligrch(1:13)//'.CIMPO.DESC'
            call jeexin(lchin, iret)
            if (iret .ne. 0) then
                if (typcha(1:4) .eq. 'SUIV') then
                    call u2mesk('F', 'CHARGES_23', 1, nomcha(1:8))
!
                else if (typcha.eq.'FIXE_PIL') then
                    call dismoi('F', 'PARA_INST', lchin(1:19), 'CARTE', ibid,&
                                parcha, iret)
                    if (parcha(1:3) .eq. 'OUI') then
                        call u2mesk('F', 'CHARGES_28', 1, nomcha(1:8))
                    endif
!
                    if (affcha(5:7) .eq. '_FT') then
                        call u2mesk('F', 'CHARGES_28', 1, nomcha(1:8))
                    else if (affcha(5:7).eq.'_FO') then
                        infoch = 'DIRI_PILO_F'
                    else
                        infoch = 'DIRI_PILO'
                    endif
                else
                    if (affcha(5:7) .eq. '_FO') then
                        infoch = 'DIRI_FO'
                        call dismoi('F', 'PARA_INST', lchin(1:19), 'CARTE', ibid,&
                                    parcha, iret)
                        if (parcha(1:3) .eq. 'OUI') then
                            infoch = 'DIRI_FT'
                        endif
                    else
                        infoch = 'DIRI_CSTE'
                    endif
                    if (typcha(1:4) .eq. 'DIDI') then
                        infoch = infoch(1:9)//'_DIDI'
                    endif
                endif
            endif
            if (infoch .ne. 'RIEN') then
                nbinfo = nbinfo + 1
                call assert(nbinfo.lt.nbinmx)
                lisinf(nbinfo) = infoch
            endif
!
! ------- CHARGE DE TYPE NEUMANN
!
            do 70 itych = 1, nbtych
                if (nomlig(itych) .eq. '.VEASS') then
                    suffix = '     '
                else
                    suffix = '.DESC'
                endif
                lchin = ligrch(1:13)//nomlig(itych)//suffix
                call jeexin(lchin, iret)
                infoch = 'RIEN'
                if (iret .ne. 0) then
                    if (nomlig(itych) .eq. '.ONDPL') then
                        infoch = 'NEUM_ONDE'
!
                    else if (nomlig(itych).eq.'.SIINT') then
                        infoch = 'NEUM_SIGM_INT'
!
                    else if (typcha.eq.'FIXE_PIL') then
                        infoch = 'NEUM_PILO'
                        if (nomlig(itych) .ne. '.VEASS') then
                            call dismoi('F', 'PARA_INST', lchin(1:19), 'CARTE', ibid,&
                                        parcha, iret)
                            if (parcha(1:3) .eq. 'OUI') then
                                call u2mess('F', 'CHARGES_28')
                            endif
                        endif
!
                    else if (typcha(1:4).eq.'SUIV') then
                        infoch = 'NEUM_SUIV'
!
                    else if (affcha(5:7).eq.'_FO') then
                        call dismoi('F', 'PARA_INST', lchin(1:19), 'CARTE', ibid,&
                                    parcha, iret)
!
                        if (parcha(1:3) .eq. 'OUI') then
                            infoch = 'NEUM_FT'
                        else
                            infoch = 'NEUM_FO'
                        endif
                    else
                        infoch = 'NEUM_CSTE'
                    endif
                endif
                if (infoch .ne. 'RIEN') then
                    nbinfo = nbinfo + 1
                    call assert(nbinfo.lt.nbinmx)
                    lisinf(nbinfo) = infoch
                endif
70          continue
!
! ------- CHARGE DE TYPE EVOL_CHAR
!
            infoch = 'RIEN'
            lchin = ligrch(1:13)//'.EVOL.CHAR'
            call jeexin(lchin, iret)
            if (iret .ne. 0) then
                if (typcha(1:4) .eq. 'SUIV') then
                    infoch = 'NEUM_SUIV'
                else
                    infoch = 'NEUM_CSTE'
                endif
                if (typcha .eq. 'FIXE_PIL') then
                    call u2mesk('F', 'CHARGES_34', 1, nomcha(1:8))
                endif
            endif
            if (infoch .ne. 'RIEN') then
                nbinfo = nbinfo + 1
                call assert(nbinfo.lt.nbinmx)
                lisinf(nbinfo) = infoch
            endif
!
! ------- CHARGE DE TYPE EXCIT_SOL
!
            infoch = 'RIEN'
            lchin = ligrch(1:13)//'.VEISS'
            call jeexin(lchin, iret)
            if (iret .ne. 0) then
                if (nomcmd .eq. 'STAT_NON_LINE') then
                    call u2mesk('F', 'CHARGES_50', 1, nomcha(1:8))
                endif
                if (typcha .eq. 'SUIV') then
                    call u2mesk('F', 'CHARGES_51', 1, nomcha(1:8))
                endif
                if (typcha .eq. 'DIDI') then
                    call u2mesk('F', 'CHARGES_52', 1, nomcha(1:8))
                endif
                if (affcha(5:6) .eq. '_F') then
                    call u2mesk('F', 'CHARGES_53', 1, nomcha(1:8))
                endif
                if (nomfct .ne. fctcsr) then
                    call u2mesk('F', 'CHARGES_54', 1, nomcha(1:8))
                endif
                infoch = 'EXCIT_SOL'
            endif
            if (infoch .ne. 'RIEN') then
                nbinfo = nbinfo + 1
                call assert(nbinfo.lt.nbinmx)
                lisinf(nbinfo) = infoch
            endif
!
! -------- CHARGES DE TYPE FORCE DE LAPLACE
!
            infc = 0
            infoch = 'RIEN'
            do 80 j = 1, nbchmx
                lchin(1:17) = ligrch(1:13)//'.FL1'
                call codent(j, 'D0', lchin(18:19))
                lchin = lchin(1:19)//'.DESC'
                call jeexin(lchin, iret)
                if (iret .ne. 0) then
                    infc = infc + 1
                else
                    goto 90
                endif
80          continue
90          continue
            if (infc .ne. 0) then
                ival = max(infmax,infc)
                infoch = 'NEUM_LAPL'
            endif
            if (infoch .ne. 'RIEN') then
                nbinfo = nbinfo + 1
                call assert(nbinfo.lt.nbinmx)
                lisinf(nbinfo) = infoch
            endif
!
! --- AJOUT DE LA CHARGE
!
            if (nbinfo .gt. 0) then
                ichar = ichar+1
                call liscad(lischa, ichar, nomcha, nomfct, nbinfo,&
                            lisinf, ival)
            endif
!
130      continue
!
! ---- PILOTAGE POSSIBLE SI IL YA DES CHARGES PILOTEES !
!
        if (nomcmd .ne. 'LIRE_RESU') then
            if (nomcmd .eq. 'STAT_NON_LINE') then
                call getvtx('PILOTAGE', 'TYPE', 1, iarg, 1,&
                            k24bid, n1)
                if (n1 .ne. 0 .and. npilo .eq. 0) then
                    call u2mess('F', 'CHARGES_39')
                endif
                if (npilo .gt. 1) then
                    call u2mess('F', 'CHARGES_40')
                endif
            endif
        endif
    endif
    call jedetr(lisdbl)
    call jedema()
end subroutine
