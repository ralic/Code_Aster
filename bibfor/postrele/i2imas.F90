subroutine i2imas(epsi, conec, coord, typ, nbm,&
                  numail, xa, ya, xb, yb,&
                  nbseg, sgtor, sgtex, mail1, mail2,&
                  facor, facex, paror, parex)
! aslint: disable=W1501
    implicit   none
!
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
!
!
!--------------ENTREES----------------------------------------------
!
    include 'jeveux.h'
!
    include 'asterfort/i2appm.h'
    include 'asterfort/i2chax.h'
    include 'asterfort/i2fini.h'
    include 'asterfort/i2isgc.h'
    include 'asterfort/i2isgt.h'
    include 'asterfort/i2nbrf.h'
    include 'asterfort/i2rgel.h'
    include 'asterfort/i2rgma.h'
    include 'asterfort/i2typf.h'
    include 'asterfort/jecreo.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeecra.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jenuno.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    integer :: nbm, numail(*)
    real(kind=8) :: xa, ya, xb, yb, epsi
    character(len=24) :: conec, coord, typ
!
!--------------SORTIES---------------------------------------------
!
    integer :: nbseg, mail1(*), mail2(*), facor(*), facex(*)
    real(kind=8) :: paror(*), parex(*), sgtor(*), sgtex(*)
!
!--------------VARIABLES LOCALES----------------------------------
!
!---------------------DESCRIPTION D' UNE MAILLE----------------------
!
    integer :: m, nbcote, nbneud, ima, iatyma
    character(len=8) :: typm, k8b
!
!---------------------DESCRIPTION D' UNE FACE------------------------
!
    integer :: nums, adrs, c
    real(kind=8) :: xd, yd, xf, yf, xi, yi
    logical :: droit
!
!---------------------DESCRIPTION DE L' INTERSECTION AVEC UNE MAILLE--
!
    real(kind=8) :: abs1, abs2, abr1, abr2
    integer :: nbpm, nbpf
!
!---------------------GESTION DE L' INTERSECTION---------------------
!
    integer :: pt, i, adrgt, k, f1or, f2or, f1ex, f2ex, fcom, m1, m2
    logical :: adansm, bdansm, elimin, atrouv, btrouv, fini, fincal
    real(kind=8) :: or, ex, ror, rex, xm, ym, xt, yt, indic, coef, crit
    real(kind=8) :: s, r1, xnewm, ynewm, xnewt, ynewt, inf, sup, sm
    integer :: f1
!
!---------------------ADRESSE DES OBJETS JEVEUX----------------------
!
    integer :: atypm, acoord, adrvlc, axsom, axint, aysom, ayint
    integer :: asloc, ar1loc, ar2loc, af1loc, af2loc, acotdr, aconec
!
!------------FONCTIONS D' ACCES JEVEUX-------------------------------
!
!
!
!
!
!
!---------INITIALISATION---------------------------------------------
!
    call jemarq()
    nbpf = 0
    nbpm = 0
    m = 0
    m1 = 0
    m2 = 0
    c = 0
    k = 1
    i = 1
    pt = 1
    adrgt = 1
    adrs = 0
    nums = 0
    fcom = 0
    f1or = 0
    f2or = 0
    f1ex = 0
    f2ex = 0
    f1 = 0
!
    droit = .false.
    adansm = .false.
    bdansm = .false.
    atrouv = .false.
    btrouv = .false.
    elimin = .false.
    fini = .false.
    fincal = .false.
!
    abs1 = 0.0d0
    abs2 = 0.0d0
    abr1 = 0.0d0
    abr2 = 0.0d0
    xd = 0.0d0
    yd = 0.0d0
    xi = 0.0d0
    yi = 0.0d0
    xf = 0.0d0
    yf = 0.0d0
    xm = 0.0d0
    ym = 0.0d0
    xt = 0.0d0
    yt = 0.0d0
    or = 0.0d0
    ex = 0.0d0
    ror = 0.0d0
    rex = 0.0d0
    r1 = 0.0d0
!
    inf = 0.0d0
    sup = 1.0d0
!
    indic = 1.0d0
    coef = 0.0d0
!
!---------RECUPERATION DES ADRESSE DES OBJETS JEVEUX-----------------
!
    call jeveuo(conec, 'L', aconec)
    call jeveuo(coord, 'L', acoord)
!
!---------CREATION DES TABLEAUX LOCAUX JEVEUX-----------------------
!
    call jecreo('&INTERSLOC', 'V V R')
    call jeecra('&INTERSLOC', 'LONMAX', 8, ' ')
    call jecreo('&INTERR1LOC', 'V V R')
    call jeecra('&INTERR1LOC', 'LONMAX', 8, ' ')
    call jecreo('&INTERR2LOC', 'V V R')
    call jeecra('&INTERR2LOC', 'LONMAX', 8, ' ')
    call jecreo('&INTERF1LOC', 'V V I')
    call jeecra('&INTERF1LOC', 'LONMAX', 8, ' ')
    call jecreo('&INTERF2LOC', 'V V I')
    call jeecra('&INTERF2LOC', 'LONMAX', 8, ' ')
    call jecreo('&INTERCOTDR', 'V V L')
    call jeecra('&INTERCOTDR', 'LONMAX', 4, ' ')
    call jecreo('&INTERXSOM', 'V V R')
    call jeecra('&INTERXSOM', 'LONMAX', 5, ' ')
    call jecreo('&INTERYSOM', 'V V R')
    call jeecra('&INTERYSOM', 'LONMAX', 5, ' ')
    call jecreo('&INTERXINT', 'V V R')
    call jeecra('&INTERXINT', 'LONMAX', 4, ' ')
    call jecreo('&INTERYINT', 'V V R')
    call jeecra('&INTERYINT', 'LONMAX', 4, ' ')
!
!---------RECUPERATION DES ADRESSES DES TABLEAUX LOCAUX-----------
!
    call jeveuo('&INTERSLOC', 'E', asloc)
    call jeveuo('&INTERR1LOC', 'E', ar1loc)
    call jeveuo('&INTERR2LOC', 'E', ar2loc)
    call jeveuo('&INTERF1LOC', 'E', af1loc)
    call jeveuo('&INTERF2LOC', 'E', af2loc)
    call jeveuo('&INTERCOTDR', 'E', acotdr)
    call jeveuo('&INTERXSOM', 'E', axsom)
    call jeveuo('&INTERYSOM', 'E', aysom)
    call jeveuo('&INTERXINT', 'E', axint)
    call jeveuo('&INTERYINT', 'E', ayint)
!
!---------BOUCLE DE BALAYAGE DES MAILLES-----------------------------
!
10  continue
    if (.not. fini) then
!
!---------------INITIALISATION DE L' INTERSECTION---------------------
!---------------AVEC LA MAILLE COURANTE          ---------------------
!
        m = m + 1
        ima = numail(m)
!
        call jeveuo(jexnum(conec, ima), 'L', adrvlc)
        call jelira(jexnum(conec, ima), 'LONMAX', nbneud, k8b)
!
!---------------RECUPERATION DU NOM DU TYPE DE LA MAILLE COURANTE----
!
        call jeveuo(typ, 'L', iatyma)
        atypm = iatyma - 1 + ima
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(atypm)), typm)
!
!---------------SI LA MAILLE COURANTE N' EST PAS UNE MAILLE---------
!---------------SURFACIQUE ALORS ON L' IGNORE              ---------
!
        if (( typm .ne. 'POI1' ) .and. ( typm .ne. 'SEG2' ) .and. ( typm .ne. 'SEG3' )) then
!
            pt = 1
!
            do 100, i = 1, 8, 1
!
            zr(asloc + i-1) = -1.0d0
            zr(ar1loc + i-1) = -1.0d0
            zr(ar2loc + i-1) = -1.0d0
            zi(af1loc + i-1) = 0
            zi(af2loc + i-1) = 0
!
100          continue
!
            do 110, i = 1, 4, 1
!
            zl(acotdr + i-1) = .false.
!
110          continue
!
!---------------SAISIE DE LA MAILLE COURANTE--------------------------
!
            call i2nbrf(nbneud, nbcote)
!
!C
            do 120, i = 1, nbcote, 1
!
            nums = zi(adrvlc+i-1)
            adrs = 3*(nums-1)
!
            zr(axsom + i-1) = zr(acoord + adrs)
            zr(aysom + i-1) = zr(acoord + adrs+1)
!
            if (nbneud .gt. nbcote) then
!
                nums = zi(adrvlc+i-1+ nbcote)
                adrs = 3*(nums-1)
!
                zr(axint + i-1) = zr(acoord + adrs)
                zr(ayint + i-1) = zr(acoord + adrs+1)
!
            else
!
                zr(axint + i-1) = 0.0d0
                zr(ayint + i-1) = 0.0d0
!
            endif
!
120          continue
!
            zr(axsom + nbcote) = zr(axsom)
            zr(aysom + nbcote) = zr(aysom)
!
!-------------BOUCLE DE BALAYAGE DES COTES DE LA MAILLE M-----------
!
            xf = zr(axsom)
            yf = zr(aysom)
!
            do 20, c = 1, nbcote, 1
!
!-------------SAISIE DU COTE COURANT-------------------------------
!
            xd = xf
            yd = yf
            xf = zr(axsom + c)
            yf = zr(aysom + c)
            xi = zr(axint + c-1)
            yi = zr(ayint + c-1)
!
            crit = sqrt( (xf-xd)**2 + (yf-yd)**2 )
            crit = epsi * crit
!
            call i2typf(crit, xd, yd, xi, yi,&
                        xf, yf, typm, droit)
!
            zl(acotdr + c-1) = droit
!
!
!-------------CALCUL DE L' INTERSECTION SEGMENT-COTE-----------------
!-------------ET RANGEMENT DES RESULTAS ELEMENTAIRES-----------------
!
            if (droit) then
!
!----------------------CAS D' UN COTE DROIT--------------------------
!
                call i2isgt(crit, xa, ya, xb, yb,&
                            xd, yd, xf, yf, nbpf,&
                            abs1, abs2, abr1, abr2)
!
                if (nbpf .ge. 1) then
!
                    call i2rgel(crit, abs1, abr1, c, zr(asloc),&
                                zr( ar1loc), zr(ar2loc), zi(af1loc), zi(af2loc), pt)
!
                endif
!
                if (nbpf .eq. 2) then
!
                    call i2rgel(crit, abs2, abr2, c, zr(asloc),&
                                zr( ar1loc), zr(ar2loc), zi(af1loc), zi(af2loc), pt)
!
                endif
!
            else
!
!-------------------CAS D' UN COTE COURBE-----------------------------
!
                call i2isgc(crit, xa, ya, xb, yb,&
                            xd, yd, xi, yi, xf,&
                            yf, nbpf, abs1, abs2, abr1,&
                            abr2, elimin)
!
                if (.not. elimin) then
!
                    if (nbpf .ge. 1) then
!
                        call i2rgel(crit, abs1, abr1, c, zr(asloc),&
                                    zr(ar1loc), zr(ar2loc), zi(af1loc), zi( af2loc), pt)
!
                    endif
!
                    if (nbpf .eq. 2) then
!
                        call i2rgel(crit, abs2, abr2, c, zr(asloc),&
                                    zr(ar1loc), zr(ar2loc), zi(af1loc), zi( af2loc), pt)
!
                    endif
!
                endif
!
            endif
!
20          continue
!
            nbpm = pt - 1
!
!--------------RANGEMENT DE L' INTERSECTION ELEMENTAIRE--------------
!--------------DANS LA STRUCTURE DE DONNEES ASSOCIEE A --------------
!--------------L' INTERSECTION GLOBALE                 --------------
!
            if (nbpm .eq. 0) then
!
                if (.not. atrouv) then
!
                    call i2appm(xa, ya, zr(axsom), zr(aysom), zr(axint),&
                                zr(ayint), zl(acotdr), nbcote, adansm)
!
                    if (adansm) then
!
                        atrouv = .true.
                        btrouv = .true.
                        or = 0.0d0
                        ex = 1.0d0
                        ror = -1.0d0
                        rex = -1.0d0
                        f1or = 0
                        f1ex = 0
                        m1 = ima
                        m2 = 0
!
                        call i2rgma(crit, or, ex, ror, rex,&
                                    m1, m2, f1or, f1ex, sgtor,&
                                    sgtex, paror, parex, facor, facex,&
                                    mail1, mail2, adrgt)
!
                        fini = .true.
!
                    endif
!
                endif
!
            endif
!
            if (nbpm .eq. 1) then
!
                s = zr(asloc)
                r1 = zr(ar1loc)
                f1 = zi(af1loc)
!
                if ((.not. atrouv) .and. ( abs(s) .lt. crit )) then
!
                    call i2appm(xb, yb, zr(axsom), zr(aysom), zr(axint),&
                                zr(ayint), zl(acotdr), nbcote, bdansm)
!
                    if (bdansm) then
!
                        or = 0.0d0
                        ex = 1.0d0
                        f1or = f1
                        f1ex = 0
                        ror = r1
                        rex = -1.0d0
                        m1 = ima
                        m2 = 0
!
                        call i2rgma(crit, or, ex, ror, rex,&
                                    m1, m2, f1or, f1ex, sgtor,&
                                    sgtex, paror, parex, facor, facex,&
                                    mail1, mail2, adrgt)
!
                        atrouv = .true.
                        btrouv = .true.
                        fini = .true.
!
                    endif
!
                endif
!
                if ((.not. btrouv) .and. (abs(1.0d0-s) .lt. crit)) then
!
                    call i2appm(xa, ya, zr(axsom), zr(aysom), zr(axint),&
                                zr(ayint), zl(acotdr), nbcote, adansm)
!
                    if (adansm) then
!
                        or = 0.0d0
                        ex = 1.0d0
                        f1ex = f1
                        f1or = 0
                        rex = r1
                        ror = -1.0d0
                        m1 = ima
                        m2 = 0
!
                        call i2rgma(crit, or, ex, ror, rex,&
                                    m1, m2, f1or, f1ex, sgtor,&
                                    sgtex, paror, parex, facor, facex,&
                                    mail1, mail2, adrgt)
!
                        atrouv = .true.
                        btrouv = .true.
                        fini = .true.
!
                    endif
!
                endif
!
                if ((abs(s).gt.crit) .and. (abs(1.0d0-s).gt.crit)) then
!
                    if (.not. atrouv) then
!
                        call i2appm(xa, ya, zr(axsom), zr(aysom), zr( axint),&
                                    zr(ayint), zl(acotdr), nbcote, adansm)
!
                        if (adansm) then
!
                            or = 0.0d0
                            ex = s
                            f1ex = f1
                            f1or = 0
                            rex = r1
                            ror = -1.0d0
                            m1 = ima
                            m2 = 0
!
                            call i2rgma(crit, or, ex, ror, rex,&
                                        m1, m2, f1or, f1ex, sgtor,&
                                        sgtex, paror, parex, facor, facex,&
                                        mail1, mail2, adrgt)
!
                            atrouv = .true.
!
                            call i2fini(crit, inf, sup, sgtor, sgtex,&
                                        mail2, adrgt, fincal)
!
                        endif
!
                    endif
!
                    if (.not. btrouv) then
!
                        call i2appm(xb, yb, zr(axsom), zr(aysom), zr( axint),&
                                    zr(ayint), zl(acotdr), nbcote, bdansm)
!
                        if (bdansm) then
!
                            ex = 1.0d0
                            or = s
                            f1or = f1
                            f1ex = 0
                            ror = r1
                            rex = -1.0d0
                            m1 = ima
                            m2 = 0
!
                            call i2rgma(crit, or, ex, ror, rex,&
                                        m1, m2, f1or, f1ex, sgtor,&
                                        sgtex, paror, parex, facor, facex,&
                                        mail1, mail2, adrgt)
!
                            btrouv = .true.
!
                            call i2fini(crit, inf, sup, sgtor, sgtex,&
                                        mail2, adrgt, fincal)
!
                        endif
!
                    endif
!
                endif
!
            endif
!
            if (nbpm .ge. 2) then
!
!--------------TRAITEMENT DES NBPM-1 SOUS SEGMENTS-------------------
!--------------DONNES PAR LA MAILLE COURANTE      -------------------
!
                ex = zr(asloc)
                f1ex = zi(af1loc)
                f2ex = zi(af2loc)
!
                do 200, k = 1, nbpm-1, 1
!
                or = ex
                ex = zr(asloc + k)
                f1or = f1ex
                f2or = f2ex
                f1ex = zi(af1loc + k)
                f2ex = zi(af2loc + k)
                fcom = 0
!
                if (f1or .eq. f1ex) then
!
                    fcom = f1or
                    ror = zr(ar1loc + k-1)
                    rex = zr(ar1loc + k)
!
                else if (f1or .eq. f2ex) then
!
                    fcom = f1or
                    ror = zr(ar1loc + k-1)
                    rex = zr(ar2loc + k)
!
                else if (f2or .eq.f1ex) then
!
                    fcom = f2or
                    ror = zr(ar2loc + k-1)
                    rex = zr(ar1loc + k)
!
                    else if ((f2or .ne. 0) .and. (f2or .eq. f2ex))&
                    then
!
                    fcom = f2or
                    ror = zr(ar2loc + k-1)
                    rex = zr(ar2loc + k)
!
                else
!
                endif
!
                if (fcom .eq. 0) then
!
                    ror = zr(ar1loc + k-1)
                    rex = zr(ar1loc + k)
                    m1 = ima
                    m2 = 0
!
                    call i2rgma(crit, or, ex, ror, rex,&
                                m1, m2, f1or, f1ex, sgtor,&
                                sgtex, paror, parex, facor, facex,&
                                mail1, mail2, adrgt)
!
                    call i2fini(crit, inf, sup, sgtor, sgtex,&
                                mail2, adrgt, fincal)
!
                    if (.not. atrouv .and. ( abs(or).lt.crit .or. abs(ex).lt.crit )) then
!
                        atrouv = .true.
!
                    endif
!
                    if (.not. btrouv .and.&
                        ( abs(1.0d0-or) .lt. crit .or. abs(1.0d0-ex) .lt. crit )) then
!
                        btrouv = .true.
!
                    endif
!
                endif
!
                if (fcom .ne. 0) then
!
                    if (zl(acotdr + fcom-1)) then
!
                        m1 = ima
                        m2 = -1
!
                        call i2rgma(crit, or, ex, ror, rex,&
                                    m1, m2, fcom, fcom, sgtor,&
                                    sgtex, paror, parex, facor, facex,&
                                    mail1, mail2, adrgt)
!
                        call i2fini(crit, inf, sup, sgtor, sgtex,&
                                    mail2, adrgt, fincal)
!
                        if (.not. atrouv .and. ( abs(or).lt.crit .or. abs(ex).lt.crit )) then
!
                            atrouv = .true.
!
                        endif
!
                        if (.not. btrouv .and.&
                            ( abs(1.0d0-or) .lt. crit .or. abs(1.0d0-ex) .lt. crit )) then
!
                            btrouv = .true.
!
                        endif
!
                    else
!
                        sm = 0.5d0*(or+ex)
                        xm = sm*(xb-xa) + xa
                        ym = sm*(yb-ya) + ya
!
                        if (fcom .ne. nbcote) then
!
                            xt = zr(axsom + fcom+1)
                            yt = zr(aysom + fcom+1)
!
                        else
!
                            xt = zr(axsom + 1)
                            yt = zr(aysom + 1)
!
                        endif
!
                        xd = zr(axsom + fcom-1)
                        yd = zr(aysom + fcom-1)
                        xf = zr(axsom + fcom)
                        yf = zr(aysom + fcom)
                        xi = zr(axint + fcom-1)
                        yi = zr(ayint + fcom-1)
!
                        call i2chax(xd, yd, xi, yi, xf,&
                                    yf, xm, ym, xt, yt,&
                                    coef, xnewm, ynewm, xnewt, ynewt)
!
                        indic = ynewm-coef*xnewm*xnewm
                        indic = indic * (ynewt-coef*xnewt*xnewt)
!
                        if (indic .ge. 0.0d0) then
!
                            m1 = ima
                            m2 = 0
!
                            call i2rgma(crit, or, ex, ror, rex,&
                                        m1, m2, fcom, fcom, sgtor,&
                                        sgtex, paror, parex, facor, facex,&
                                        mail1, mail2, adrgt)
!
                            call i2fini(crit, inf, sup, sgtor, sgtex,&
                                        mail2, adrgt, fincal)
!
                            if (.not. atrouv .and.&
                                ( abs(or) .lt.crit .or. abs(ex).lt.crit )) then
!
                                atrouv = .true.
!
                            endif
!
                            if (.not. btrouv .and.&
                                ( abs(1.0d0- or) .lt. crit .or. abs(1.0d0-ex) .lt. crit )) then
!
                                btrouv = .true.
!
                            endif
!
                        endif
!
                    endif
!
                endif
!
200              continue
!
!--------------CAS PATHOLOGIQUE DU A LA POSSIBILITE-----------------
!--------------DE PERTE DE CONVEXITE DE LA MAILLE  -----------------
!--------------COURANTE   PERTE ASSOCIEE A LA      -----------------
!--------------PRESENCE D' UNE FACE COURBE         -----------------
!
                if (.not. btrouv .and. abs(zr(asloc + nbpm-1)-1.0d0) .gt. crit) then
!
                    call i2appm(xb, yb, zr(axsom), zr(aysom), zr(axint),&
                                zr(ayint), zl(acotdr), nbcote, bdansm)
!
                    if (bdansm) then
!
                        or = zr(asloc + nbpm-1)
                        ex = 1.0d0
                        f1or = zi(af1loc + nbpm-1)
                        f1ex = 0
                        ror = zr(ar1loc + nbpm-1)
                        rex = -1.0d0
                        m1 = ima
                        m2 = 0
!
                        call i2rgma(crit, or, ex, ror, rex,&
                                    m1, m2, f1or, f1ex, sgtor,&
                                    sgtex, paror, parex, facor, facex,&
                                    mail1, mail2, adrgt)
!
                        btrouv = .true.
!
                        call i2fini(crit, inf, sup, sgtor, sgtex,&
                                    mail2, adrgt, fincal)
!
                    endif
!
                endif
!
                if (.not. atrouv .and. abs(zr(asloc)) .gt. crit .and. abs(1.0d0-zr(ar1loc))&
                    .gt. crit) then
!
                    call i2appm(xa, ya, zr(axsom), zr(aysom), zr(axint),&
                                zr(ayint), zl(acotdr), nbcote, adansm)
!
                    if (adansm) then
!
                        ex = zr(asloc)
                        or = 0.0d0
                        f1ex = zi(af1loc)
                        f1or = 0
                        rex = zr(ar1loc)
                        ror = -1.0d0
                        m1 = ima
                        m2 = 0
!
                        call i2rgma(crit, or, ex, ror, rex,&
                                    m1, m2, f1or, f1ex, sgtor,&
                                    sgtex, paror, parex, facor, facex,&
                                    mail1, mail2, adrgt)
!
                        atrouv = .true.
!
                        call i2fini(crit, inf, sup, sgtor, sgtex,&
                                    mail2, adrgt, fincal)
!
                    endif
!
                endif
!
            endif
!
        endif
!
        fini = ( fini .or. ( m .eq. nbm ) .or. fincal)
!
        goto 10
!
    endif
!
    nbseg = adrgt - 1
!
    call jedetr('&INTERSLOC')
    call jedetr('&INTERR1LOC')
    call jedetr('&INTERR2LOC')
    call jedetr('&INTERF1LOC')
    call jedetr('&INTERF2LOC')
    call jedetr('&INTERCOTDR')
    call jedetr('&INTERXSOM')
    call jedetr('&INTERYSOM')
    call jedetr('&INTERXINT')
    call jedetr('&INTERYINT')
!
    call jedema()
end subroutine
