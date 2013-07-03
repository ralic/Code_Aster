subroutine i2imac(epsi, conec, coord, typ, nbm,&
                  numail, xc, yc, r, alfinf,&
                  alfsup, nbseg, sgtor, sgtex, mail1,&
                  mail2, facor, facex, paror, parex)
! aslint: disable=W1501
    implicit  none
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
#include "jeveux.h"
!
#include "asterc/r8pi.h"
#include "asterfort/i2appm.h"
#include "asterfort/i2fini.h"
#include "asterfort/i2iacs.h"
#include "asterfort/i2nbrf.h"
#include "asterfort/i2rgel.h"
#include "asterfort/i2rgma.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/u2mess.h"
    integer :: nbm, numail(*)
    real(kind=8) :: xc, yc, r, epsi, alfinf, alfsup
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
    integer :: m, ima, nbcote, nbneud, iatyma
    character(len=8) :: typm, k8b
!
!---------------------DESCRIPTION D' UNE FACE------------------------
!
    integer :: nums, adrs, c
    real(kind=8) :: xd, yd, xf, yf
!
!---------------------DESCRIPTION DE L' INTERSECTION AVEC UNE MAILLE--
!
    real(kind=8) :: abs1, abs2, abr1, abr2
    integer :: nbpm, nbpf
!
!---------------------GESTION DE L' INTERSECTION---------------------
!
    integer :: i, adrgt, k, f1or, f2or, f1ex, f2ex, fcom, m1, m2, pt, f1
    logical :: adansm, bdansm, elimin, atrouv, btrouv, fini, fincal
    real(kind=8) :: or, ex, ror, rex, xm, ym
    real(kind=8) :: s, r1, xa, ya, xb, yb, sm
!
!---------------------DIVERS-----------------------------------------
!
    real(kind=8) :: pi
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
    xf = 0.0d0
    yf = 0.0d0
    xm = 0.0d0
    ym = 0.0d0
    or = 0.0d0
    ex = 0.0d0
    ror = 0.0d0
    rex = 0.0d0
    s = 0.0d0
    r1 = 0.0d0
    pi=r8pi()
!
    if (alfinf .ge. alfsup) call u2mess('F', 'INTEMAIL_3')
    if (alfinf .lt. -pi .or. alfsup .gt. pi) call u2mess('F', 'INTEMAIL_4')
!
    xa = xc + r*cos(alfinf)
    ya = yc + r*sin(alfinf)
    xb = xc + r*cos(alfsup)
    yb = yc + r*sin(alfsup)
!
!---------RECUPERATION DES ADRESSES DES OBJETS JEVEUX-----------------
!
    call jeveuo(conec, 'L', aconec)
    call jeveuo(coord, 'L', acoord)
!
!---------CREATION DES TABLEAUX LOCAUX JEVEUX-----------------------
!
    call jecreo('&INTERSLOC', 'V V R')
    call jeecra('&INTERSLOC', 'LONMAX', 16, ' ')
    call jecreo('&INTERR1LOC', 'V V R')
    call jeecra('&INTERR1LOC', 'LONMAX', 16, ' ')
    call jecreo('&INTERR2LOC', 'V V R')
    call jeecra('&INTERR2LOC', 'LONMAX', 16, ' ')
    call jecreo('&INTERF1LOC', 'V V I')
    call jeecra('&INTERF1LOC', 'LONMAX', 16, ' ')
    call jecreo('&INTERF2LOC', 'V V I')
    call jeecra('&INTERF2LOC', 'LONMAX', 16, ' ')
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
!---------------AVEC LA MAILLE &INTERCOURANTE    ---------------------
!
        m = m + 1
        ima = numail(m)
!
        call jeveuo(jexnum(conec, ima), 'L', adrvlc)
        call jelira(jexnum(conec, ima), 'LONMAX', nbneud, k8b)
!
!-------------RECUPERATION DU NOM DU TYPE DE LA MAILLE COURANTE----
!
        call jeveuo(typ, 'L', iatyma)
        atypm = iatyma - 1 + ima
        call jenuno(jexnum('&CATA.TM.NOMTM', zi(atypm)), typm)
!
!---------------SI LA MAILLE COURANTE N' EST PAS UNE MAILLE---------
!---------------SURFACIQUE ALORS ON L' IGNORE              ---------
!
        if ((typm .ne. 'POI1') .and. (typm .ne. 'SEG2') .and. (typm .ne. 'SEG3')) then
!
            pt = 1
!
            do 100, i = 1, 16, 1
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
            zl(acotdr + i-1) = .true.
!
110          continue
!
!---------------SAISIE DE LA MAILLE COURANTE--------------------------
!
            call i2nbrf(nbneud, nbcote)
!
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
!
!-------------CALCUL DE L' INTERSECTION SEGMENT-COTE-----------------
!-------------ET RANGEMENT DES RESULTAS ELEMENTAIRES-----------------
!
!
!----------------------CAS D' UN COTE DROIT--------------------------
!
            call i2iacs(epsi, xc, yc, r, alfinf,&
                        alfsup, xd, yd, xf, yf,&
                        nbpf, abs1, abs2, abr1, abr2,&
                        elimin)
!
            if (.not. elimin) then
!
                if (nbpf .ge. 1) then
!
                    call i2rgel(epsi, abs1, abr1, c, zr(asloc),&
                                zr( ar1loc), zr(ar2loc), zi(af1loc), zi(af2loc), pt)
!
                endif
!
                if (nbpf .eq. 2) then
!
                    call i2rgel(epsi, abs2, abr2, c, zr(asloc),&
                                zr( ar1loc), zr(ar2loc), zi(af1loc), zi(af2loc), pt)
!
                endif
!
            endif
!
!
!-------------------CAS D' UN COTE COURBE NON TRAITE (FICHE 19855)
!
!
20          continue
!
            nbpm = pt - 1
            if (nbpm .ge. 1) then
            endif
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
                        or = alfinf
                        ex = alfsup
                        ror = -1.0d0
                        rex = -1.0d0
                        f1or = 0
                        f1ex = 0
                        m1 = ima
                        m2 = 0
!
                        call i2rgma(epsi, or, ex, ror, rex,&
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
                if ((.not. atrouv) .and. ( abs(s-alfinf) .lt. epsi )) then
!
                    call i2appm(xb, yb, zr(axsom), zr(aysom), zr(axint),&
                                zr(ayint), zl(acotdr), nbcote, bdansm)
!
                    if (bdansm) then
!
                        or = alfinf
                        ex = alfsup
                        f1or = f1
                        f1ex = 0
                        ror = r1
                        rex = -1.0d0
                        m1 = ima
                        m2 = 0
!
                        call i2rgma(epsi, or, ex, ror, rex,&
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
                if ((.not. btrouv) .and. (abs(alfsup-s).lt.epsi)) then
!
                    call i2appm(xa, ya, zr(axsom), zr(aysom), zr(axint),&
                                zr(ayint), zl(acotdr), nbcote, adansm)
!
                    if (adansm) then
!
                        or = alfinf
                        ex = alfsup
                        f1ex = f1
                        f1or = 0
                        rex = r1
                        ror = -1.0d0
                        m1 = ima
                        m2 = 0
!
                        call i2rgma(epsi, or, ex, ror, rex,&
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
                if (( abs(alfinf-s) .gt. epsi ) .and. ( abs(alfsup-s) .gt. epsi )) then
!
                    if (.not. atrouv) then
!
                        call i2appm(xa, ya, zr(axsom), zr(aysom), zr( axint),&
                                    zr(ayint), zl(acotdr), nbcote, adansm)
!
                        if (adansm) then
!
                            or = alfinf
                            ex = s
                            f1ex = f1
                            f1or = 0
                            rex = r1
                            ror = -1.0d0
                            m1 = ima
                            m2 = 0
!
                            call i2rgma(epsi, or, ex, ror, rex,&
                                        m1, m2, f1or, f1ex, sgtor,&
                                        sgtex, paror, parex, facor, facex,&
                                        mail1, mail2, adrgt)
!
                            atrouv = .true.
!
                            call i2fini(epsi, alfinf, alfsup, sgtor, sgtex,&
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
                            ex = alfsup
                            or = s
                            f1or = f1
                            f1ex = 0
                            ror = r1
                            rex = -1.0d0
                            m1 = ima
                            m2 = 0
!
                            call i2rgma(epsi, or, ex, ror, rex,&
                                        m1, m2, f1or, f1ex, sgtor,&
                                        sgtex, paror, parex, facor, facex,&
                                        mail1, mail2, adrgt)
!
                            btrouv = .true.
!
                            call i2fini(epsi, alfinf, alfsup, sgtor, sgtex,&
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
                sm = 0.5d0*(or+ex)
                xm = xc + r*cos(sm)
                ym = yc + r*sin(sm)
!
                call i2appm(xm, ym, zr(axsom), zr(aysom), zr(axint),&
                            zr(ayint), zl(acotdr), nbcote, elimin)
!
                if (elimin) then
!
                    ror = zr(ar1loc + k-1)
                    rex = zr(ar1loc + k)
                    m1 = ima
                    m2 = 0
!
                    if (fcom .ne. 0) then
!
                        f1or = fcom
                        f2or = fcom
!
                    endif
!
                    call i2rgma(epsi, or, ex, ror, rex,&
                                m1, m2, f1or, f1ex, sgtor,&
                                sgtex, paror, parex, facor, facex,&
                                mail1, mail2, adrgt)
!
                    call i2fini(epsi, alfinf, alfsup, sgtor, sgtex,&
                                mail2, adrgt, fincal)
!
                    if ((.not. atrouv) .and.&
                        ( (abs(or-alfinf) .lt.epsi) .or. (abs(ex-alfinf).lt.epsi) )) then
!
                        atrouv = .true.
!
                    endif
!
                    if ((.not. btrouv) .and.&
                        ( (abs(or-alfsup) .lt.epsi) .or. (abs(ex-alfsup).lt.epsi) )) then
!
                        btrouv = .true.
!
                    endif
!
                endif
!
200              continue
!
!--------------CAS PATHOLOGIQUE DU A LA POSSIBILITE-----------------
!--------------DE PRESENCE DE A ET/OU B DANS LA    -----------------
!--------------MAILLE COURANTE, BIEN QUE CETTE     -----------------
!--------------MAILLE DONNE PLUS DE 2 POINTS       -----------------
!
                if ((.not. btrouv) .and. (abs(zr(asloc + nbpm-1)- alfsup) .gt. epsi)) then
!
                    call i2appm(xb, yb, zr(axsom), zr(aysom), zr(axint),&
                                zr(ayint), zl(acotdr), nbcote, bdansm)
!
                    if (bdansm) then
!
                        or = zr(asloc + nbpm-1)
                        ex = alfsup
                        f1or = zi(af1loc + nbpm-1)
                        f1ex = 0
                        ror = zr(ar1loc + nbpm-1)
                        rex = -1.0d0
                        m1 = ima
                        m2 = 0
!
                        call i2rgma(epsi, or, ex, ror, rex,&
                                    m1, m2, f1or, f1ex, sgtor,&
                                    sgtex, paror, parex, facor, facex,&
                                    mail1, mail2, adrgt)
!
                        btrouv = .true.
!
                        call i2fini(epsi, alfinf, alfsup, sgtor, sgtex,&
                                    mail2, adrgt, fincal)
!
                    endif
!
                endif
!
                if ((.not. atrouv) .and. (abs(zr(asloc)-alfinf) .gt. epsi)) then
!
                    call i2appm(xa, ya, zr(axsom), zr(aysom), zr(axint),&
                                zr(ayint), zl(acotdr), nbcote, adansm)
!
                    if (adansm) then
!
                        ex = zr(asloc)
                        or = alfinf
                        f1ex = zi(af1loc)
                        f1or = 0
                        rex = zr(ar1loc)
                        ror = -1.0d0
                        m1 = ima
                        m2 = 0
!
                        call i2rgma(epsi, or, ex, ror, rex,&
                                    m1, m2, f1or, f1ex, sgtor,&
                                    sgtex, paror, parex, facor, facex,&
                                    mail1, mail2, adrgt)
!
                        atrouv = .true.
!
                        call i2fini(epsi, alfinf, alfsup, sgtor, sgtex,&
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
