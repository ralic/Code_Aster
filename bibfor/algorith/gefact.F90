subroutine gefact(duree, nominf)
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
! GENERATION DE FCT ALEATOIRES :
!        - PREPARATION (DISCRETISATION-PROLONGEMENT) DE L INTERSPECTRE
!            (EN FONCTION DES CARACTERISTIQUES DU TEMPOREL A GENERER)
!        - FACTORISATION DE L INTERSPECTRE
!
! ----------------------------------------------------------------------
!
!      IN   :
!             DUREE : DUREE DU TEMPOREL A SIMULER (NEGATIVE SI ELLE
!                     N'EST PAS UNE DONNEE)
!      OUT  :
!             NOMINF : NOM DE L'INTERSPECTRE FACTORISE
!
! ----------------------------------------------------------------------
!
!
    implicit none
!
! 0.1. ==> ARGUMENTS
!
    include 'jeveux.h'
!
    include 'asterc/getres.h'
    include 'asterc/getvid.h'
    include 'asterc/getvis.h'
    include 'asterc/getvr8.h'
    include 'asterc/getvtx.h'
    include 'asterc/r8prem.h'
    include 'asterfort/assert.h'
    include 'asterfort/facint.h'
    include 'asterfort/folocx.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jedetr.h'
    include 'asterfort/jeexin.h'
    include 'asterfort/jelira.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/jexnum.h'
    include 'asterfort/ordis.h'
    include 'asterfort/titre.h'
    include 'asterfort/u2mesg.h'
    include 'asterfort/u2mesk.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/wkvect.h'
    real(kind=8) :: duree
!
!
! 0.2. ==> COMMUNS
!
! 0.3. ==> VARIABLES LOCALES
!
!
    integer :: l, n1, nbpoin, nbpini, iret, ier
    integer :: dim, long, dim2, dim3, dim4
    integer :: nnn, nbmr, nbpt1, nbpt2, longh
    integer :: nbfc, nbval, nbval1, indice
    integer :: i, ii, jj, j, k, kf, lval2, lvalc, ipas
    integer :: iinf, isup, ls, lr, ld, lu, lv, lw, ix, iy
    integer :: lval, lval1, lchdes, inuor, lnuor, jnuor
    integer :: vali
!
    real(kind=8) :: prec, r8b, epsi
    real(kind=8) :: freqi, freqf, freq, frinit, fmax, fmin
    real(kind=8) :: pmin, dfreq, difpas, dt
    real(kind=8) :: pas, pas1, resure, resuim, x1, y1, x2, y2
    real(kind=8) :: pui2, pui2d, pui3d
    real(kind=8) :: valr
!
    character(len=1) :: coli
    character(len=3) :: interp
    character(len=8) :: k8b, intesp
    character(len=16) :: k16bid, nomcmd, prolgd
    character(len=19) :: k19bid, nominf, nomint
    character(len=24) :: chvale, chdesc, chnuor, nomobj
    character(len=24) :: chnumi, chnumj, chfreq, chval
    logical :: lfreqf, lfreqi, lnbpn, linter, lprem, diag
    integer :: iarg
    integer :: i1, lnumi, lnumj, lfreq, nbfreq
!
!     ----------------------------------------------------------------
!     --- INITIALISATION  ---
!
    call jemarq()
!
    call getres(k19bid, k16bid, nomcmd)
!
!===============
! 1. LECTURE DES DONNEES LIEES A L INTERSPECTRE ET VERIFS
!===============
!
    call getvid(' ', 'INTE_SPEC', 1, iarg, 1,&
                nomint, l)
!
    call getvtx(' ', 'INTERPOL', 1, iarg, 2,&
                interp, n1)
    linter = (interp.eq.'NON')
!
    call getvis(' ', 'NB_POIN', 0, iarg, 1,&
                nbpoin, l)
    lnbpn = l .ne. 0
    nbpini = nbpoin
!
!=====
!  1.1  RECUPARATION DES DIMENSIONS, DES NUMEROS D'ORDRE, ...
!=====
    intesp = nomint(1:8)
    chnumi = intesp//'.NUMI'
    chnumj = intesp//'.NUMJ'
    chfreq = intesp//'.FREQ'
    chval = intesp//'.VALE'
    call jeveuo(chnumi, 'L', lnumi)
    call jeveuo(chnumj, 'L', lnumj)
    call jeveuo(chfreq, 'L', lfreq)
    call jelira(chnumi, 'LONMAX', nbmr, k8b)
    call jelira(chfreq, 'LONMAX', nbfreq, k8b)
!
    nomobj = '&&GEFACT.TEMP.NUOR'
    call wkvect(nomobj, 'V V I', nbmr, jnuor)
    do 150 i1 = 1, nbmr
        zi(jnuor-1+i1) = zi(lnumi-1+i1)
150  end do
    call ordis(zi(jnuor), nbmr)
    call wkvect('&&GEFACT.MODE', 'V V I', nbmr, inuor)
    nnn = 1
    zi(inuor) = zi(jnuor)
    do 10 i = 2, nbmr
        if (zi(jnuor+i-1) .eq. zi(inuor+nnn-1)) goto 10
        nnn = nnn + 1
        zi(inuor+nnn-1) = zi(jnuor+i-1)
10  end do
!
!=====
! 1.2 DONNEES ECHANTILLONNAGE FREQUENTIEL, DEDUCTION DES DONNEES
!     MANQUANTES (POUR RESPECTER ENTRE AUTRE LE TH. DE SHANNON)
!=====
    dim = nnn
    nbfc = dim * ( dim + 1 ) / 2
    call assert(nbfc.eq.nbmr)
!
! 1.2.1 CAS OU ON UTILISE LA DISCRETISATION DE L INTERSPECTRE :
!     VERIFICATION DE LA COHERENCE DE LA DISCRETISATION DES FONCTIONS
!     DANS LE CAS OU CETTE DISCRETISATION EST CONSERVEE
    if (linter) then
        nbval = nbfreq
        pas = (zr(lfreq+nbval-1)-zr(lfreq))/ (nbval-1)
        prec = 1.d-06
        do 100 ii = 1, nbval-1
            pas1 = zr(lfreq+ii) - zr(lfreq+ii-1)
            difpas = abs(pas1-pas)
            if (difpas .gt. prec) then
                call u2mess('F', 'ALGORITH3_78')
            endif
100      continue
!
        if (( lnbpn ) .and. (nbpini.lt.nbval)) then
            freqf = zr(lfreq+nbpini-1)
            valr = freqf
            call u2mesg('A', 'ALGORITH15_11', 0, ' ', 0,&
                        0, 1, valr)
        else
            freqf = zr(lfreq+nbval-1)
        endif
        dfreq = pas
        duree = 1.d0 / dfreq
        freqi = zr(lfreq)
        frinit = mod(freqi,dfreq)
!
        nbpoin = 2**(int(log(freqf/dfreq)/log(2.d0))+1)
        if (lnbpn) then
            if (nbpoin .gt. nbpini) then
                vali = nbpoin
                r8b = 0.d0
                call u2mesg('A', 'ALGORITH15_12', 0, ' ', 1,&
                            vali, 0, r8b)
            else
                pui2 = log(dble(nbpini))/log(2.d0)
                pui2d = abs( pui2 - aint( pui2 ))
                pui3d = abs( 1.d0 - pui2d )
                if (pui2d .ge. 1.d-06 .and. pui3d .ge. 1.d-06) then
                    nbpini = 2**(int(pui2)+1)
                    call u2mess('A', 'ALGORITH3_80')
                endif
                nbpoin = nbpini
            endif
        endif
!
    else
! 1.2.2 CAS OU ON PEUT INTERPOLER L INTERSPECTRE
!
        call getvr8(' ', 'FREQ_FIN', 0, iarg, 1,&
                    freqf, l)
        lfreqf = l .ne. 0
!
        call getvr8(' ', 'FREQ_INIT', 0, iarg, 1,&
                    freqi, l)
        lfreqi = l .ne. 0
!
!      RECHERCHE DES FREQUENCES MIN ET MAX ET DU PAS EN FREQUENCE MIN
!      DE L INTERSPECTRE
        pmin=1.d+10
        fmax = 0.d0
        fmin = 1.d+10
        nbval = nbfreq
        do 35 j = 1, nbval-1
            pas= abs(zr(lfreq+j) - zr(lfreq+j-1))
            if (pas .lt. pmin) pmin = pas
35      continue
        freq = zr(lfreq+nbval-1)
        if (freq .gt. fmax) fmax = freq
        freq = zr(lfreq)
        if (freq .lt. fmin) fmin = freq
!
        if (.not. lfreqf) freqf = fmax
        if (.not. lfreqi) freqi = fmin
!
!     DETERMINATION DES PARAMETRES DE L ALGO.
        if (duree .gt. 0.d0) then
!     LA DUREE EST UNE DONNEE
            dfreq = 1.d0 / duree
            if (lnbpn) then
                dt = duree/nbpoin/2.d0
                if (1.d0/dt .lt. 2.d0*freqf) then
                    nbpoin = 2**(int(log(2.d0*freqf*duree)/log(2.d0)))
                    vali = nbpoin
                    r8b = 0.d0
                    call u2mesg('A', 'ALGORITH15_13', 0, ' ', 1,&
                                vali, 0, r8b)
                endif
            else
                nbpoin = 2**(int(log(2.d0*freqf*duree)/log(2.d0)))
                if (( dfreq.gt. 2*pmin) .and. (pmin.gt.0.d0)) then
                    valr = 1.d0/pmin
                    call u2mesg('A', 'ALGORITH15_14', 0, ' ', 0,&
                                0, 1, valr)
                endif
            endif
        else
!     LA DUREE EST UNE INCONNUE
            if (lnbpn) then
                pui2 = log(dble(nbpoin))/log(2.d0)
                pui2d = abs( pui2 - aint( pui2 ))
                pui3d = abs( 1.d0 - pui2d )
                if (pui2d .ge. 1.d-06 .and. pui3d .ge. 1.d-06) then
                    call u2mess('A', 'ALGORITH3_80')
                    nbpoin = 2**(int(pui2)+1)
                endif
                dfreq=freqf/(nbpoin-1)
                frinit = freqf - dble(nbpoin)*dfreq
                if (frinit .lt. 0.d0) frinit =0.d0
                if ((dfreq .gt. pmin ) .and. (pmin.gt.0.d0)) then
                    vali = nbpoin
                    valr = (freqf-freqi)/pmin+1
                    call u2mesg('A', 'ALGORITH15_15', 0, ' ', 1,&
                                vali, 1, valr)
                endif
            else
                if (pmin .gt. 0.d0) then
                    dfreq=pmin
                    nbpoin = 2**(int(log(2.d0*freqf/pmin)/log(2.d0)))
                    if (nbpoin .lt. 256) nbpoin =256
                else
                    nbpoin =256
                    dfreq = (freqf-freqi)/dble(nbpoin-1)
                endif
            endif
            duree = 1.d0 / dfreq
        endif
!
        if (dble(nbpoin-1)*dfreq .gt. (freqf-freqi)) then
            frinit = freqf - dble(nbpoin)*dfreq
            if (frinit .lt. 0.d0) frinit =0.d0
        else
            frinit = freqi
        endif
    endif
!
!
!===============
! 3. LECTURE DES VALEURS DES FONCTIONS ET/OU INTERPOLATION-PROLONGEMENT
!===============
!
    nbpt1 = nbpoin
    nbpt2 = nbpoin*2
    long = nbfc*nbpt2 + nbpt1
    longh = dim*dim*nbpt2 + nbpt1
!
!     --- CREATION D'UN VECTEUR TEMP.VALE POUR STOCKER LES VALEURS
!           DES FONCTIONS  ---
!
    call wkvect('&&GEFACT.TEMP.VALE', 'V V R', long, lval)
!
!
!     --- ON STOCKE LES FREQUENCES ET ON RECHERCHE LES INDICES AU
!         DE LA DES QUELLES LA MATRICE EST NULLE---
    lprem = .true.
    iinf = 0
    isup = nbpt1+1
    do 70 k = 1, nbpt1
        freq = frinit + (k-1)*dfreq
        zr(lval+k-1) = freq
        if (freq .lt. freqi) iinf= k
        if ((freq.gt.freqf) .and. lprem) then
            isup = k
            lprem = .false.
        endif
70  end do
!     ------------------------------------------------------------------
!     --- CHANGER LA FREQ INIT. A 0 HZ POUR LE CAS SANS INTERPOL
    if (linter) zr(lval)=0.d0
!     ------------------------------------------------------------------
    lval1 = lval + nbpt1
!
!     --- POUR CHAQUE FONCTION CALCUL DE X,Y POUR CHAQUE FREQ.
!     (ON PROLONGE PAR 0 EN DEHORS DE (FREQI,FREQF)), PUIS ON STOCKE ---
    do 80 kf = 1, nbfc
        call jeveuo(jexnum(chval, kf), 'L', lval2)
        diag = .false.
        if (zi(lnumi-1+kf) .eq. zi(lnumj-1+kf)) diag = .true.
!
        k8b = ' '
        do 120 ipas = 1, nbpt1
            freq = frinit + (ipas-1)*dfreq
            ix = lval1 + (kf-1)*nbpt2 + ipas - 1
            iy = lval1 + (kf-1)*nbpt2 + ipas - 1 + nbpt1
            if ((ipas.le.iinf) .or. (ipas.ge.isup)) then
                zr(ix) = 0.d0
                zr(iy) = 0.d0
            else
                if (linter) then
                    if (diag) then
                        resure = zr(lval2+ipas-iinf-1)
                        resuim = 0.d0
                    else
                        resure = zr(lval2+2*(ipas-iinf-1))
                        resuim = zr(lval2+2*(ipas-iinf-1)+1)
                    endif
                else
! ON INTERPOLLE
                    prolgd = 'CC      '
                    epsi = sqrt ( r8prem() )
                    call folocx(zr(lfreq), nbfreq, freq, prolgd, indice,&
                                epsi, coli, ier)
                    if (coli .eq. 'C') then
                        if (diag) then
                            resure=zr(lval2+indice-1)
                            resuim = 0.d0
                        else
                            resure=zr(lval2+2*(indice-1))
                            resuim=zr(lval2+2*(indice-1)+1)
                        endif
                    else if ((coli.eq.'I') .or. (coli.eq.'E')) then
                        x1 = zr(lfreq+indice-1)
                        x2 = zr(lfreq+indice)
                        if (diag) then
                            y1 = zr(lval2+indice-1)
                            y2 = zr(lval2+indice)
                            resure= y1+(freq-x1)*(y2-y1)/(x2-x1)
                            resuim = 0.d0
                        else
                            y1 = zr(lval2+2*(indice-1))
                            y2 = zr(lval2+2*indice)
                            resure= y1+(freq-x1)*(y2-y1)/(x2-x1)
                            y1 = zr(lval2+2*(indice-1)+1)
                            y2 = zr(lval2+2*indice+1)
                            resuim= y1+(freq-x1)*(y2-y1)/(x2-x1)
                        endif
                    else
                        call u2mesk('A', 'PREPOST3_6', 1, coli)
                    endif
                endif
                zr(ix) = resure
                zr(iy) = resuim
            endif
120      continue
80  end do
    nbval1 = nbpt1
!
!===============
! 4. FACTORISATION DES MATRICES INTERSPECTRALES (UNE PAR FREQ.)
!===============
!
!               1234567890123456789
    nominf = '&&INTESPECFACT     '
!
!     --- CREATION DE L'OBJET NOMINF//'.VALE'
    chvale = nominf//'.VALE'
    call wkvect(chvale, 'V V R', longh, lvalc)
!
!     --- CREATION DE L'OBJET NOMINF//'.DESC'
    chdesc = nominf//'.DESC'
    call wkvect(chdesc, 'V V I', 3, lchdes)
    zi(lchdes) = nbval1
    zi(lchdes+1) = dim
    zi(lchdes+2) = dim*dim
!
!     --- CREATION DE L'OBJET NOMINF//'.NUOR'
    chnuor = nominf//'.NUOR'
    call wkvect(chnuor, 'V V I', dim, lnuor)
    call jeveuo('&&GEFACT.MODE', 'L', inuor)
    do 125 i = 1, dim
        zi(lnuor-1+i) = zi(inuor-1+i)
125  end do
!
    dim2 = dim*dim
    dim3 = dim2 + dim
    dim4 = 2*dim
    call wkvect('&&GEFACT.TEMP.VALS', 'V V C', dim2, ls)
    call wkvect('&&GEFACT.TEMP.VALR', 'V V C', dim2, lr)
    call wkvect('&&GEFACT.TEMP.VALD', 'V V R', dim, ld)
    call wkvect('&&GEFACT.TEMP.VALU', 'V V C', dim2, lu)
    call wkvect('&&GEFACT.TEMP.VALV', 'V V R', dim3, lv)
    call wkvect('&&GEFACT.TEMP.VALW', 'V V C', dim4, lw)
!
    call facint(nbval1, dim, longh, zr(lval), zr(lvalc),&
                long, zc(ls), zc(lr), zr(ld), zc(lu),&
                zr(lv), zc(lw))
!
    nbpt1 = nbval1
    do 130 jj = 1, nbpt1
        zr(lvalc+jj-1) = zr(lval+jj-1)
130  end do
!
    call titre()
!
    call jedetr('&&GEFACT.MODE')
    call jedetr(nomobj)
    call jeexin('&&GEFACT.FONCTION', iret)
    if (iret .ne. 0) call jedetr('&&GEFACT.FONCTION')
    call jedetr('&&GEFACT.TEMP.VALE')
    call jedetr('&&GEFACT.TEMP.VALD')
    call jedetr('&&GEFACT.TEMP.VALR')
    call jedetr('&&GEFACT.TEMP.VALS')
    call jedetr('&&GEFACT.TEMP.VALU')
    call jedetr('&&GEFACT.TEMP.VALV')
    call jedetr('&&GEFACT.TEMP.VALW')
    call jedetr('&&GEFACT.TEMP.FONC')
    call jedema()
end subroutine
