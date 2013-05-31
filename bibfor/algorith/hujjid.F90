subroutine hujjid(mod, mater, indi, deps, prox,&
                  proxc, yd, yf, vind, r,&
                  drdy, iret)
! aslint: disable=W1501
    implicit none
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!  --------------------------------------------------------------------
!  INTEGRATION PLASTIQUE (MECANISME DEVIATOIRE SEUL) DE LA LOI HUJEUX
!
!  RESOLUTION PAR METHODE DE NEWTON   DRDY(DY).DDY = - R(DY)
!
!  CALCUL DU SECOND MEMBRE : - R(DY)
!  CALCUL DU JACOBIEN      : DRDY(DY)
!  DY   =  ( SIG     , ESPVP     , R       , LAMBDA   )
!  R    = -( LE      , LEVP      , LR      , LF       )
!  DRDY =  ( DLEDS   , DLEDEVP   , DLEDR   , DLEDLA   )
!          ( DLEVPDS , DLEVPDEVP , DLEVPDR , DLEVPDLA )
!          ( DLRDS   , DLRDEVP   , DLRDR   , DLRDLA   )
!          ( DLFDS   , DLFDEVP   , DLFDR   , DLFDLA   )
! =====================================================================
!  IN   MOD   :  MODELISATION
!       MATER :  COEFFICIENTS MATERIAU
!       INDI  :  INDICE DES MECANISMES SUPPOSES ACTIFS
!       DEPS  :  INCREMENT DE DEFORMATION
!       YD    :  VARIABLES A T = (SIGD, VIND, DLAMBDAD)
!       YF    :  VARIABLES A T+DT = (SIGF, VINF, DLAMBDAF)
!       VIND  :  VARIABLES INTERNES A T
!  VAR  IND   :  TABLEAU DES NUMEROS DE MECANISMES ACTIFS
!  OUT  R     :  SECOND MEMBRE
!       DRDY  :  JACOBIEN
!       IRET  :  CODE RETOUR
!                = 0 OK
!                = 1 NOOK : SI LA SUBDIVISION DU PAS DE TEMPS EST ACTIV
!                           DANS STAT_NON_LINE, IL Y A SUBDIVISION
! =====================================================================
! =====================================================================
    include 'jeveux.h'
    include 'asterfort/hujddd.h'
    include 'asterfort/hujksi.h'
    include 'asterfort/hujpic.h'
    include 'asterfort/hujprc.h'
    include 'asterfort/hujprj.h'
    include 'asterfort/hujpxd.h'
    include 'asterfort/infniv.h'
    include 'asterfort/lcicma.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcprmm.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/tecael.h'
    include 'asterfort/trace.h'
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi, nmod, i, j, k, kk, l
    integer :: indi(7), nbmeca, iret, iadzi, iazk24
    integer :: ifm, niv, nbmect
    parameter   (nmod = 18)
    real(kind=8) :: depsp(6), depse(6)
    real(kind=8) :: sigd(3), sigf(6), p(7), q(7)
    real(kind=8) :: yd(nmod), yf(nmod), drdy(nmod, nmod)
    real(kind=8) :: mater(22, 2), n, beta, d, m, pco, pref, pc
    real(kind=8) :: degr, phi, angdil, mdil, b, dksidr(7)
    real(kind=8) :: rc(7), dlambd(7), depsds(6, 6)
    real(kind=8) :: hooknl(6, 6), hook(6, 6), dhokds(6, 6)
    real(kind=8) :: i1f, e, nu, al, demu, coef0, dcoef0
    real(kind=8) :: le(6), levp, lr(4), lf(7), r(nmod), delta(6)
    real(kind=8) :: dleds(6, 6), dledev(6), dledr(6, 4), dledla(6, 7)
    real(kind=8) :: dlevds(6), dlevde, dlevdr(4), dlevdl(7)
    real(kind=8) :: dlrds(4, 6), dlrdle(4), dlrdr(4, 4), dlrdla(4, 7)
    real(kind=8) :: dlfds(7, 6), dlfdle(7), dlfdr(7, 4), dlfdla(7, 7)
    real(kind=8) :: cde(6), ctild(6), cd2fds(6, 6)
    real(kind=8) :: dledr1(6), psi(42), ad(7), ksi(7)
    real(kind=8) :: dpsids(6, 6), dfds(6), dlek(6)
    real(kind=8) :: epsvp, deps(6), th(2), prod
    real(kind=8) :: acyc, amon, cmon, ccyc, xh(2)
    real(kind=8) :: zero, un, d12, d13, deux, la, alpha
    real(kind=8) :: tole1, coef, mul, ccond, vind(*), si, sigdc(9)
    real(kind=8) :: prodc, prodm, ps, scxh, sxh, fac
    real(kind=8) :: e1, e2, e3, nu12, nu13, nu23, g1, g2, g3, nu21, nu31, nu32
    real(kind=8) :: ptrac, piso, pk, dpsi, denom, pcoh
    real(kind=8) :: sc(6), tc(6), xc(6), scxc, xctc, rtrac
    character(len=8) :: mod, nomail
    logical :: debug, prox(4), proxc(4), dila
! =====================================================================
    parameter   ( d12    = 0.5d0  )
    parameter   ( d13    = 0.333333333334D0  )
    parameter   ( un     = 1.d0   )
    parameter   ( zero   = 0.d0   )
    parameter   ( deux   = 2.d0   )
    parameter   ( tole1   = 1.d-7 )
    parameter   ( degr = 0.0174532925199D0 )
!
! =====================================================================
    common /tdim/   ndt, ndi
    common /meshuj/ debug
! =====================================================================
    call infniv(ifm, niv)
! =====================================================================
! --- PROPRIETES HUJEUX MATERIAU --------------------------------------
! =====================================================================
    n = mater(1,2)
    beta = mater(2,2)
    d = mater(3,2)
    b = mater(4,2)
    phi = mater(5,2)
    angdil = mater(6,2)
    pco = mater(7,2)
    pref = mater(8,2)
    acyc = mater(9,2)
    amon = mater(10,2)
    ccyc = deux*mater(11,2)
    cmon = mater(12,2)
    m = sin(degr*phi)
    mdil = sin(degr*angdil)
    coef = mater(20,2)
    alpha = coef*d12
    ptrac = mater(21,2)
    piso = zero
!
! --- PARAMETRE NECESSAIRE POUR GERER LA TRACTION
    rtrac = 1.d-6 * abs(pref)
! =====================================================================
! --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES --------------------
! =====================================================================
    i1f = d13 * trace(ndi,yf)
    if ((i1f/pref) .lt. tole1) i1f = tole1*pref
!
    do 11 i = 1, 4
        prox(i) = .false.
        proxc(i) = .false.
11  continue
!
    do 6 i = 1, ndt
        sigf(i) = yf(i)
        psi(i) = zero
        psi(ndt+i) = zero
        psi(2*ndt+i) = zero
        psi(3*ndt+i) = zero
        psi(4*ndt+i) = zero
        psi(5*ndt+i) = zero
        psi(6*ndt+i) = zero
 6  continue
!
    do 3 i = 1, 9
        sigdc(i)=zero
 3  continue
!
    nbmeca = 0
    nbmect = 0
    do 4 k = 1, 7
        if (indi(k) .gt. 0) then
            nbmect = nbmect + 1
            if (indi(k) .le. 8) nbmeca = nbmeca + 1
        endif
        dlambd(k) = zero
        ad(k) = zero
        ksi(k) = zero
        q(k) = zero
        p(k) = zero
 4  continue
!
    do 5 k = 1, nbmect
        kk = indi(k)
!
        dlambd(k) = yf(ndt+1+nbmeca+k)
!
        if (kk .le. 8) rc(k) = yf(ndt+1+k)
!
        call hujddd('PSI   ', indi(k), mater, indi, yf,&
                    vind, psi((k-1)*ndt+1), dpsids, iret)
        if (iret .eq. 1) goto 1000
!
        if (indi(k) .lt. 4) then
!
            call hujprj(indi(k), sigf, sigd, p(k), q(k))
            if (p(k) .ge. ptrac) goto 999
            call hujksi('DKSIDR', mater, rc(k), dksidr(k), iret)
            call hujksi('KSI   ', mater, rc(k), ksi(k), iret)
            if (iret .eq. 1) goto 1000
            ad(k) = acyc+ksi(k)*(amon-acyc)
!
        else if (indi(k) .eq. 4) then
!
            ksi(k) = un
            p(k) = (yf(1)+yf(2)+yf(3))*d13
!
        else if ((indi(k) .lt. 8) .and. (indi(k) .gt. 4)) then
!
            call hujprc(k, indi(k)-4, sigf, vind, mater,&
                        yf, p(k), q( k), sigdc(3*k-2))
            if (p(k) .ge. ptrac) goto 999
            call hujksi('DKSIDR', mater, rc(k), dksidr(k), iret)
            call hujksi('KSI   ', mater, rc(k), ksi(k), iret)
            if (iret .eq. 1) goto 1000
            ad(k) = deux*(acyc+ksi(k)*(amon-acyc))
!
            th(1) = vind(4*indi(k)-9)
            th(2) = vind(4*indi(k)-8)
            prod = sigdc(3*k-2)*th(1) + sigdc(3*k)*th(2)/deux
!
            if ((-q(k)/pref.lt.tole1) .or. ((un+prod/q(k)).lt.tole1)) then
                kk = kk - 4
                call hujpxd(indi(k), mater, sigf, vind, prox(kk),&
                            proxc(kk))
            else
                ad(k) = (acyc+ksi(k)*(amon-acyc))*(un+prod/q(k))
            endif
!
        else if (indi(k) .eq. 8) then
!
            ksi(k) = un
            call hujpic(k, indi(k), sigf, vind, mater,&
                        yf, p(k))
!
        else if ((indi(k).gt.8).and.(indi(k).lt.12)) then
            goto 5
!
        else
            call u2mess('F', 'COMPOR1_8')
        endif
!
 5  continue
!
    epsvp = yf(ndt+1)
    pc = pco*exp(-beta*epsvp)
    cmon = cmon * pc/pref
    ccyc = ccyc * pc/pref
!
! --- CONDITIONNEMENT DE LA MATRICE JACOBIENNE
    ccond= mater(1,1)
! =====================================================================
! --- OPERATEURS DE RIGIDITE ET DE SOUPLESSE (LINEAIRES OU NON LINEA.)
! =====================================================================
! --- OPERATEURS LINEAIRES --------------------------------------------
! =====================================================================
    call lcinma(zero, hook)
!
    if (mod(1:2) .eq. '3D' .or. mod(1:6) .eq. 'D_PLAN' .or. mod(1:4) .eq. 'AXIS') then
!
        if (mater(17,1) .eq. un) then
!
            e = mater(1,1)
            nu = mater(2,1)
            al = e*(un-nu) /(un+nu) /(un-deux*nu)
            demu = e /(un+nu)
            la = e*nu/(un+nu)/(un-deux*nu)
!
            do 30 i = 1, ndi
                do 30 j = 1, ndi
                    if (i .eq. j) hook(i,j) = al
                    if (i .ne. j) hook(i,j) = la
30              continue
            do 35 i = ndi+1, ndt
                hook(i,i) = demu
35          continue
!
        else if (mater(17,1).eq.deux) then
!
            e1 = mater(1,1)
            e2 = mater(2,1)
            e3 = mater(3,1)
            nu12 = mater(4,1)
            nu13 = mater(5,1)
            nu23 = mater(6,1)
            g1 = mater(7,1)
            g2 = mater(8,1)
            g3 = mater(9,1)
            nu21 = mater(13,1)
            nu31 = mater(14,1)
            nu32 = mater(15,1)
            denom= mater(16,1)
!
            hook(1,1) = (un - nu23*nu32)*e1/denom
            hook(1,2) = (nu21 + nu31*nu23)*e1/denom
            hook(1,3) = (nu31 + nu21*nu32)*e1/denom
            hook(2,2) = (un - nu13*nu31)*e2/denom
            hook(2,3) = (nu32 + nu31*nu12)*e2/denom
            hook(3,3) = (un - nu21*nu12)*e3/denom
            hook(2,1) = hook(1,2)
            hook(3,1) = hook(1,3)
            hook(3,2) = hook(2,3)
            hook(4,4) = g1
            hook(5,5) = g2
            hook(6,6) = g3
!
        else
            call u2mess('F', 'COMPOR1_38')
        endif
!
!
! =====================================================================
! --- CP/1D -----------------------------------------------------------
! =====================================================================
    else if (mod(1:6) .eq. 'C_PLAN' .or. mod(1:2) .eq. '1D') then
        call u2mess('F', 'COMPOR1_4')
    endif
!
!
! =====================================================================
! --- OPERATEUR NON LINEAIRE ------------------------------------------
! =====================================================================
    coef0 = ((i1f -piso)/pref) ** n
    do 40 i = 1, ndt
        do 40 j = 1, ndt
            hooknl(i,j) = coef0*hook(i,j)
40      continue
!
!
! =====================================================================
! --- DERIVEE PAR RAPPORT A DS DE L'OPERATEUR NON LINEAIRE: DHOOKDS ---
! =====================================================================
    dcoef0 = d13*n/pref * ((i1f -piso)/pref)**(n-1)
    do 41 i = 1, ndt
        do 41 j = 1, ndt
            dhokds(i,j) = dcoef0*hook(i,j)
41      continue
!
!
! =====================================================================
! --- I. CALCUL DE DLEDS (6X6) ----------------------------------------
! =====================================================================
! ---> I.1. CALCUL DE CTILD = DHOOKDS*(DEPS - DEPSP)
! ---> I.1.1. CALCUL DE DEPSP A T+DT
    do 50 i = 1, ndt
        depsp(i) = zero
50  continue
!
    do 51 k = 1, nbmect
        kk = (k-1)*ndt
        do 53 i = 1, ndt
            depsp(i) = depsp(i) + dlambd(k)*psi(kk+i)
53      continue
51  continue
!
! ------------ FIN I.1.1.
    do 52 i = 1, ndt
        depse(i) = deps(i) - depsp(i)
52  continue
    call lcprmv(dhokds, depse, ctild)
! ------------ FIN I.1.
! ---> I.2. CALCUL DE CD2FDS = HOOK * DEPSDS
!                     (6X6)    (6X6)  (6X6)
    call lcinma(zero, depsds)
!
    do 60 k = 1, nbmect
        kk = indi(k)
        if ((kk .eq. 4) .or. (kk.eq.8)) goto 610
!
        if (kk .gt. 8) goto 610
!
        call hujddd('DPSIDS', kk, mater, indi, yf,&
                    vind, dfds, dpsids, iret)
        if (iret .eq. 1) goto 1000
        do 60 i = 1, ndt
            do 60 j = 1, ndt
                depsds(i,j) = depsds(i,j) + dlambd(k)*dpsids(i,j)
60          continue
610  continue
!
    call lcprmm(hooknl, depsds, cd2fds)
!
! ------------ FIN I.2.
    call lcinma(zero, dleds)
    do 63 i = 1, ndt
        dleds(i,i) = un
63  continue
!
    do 61 i = 1, ndt
        do 62 j = 1, ndi
            dleds(i,j) = dleds(i,j) - (ctild(i) - cd2fds(i,j))
62      continue
        do 61 j = ndi+1, ndt
            dleds(i,j) = dleds(i,j) + cd2fds(i,j)
61      continue
!
! =====================================================================
! --- II. CALCUL DE DLEDR (6XNBMEC) -----------------------------------
! =====================================================================
!
    do 70 i = 1, ndt
        do 70 k = 1, 4
            dledr(i,k) = zero
70      continue
!
    if (nbmeca .eq. 0) goto 710
!
    do 71 k = 1, nbmeca
        kk = indi(k)
        pk = p(k) -ptrac
!
        if ((kk.eq.4) .or. (kk.eq.8)) goto 710
        if (kk .gt. 8) goto 710
!
        if (kk .lt. 4) then
!
!kh --- traction
            if ((p(k)/pref) .gt. tole1) then
                dpsi =mdil+q(k)/p(k)
            else
                if (debug) write(6,'(A)')'HUJJID :: TRACTION MONOTONE'
                dpsi =mdil+1.d+6*q(k)/pref
            endif
!
            mul = - dlambd(k)*alpha*dpsi*dksidr(k)
!
            do 72 i = 1, ndi
                if (i .ne. kk) then
                    delta(i) = mul
                else
                    delta(i) = zero
                endif
72          continue
!
            do 73 i = ndi+1, ndt
                delta(i) = zero
73          continue
!
        else if ((kk .lt. 8) .and. (kk .gt. 4)) then
! ---> MECANISME CYCLIQUE DEVIATOIRE
!
            th(1) = vind(4*kk-9)
            th(2) = vind(4*kk-8)
            call hujprj(indi(k)-4, sigf, sigd, ps, prod)
            prodc = 2.d0*sigdc(3*k-2)*th(1) + sigdc(3*k)*th(2)
            prodm = 2.d0*sigd(1)*th(1) + sigd(3)*th(2)
            ps = 2.d0*sigdc(3*k-2)*sigd(1)+sigdc(3*k)*sigd(3)
!
!kh --- traction
            if ((p(k)/pref) .gt. tole1) then
                if ((-q(k)/pref) .gt. tole1) then
                    dpsi =mdil+ps/2.d0/p(k)/q(k)
                else
                    dpsi =mdil
                endif
            else
                if (debug) write(6,'(A)')'HUJJID :: TRACTION CYCLIQUE'
                if ((-q(k)/pref) .gt. tole1) then
                    dpsi = mdil+ps/2.d-6/pref/q(k)
                else
                    dpsi = mdil
                endif
            endif
!
            si = un
            do 74 i = 1, ndi
                if (i .ne. (kk-4)) then
                    if ((-q(k)/pref) .gt. tole1) then
                        delta(i) = dlambd(k)*(m*pk*(un-b*log(pk/pc))/ (2.d0*q(k))*(th(1)*si-sigdc&
                                   &(3*k-2)*si*prodc/ (2.d0*q(k)**2.d0)) -alpha* (dksidr(k)*dpsi+&
                                   & ksi(k)/2.d0*m*(un-b*log(pk/pc))*(prodm -ps*prodc/(2.d0*q(k)*&
                                   &*2.d0))/q(k)))
                    else
                        delta(i) = dlambd(k)*(-alpha)*dksidr(k)*mdil
                    endif
                    si = - si
                else
                    delta(i) = zero
                endif
74          continue
!
            do 75 i = ndi+1, ndt
                delta(i) = zero
75          continue
!
            if ((-q(k)/pref) .gt. tole1) then
                delta(ndt+5-kk)= dlambd(k)*(m*pk*(un-b*log(pk/pc))/&
                (2.d0*q(k))*(th(2)-sigdc(3*k)*prodc/ (2.d0*q(k)**2.d0)&
                ))
            else
                delta(ndt+5-kk)= zero
            endif
!
        endif
!
        call lcprmv(hooknl, delta, dledr1)
        do 76 i = 1, ndt
            dledr(i,k) = dledr1(i) /abs(pref)
76      continue
!
71  end do
!
710  continue
!
! =====================================================================
! --- III. CALCUL DE DLEDEVP (6X1) ------------------------------------
! =====================================================================
    do 80 i = 1, ndt
        dledev(i) = zero
80  continue
!
!
! =====================================================================
! --- IV. CALCUL DE DLEDLA (6XNBMEC) ----------------------------------
! =====================================================================
    do 90 k = 1, 6
        do 90 l = 1, 7
            dledla(k,l) = zero
90      continue
!
    do 91 k = 1, nbmect
        kk = (k-1)*ndt+1
        call lcprmv(hooknl, psi(kk), dlek)
        do 91 i = 1, ndt
            dledla(i,k) = dlek(i) /ccond
91      continue
!
! =====================================================================
! --- V. CALCUL DE DLRDS (NBMECX6) ------------------------------------
! =====================================================================
    do 100 k = 1, 4
        do 100 i = 1, ndt
            dlrds(k,i) = zero
100      continue
!
! =====================================================================
! --- VI. CALCUL DE DLRDR (NBMECXNBMEC) -------------------------------
! =====================================================================
    do 110 k = 1, 4
        do 110 l = 1, 4
            dlrdr(k,l) = zero
110      continue
!
    if (nbmeca .eq. 0) goto 101
!
    do 111 k = 1, nbmeca
!
        kk = indi(k)
!
        if (kk .lt. 4) then
            mul = (un-rc(k))/ad(k)
            dlrdr(k,k) = un + deux*dlambd(k)*mul + dlambd(k)*dksidr(k) *(amon-acyc)*mul**deux
        else if (kk.eq.4) then
            dlrdr(k,k) = un + deux*dlambd(k)*(un-rc(k))/cmon
!
        else if ((kk .gt. 4) .and. (kk .lt. 8)) then
            mul = (un-rc(k))/ad(k)
            dlrdr(k,k) = un + deux*dlambd(k)*(mul + dksidr(k)*(amon- acyc)*mul**deux)
!
        else if (kk .eq. 8) then
            dlrdr(k,k) = un + deux*dlambd(k)*(un-rc(k))/ccyc
!
        endif
!
!        DLRDR(K,K) = DLRDR(K,K)*CCOND/PREF
        dlrdr(k,k) = dlrdr(k,k)
!
111  continue
!
101  continue
!
! =====================================================================
! --- VII. CALCUL DE DLRDLA (NBMECXNBMEC) -----------------------------
! =====================================================================
    do 112 k = 1, 4
        do 112 l = 1, 7
            dlrdla(k,l) = zero
112      continue
!
    if (nbmeca .eq. 0) goto 102
!
    do 113 k = 1, nbmeca
        kk = indi(k)
        if (kk .lt. 4) then
            dlrdla(k,k) = -( un-rc(k) )**deux /ad(k)
        else if (kk.eq.4) then
            dlrdla(k,k) = -( un-rc(k) )**deux /cmon
!
        else if ((kk .gt. 4) .and. (kk .lt. 8)) then
            dlrdla(k,k) = -( un-rc(k) )**deux /ad(k)
!
        else if (kk .eq. 8) then
            dlrdla(k,k) = -( un-rc(k) )**deux /ccyc
!
        endif
        dlrdla(k,k) = dlrdla(k,k)/ccond*abs(pref)
113  end do
!
102  continue
!
! =====================================================================
! --- VIII. CALCUL DE DLRDEVP (NBMECX1) -------------------------------
! =====================================================================
    do 103 k = 1, 4
        dlrdle(k) = zero
103  continue
!
    if (nbmeca .eq. 0) goto 104
!
    do 120 k = 1, nbmeca
        kk = indi(k)
        if (kk .lt. 4) then
            dlrdle(k) = zero
        else if (kk .eq. 4) then
            dlrdle(k) = -dlambd(k)*beta*( un-rc(k) )**deux /cmon
!
        else if ((kk .gt. 4) .and. (kk .lt. 8)) then
!
! --- INITIALISATION DES VARIABLES D'HISTOIRE
            xh(1) = vind(4*kk-11)
            xh(2) = vind(4*kk-10)
            th(1) = vind(4*kk-9)
            th(2) = vind(4*kk-8)
!
! --- CALCUL DE F = M(1-BLOG((PK-PTRAC)/PC))
            pk =p(k) -ptrac
            fac = m*b*pk*beta
!
! --- CALCUL DE D(SIG-CYC)/D(EVP)
            do 121 i = 1, ndt
                xc(i) = zero
                sc(i) = zero
                tc(i) = zero
121          continue
!
            si = un
            do 122 i = 1, ndi
                if (i .ne. (kk-4)) then
                    sc(i) = sigdc(3*k-2)*si
                    tc(i) = th(1)*si
                    xc(i) = xh(1)*si
                    si = -si
                endif
122          continue
            sc(ndt+5-kk) = sigdc(3*k)
            tc(ndt+5-kk) = th(2)
            xc(ndt+5-kk) = xh(2)
!
            scxc = zero
            xctc = zero
            do 123 i = 1, ndt
                xc(i) = fac*(xc(i)-tc(i)*rc(k))
                scxc = scxc + sc(i)*xc(i)
                xctc = xctc + xc(i)*tc(i)
123          continue
!
! --- CALCUL DU PRODUIT SCALAIRE ENTRE TH ET SIG-CYC
            prod = deux*sigdc(3*k-2)*th(1) + sigdc(3*k)*th(2)
!
! --- CALCUL DE DLRDLE
            dlrdle(k) = zero
            if ((q(k).gt.tole1) .and. ((2.d0*q(k)+prod).gt.tole1)) then
                dlrdle(k) = -dlambd(k)*(un-rc(k))**deux/ad(k) *(un+ prod/(deux*q(k))) *(scxc/q(k)&
                            &*prod-2.d0*q(k)*xctc) /(2.d0*q(k)+prod)**2.d0
!
            endif
!
        else if (kk.eq. 8) then
            dlrdle(k) = -dlambd(k)*beta*( un-rc(k) )**deux /ccyc
!
        endif
        dlrdle(k) = dlrdle(k)*abs(pref)/ccond
120  end do
!
104  continue
! =====================================================================
! --- IX. CALCUL DE DLEVPDS (1X6) -------------------------------------
! =====================================================================
    do 130 i = 1, ndt
        dlevds(i) = zero
130  continue
!
    do 131 k = 1, nbmect
        kk =indi(k)
        pk =p(k) -ptrac
        if ((kk.eq.4) .or. (kk.eq.8)) goto 1310
        if (kk .gt. 8) goto 1310
!
!kh --- traction
        if ((p(k)/pref) .lt. tole1) then
            dila =.true.
            pcoh = 1.d-6*pref
        else
            dila =.false.
            pcoh = p(k)
        endif
!
        if (kk .lt. 4) then
!
            call hujprj(kk, sigf, sigd, coef0, mul)
            if ((-q(k)/pref) .le. tole1) goto 131
            dlevds(ndt+1-kk) = dlevds(ndt+1-kk) + dlambd(k) * ksi(k)* coef*sigd(3) /pcoh/q(k)/2.d&
                               &0
!
            si = un
            do 132 i = 1, ndi
                if (i .ne. kk .and. (.not.dila)) then
                    dlevds(i) = dlevds(i) + dlambd(k)*ksi(k)*coef*( sigd(1)*si /p(k)/q(k)/2.d0 - &
                                &d12*q(k) /p(k)**deux)
                    si = -si
                else if (i.ne.kk .and. dila) then
                    dlevds(i) = dlevds(i) + dlambd(k)*ksi(k)*coef* sigd(1)*si /pcoh/q(k)/2.d0
                    si = -si
                endif
132          continue
!
        else if ((kk.lt. 8) .and. (kk.gt. 4)) then
!
            if ((-q(k)/pref) .le. tole1) goto 131
!
            call hujprj(kk-4, sigf, sigd, coef0, mul)
            ps = 2.d0*sigd(1)*sigdc(3*k-2)+sigd(3)*sigdc(3*k)
!
            xh(1) = vind(4*kk-11)
            xh(2) = vind(4*kk-10)
            th(1) = vind(4*kk-9)
            th(2) = vind(4*kk-8)
!
            sxh = 2*sigd(1)*(xh(1)-th(1)*rc(k))+ sigd(3)*(xh(2)-th(2)* rc(k))
            scxh = 2*sigdc(3*k-2)*(xh(1)-th(1)*rc(k))+ sigdc(3*k)*(xh( 2)-th(2)*rc(k))
!
            fac = d12*m*(un-b*(un+log(pk/pc)))
!
            if ((-q(k)/pref) .gt. tole1) then
                dlevds(ndt+5-kk) = dlevds(ndt+5-kk) + dlambd(k) * ksi( k)*coef/(2.d0*pcoh*q(k))* &
                                   &(sigd(3)+sigdc(3*k)*(un-ps/ q(k)**2.d0/2.d0))
            endif
!
            si = un
            do 133 i = 1, ndi
                if (i .ne. (kk-4)) then
                    if ((-q(k)/pref) .gt. tole1) then
                        dlevds(i) = dlevds(i) + dlambd(k)*ksi(k)*coef/ (2.d0*pcoh*q(k))* (sigdc(3&
                                    &*k-2)*si*(un-ps/( 2.d0*q(k)**2.d0))+sigd(1)*si -fac*(sxh-scx&
                                    &h* ps*d12/q(k)**2.d0)-d12*ps /pcoh)
                        si = -si
                    endif
                endif
133          continue
        endif
!
        do 134 i = 1, ndt
            dlevds(i) = dlevds(i)/ccond
134      continue
!
131  continue
1310  continue
! =====================================================================
! --- X. CALCUL DE DLEVPDEVP (1X1) ------------------------------------
! =====================================================================
    dlevde = un
    do 140 k = 1, nbmeca
        kk = indi(k)
        if ((kk.gt.4) .and. (kk.lt.8)) then
!
            call hujprj(kk-4, sigf, sigd, coef0, mul)
            if (q(k) .gt. tole1) then
                xh(1) = vind(4*kk-11)
                xh(2) = vind(4*kk-10)
                th(1) = vind(4*kk-9)
                th(2) = vind(4*kk-8)
                prodc = 2.d0*sigdc(3*k-2)*(xh(1)-rc(k)*th(1)) + (sigdc(3*k)*(xh(2)-rc(k)*th(2)))
                prodm = 2.d0*sigd(1)*(xh(1)-rc(k)*th(1)) + (sigd(3)*( xh(2)-rc(k)*th(2)))
                ps = 2.d0*sigd(1)*sigdc(3*k-2)+sigd(3)+sigdc(3*k)
                if ((-q(k)/pref) .gt. tole1) then
                    dlevde = dlevde + dlambd(k)*coef*ksi(k)/q(k)/2.d0* m*b*beta*(prodm - ps/2.d0/&
                             &q(k)**2.d0* prodc)
                endif
            endif
        endif
140  continue
!
! =====================================================================
! --- XI. CALCUL DE DLEVPDR (1XNBMEC) ---------------------------------
! =====================================================================
    do 141 i = 1, 4
        dlevdr(i) = zero
141  end do
!
    if (nbmeca .eq. 0) goto 152
!
    do 151 k = 1, nbmeca
!
        kk = indi(k)
        pk =p(k) -ptrac
!
        if (kk .lt. 4) then
!
!kh --- traction
            if ((p(k)/pref) .gt. tole1) then
                dpsi =mdil+q(k)/p(k)
            else
                if (debug) write(6,'(A)')'HUJJID :: TRACTION MONOTONE'
                dpsi =mdil+1.d+6*q(k)/pref
            endif
!
            dlevdr(k) = dlambd(k)*coef*dksidr(k)*dpsi
!
        else if (kk .eq. 4) then
!
            dlevdr(k) = zero
!
        else if ((kk .gt. 4) .and. (kk .lt.8)) then
!
            call hujprj(kk-4, sigf, sigd, coef0, mul)
            th(1) = vind(4*kk-9)
            th(2) = vind(4*kk-8)
!
            prodc = 2.d0*sigdc(3*k-2)*th(1) + sigdc(3*k)*th(2)
            prodm = 2.d0*sigd(1)*th(1) + sigd(3)*th(2)
            ps = 2.d0*sigd(1)*sigdc(3*k-2)+sigd(3)*sigdc(3*k)
!
!kh --- traction
            if ((p(k)/pref) .gt. tole1) then
                if ((-q(k)/pref) .gt. tole1) then
                    dpsi =mdil+ps/2.d0/p(k)/q(k)
                else
                    dpsi =mdil
                endif
            else
                if (debug) write(6,'(A)')'HUJJID :: TRACTION CYCLIQUE'
                if ((-q(k)/pref) .gt. tole1) then
                    dpsi = mdil+ps/2.d-6/pref/q(k)
                else
                    dpsi = mdil
                endif
            endif
!
            if ((-q(k)/pref) .gt. tole1) then
                dlevdr(k) = dlambd(k)*coef* (dksidr(k)*dpsi +ksi(k)*m* (un-b*log(pk/pc))/(2.d0*q(&
                            &k))* (prodm-ps*prodc/(2.d0* q(k)**2.d0)))
            else
                dlevdr(k) = dlambd(k)*coef*dksidr(k)*mdil
            endif
!
        else if (kk .eq. 8) then
            dlevdr(k) = zero
!
        endif
        dlevdr(k) = dlevdr(k)*ccond/abs(pref)
151  continue
!
152  continue
! =====================================================================
! --- XII. CALCUL DE DLEVPDLA (1XNBMEC) -------------------------------
! =====================================================================
    do 153 k = 1, 7
        dlevdl(k) = zero
153  end do
!
    do 161 k = 1, nbmect
!
        kk = indi(k)
        pk =p(k) -ptrac
!
        if (kk .lt. 4) then
!
!kh --- traction
            if ((p(k)/pref) .gt. tole1) then
                dpsi =mdil+q(k)/p(k)
            else
                if (debug) write(6,'(A)')'HUJJID :: TRACTION MONOTONE'
                dpsi =mdil+1.d+6*q(k)/pref
            endif
!
            dlevdl(k) = ksi(k)*coef*dpsi
        else if (kk .eq. 4) then
            dlevdl(k) = un
!
        else if ((kk .gt. 4) .and. (kk .lt.8)) then
!
            call hujprj(kk-4, sigf, sigd, coef0, mul)
            ps = 2.d0*sigd(1)*sigdc(3*k-2)+sigd(3)*sigdc(3*k)
!
!kh --- traction
            if ((p(k)/pref) .gt. tole1) then
                if ((-q(k)/pref) .gt. tole1) then
                    dpsi =mdil+ps/2.d0/p(k)/q(k)
                else
                    dpsi =mdil
                endif
            else
                if (debug) write(6,'(A)')'HUJJID :: TRACTION CYCLIQUE'
                if ((-q(k)/pref) .gt. tole1) then
                    dpsi = mdil+ps/2.d-6/pref/q(k)
                else
                    dpsi = mdil
                endif
            endif
!
            dlevdl(k) = ksi(k)*coef*dpsi
!
        else if (kk .eq. 8) then
!
            if (vind(22) .eq. un) then
                dlevdl(k) = -un
            else
                dlevdl(k) = un
            endif
!
        endif
161  continue
!
!
! =====================================================================
! --- XIII. CALCUL DE DLFDS (NBMECX6) ---------------------------------
! =====================================================================
    do 162 k = 1, 7
        do 162 i = 1, 6
            dlfds(k,i) = zero
162      continue
!
    do 171 k = 1, nbmect
        kk = indi(k)
        call hujddd('DFDS  ', kk, mater, indi, yf,&
                    vind, dfds, dpsids, iret)
        if (iret .eq. 1) goto 1000
        do 171 i = 1, ndt
            dlfds(k,i) = dfds(i)
171      continue
!
!
! =====================================================================
! --- XIV. CALCUL DE DLFDR (NBMECXNBMEC) ------------------------------
! =====================================================================
    do 180 k = 1, 7
        do 180 l = 1, 4
            dlfdr(k,l) = zero
180      continue
!
    if (nbmeca .eq. 0) goto 182
!
    do 181 k = 1, nbmeca
!
        kk = indi(k)
        pk =p(k) -ptrac
!
        if (kk .lt. 4) then
!
            dlfdr(k,k) = m*pk*( un-b*log(pk/pc) )
!
        else if ((kk .eq. 4).or.(kk.eq.8)) then
!
            dlfdr(k,k) = d*pc
!
        else if ((kk .gt. 4) .and. (kk .lt.8)) then
!
            th(1) = vind(4*kk-9)
            th(2) = vind(4*kk-8)
            prod = sigdc(3*k-2)*th(1) + sigdc(3*k)*th(2)*d12
!
            if (((-q(k)/pref).gt.tole1) .and. (prod/q(k).ne.-un)) then
                dlfdr(k,k) = m*pk*( un-b*log(pk/pc) ) *(un+prod/q(k))
            else
                dlfdr(k,k) = m*pk*( un-b*log(pk/pc) )
            endif
        endif
        dlfdr(k,k) = dlfdr(k,k)/abs(pref)
181  continue
!
182  continue
! =====================================================================
! --- XV. CALCUL DE DLFDEVP (NBMECX1) ---------------------------------
! =====================================================================
    do 183 k = 1, 7
        dlfdle(k) = zero
183  continue
!
    do 190 k = 1, nbmect
!
        kk = indi(k)
        pk = p(k) -ptrac
!
        if (kk .lt. 4) then
!
            dlfdle(k) = -m*b*pk*rc(k)*beta /ccond
!
        else if (kk .eq. 4) then
!
            dlfdle(k) = -rc(k)*d*pc*beta /ccond
!
        else if ((kk .gt. 4) .and. (kk .lt.8)) then
!
            xh(1) = vind(4*kk-11)
            xh(2) = vind(4*kk-10)
            th(1) = vind(4*kk-9)
            th(2) = vind(4*kk-8)
            prod = sigdc(3*k-2)*(xh(1)-rc(k)*th(1)) + (sigdc(3*k)*(xh( 2)-rc(k)*th(2)))*d12
            if ((-q(k)/pref) .gt. tole1) then
                dlfdle(k) = m*b*pk*(prod/q(k)-rc(k))*beta /ccond
            else
                dlfdle(k) = m*b*pk*(-rc(k))*beta /ccond
            endif
        else if (kk .eq. 8) then
!
            if (vind(22) .eq. un) then
                dlfdle(k) = -d*pc*beta*(rc(k)-vind(21))/ccond
            else
                dlfdle(k) = -d*pc*beta*(vind(21)+rc(k)) /ccond
            endif
!
        endif
190  continue
!
!
! =====================================================================
! --- XVI. CALCUL DE DLFDLA (NBMECXNBMEC) -----------------------------
! =====================================================================
    do 200 k = 1, 7
        do 200 l = 1, 7
            dlfdla(k,l) = zero
200      continue
!
! =====================================================================
! --- XVII. CALCUL DE LE (6) ---------------------------------------
! =====================================================================
! ---- XVII.1. CALCUL DE CDE = C*DEPSE
!                        6X1
! REMARQUE: ON A DEJA DEPSE CALCULE AU I.1.
    call lcprmv(hooknl, depse, cde)
    do 210 i = 1, ndt
        le(i) = yf(i) - yd(i) - cde(i)
210  continue
!
!
! =====================================================================
! --- XVIII. CALCUL DE LEVP (1X1) -------------------------------------
! =====================================================================
    levp = yf(ndt+1) - yd(ndt+1)
    do 220 k = 1, nbmect
!
        kk = indi(k)
        pk =p(k) -ptrac
!
        if (kk .lt. 4) then
!
!kh --- traction
            if ((p(k)/pref) .gt. tole1) then
                dpsi =mdil+q(k)/p(k)
            else
                if (debug) write(6,'(A)')'HUJJID :: TRACTION MONOTONE'
                dpsi =mdil+1.d+6*q(k)/pref
            endif
            levp = levp + coef*dlambd(k)*ksi(k)*dpsi
!
        else if (kk .eq. 4) then
!
            levp = levp + dlambd(k)
!
        else if ((kk .gt. 4) .and. (kk .lt.8)) then
!
            call hujprj(kk-4, sigf, sigd, coef0, mul)
            ps = 2.d0*sigd(1)*sigdc(3*k-2)+sigd(3)*sigdc(3)
!
!kh --- traction
            if ((p(k)/pref) .gt. tole1) then
                if ((-q(k)/pref) .gt. tole1) then
                    dpsi =mdil+ps/2.d0/p(k)/q(k)
                else
                    dpsi =mdil
                endif
            else
                if (debug) write(6,'(A)')'HUJJID :: TRACTION CYCLIQUE'
                if ((-q(k)/pref) .gt. tole1) then
                    dpsi = mdil+ps/2.d-6/pref/q(k)
                else
                    dpsi = mdil
                endif
            endif
!
            levp = levp + coef*dlambd(k)*ksi(k)*dpsi
!
        else if (kk .eq. 8) then
!
            if (vind(22) .gt. zero) then
                levp = levp - dlambd(k)
            else
                levp = levp + dlambd(k)
            endif
!
        endif
!
220  continue
!
!
! =====================================================================
! --- XIX. CALCUL DE LR (NBMECX1) -------------------------------------
! =====================================================================
    do 221 k = 1, 4
        lr(k) = zero
221  continue
!
    if (nbmeca .eq. 0) goto 231
    do 230 k = 1, nbmeca
        kk = indi(k)
        if (kk .lt. 4) then
            lr(k) = yf(ndt+1+k) - yd(ndt+1+k) - dlambd(k)/ad(k)*(un- rc(k))**deux
        else if (kk .eq. 4) then
            lr(k) = yf(ndt+1+k) - yd(ndt+1+k) - dlambd(k)/cmon*(un-rc( k))**deux
!
        else if ((kk .gt. 4) .and. (kk .lt.8)) then
            th(1) = vind(4*indi(k)-9)
            th(2) = vind(4*indi(k)-8)
            prod = sigdc(3*k-2)*th(1) + sigdc(3*k)*th(2)/deux
!
            if ((-q(k)/pref.lt.tole1) .or. ((un+prod/q(k)).lt.tole1)) then
                ad(k) = (acyc+ksi(k)*(amon-acyc))
            else
                ad(k) = (acyc+ksi(k)*(amon-acyc))*(un+prod/q(k))
            endif
            lr(k) = yf(ndt+1+k) - yd(ndt+1+k) - dlambd(k)/ad(k)*(un- rc(k))**deux
        else if (kk .eq. 8) then
            lr(k) = yf(ndt+1+k) - yd(ndt+1+k) - dlambd(k)/ccyc*(un-rc( k))**deux
!
        endif
230  continue
!
231  continue
! =====================================================================
! --- XX. CALCUL DE LF (NBMECX1) --------------------------------------
! =====================================================================
    do 232 k = 1, 7
        lf(k) = zero
232  continue
!
    do 240 k = 1, nbmect
        kk = indi(k)
        pk =p(k) -ptrac
        if (kk .lt. 4) then
            lf(k) = q(k) + m*pk*rc(k)*( un-b*log(pk/pc) )
        else if (kk .eq. 4) then
            lf(k) = abs(p(k)) + rc(k)*d*pc
        else if ((kk .gt. 4) .and. (kk .lt.8)) then
            lf(k) = q(k) + m*pk*rc(k)*( un-b*log(pk/pc) )
        else if (kk .eq. 8) then
            lf(k) = abs(p(k)) + rc(k)*d*pc
        else if (kk .gt. 8) then
            call hujprj(kk-8, yf, sigd, pk, ps)
            lf(k) = pk + deux*rtrac - ptrac
        endif
240  continue
!
!
! =====================================================================
! --- ASSEMBLAGE DE R : -----------------------------------------------
! =====================================================================
!     R    = -( LE       , LEVP       , LR       , LF       )
! =====================================================================
! --- ASSEMBLAGE DE DRDY
! =====================================================================
!     DRDY =  ( DLEDS    , DLEDEVP    , DLEDR    , DLEDLA   )
!             ( DLEVPDS  , DLEVPDEVP  , DLEVPDR  , DLEVPDLA )
!             ( DLRDS    , DLRDEVP    , DLRDR    , DLRDLA   )
!             ( DLFDS    , DLFDEVP    , DFLFDR   , DFLFDLA  )
! =====================================================================
! --- ASSEMBLAGE DE R -------------------------------------------------
! =====================================================================
    do 850 i = 1, ndt
        r(i) = -le(i) /ccond
850  continue
    r(ndt+1) = -levp
!
    if (nbmeca .eq. 0) goto 951
    do 950 k = 1, nbmeca
        r(ndt+1+k) = -lr(k) /ccond*abs(pref)
        r(ndt+1+nbmeca+k) = -lf(k) /ccond
950  continue
951  continue
!
    if (nbmeca .lt. nbmect) then
        do 952 k = 1, nbmect
            if (indi(k) .gt. 8) then
                r(ndt+1+nbmeca+k) = -lf(k)/ccond
            endif
952      continue
    endif
! =====================================================================
! --- ASSEMBLAGE DE DRDY ----------------------------------------------
! =====================================================================
! DLEDDY
    call lcicma(dleds, 6, 6, ndt, ndt,&
                1, 1, drdy, nmod, nmod,&
                1, 1)
    call lcicma(dledev, 6, 1, ndt, 1,&
                1, 1, drdy, nmod, nmod,&
                1, ndt+1)
    call lcicma(dledr, 6, 4, ndt, nbmeca,&
                1, 1, drdy, nmod, nmod,&
                1, ndt+2)
    call lcicma(dledla, 6, 7, ndt, nbmect,&
                1, 1, drdy, nmod, nmod,&
                1, ndt+2+nbmeca)
! DLEVPDDY
    call lcicma(dlevds, 1, 6, 1, ndt,&
                1, 1, drdy, nmod, nmod,&
                ndt+1, 1)
    drdy(ndt+1,ndt+1) = dlevde
    call lcicma(dlevdr, 1, 4, 1, nbmeca,&
                1, 1, drdy, nmod, nmod,&
                ndt+1, ndt+2)
    call lcicma(dlevdl, 1, 7, 1, nbmect,&
                1, 1, drdy, nmod, nmod,&
                ndt+1, ndt+2+nbmeca)
! DLRDDY
    call lcicma(dlrds, 4, 6, nbmeca, ndt,&
                1, 1, drdy, nmod, nmod,&
                ndt+2, 1)
    call lcicma(dlrdle, 4, 1, nbmeca, 1,&
                1, 1, drdy, nmod, nmod,&
                ndt+2, ndt+1)
    call lcicma(dlrdr, 4, 4, nbmeca, nbmeca,&
                1, 1, drdy, nmod, nmod,&
                ndt+2, ndt+2)
    call lcicma(dlrdla, 4, 7, nbmeca, nbmect,&
                1, 1, drdy, nmod, nmod,&
                ndt+2, ndt+2+nbmeca)
! DLFDDY
    call lcicma(dlfds, 7, 6, nbmect, ndt,&
                1, 1, drdy, nmod, nmod,&
                ndt+2+nbmeca, 1)
    call lcicma(dlfdle, 7, 1, nbmect, 1,&
                1, 1, drdy, nmod, nmod,&
                ndt+2+nbmeca, ndt+1)
    call lcicma(dlfdr, 7, 4, nbmect, nbmeca,&
                1, 1, drdy, nmod, nmod,&
                ndt+2+nbmeca, ndt+2)
    call lcicma(dlfdla, 7, 7, nbmect, nbmect,&
                1, 1, drdy, nmod, nmod,&
                ndt+2+nbmeca, ndt+2+nbmeca)
!
    goto 1000
!
999  continue
    if (debug) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        write(ifm,'(10(A))') 'HUJJID :: LOG(PK/PC) NON DEFINI DANS ',&
        'LA MAILLE ',nomail
    endif
    iret=1
1000  continue
!
! =====================================================================
!        CALL JEDEMA ()
! =====================================================================
end subroutine
