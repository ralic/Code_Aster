subroutine hujtid(mod, imat, sigr, vin, dsde,&
                  iret)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ---------------------------------------------------------------------
! CALCUL DE LA MATRICE TANGENTE DU PROBLEME CONTINU DE LA LOI DE HUJEUX
! POUR LE MECANISME PLASTIQUE DEVIATOIRE
! IN   MOD     :  MODELISATION
!      IMAT    :  ADRESSE DU MATERIAU CODE
!      SIG     :  CONTRAINTES
!      VIN     :  VARIABLES INTERNES
! OUT  DSDE    :  MATRICE TANGENTE
! ======================================================================
    include 'asterfort/hujddd.h'
    include 'asterfort/hujksi.h'
    include 'asterfort/hujmat.h'
    include 'asterfort/hujpic.h'
    include 'asterfort/hujprc.h'
    include 'asterfort/hujprj.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcprmm.h'
    include 'asterfort/mgauss.h'
    include 'asterfort/trace.h'
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi, i, j, k, kk, l, ll, nvi
    integer :: nbmeca, ind(7), iret, imat, nbmect
    real(kind=8) :: n, beta, dhuj, m, pco, pref, pc
    real(kind=8) :: phi, angdil, mdil, degr, bhuj
    real(kind=8) :: rc(7), yd(18), dpsids(6, 6), p(7), q(7)
    real(kind=8) :: mater(22, 2), vin(*), sig(6), dsde(6, 6)
    real(kind=8) :: hook(6, 6), i1, e, nu, al, demu
    real(kind=8) :: coef, zero, d13, un, deux
    real(kind=8) :: dfdevp, evl, tp, tp1, tempf
    real(kind=8) :: psi(42), dfds(42), b1(7, 7), b2(7, 7), b(7, 7)
    real(kind=8) :: d(7, 6), te(6, 6), sigd(21), b3(7), la
    real(kind=8) :: acyc, amon, cmon, ksi(7), ad(7), x4, ccyc
    real(kind=8) :: tole1, det, xk(2), th(2), prod, ps, dev(3)
    real(kind=8) :: sigr(6), ptrac, piso, pk, dpsi, pt, qt
    real(kind=8) :: e1, e2, e3, nu12, nu13, nu23, g1, g2, g3, nu21, nu31, nu32
    real(kind=8) :: delta
    character(len=8) :: mod
!
    common /tdim/ ndt, ndi
! ======================================================================
    parameter   ( tole1 = 1.d-7 )
    parameter   ( zero = 0.d0 )
    parameter   ( d13  = 0.333333333334D0 )
    parameter   ( un   = 1.d0 )
    parameter   ( deux = 2.d0 )
    parameter   ( degr = 0.0174532925199D0 )
! ======================================================================
    tempf = 0.d0
    call hujmat(mod, imat, tempf, mater, ndt,&
                ndi, nvi)
    do 5 i = 1, ndt
        sig(i) = sigr(i)
 5  continue
!
    if (ndt .lt. 6) then
        sig(5)=zero
        sig(6)=zero
        ndt = 6
    endif
!
    do 6 i = 1, 7
        ind(i) = 0
 6  continue
!
!
! ======================================================================
! - RECUPERATION DES GRANDEURS UTILES : I1, VARIABLES INTERNES R ET X, -
! ======================================================================
    n = mater(1,2)
    beta = mater(2,2)
    dhuj = mater(3,2)
    bhuj = mater(4,2)
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
    ptrac = mater(21,2)
!        PISO   = 1.5D0*MATER(21,2)
    piso = zero
!
!
! =====================================================================
! --- CALCUL DE LA TRACE DE SIG ---------------------------------------
! =====================================================================
    iret = 0
    i1 = d13*trace(ndi,sig)
!
!
! ---> INITIALISATION DE NBMECA, IND ET YD PAR VIN
    do 11 k = 1, 6
        psi(k) = zero
        psi(6+k) = zero
        dfds(12+k) = zero
        dfds(18+k) = zero
        dfds(24+k) = zero
        dfds(30+k) = zero
        dfds(36+k) = zero
11  continue
!
    do 9 i = 1, 21
        sigd(k) = zero
 9  continue
!
    do 12 k = 1, 7
        rc(k) = zero
        p(k) = zero
        q(k) = zero
        ad(k) = zero
        ksi(k) = zero
12  continue
!
    do 13 k = 1, 18
        yd(k) = zero
13  continue
!
! --- MODIFICATION A APPORTER POUR MECANISMES CYCLIQUES
    yd(ndt+1) = vin(23)
    nbmeca = 0
    do 16 k = 1, 8
        if (vin(23+k) .eq. un) then
!
            nbmeca = nbmeca+1
            yd(ndt+1+nbmeca) = vin(k)
            rc(nbmeca) = vin(k)
!
            if (k .lt. 4) then
                call hujprj(k, sig, sigd(nbmeca*3-2), p(nbmeca), q(nbmeca))
                if (((p(nbmeca) -ptrac)/pref) .le. tole1) then
                    iret = 1
                    goto 999
                endif
                call hujksi('KSI   ', mater, rc(nbmeca), ksi(nbmeca), iret)
                if (iret .eq. 1) goto 999
                ad(nbmeca) = acyc+ksi(nbmeca)*(amon-acyc)
            endif
!
            if ((k .gt.4) .and. (k .lt. 8)) then
                call hujprc(nbmeca, k-4, sig, vin, mater,&
                            yd, p( nbmeca), q(nbmeca), sigd(nbmeca*3-2))
                if (((p(nbmeca) -ptrac)/pref) .le. tole1) then
                    iret = 1
                    goto 999
                endif
                call hujksi('KSI   ', mater, rc(nbmeca), ksi(nbmeca), iret)
                if (iret .eq. 1) goto 999
                ad(nbmeca) = deux*(acyc+ksi(nbmeca)*(amon-acyc))
            endif
!
            if (k .eq. 8) then
                call hujpic(nbmeca, k, sig, vin, mater,&
                            yd, p(nbmeca))
            endif
!
            ind(nbmeca) = k
        endif
16  continue
!
    nbmect = nbmeca
    do 8 i = 1, 3
        call hujprj(i, sig, dev, pt, qt)
        if (abs((pt+2*50.d0-ptrac)/pref) .lt. tole1) then
            nbmect = nbmect + 1
            ind(nbmect) = 8+i
        endif
 8  continue
!
    call lceqvn(ndt, sig, yd)
!
    do 17 k = 1, nbmeca
        call hujddd('DFDS  ', ind(k), mater, ind, yd,&
                    vin, dfds((k-1)*ndt+1), dpsids, iret)
        if (iret .eq. 1) goto 999
        call hujddd('PSI   ', ind(k), mater, ind, yd,&
                    vin, psi((k-1)*ndt+1), dpsids, iret)
        if (iret .eq. 1) goto 999
17  continue
    pc = pco*exp(-beta*yd(ndt+1))
    cmon = cmon * pc/pref
    ccyc = ccyc * pc/pref
!
! =====================================================================
! --- OPERATEUR DE RIGIDITE CALCULE A ITERATION ----------------------
! =====================================================================
    call lcinma(zero, hook)
!
    if (mod(1:2) .eq. '3D' .or. mod(1:6) .eq. 'D_PLAN' .or. mod(1:4) .eq. 'AXIS') then
!
        if (mater(17,1) .eq. un) then
!
            e = mater(1,1)*((i1 -piso)/pref)**n
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
            e1 = mater(1,1)*((i1 -piso)/pref)**n
            e2 = mater(2,1)*((i1 -piso)/pref)**n
            e3 = mater(3,1)*((i1 -piso)/pref)**n
            nu12 = mater(4,1)
            nu13 = mater(5,1)
            nu23 = mater(6,1)
            g1 = mater(7,1)*((i1 -piso)/pref)**n
            g2 = mater(8,1)*((i1 -piso)/pref)**n
            g3 = mater(9,1)*((i1 -piso)/pref)**n
            nu21 = mater(13,1)
            nu31 = mater(14,1)
            nu32 = mater(15,1)
            delta= mater(16,1)
!
            hook(1,1) = (un - nu23*nu32)*e1/delta
            hook(1,2) = (nu21 + nu31*nu23)*e1/delta
            hook(1,3) = (nu31 + nu21*nu32)*e1/delta
            hook(2,2) = (un - nu13*nu31)*e2/delta
            hook(2,3) = (nu32 + nu31*nu12)*e2/delta
            hook(3,3) = (un - nu21*nu12)*e3/delta
            hook(2,1) = hook(1,2)
            hook(3,1) = hook(1,3)
            hook(3,2) = hook(2,3)
            hook(4,4) = g1
            hook(5,5) = g2
            hook(6,6) = g3
!
        else
            call u2mess('F', 'COMPOR1_34')
        endif
!
    else if (mod(1:6) .eq. 'C_PLAN' .or. mod(1:2) .eq. '1D') then
!
        call u2mess('F', 'COMPOR1_4')
!
    endif
!
!
! =====================================================================
! --- I. CALCUL DE B(K,L) (NBMECAXNBMECA) -----------------------------
! =====================================================================
! ---> I.1. CALCUL DE B1(K,L) = E(K)*HOOK*PSI(L)
!             (TERME SYMETRIQUE)
    do 45 k = 1, nbmect
        do 45 l = 1, nbmect
            b1(k,l) = zero
45      continue
!
    do 40 k = 1, nbmeca
        kk = (k-1)*ndt
        do 40 l = 1, nbmeca
            ll = (l-1)*ndt
            do 40 i = 1, ndt
                do 40 j = 1, ndt
                    b1(k,l) = b1(k,l) - hook(i,j)*dfds(kk+i)*psi(ll+j)
40              continue
!
! ------------ FIN I.1.
! ---> I.2. CALCUL DE B2(K,L) = DFDEVP(K)*EVL(L)
!           TERME NON SYMETRIQUE
    do 41 k = 1, nbmeca
!
        kk = ind(k)
        pk = p(k) -ptrac
        dfdevp = zero
!
        if (kk .lt. 4) then
!
            dfdevp = -m*bhuj*beta*rc(k)*pk
!
        else if (kk .eq. 4) then
!
            dfdevp = -beta*rc(k)*dhuj*pc
!
!af 04/06/07 Debut
        else if ((kk .gt. 4) .and. (kk .lt. 8)) then
!
            xk(1) = vin(4*kk-11)
            xk(2) = vin(4*kk-10)
            th(1) = vin(4*kk-9)
            th(2) = vin(4*kk-8)
            prod = sigd(3*k-2)*(xk(1)-rc(k)*th(1)) + sigd(3*k)*(xk(2)- rc(k)*th(2))/deux
            if (q(k) .lt. tole1) then
                dfdevp = -m*pk*beta*bhuj*rc(k)
            else
                dfdevp = -m*pk*beta*bhuj*(-prod/q(k)+rc(k))
            endif
!
        else if (kk .eq. 8) then
!
            x4 = vin(21)
            if (vin(22) .eq. un) then
                dfdevp = -beta*pc*dhuj*(rc(k)-x4)
            else
                dfdevp = -beta*pc*dhuj*(x4+rc(k))
            endif
!
        endif
!
        do 41 l = 1, nbmeca
!
            ll = ind(l)
            evl = zero
!
            if (ll .lt. 4) then
!
!kh --- traction
                if ((p(l)/pref) .gt. tole1) then
                    dpsi = mdil+q(l)/p(l)
                else
                    dpsi = mdil+1.d+6*q(l)/pref
                endif
                evl = -ksi(l)*coef*dpsi
!
            else if (ll .eq. 4) then
!
                evl = -un
!
            else if ((ll .gt. 4) .and. (ll .lt. 8)) then
!
                call hujprj(ll-4, sig, dev, tp, tp1)
                ps = 2*sigd(3*l-2)*dev(1)+sigd(3*l)*dev(3)
!kh --- traction
                if ((p(l)/pref) .gt. tole1) then
                    if ((-q(l)/pref) .lt. tole1) then
                        dpsi = mdil
                    else
                        dpsi = mdil+ps/(2.d0*p(l)*q(l))
                    endif
                else
                    if ((-q(l)/pref) .lt. tole1) then
                        dpsi = mdil
                    else
                        dpsi =mdil+ps/(2.d-6*pref*q(l))
                    endif
                endif
!
                evl = -ksi(l)*coef*dpsi
!
            else if (ll .eq. 8) then
!
                if (vin(22) .eq. un) then
                    evl = un
                else
                    evl = - un
                endif
!
            endif
!
            b2(k,l) = dfdevp*evl
!
41      continue
!
! ------------ FIN I.2.
! ---> I.3. CALCUL DE B3(K) = DFDR(K) * [ (1 -RK)**2 /AK ]
!           TERME DIAGONAL
    do 43 k = 1, nbmeca
!
        kk = ind(k)
        pk = p(k) -ptrac
        b3(k) = zero
!
        if (kk .lt. 4) then
!
            b3(k) = m*pk*(un-bhuj*log(pk/pc)) * (un-rc(k))**deux /ad( k)
!
        else if (kk .eq. 4) then
!
            b3(k) = dhuj*pc * (un-rc(k))**deux /cmon
!
        else if ((kk .gt. 4) .and. (kk .lt. 8)) then
!
            xk(1) = vin(4*kk-11)
            xk(2) = vin(4*kk-10)
            th(1) = vin(4*kk-9)
            th(2) = vin(4*kk-8)
            prod = sigd(3*k-2)*th(1) + sigd(3*k)*th(2)/deux
            b3(k) = m*pk*(un-bhuj*log(pk/pc))*deux* (un-rc(k))**deux / ad(k)
!
        else if (kk .eq. 8) then
!
            b3(k) = dhuj*pc*(un-rc(k))**deux /ccyc
!
        endif
!
43  continue
! ------------ FIN I.3.
!
    do 42 k = 1, nbmeca
        do 44 l = 1, nbmeca
            b(k,l) = b1(k,l) + b2(k,l)
44      continue
        b(k,k) = b(k,k) + b3(k)
42  continue
!
! =====================================================================
! --- II. CALCUL DE D(K,I) = E(K)*HOOK (NBMECAXNDT) -----------------
! =====================================================================
    do 51 k = 1, nbmeca
        do 51 i = 1, ndt
            d(k,i) = zero
51      continue
!
    do 50 k = 1, nbmeca
        kk = (k-1)*ndt
        do 50 i = 1, ndt
            do 50 j = 1, ndt
                d(k,i) = d(k,i) - hook(j,i)*dfds(kk+j)
50          continue
!
! =====================================================================
! --- III. CALCUL DE D = B-1*D ----------------------------------------
! =====================================================================
    call mgauss('NCVP', b, d, 7, nbmeca,&
                ndt, det, iret)
    if (iret .eq. 1) call u2mess('F', 'COMPOR1_6')
!
! =====================================================================
! --- IV. CALCUL DE TE = IDEN6 - E*D (6X6) ----------------------------
! =====================================================================
    call lcinma(zero, te)
    do 61 i = 1, ndt
        te(i,i) = un
61  continue
!
    do 60 k = 1, nbmeca
        kk = (k-1)*ndt
        do 60 i = 1, ndt
            do 60 j = 1, ndt
                te(i,j) = te(i,j) - psi(kk+i)*d(k,j)
60          continue
!
!
! =====================================================================
! --- V. CALCUL DE LA MATRICE TANGENTE EXPLICITE DSDE(I,J,K,L) = ------
! =====================================================================
!
!    HOOK(I,J,K,L) - HOOK(I,J,P,Q)*TE(P,Q,K,L)
!
! =====================================================================
    do 820 j = 1, ndt
        do 820 i = 1, ndt
            dsde(i,j) = zero
820      continue
    call lcprmm(hook, te, dsde)
!
    goto 1000
!
! =====================================================================
!        CALL JEDEMA ()
! =====================================================================
999  continue
    call u2mess('A', 'COMPOR1_14')
!
1000  continue
!
end subroutine
