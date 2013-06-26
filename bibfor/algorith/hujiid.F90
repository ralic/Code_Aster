subroutine hujiid(mod, mater, indi, deps, i1e,&
                  yd, vind, dy, loop, dsig,&
                  bnews, mtrac, iret)
! aslint: disable=W1501
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     ----------------------------------------------------------------
!     LOI HUJEUX :  MECANISMES ISOTROPE ET DEVIATOIRE
!     CALCUL DE LA SOLUTION D ESSAI EXPLICITE
!            DY = (DSIG, DR, DEPSVP, DLAMB)
!     AVEC   Y  = ( SIG,  R,  EPSI_VOLU_P,  DLAMBDA )
!     A PARTIR DE LA PREDICTION ELASTIQUE
!     ----------------------------------------------------------------
!     IN   MOD      :  MODELISATION
!          MATER    :  COEFFICIENTS MATERIAU A T+DT
!          INDI     :  INDICE DES MECANISMES ACTIVES
!          DEPS     :  INCREMENT DE DEFORMATION
!          YD       :  VARIABLES A T = (SIGD, VIND, DLAMB)
!          VIND     :  VARIABLES INTERNES A T
!          I1E      :  TRACE(SIGE): CONTRAINTE DE PREDICTION
!          LOOP     :  UTILISE PREDICTION ELASTIQUE (= .FALSE.) OU
!                                         PLASTIQUE (= .TRUE. )
!          DSIG     :  INCREMENT DE CONTRAINTE PLASTIQUE NECESSAIRE
!                      POUR PREDICTION PLASTIQUE
!     OUT  DY       :  SOLUTION D ESSAI (DSIG, DVIN, DDLAMB)
!          INDI     :  MECANISMES ACTIVES + TRACTION (1 A 3 SUPPL)
!     ----------------------------------------------------------------
!     Y CONTIENT LES CONTRAINTES : SIG
!                LES VARIABLES D'ECROUISSAGE : R, EPSI_VOLU_P
!                LES MULTIPLICATEURS PLASTIQUES : DLAMBDA
! ====================================================================
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/hujddd.h'
    include 'asterfort/hujksi.h'
    include 'asterfort/hujpic.h'
    include 'asterfort/hujprc.h'
    include 'asterfort/hujprj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/lcsovn.h'
    include 'asterfort/mgauss.h'
    include 'asterfort/tecael.h'
    include 'asterfort/trace.h'
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi, i, j, k, kk, l, ll, nbmect
    integer :: nbmeca, indi(7), iret, iadzi, iazk24
    integer :: ifm, niv
    real(kind=8) :: deps(6), depse(6), hooknl(6, 6), i1e
    real(kind=8) :: dsig(6), sigd(3), p(7), q(7), pe(7), qe(7)
    real(kind=8) :: yd(18), ye(18), dy(18), f2(7), dp(3)
    real(kind=8) :: mater(22, 2), n, beta, b, d, m, pco, pc, pref
    real(kind=8) :: phi, angdil, mdil, acyc, amon, cmon, ccyc
    real(kind=8) :: rc(7), epsvp, ad(7), ksi(7)
    real(kind=8) :: e, nu, al, demu, i1de, sige(6)
    real(kind=8) :: dfdl(7, 7), dr(7), depsvp, rtrac
    real(kind=8) :: degr, zero, un, d13, deux
    real(kind=8) :: det, tol, tole1, coef, vind(*)
    real(kind=8) :: psi(42), dfds(6), dpsids(6, 6)
    real(kind=8) :: sigdc(12), sigdce(12), prod
    real(kind=8) :: xk(2), th(2), la, ps, tp, tp1, s, ds
    real(kind=8) :: ptrac, piso, pek, dpsi, dsigt(6), sigt(6)
    real(kind=8) :: e1, e2, e3, nu12, nu13, nu23, g1, g2, g3, nu21, nu31, nu32
    real(kind=8) :: delta
    real(kind=8) :: factor, maxi, cohes, vec(3), pt, qt
    character(len=8) :: mod, nomail
    logical :: debug, loop, bnews(3), mtrac
! ====================================================================
    parameter   ( d13  = .3333333333334d0 )
    parameter   ( un   = 1.d0 )
    parameter   ( zero = 0.d0 )
    parameter   ( deux = 2.d0 )
    parameter   ( tole1 = 1.d-7 )
    parameter   ( degr = 0.0174532925199d0 )
!
!
! ====================================================================
    common /tdim/     ndt, ndi
    common /meshuj/   debug
! ====================================================================
    call infniv(ifm, niv)
!
    do 7 i = 1, 18
        ye(i) = zero
 7  continue
! ====================================================================
! --- PROPRIETES HUJEUX MATERIAU -------------------------------------
! ====================================================================
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
    ptrac = mater(21,2)
    piso = zero
    rtrac = abs(1.d-6*pref)
!
!
! ====================================================================
! --- PREMIER INVARIANT ET AUTRES GRANDEURS UTILES -------------------
! ====================================================================
    iret = 0
    nbmeca = 0
!
    do 4 k = 1, 4
        if (indi(k) .gt. 0) nbmeca=nbmeca+1
        q(k) = zero
        qe(k) = zero
 4  continue
!
!
! ====================================================================
! --- OPERATEUR DE RIGIDITE NON LINEAIRE -----------------------------
! ====================================================================
! --- OPERATEUR LINEAIRE NON LINEAIRE --------------------------------
! ====================================================================
    if ((i1e-piso) .ge. zero) then
        iret = 1
        goto 9999
    endif
!
    call lcinma(zero, hooknl)
!
    if (mod(1:2) .eq. '3D' .or. mod(1:6) .eq. 'D_PLAN' .or. mod(1:4) .eq. 'AXIS') then
!
        if (mater(17,1) .eq. un) then
!
            e = mater(1,1)*((i1e -piso)/pref)**n
            nu = mater(2,1)
            al = e*(un-nu) /(un+nu) /(un-deux*nu)
            demu = e /(un+nu)
            la = e*nu/(un+nu)/(un-deux*nu)
!
            do 30 i = 1, ndi
                do 30 j = 1, ndi
                    if (i .eq. j) hooknl(i,j) = al
                    if (i .ne. j) hooknl(i,j) = la
30              continue
            do 35 i = ndi+1, ndt
                hooknl(i,i) = demu
35          continue
!
        else if (mater(17,1).eq.deux) then
!
            e1 = mater(1,1)*((i1e -piso)/pref)**n
            e2 = mater(2,1)*((i1e -piso)/pref)**n
            e3 = mater(3,1)*((i1e -piso)/pref)**n
            nu12 = mater(4,1)
            nu13 = mater(5,1)
            nu23 = mater(6,1)
            g1 = mater(7,1)*((i1e -piso)/pref)**n
            g2 = mater(8,1)*((i1e -piso)/pref)**n
            g3 = mater(9,1)*((i1e -piso)/pref)**n
            nu21 = mater(13,1)
            nu31 = mater(14,1)
            nu32 = mater(15,1)
            delta= mater(16,1)
!
            hooknl(1,1) = (un - nu23*nu32)*e1/delta
            hooknl(1,2) = (nu21 + nu31*nu23)*e1/delta
            hooknl(1,3) = (nu31 + nu21*nu32)*e1/delta
            hooknl(2,2) = (un - nu13*nu31)*e2/delta
            hooknl(2,3) = (nu32 + nu31*nu12)*e2/delta
            hooknl(3,3) = (un - nu21*nu12)*e3/delta
            hooknl(2,1) = hooknl(1,2)
            hooknl(3,1) = hooknl(1,3)
            hooknl(3,2) = hooknl(2,3)
            hooknl(4,4) = g1
            hooknl(5,5) = g2
            hooknl(6,6) = g3
!
        else
            call u2mess('F', 'COMPOR1_39')
        endif
!
    else if (mod(1:6) .eq. 'C_PLAN' .or. mod(1:2) .eq. '1D') then
!
        call u2mess('F', 'COMPOR1_4')
!
    endif
!
!
! ====================================================================
! --- ON CALCULE DE TOUTES FACONS UNE PREDICTION ---------------------
! --- ELASTIQUE EN TANT QUE DE BESOIN --------------------------------
! ====================================================================
    if (.not. loop) call lcprmv(hooknl, deps, dsig)
    call lcsovn(ndt, yd, dsig, ye)
!      LOOP = .FALSE.
! ====================================================================
! --- CALCUL DE L'INCRMEENT DE CONTRAINTES ELASTIQUE SI LOOP ACTIVE
!     CECI EST FAIT POUR DETECTER LES SOLLICITATIONS POSSIBLES
!     DES MECANISMES DE TRACTION A PARTIR DU TIR ELASTIQUE ET NON DE
!     L'ETAT DE CONTRAINTES CONVERGES PRECEDENT
! ====================================================================
    if (loop) then
        call lcprmv(hooknl, deps, dsigt)
    else
        call lceqvn(ndt, dsig, dsigt)
    endif
    call lcsovn(ndt, yd, dsigt, sigt)
!
! --- FAUT-IL CONSIDERER LES MECANISMES DE TRACTION?
    nbmect = nbmeca
    call lceqvn(ndt, ye, sige)
!
!
    if (debug) write(6,*)'BNEWS =',(bnews(i),i=1,3)
    do 36 i = 1, 3
        call hujprj(i, sigt, sigd, pt, qt)
        if ((((pt+deux*rtrac-ptrac)/abs(pref)).ge.-r8prem()) .and. ( .not.bnews(i))) then
            nbmect = nbmect + 1
            indi(nbmect) = 8 + i
        else if ((.not.bnews(i)).and.(mtrac)) then
            nbmect = nbmect + 1
            indi(nbmect) = 8 + i
        endif
36  continue
!
    maxi = un
    cohes = -rtrac+ptrac
    factor = un
!
    if ((nbmect.ne.nbmeca) .and. (nbmeca.eq.0)) goto 51
!
    do 50 i = 1, ndi
        call hujprj(i, sige, sigd, pe(i), qe(i))
        call hujprj(i, yd, sigd, p(i), q(i))
        call hujprj(i, dsig, sigd, dp(i), q(i))
        if ((pe(i).gt.cohes) .and. (dp(i).gt.tole1)) then
            factor = (-p(i)+cohes)/dp(i)
            if ((factor.gt.zero) .and. (factor.lt.maxi)) then
                maxi = factor
            endif
        endif
50  continue
!
! ---> SI IL EXISTE SIG(I)>0, ALORS MODIFICATION DE LA PREDICTION
    if (maxi .lt. un) then
        do 60 i = 1, ndt
            dsig(i) = maxi * dsig(i)
60      continue
        if (debug) then
            write (6,'(A,A,E12.5)')&
     &   'HUJIID DEBUT : APPLICATION DE FACTOR POUR MODIFIER ',&
     &    'LA PREDICTION -> FACTOR =',factor
            write(6,*)'YE =',(yd(i)+dsig(i),i=1,ndt)
        endif
        call lcsovn(ndt, yd, dsig, ye)
    endif
51  continue
!
    if ((nbmeca .eq. 1) .and. ((indi(1).eq.4) .or. (indi(1).eq.8))) then
!
        do 31 i = ndt+1, 18
            dy(i)=zero
31      continue
        do 32 i = 1, ndt
            dy(i) = dsig(i)
32      continue
!
        goto 9999
!
    endif
!
    do 3 k = 1, 42
        psi(k) = zero
 3  continue
!
    do 6 k = 1, 7
        pe(k) = zero
        q(k) = zero
        qe(k) = zero
        p(k) = zero
 6  continue
!
    do 5 k = 1, nbmect
!
        call hujddd('PSI   ', indi(k), mater, indi, yd,&
                    vind, psi((k-1)*ndt+1), dpsids, iret)
        if (iret .eq. 1) goto 999
!
        if (indi(k) .le. 8) then
!
            rc(k) = yd(ndt+1+k)
!
            if (indi(k) .lt. 4) then
!
                call hujprj(indi(k), yd, sigd, p(k), q(k))
                call hujprj(indi(k), ye, sigd, pe(k), qe(k))
                if (((p(k) -ptrac)/pref) .le. tole1 .or. ((pe(k)- ptrac)/pref) .le. tole1) &
                goto 999
                call hujksi('KSI   ', mater, rc(k), ksi(k), iret)
                if (iret .eq. 1) goto 999
                ad(k) = acyc+ksi(k)*(amon-acyc)
!
            else if ((indi(k) .gt. 4) .and. (indi(k) .lt. 8)) then
!
                call hujprc(k, indi(k)-4, yd, vind, mater,&
                            yd, p(k), q(k), sigdc(3*k-2))
                call hujprc(k, indi(k)-4, ye, vind, mater,&
                            yd, pe(k), qe(k), sigdce(3*k-2))
                if (((p(k) -ptrac)/pref) .le. tole1 .or. ((pe(k)- ptrac)/pref) .le. tole1) &
                goto 999
                call hujksi('KSI   ', mater, rc(k), ksi(k), iret)
                if (iret .eq. 1) goto 999
!
                th(1) = vind(4*indi(k)-9)
                th(2) = vind(4*indi(k)-8)
                prod = sigdce(3*k-2)*th(1) + sigdce(3*k)*th(2)/deux
!
                if (qe(k) .lt. tole1) then
                    ad(k) = (acyc+ksi(k)*(amon-acyc))
                else if ((un+prod/qe(k)).lt.tole1) then
                    ad(k) = (acyc+ksi(k)*(amon-acyc))
                else
                    ad(k) = (acyc+ksi(k)*(amon-acyc))*(un+prod/qe(k))
                endif
!
            else if (indi(k) .eq. 8) then
!
                call hujpic(k, indi(k), yd, vind, mater,&
                            yd, p(k))
                call hujpic(k, indi(k), ye, vind, mater,&
                            yd, pe(k))
!
                if (((p(k) -piso)/pref) .le. tole1 .or. ((pe(k)-piso)/ pref) .le. tole1) &
                goto 999
!
            endif
!
            ye(ndt+1+k) = yd(ndt+1+k)
!
        endif
!
 5  end do
!
    epsvp = yd(ndt+1)
    ye(ndt+1) = yd(ndt+1)
    pc = pco*exp(-beta*epsvp)
!
    cmon = cmon * pc/pref
    ccyc = ccyc * pc/pref
!
    coef = mater(20,2)
!
! ====================================================================
! --- CALCUL DE DLAMBI, DLAMBD ---------------------------------------
! ====================================================================
! --- PAR RESOLUTION DU SYSTEME : ------------------------------------
!        _
!       (     D FD               D FD
!       (   --------  * DDLD + --------  * DDLI = - F2(1:3)
!       (   D DLAMBD           D DLAMBI
!       (
!       (     D FI               D FI
!       (   --------  * DDLD + --------  * DDLI = - F2(4)
!       (_  D DLAMBD           D DLAMBI
!
! ====================================================================
    do 45 k = 1, nbmect
        do 45 l = 1, nbmect
            dfdl(k,l) = zero
45      continue
!
!
! ---> I. CALCUL DE DF. / DDLAMB. POUR DDLAMB. = 0
! ---> I.1. CALCUL DE DFDSE(K)*HOOKNL*PSI-(L)
    do 40 k = 1, nbmect
        kk = indi(k)
        call hujddd('DFDS  ', kk, mater, indi, ye,&
                    vind, dfds, dpsids, iret)
        if (iret .eq. 1) goto 999
        do 40 l = 1, nbmect
            ll = (l-1)*ndt
            do 40 i = 1, ndt
                do 40 j = 1, ndt
                    dfdl(k,l) = dfdl(k,l) - hooknl(i,j)*dfds(i)*psi( ll+j)
40              continue
!
! ---- FIN I.1.
! ---> I.2. CALCUL DE DFDEVPE(K)*DEVPDDLAMB-(L)
! ----  I.2.1. MECANISME DEVIATOIRE MONOTONE
    do 11 k = 1, nbmect
!
        kk = indi(k)
        pek =pe(k) -ptrac
        if (kk .lt. 4) then
!
            f2(k) = -qe(k) - m*pek*rc(k) * ( un - b*log(pek/pc) )
            if (f2(k) .gt. zero) f2(k) = zero
!
            do 12 l = 1, nbmeca
!
                ll = indi(l)
!
                if (ll .lt. 4) then
!
!kh -- traction
                    if ((p(l)/pref) .gt. tole1) then
                        dpsi = mdil+q(l)/p(l)
                    else
                        dpsi = mdil+1.d+6*q(l)/pref
                    endif
                    dfdl(k,l) = dfdl(k,l) + b*m*pek*rc(k)*beta * ksi(l)*coef*dpsi
!
                else if (ll .eq. 4) then
!
                    dfdl(k,l) = dfdl(k,l) + b*m*pek*rc(k)*beta
!
                else if ((ll .gt. 4) .and. (ll .lt. 8)) then
!
                    call hujprj(ll-4, ye, sigd, tp, tp1)
                    ps = 2*sigd(1)*sigdce(3*l-2)+sigd(3)*sigdce(3*l)
!kh -- traction
                    if (((p(l)/pref).gt.tole1) .and. ((-q(l)/pref) .gt.tole1)) then
                        dpsi =mdil+ps/(2.d0*p(l)*q(l))
                        elseif (((p(l)/pref).le.tole1) .and. ((-q(l)/pref)&
                    .gt.tole1)) then
                        dpsi =mdil+ps/(2.d-6*pref*q(l))
                    else
                        dpsi =mdil
                    endif
                    dfdl(k,l) = dfdl(k,l) + b*m*pek*rc(k)*beta * ksi(l)*coef * dpsi
!
                else if (ll .eq. 8) then
!
                    if (vind(22) .eq. un) then
                        dfdl(k,l) = dfdl(k,l) - b*m*pek*rc(k)*beta
                    else
                        dfdl(k,l) = dfdl(k,l) + b*m*pek*rc(k)*beta
                    endif
!
                endif
!
12          continue
!
! ---- I.2.2. MECANISME ISOTROPE MONOTONE
        else if (kk .eq. 4) then
!
            if (k .ne. nbmeca) call u2mess('F', 'COMPOR1_5')
            i1de = d13*trace(ndi,ye)
            f2(k) = -abs(i1de) - rc(k)*d*pc
            if (f2(k) .gt. zero) f2(k) = zero
!
            dfdl(k,k) = dfdl(k,k) + rc(k)*d*pc*beta
!
            do 13 l = 1, nbmeca-1, 1
                ll = indi(l)
                if (ll .lt. 4) then
!
!kh --- traction
                    if ((p(l)/pref) .gt. tole1) then
                        dpsi =mdil+q(l)/p(l)
                    else
                        dpsi = mdil+1.d+6*q(l)/pref
                    endif
!
                    dfdl(k,l) = dfdl(k,l) + rc(k)*d*pc*beta * ksi(l)* coef*dpsi
!
                else if ((ll.gt.4).and.(ll.lt.8)) then
!
                    call hujprj(ll-4, ye, sigd, tp, tp1)
                    ps = 2*sigd(1)*sigdce(3*l-2)+sigd(3)*sigdce(3*l)
!kh --- traction
                    if (((p(l)/pref).gt.tole1) .and. ((-q(l)/pref) .gt.tole1)) then
                        dpsi =mdil+ps/(2.d0*p(l)*q(l))
                        elseif (((p(l)/pref).le.tole1) .and. ((-q(l)/pref)&
                    .gt.tole1)) then
                        dpsi =mdil+ps/(2.d-6*pref*q(l))
                    else
                        dpsi =mdil
                    endif
                    dfdl(k,l) = dfdl(k,l) + rc(k)*d*pc*beta * ksi(l)* coef*mdil
                endif
13          continue
!
! --- I.2.3. MECANISME DEVIATOIRE CYCLIQUE
        else if ((kk .lt. 8) .and. (kk .gt. 4)) then
!
            f2(k) = -qe(k) - m*pek*rc(k) * ( un - b*log(pek/pc) )
            if (f2(k) .gt. zero) f2(k) = zero
!
            xk(1) = vind(4*kk-11)
            xk(2) = vind(4*kk-10)
            th(1) = vind(4*kk-9)
            th(2) = vind(4*kk-8)
            prod = sigdce(3*k-2)*(xk(1)-rc(k)*th(1)) + sigdce(3*k)*( xk(2)-rc(k)*th(2))/deux
!
            do 14 l = 1, nbmeca
!
                ll = indi(l)
                if (ll .lt. 4) then
!
!kh --- traction
                    if ((p(l)/pref) .gt. tole1) then
                        dpsi =mdil+q(l)/p(l)
                    else
                        dpsi =mdil+1.d+6*q(l)/pref
                    endif
!
                    if ((-qe(k)/pref) .gt. tole1) then
                        dfdl(k,l) = dfdl(k,l) + b*m*pek*beta* (-prod/ qe(k)+ rc(k)) * ksi(l)*coef&
                                    &*dpsi
                    else
                        dfdl(k,l) = dfdl(k,l) + b*m*pek*beta*rc(k) * ksi(l)*coef*dpsi
                    endif
!
                else if (ll .eq. 4) then
!
                    if ((-qe(k)/pref) .lt. tole1) then
                        dfdl(k,l) = dfdl(k,l) + b*m*pek*beta*rc(k)
                    else
                        dfdl(k,l) = dfdl(k,l) + b*m*pek*beta* (-prod/ qe(k) + rc(k))
                    endif
!
                else if ((ll .gt. 4) .and. (ll .lt. 8)) then
!
                    call hujprj(ll-4, ye, sigd, tp, tp1)
                    ps = 2*sigd(1)*sigdce(3*l-2)+sigd(3)*sigdce(3*l)
!kh --- traction
                    if (((p(l)/pref).gt.tole1) .and. ((-q(l)/pref) .gt.tole1)) then
                        dpsi =mdil+ps/(2.d0*p(l)*q(l))
                        elseif (((p(l)/pref).le.tole1) .and. ((-q(l)/pref)&
                    .gt.tole1)) then
                        dpsi =mdil+ps/(2.d-6*pref*q(l))
                    else
                        dpsi =mdil
                    endif
!
                    if ((-qe(k)/pref) .lt. tole1) then
                        dfdl(k,l) = dfdl(k,l) + b*m*pek*beta*rc(k) * ksi(l)*coef*dpsi
                    else
                        dfdl(k,l) = dfdl(k,l) + b*m*pek*beta* (-prod/ qe(k) + rc(k)) * ksi(l)*coe&
                                    &f*dpsi
                    endif
!
                else if (ll .eq. 8) then
!
                    if ((-qe(k)/pref) .lt. tole1) then
                        if (vind(22) .eq. un) then
                            dfdl(k,l) = dfdl(k,l) - b*m*pek*beta*rc(k)
                        else
                            dfdl(k,l) = dfdl(k,l) + b*m*pek*beta*rc(k)
                        endif
                    else
                        if (vind(22) .eq. un) then
                            dfdl(k,l) = dfdl(k,l) - b*m*pek*beta* (-prod*qe(k) + rc(k))
                        else
                            dfdl(k,l) = dfdl(k,l) + b*m*pek*beta* (-prod*qe(k) + rc(k))
                        endif
                    endif
                endif
!
14          continue
!
! --- I.2.4. MECANISME ISOTROPE CYCLIQUE
        else if (kk .eq. 8) then
!
            f2(k) = -abs(pe(k)) - d*rc(k)*pc
            if (f2(k) .gt. zero) f2(k) = zero
!
            if (vind(22) .eq. un) then
                dfdl(k,k) = dfdl(k,k) - d*pc*beta* (rc(k)-vind(21))
            else
                dfdl(k,k) = dfdl(k,k) + d*pc*beta* (rc(k)+vind(21))
            endif
!
            do 15 l = 1, nbmeca-1, 1
!
                ll = indi(l)
                if (ll .lt. 4) then
!
!kh --- traction
                    if ((p(l)/pref) .gt. tole1) then
                        dpsi =mdil+q(l)/p(l)
                    else
                        dpsi =mdil+1.d+6*q(l)/pref
                    endif
!
                    if (vind(22) .eq. un) then
                        dfdl(k,l) = dfdl(k,l) + d*pc*beta* (rc(k)- vind(21)) *ksi(l)*coef*dpsi
                    else
                        dfdl(k,l) = dfdl(k,l) + d*pc*beta* (rc(k)+ vind(21)) *ksi(l)*coef*dpsi
                    endif
!
                else if ((ll.lt.8) .and. (ll.gt.4)) then
!
                    call hujprj(ll-4, ye, sigd, tp, tp1)
                    ps = 2*sigd(1)*sigdce(3*l-2)+sigd(3)*sigdce(3*l)
!
!kh --- traction
                    if (((p(l)/pref).gt.tole1) .and. ((-q(l)/pref) .gt.tole1)) then
                        dpsi =mdil+ps/(2.d0*p(l)*q(l))
                        elseif (((p(l)/pref).le.tole1) .and. ((-q(l)/pref)&
                    .gt.tole1))then
                        dpsi =mdil+ps/(2.d-6*pref*q(l))
                    else
                        dpsi =mdil
                    endif
!
                    if (vind(22) .eq. un) then
                        dfdl(k,l) = dfdl(k,l) + d*pc*beta* (rc(k)- vind(21)) *ksi(l)*coef*dpsi
                    else
                        dfdl(k,l) = dfdl(k,l) + d*pc*beta* (rc(k)+ vind(21)) *ksi(l)*coef*dpsi
                    endif
                endif
15          continue
!
        else if (kk.gt.8) then
            call hujprj(kk-8, ye, vec, tp, tp1)
            f2(k) = -tp - deux*rtrac + ptrac
            if (f2(k) .gt. zero) f2(k) = zero
        endif
11  continue
!
! ---- FIN I.2.
! ---> I.3. CALCUL DE DFDRE(K)*DRDLAMB-(K)
    if (nbmeca .eq. 0) goto 160
    do 16 k = 1, nbmeca, 1
!
        kk = indi(k)
        pek =pe(k) -ptrac
!
        if (kk .lt. 4) then
!
            dfdl(k,k) = dfdl(k,k) + m*pek*(un-b*log(pek/pc)) * (un-rc( k))**deux /ad(k)
!
        else if (kk .eq. 4) then
!
            dfdl(k,k) = dfdl(k,k) + d*pc * (un-rc(k))**deux /cmon
!
        else if ((kk .gt. 4) .and. (kk .lt. 8)) then
!
            th(1) = vind(4*kk-9)
            th(2) = vind(4*kk-8)
!
            prod = sigdce(3*k-2)*th(1) + sigdce(3*k)*th(2)/deux
            if ((-qe(k)/pref) .lt. tole1) then
                dfdl(k,k) = dfdl(k,k) + m*pek*(un-b*log(pek/pc)) * (un-rc(k))**deux /ad(k)
!
            else
                dfdl(k,k) = dfdl(k,k) + m*pek*(un-b*log(pek/pc)) * (un+prod/qe(k)) * (un-rc(k))**&
                            &deux /ad(k)
!
                if (dfdl(k,k) .eq. zero) dfdl(k,k) = dfdl(k,k) + 2.d0*m* pek*(un-b*log(pek/pc)) *&
                                                     & (un-rc(k))**deux /ad(k)
            endif
!
        else if (kk .eq. 8) then
!
            dfdl(k,k) = dfdl(k,k) + d*pc*(un-rc(k))**deux /ccyc
!
        endif
16  continue
!
160  continue
! ---- RESOLUTION PAR PIVOT DE GAUSS
!
    call mgauss('NCVP', dfdl, f2, 7, nbmect,&
                1, det, iret)
    if (iret .eq. 1) goto 9999
!
! --- MULTIPLICATEUR PLASTIQUE NEGATIF NON AUTORISE
    do 17 k = 1, nbmect
        if (f2(k) .lt. zero) f2(k)=zero
17  continue
!
!
! ====================================================================
! --- CALCUL DES INCREMENTS DE DEFORMATIONS ELASTIQUE ----------------
! ====================================================================
    do 240 i = 1, ndt
        depse(i) = deps(i)
240  continue
!
    do 242 k = 1, nbmect
        kk = (k-1)*ndt
        do 242 i = 1, ndt
            depse(i) = depse(i) - f2(k)*psi(kk+i)
242      continue
!
! ====================================================================
! --- CALCUL INCREMENT DE CONTRAINTES  DSIG = HOOKNL-.DEPSE ----------
! ====================================================================
    if (.not. loop) call lcprmv(hooknl, depse, dsig)
    call lcsovn(ndt, yd, dsig, ye)
!
    maxi = un
    cohes = -rtrac+ptrac
    factor = un
!
    do 280 i = 1, ndi
        call hujprj(i, ye, sigd, pe(i), qe(i))
        call hujprj(i, yd, sigd, p(i), q(i))
        call hujprj(i, dsig, sigd, dp(i), q(i))
        if (pe(i) .gt. cohes .and. dp(i) .gt. tole1) then
            factor = (-p(i)+cohes)/dp(i)
            if ((factor.gt.zero) .and. (factor.lt.maxi)) then
                maxi = factor
            endif
        endif
280  continue
!
!KH ON IMPOSE UNE VARIATION DE DSIGMA < 50% DE SIGMA_INIT
!AF A CONDITION QU'IL N'Y AIT PAS DE MECANISMES DE TRACTION ACTIVES
    if (nbmect .eq. nbmeca) then
        s=0.d0
        ds=0.d0
        tol = .5d0
        do 282 i = 1, ndt
            s = s+yd(i)**2.d0
            ds= ds+dsig(i)**2.d0
282      continue
        s=sqrt(s)
        ds=sqrt(ds)
!
        factor = un
        if ((-s/pref) .gt. tole1) then
            if (ds/s .gt. tol) factor = tol*s/ds
        else if ((-ds/pref) .gt. tol) then
            factor = -tol*pref/ds
        endif
!
        maxi=min(factor,maxi)
    endif
! ---> SI IL EXISTE SIG(I)>0, ALORS MODIFICATION DE LA PREDICTION
    if (maxi .lt. un) then
        do 290 i = 1, ndt
            dsig(i) = maxi * dsig(i)
290      continue
        if (debug) then
            write (6,'(A,A,E12.5)')&
     &     'HUJIID FIN:: APPLICATION DE FACTOR POUR MODIFIER ',&
     &     'LA PREDICTION -> FACTOR =',factor
            write(6,*)'YE =',(yd(i)+dsig(i),i=1,ndt)
        endif
    endif
!
! ====================================================================
! --- CALCUL INCREMENT DE LA VARIABLE INTERNE RC ---------------------
! ====================================================================
    if (nbmeca .eq. 0) goto 281
!
    do 250 k = 1, nbmeca
        kk = indi(k)
        if (kk .lt. 4) then
            dr(k) = f2(k) *(un-rc(k))**deux /ad(k)
            if ((yd(ndt+1+k)+dr(k)) .gt. un) then
                f2(k) = (un-rc(k))/((un-rc(k))**deux /ad(k))
                dr(k) = f2(k)*(un-rc(k))**deux/ad(k)
            endif
        else if (kk.eq.4) then
            dr(k) = f2(k) *(un-rc(k))**deux /cmon
!
        else if ((kk .lt. 8) .and. (kk .gt. 4)) then
            dr(k) = f2(k) *(un-rc(k))**deux /ad(k)
            if ((yd(ndt+1+k)+dr(k)) .gt. vind(kk-4)) then
                f2(k) = (vind(kk-4)-rc(k))/ ((un-rc(k))**deux /ad(k))
                dr(k) = f2(k)*(un-rc(k))**deux/ad(k)
            endif
!
        else if (kk .eq. 8) then
            dr(k) = f2(k) *(un-rc(k))**deux /ccyc
!
        endif
250  continue
!
281  continue
! ====================================================================
! --- CALCUL INCREMENT DE LA VARIABLE INTERNE DEPSVP -----------------
! ====================================================================
    depsvp = zero
    do 251 k = 1, nbmect
        kk=indi(k)
        if (kk .lt. 4) then
!
!kh --- traction
            if ((p(k)/pref) .gt. tole1) then
                dpsi =mdil+q(k)/p(k)
            else
                dpsi =mdil+1.d+6*q(k)/pref
            endif
            depsvp = depsvp - f2(k)*coef*ksi(k)*dpsi
!
        else if (kk .eq. 4) then
!
            depsvp = depsvp - f2(k)
!
        else if ((kk .lt. 8) .and. (kk .gt. 4)) then
!
            call hujprj(kk-4, yd, sigd, tp, tp1)
            ps = 2*sigd(1)*sigdc(3*k-2)+sigd(3)*sigdc(3*k)
!kh --- traction
            if (((p(k)/pref).gt.tole1) .and. ((-q(k)/pref).gt.tole1)) then
                dpsi =mdil+ps/(2.d0*p(k)*q(k))
                elseif (((p(k)/pref).le.tole1) .and. ((-q(k)/pref)&
            .gt.tole1)) then
                dpsi =mdil+ps/(2.d-6*pref*q(k))
            else
                dpsi =mdil
            endif
!
            depsvp = depsvp - f2(k)*coef*ksi(k)*dpsi
!
        else if (kk .eq. 8) then
            if (vind(22) .eq. un) then
                depsvp = depsvp + f2(k)
            else
                depsvp = depsvp - f2(k)
            endif
!
        endif
!
251  continue
!
! ====================================================================
! --- SOLUTION D ESSAI -----------------------------------------------
! ====================================================================
    do 259 i = 1, 18
        dy(i) = zero
259  continue
!
    do 260 i = 1, ndt
        dy(i) = dsig(i)
260  continue
!
    if (abs(depsvp) .lt. 1.d-1) then
        dy(ndt+1) = depsvp
    else
        dy(ndt+1) = zero
    endif
!
    if (nbmeca .eq. 0) goto 271
    do 270 k = 1, nbmeca
        dy(ndt+1+k) = dr(k)
        dy(ndt+1+nbmeca+k) = f2(k)
270  continue
!
271  continue
!
    if (nbmeca .lt. nbmect) then
        do 272 i = 1, nbmect
            if (indi(i) .gt. 8) dy(ndt+1+nbmeca+i) = f2(i)
272      continue
    endif
!
    goto 9999
!
999  continue
!
    if (debug) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3) (1:8)
        write (ifm,'(10(A))')&
     &    'HUJIID :: LOG(PK/PC) NON DEFINI DANS LA MAILLE',nomail
        write (ifm,'(A)') '          ON NE FAIT PAS LA PREDICTION'
    endif
!
    do 300 i = 1, 18
        dy(i) = zero
300  continue
!
9999  continue
!
end subroutine
