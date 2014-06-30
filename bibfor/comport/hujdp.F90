subroutine hujdp(mod, deps, sigd, sigf, mater,&
                 vin, ndec, iret)
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
!       ---------------------------------------------------------------
!       TESTS SUR LES CRITERES D'EVOLUTION RELATIFS AUX CONTRAINTES
!       IN  DEPS   :  INCREMENT DE DEFORMATION
!           SIGD   :  CONTRAINTE  A T
!           SIGF   :  CONTRAINTE ELASTIQUE A T+DT
!           MATER  :  PROPRIETES MATERIAU
!       OUT NDEC   :  NOMBRE DE REDECOUPAGE DE DEPS
!           IRET   :  CODE RETOUR
!       ---------------------------------------------------------------
#include "asterfort/hujddd.h"
#include "asterfort/hujprc.h"
#include "asterfort/hujprj.h"
#include "asterfort/lceqvn.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcprmv.h"
#include "asterfort/utmess.h"
    integer :: ndt, ndi, i, j, ndec, iret, indi(7), nbmeca, ni
    real(kind=8) :: di1d, i1d, n, pref, k0
    real(kind=8) :: deps(6), sigd(6), sigf(6)
    real(kind=8) :: mater(22, 2), tole1, tol, ri, vin(*)
    real(kind=8) :: zero, un, d13, deux, depsv
    real(kind=8) :: epsvp, beta, d, fr, i1e
    real(kind=8) :: dfds(6), e, nu, al, la, demu
    real(kind=8) :: hooknl(6, 6), dsig(6), fidsig
    real(kind=8) :: pco, pf, pd, qf, qd, tauf(3)
    real(kind=8) :: taud(3), rela1, rela2
    real(kind=8) :: piso, c11, c12, c13, c22, c23, c33
    real(kind=8) :: e1, e2, e3, nu12, nu13, nu23, g1, g2, g3, nu21, nu31, nu32
    real(kind=8) :: delta
    real(kind=8) :: yf(18), sigdc(3), prodd, prodf, mat(6, 6)
    real(kind=8) :: m, degr, b, phi, ptrac
    character(len=8) :: mod
    logical(kind=1) :: debug
!
    common /tdim/ ndt, ndi
    common /meshuj/ debug
!
    parameter   ( degr = 0.0174532925199d0 )
!
    data zero, d13, un, deux, tol&
     &/ 0.d0, 0.33333333333334d0, 1.d0, 2.d0, 1.d-7 /
!
    if (ndec .gt. 1) then
        iret =1
        goto 500
    endif
!
!      PISO  = 1.5d0*MATER(21,2)
    piso = zero
    pref = mater(8,2)
    n = mater(1,2)
    beta = mater(2,2)
    d = mater(3,2)
    pco = mater(7,2)
    phi = mater(5,2)
    m = sin(degr*phi)
    b = mater(4,2)
    ptrac = mater(21,2)
    epsvp = vin(23)
    depsv = deps(1)+deps(2)+deps(3)
    tole1 = 0.05d0
!
!
! ----------------------------------------------------
! 1 --- CRITERE LIMITANT L EVOLUTION DE P: DP/P < TOLE1
! ----------------------------------------------------
    i1d = d13*(sigd(1)+sigd(2)+sigd(3))
    if (mater(17,1) .eq. un) then
!
        k0 = d13*mater(1,1) /(un-deux*mater(2,1))
        depsv= deps(1)+deps(2)+deps(3)
        di1d = depsv*k0*((i1d -piso)/pref)**n
!
    else if (mater(17,1).eq.deux) then
!
        e1 = mater(1,1)*((i1d -piso)/pref)**n
        e2 = mater(2,1)*((i1d -piso)/pref)**n
        e3 = mater(3,1)*((i1d -piso)/pref)**n
        nu12 = mater(4,1)
        nu13 = mater(5,1)
        nu23 = mater(6,1)
        nu21 = mater(13,1)
        nu31 = mater(14,1)
        nu32 = mater(15,1)
        delta= mater(16,1)
!
        c11 = (un - nu23*nu32)*e1/delta
        c12 = (nu21 + nu31*nu23)*e1/delta
        c13 = (nu31 + nu21*nu32)*e1/delta
        c22 = (un - nu13*nu31)*e2/delta
        c23 = (nu32 + nu31*nu12)*e2/delta
        c33 = (un - nu21*nu12)*e3/delta
!
        di1d = (c11+c12+c13)*deps(1) + (c12+c22+c23)*deps(2) + (c13+ c23+c33)*deps(3)
        di1d = d13*di1d
!
    else
        call utmess('F', 'COMPOR1_35')
    endif
!
    if ((i1d/pref) .gt. tol) then
        ri = di1d /i1d
    else if ((-piso/pref) .gt. tol) then
        ri = di1d /(-piso)
    else
        ri = zero
        write(6,'(A)')'HUJDP :: DP/P NON CONTROLE CAR P VOISIN DE ZERO'
    endif
!
    if (ri .gt. un) then
        ri = un
    else if (ri.lt.tole1) then
        ri = tole1
    endif
    ndec = nint(ri/tole1)
!
!
! ----------------------------------------------------------------
! 2 --- CRITERE A RESPECTER POUR L EVOLUTION DU MECANISME ISOTROPE
!   ---         FR/(DFDS*C*DEPS)*TOLE1 > TETA = 1/RI
! ====================================================================
! -------------------- 2.1 CONSTRUCTION DE C -------------------------
! ====================================================================
    call lcinma(zero, hooknl)
    i1e = d13*(sigf(1)+sigf(2)+sigf(3))
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
            call utmess('F', 'COMPOR1_35')
        endif
!
    else if (mod(1:6) .eq. 'C_PLAN' .or. mod(1:2) .eq. '1D') then
!
        call utmess('F', 'COMPOR1_4')
!
    endif
!
    call lcprmv(hooknl, deps, dsig)
!
!
! ====================================================================
! -------------- 2.2 CALCUL DE FIDSIG = DFDS*DSIG --------------------
! ====================================================================
!
! 1- MECANISME ISOTROPE
    fidsig = zero
    do 40 i = 1, ndi
        fidsig = fidsig - d13*dsig(i)
40  continue
    fr = d*pco*exp(-beta*epsvp)
    ri = abs(fidsig/fr)
!
    if (ri .gt. un) then
        ri = un
    else if (ri.lt.tole1) then
        ri = tole1
    endif
    ni = nint(ri/tole1)
    if (ndec .lt. ni) ndec = ni
!
! 2- MECANISME DEVIATOIRE
    do 41 i = 1, ndt
        dfds(i) = zero
41  continue
!
    do 42 i = 1, 18
        yf(i) = zero
42  continue
!
    do 43 i = 1, 7
        indi(i) = 0
43  continue
!
    yf(7) = epsvp
    call lceqvn(ndt, sigd, yf)
!
    nbmeca = 0
    do 44 i = 1, 8
        if (vin(23+i) .eq. un) then
            nbmeca = nbmeca + 1
            indi(nbmeca) = i
            yf(ndt+1+nbmeca) = vin(i)
        else if ((vin(23+i).eq.zero).and.(i.lt.5)) then
            nbmeca = nbmeca + 1
            indi(nbmeca) = i
            yf(ndt+1+nbmeca) = vin(i)
        endif
44  end do
!
    do 47 i = 1, 3
!
        call hujprj(i, sigd, taud, pd, qd)
!
        if ((vin(23+i).eq.un) .or. (vin(23+i).eq.zero)) then
            call hujddd('DFDS  ', i, mater, indi, yf,&
                        vin, dfds, mat, iret)
            fr = -m*(pd-ptrac)*(un-b*log((pd-ptrac)/ (pco*exp(-beta* epsvp))))*vin(i)
!
        else if (vin(27+i).eq.un) then
            call hujddd('DFDS  ', i+4, mater, indi, yf,&
                        vin, dfds, mat, iret)
            fr = -m*(pd-ptrac)*(un-b*log((pd-ptrac)/ (pco*exp(-beta* epsvp))))*vin(i+4)
!
        endif
!
        fidsig = 0
        do 46 j = 1, ndt
            fidsig = fidsig + dfds(j)*dsig(j)
46      continue
!
        ri = abs(fidsig/fr)
        if (ri .gt. un) then
            ri = un
        else if (ri.lt.tole1) then
            ri = tole1
        endif
        ni = nint(ri/tole1)
        if (ndec .lt. ni) ndec = ni
!
47  continue
!
! -------------------------------------------------------
! 3 --- CRITERE LIMITANT L EVOLUTION DE Q: DQ/PREF < TOLE1
! -------------------------------------------------------
    do 45 i = 1, 3
!
! 1- CAS DEVIATOIRE CYCLIQUE
        if (vin(27+i) .eq. un) then
! --- INITIALISATION DES VARIABLES NECESSAIRES A PROD/Q(K)
! --- AVEC PROD = PRODUIT SCALAIRE (SIGDC*TH)
            call lceqvn(ndt, sigf, yf)
            yf(8) = vin(4+i)
            call hujprc(1, i, sigf, vin, mater,&
                        yf, pf, qf, sigdc)
            prodf = sigdc(1)*vin(4*i+7)+sigdc(3)*vin(4*i+8)/deux
            prodf = qf+prodf
            call lceqvn(ndt, sigd, yf)
            call hujprc(1, i, sigd, vin, mater,&
                        yf, pd, qd, sigdc)
            prodd = sigdc(1)*vin(4*i+7)+sigdc(3)*vin(4*i+8)/deux
            prodd = qd+prodd
!
            if ((qd.lt.tol) .or. (prodf.lt.tol)) then
                ri = zero
            else
                ri = abs(un-qf*prodd/qd/prodf)
            endif
            if (ri .gt. un) then
                ri = un
            else if (ri.lt.tole1) then
                ri = tole1
            endif
            ni = nint(ri/tole1)
            if (ndec .lt. ni) ndec = ni
!
! 2- CAS DEVIATOIRE MONOTONE
        else
            call hujprj(i, sigd, taud, pd, qd)
            call hujprj(i, sigf, tauf, pf, qf)
            fr = m*(pd -ptrac)* (un-b*log((pd-ptrac)/pco*exp(-beta* epsvp)))*vin(i)
!
            if (abs(fr/pref) .lt. tol) then
                ri =zero
            else
                ri =abs((qf-qd)/fr)
            endif
!
            rela1 = zero
            rela2 = zero
            if (abs(fr/pref) .gt. tol) then
                rela1 = abs((tauf(1)-taud(1))/fr)
                rela2 = abs((tauf(3)-taud(3))/fr)
            endif
            if (rela1 .gt. ri) ri = rela1
            if (rela2 .gt. ri) ri = rela2
!
            if (ri .gt. un) then
                ri = un
            else if (ri.lt.tole1) then
                ri = tole1
            endif
            ni = nint(ri/tole1)
            if (ndec .lt. ni) ndec = ni
        endif
!
45  end do
!
!KH ----
!KH ON CONTROLE L'INCREMENT DE CONTRAINTE: DS/S- <= 10%
!KH
    prodd = 0.d0
    prodf = 0.d0
!
    do 451 i = 1, ndt
        prodd = prodd + sigd(i)**deux
        prodf = prodf + (sigf(i)-sigd(i))**deux
451  end do
    prodd = sqrt(prodd)
    prodf = sqrt(prodf)
!
!      TOLE1 = 0.1
    if ((-prodd/pref) .gt. tol) then
        ri = prodf/prodd/tole1
        if (ri .gt. 1.d0) then
            ni = int(ri)
            if (ndec .lt. ni) ndec = ni
!            WRITE (6,'(A,I6)') 'HUJDP :: NSUBD =',NI
        endif
    endif
!      IF (NDEC.GT.1) WRITE (6,'(A,I4)') ' NDEC =',NDEC
!
500  continue
end subroutine
