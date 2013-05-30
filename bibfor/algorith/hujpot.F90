subroutine hujpot(mod, mater, vind, depsh, sigd,&
                  sige, etatf, rdctps, iret, aredec)
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
!   ------------------------------------------------------------------
!   DEFINITION DU DOMAINE POTENTIEL DES MECANISMES ACTIFS
!   IN  MOD    :  MODELISATION
!       MATER  :  COEFFICIENTS MATERIAU A T+DT
!       VIND   :  VARIABLES INTERNES  A T
!       DEPS   :  INCREMENT DE DEFORMATION
!       SIGD   :  CONTRAINTE A T
!       SIGE   :  CONTRAINTE A T+DT  (ELAS)
!
!   OUT VIND   :  VARIABLES INTERNES MODIFIEES PAR LES NOUVEAUX
!                 MECANISMES
!       ETATF  :  ETAT PLASTIQUE OU ELASTIQUE DU POINT CONSIDERE
!       RDCTPS :  REDECOUPAGE DU PAS DE TEMPS SI NECESSAIRE
!       AREDEC :  DECOUPAGE LOCAL ACTIF = .TRUE.
!       IRET   :  CODE RETOUR DE  L'INTEGRATION DE LA LOI DE HUJEUX
!                    IRET=0 => PAS DE PROBLEME
!                    IRET=1 => ECHEC
!   ------------------------------------------------------------------
    include 'asterc/r8prem.h'
    include 'asterfort/hujcdc.h'
    include 'asterfort/hujcic.h'
    include 'asterfort/hujcrd.h'
    include 'asterfort/hujcri.h'
    include 'asterfort/hujddd.h'
    include 'asterfort/hujmed.h'
    include 'asterfort/hujmei.h'
    include 'asterfort/hujpxd.h'
    include 'asterfort/hujpxs.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcprmv.h'
    include 'asterfort/u2mess.h'
    integer :: ndt, ndi, elas, jj, iret
    integer :: i, indi(7), j, mono(7), hist(4, 2)
    real(kind=8) :: tole1, sigd(6), sige(6)
    real(kind=8) :: vind(*), charge
    real(kind=8) :: mater(22, 2), un, zero, pref
    real(kind=8) :: al, la, demu, e, nu, i1e, ye(18), yd(18)
    real(kind=8) :: hooknl(6, 6), dfds(6), depsh(6), dsig(6)
    real(kind=8) :: actif, dpsids(6, 6), n, deux, seuil
    real(kind=8) :: deps(6)
    real(kind=8) :: e1, e2, e3, nu12, nu13, nu23, g1, g2, g3, nu21, nu31, nu32
    real(kind=8) :: delta
    real(kind=8) :: piso
    logical :: debug, prox, rdctps, aredec, bid
    character(len=7) :: etatf
    character(len=8) :: mod
    real(kind=8) :: vinm(50), seuilm
!
! ----------------------------------------------------------------------
    common /tdim/   ndt, ndi
    common /meshuj/ debug
! ----------------------------------------------------------------------
    parameter   (tole1 = 1.d-7)
    parameter   (un   = 1.d0)
    parameter   (zero = 0.d0)
    parameter   (deux = 2.d0)
!
! ======================================================================
! -------------------- DETERMINATION DES CRITERES ACTIFS A T ----------
! ======================================================================
    if (debug) write(6,'(A)')' ==> HUJPOT'
    if (debug) write(6,*)'     INIT - VIND=',(vind(23+i),i=1,8)
! --- MISE A ZERO POUR CORRIGER ZERO NUMERIQUE
    do 10 i = 1, ndt
        deps(i) = depsh(i)
        if (abs(deps(i)) .lt. r8prem()) deps(i)=zero
10  continue
    elas = 0
    rdctps = .false.
!
! ====================================================================
! --- CONSTRUCTION DES SURFACES CYCLIQUES PRECEDENTES -----------
! ====================================================================
    call lceqvn(50, vind, vinm)
    do 50 i = 1, 3
        if ((vind(5*i+31).ne.zero) .or. (vind(5*i+32).ne.zero)) then
            vinm(4*i+5) = vind(5*i+31)
            vinm(4*i+6) = vind(5*i+32)
            vinm(4*i+7) = vind(5*i+33)
            vinm(4*i+8) = vind(5*i+34)
            vinm(i+4) = vind(5*i+35)
        endif
50  continue
!
! ====================================================================
! --- PROPRIETES MATERIAU HUJEUX -------------------------------------
! ====================================================================
    n = mater(1,2)
    pref = mater(8,2)
    piso = 1.5d0*mater(21,2)
    piso = zero
!
! ====================================================================
! ------------------ INITIALISATION VARIABLES ------------------------
! ====================================================================
!
    do 14 i = 1, 7
        mono(i) = 0
        indi(i) = 0
14  continue
    do 15 i = 1, 4
        hist(i,1) = 0
        hist(i,2) = 0
        mono(i) = i
15  continue
    do 25 i = 1, ndt
        ye(i) = sige(i)
        yd(i) = sigd(i)
25  continue
    ye(ndt+1) = vind(23)
    yd(ndt+1) = vind(23)
!
! ====================================================================
! --------------------- I) CONSTRUCTION DE C -------------------------
! ====================================================================
    call lcinma(zero, hooknl)
    i1e = (sige(1)+sige(2)+sige(3))/3.d0
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
            call u2mess('F', 'COMPOR1_37')
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
! -------------- CALCUL DE DSIGMA = C*DEPSILON -----------------------
! ====================================================================
    call lcprmv(hooknl, deps, dsig)
!
!
! ====================================================================
! ----------- DETERMINATION DES CRITERES ACTIFS PRECEDEMMENT ---------
! ====================================================================
    j = 0
    do 400 i = 1, 4
!
        if ((vind(23+i).eq.un) .or. (vind(23+i).eq.zero)) then
            ye(ndt+1+i) = vind(i)
            yd(ndt+1+i) = vind(i)
            hist(i,1) = i
            indi(i) = i
            if (vind(23+i) .eq. un) then
                hist(i,2) = 1
            else
                hist(i,2) = 0
            endif
        else
            ye(ndt+1+i) = vind(i+4)
            yd(ndt+1+i) = vind(i+4)
            hist(i,1) = i + 4
            indi(i) = i + 4
            if (vind(27+i) .eq. un) then
                hist(i,2) = 1
            else
                hist(i,2) = 0
            endif
        endif
!
!
! ====================================================================
! --------------------- CALCUL DE DFDS*C(SIGE)*DEPS ------------------
! ====================================================================
        call hujddd('DFDS  ', indi(i), mater, indi, ye,&
                    vind, dfds, dpsids, iret)
        if (iret .eq. 1) then
            iret = 0
            if (.not.aredec) then
                rdctps = .true.
                goto 999
            else
                call hujddd('DFDS  ', indi(i), mater, indi, yd,&
                            vind, dfds, dpsids, iret)
                if (iret .eq. 1) goto 999
            endif
        endif
        actif = zero
        do 45 jj = 1, ndt
            actif = actif + dsig(jj)*dfds(jj)
45      continue
!
        actif = actif/mater(1,1)
        charge = -un
        if (indi(i) .gt. 4) then
            ye(ndt+1+i) = vind(i)
            yd(ndt+1+i) = vind(i)
            call hujddd('DFDS  ', mono(i), mater, mono, ye,&
                        vind, dfds, dpsids, iret)
!
            if (iret .eq. 1) then
                iret = 0
                if (.not.aredec) then
                    rdctps = .true.
                    goto 999
                else
                    call hujddd('DFDS  ', mono(i), mater, mono, yd,&
                                vind, dfds, dpsids, iret)
                    if (iret .eq. 1) goto 999
                endif
            endif
!
            charge = zero
            do 46 jj = 1, ndt
                charge = charge + dsig(jj)*dfds(jj)
46          continue
            charge = charge/mater(1,1)
        endif
!
        if (debug) then
            write(6,*)'INDI   = ',indi(i)
            write(6,*)'ACTIF  = ',actif
            write(6,*)'CHARGE = ',charge
            write(6,*)
        endif
!
! ====================================================================
! --------------------- CRITERE MONOTONE ACTIF ? ---------------------
! ====================================================================
        if (charge .ge. (-r8prem())) then
            if (indi(i) .ne. 8) then
                call hujpxd(indi(i), mater, sigd, vind, prox,&
                            bid)
                if (prox) then
                    vind(19+indi(i)) = un
                    vind(23+indi(i)) = zero
                    vind(indi(i)*4-11) = zero
                    vind(indi(i)*4-10) = zero
                    vind(indi(i)*4-9) = zero
                    vind(indi(i)*4-8) = zero
                    vind(indi(i)) = mater(18,2)
                    goto 400
!              ELSEIF(.NOT.AREDEC)THEN
! --> SINON ==> REDECOUPAGE DU PAS DE TEMPS
!                RDCTPS = .TRUE.
!                GOTO 999
                endif
            else
                call hujpxs(mater, sigd, vind, prox)
                if (prox) then
                    vind(21) = zero
                    vind(22) = zero
                    vind(27) = un
                    vind(31) = zero
                    goto 400
!              ELSEIF(.NOT.AREDEC)THEN
! --> SINON ==> REDECOUPAGE DU PAS DE TEMPS
!                RDCTPS = .TRUE.
!                GOTO 999
                endif
            endif
        endif
!
!
! =====================================================================
! -------------------------- CRITERE ACTIF ----------------------------
! =====================================================================
        if (indi(i) .lt. 5) then
!
! **************************
! --- CRITERES MONOTONES ---
! **************************
            if (hist(i,2) .eq. 1) then
                if (actif .ge. (-r8prem())) then
                    vind(23+indi(i)) = un
                else
                    vind(23+indi(i)) = -un
                    if (indi(i) .lt. 4) then
                        call hujmed(indi(i), mater, vind, sigd)
                        vind(i+4) = mater(18,2)
                        call hujcdc(indi(i), mater, sige, vind, seuil)
                        if (vind(i+4) .eq. un) seuil = - un
                    else if (indi(i).eq.4) then
                        call hujmei(vind)
                        vind(8) = mater(19,2)
                        call hujcic(mater, sige, vind, seuil)
                    endif
                    if (seuil .gt. tole1) then
                        vind(27+indi(i)) = un
                    else
                        vind(27+indi(i)) = zero
                        elas = elas + 1
                    endif
                endif
            else
                if (actif .ge. (-r8prem())) then
                    if (indi(i) .lt. 4) then
                        call hujcrd(i, mater, sige, vind, seuil)
                    else
                        call hujcri(mater, sige, vind, seuil)
                    endif
                    if (seuil .gt. tole1) then
                        vind(23+indi(i)) = un
                    else
                        vind(23+indi(i)) = zero
                        elas = elas + 1
                    endif
                else
                    vind(23+indi(i)) = zero
                    elas = elas + 1
                endif
            endif
        else
!
!
! **************************
! --- CRITERES CYCLIQUES ---
! **************************
            if (hist(i,2) .eq. 1) then
                if (actif .ge. (-r8prem())) then
                    if (vind(i+4) .lt. un) then
                        vind(23+indi(i)) = un
                    else
                        vind(23+indi(i)) = zero
                    endif
                else
                    if (indi(i) .lt. 8) then
                        call hujmed(indi(i), mater, vind, sigd)
                        vind(i+4) = mater(18,2)
                        call hujcdc(indi(i)-4, mater, sige, vind, seuil)
                        if (vind(i+4) .eq. un) seuil = -un
                    else
                        call hujmei(vind)
                        vind(8) = mater(19,2)
                        call hujcic(mater, sige, vind, seuil)
                    endif
                    if (seuil .gt. tole1) then
                        vind(23+indi(i)) = un
                    else
                        vind(23+indi(i)) = zero
                        elas = elas + 1
                    endif
                endif
            else
                if (actif .ge. (-r8prem())) then
                    if ((indi(i).gt.4) .and. (indi(i).lt.8)) then
                        call hujcdc(indi(i)-4, mater, sige, vind, seuil)
                        if (vind(i+4) .eq. un) seuil = -un
                        if (seuil .gt. tole1) then
                            vind(23+indi(i)) = un
                            if ((vind(5*i+31).ne.zero) .or. (vind(5*i+ 32).ne.zero)) then
                                call hujcdc(indi(i)-4, mater, sige, vinm, seuilm)
                                if (seuilm .gt. tole1) then
                                    vind(4*i+5) = vind(5*i+31)
                                    vind(4*i+6) = vind(5*i+32)
                                    vind(4*i+7) = vind(5*i+33)
                                    vind(4*i+8) = vind(5*i+34)
                                    vind(i+4) = vind(5*i+35)
                                    vind(5*i+31) = zero
                                    vind(5*i+32) = zero
                                    vind(5*i+33) = zero
                                    vind(5*i+34) = zero
                                    vind(5*i+35) = mater(18,2)
                                endif
                            endif
                        else
                            vind(23+indi(i)) = zero
                            elas = elas + 1
                        endif
                    else
                        call hujcic(mater, sige, vind, seuil)
                        if (seuil .gt. tole1) then
                            vind(23+indi(i)) = un
                        else
                            vind(23+indi(i)) = zero
                            elas = elas + 1
                        endif
                    endif
                else
                    seuil = zero
                    if (indi(i) .lt. 8) then
                        if (vind(i+4) .ne. mater(18,2)) then
                            call hujmed(indi(i), mater, vind, sigd)
                            vind(i+4) = mater(18,2)
                            call hujcdc(indi(i)-4, mater, sige, vind, seuil)
                        endif
                    else
                        call hujmei(vind)
                        vind(8) = mater(19,2)
                        call hujcic(mater, sige, vind, seuil)
                    endif
                    if (seuil .gt. tole1) then
                        vind(23+indi(i)) = un
                    else
                        vind(23+indi(i)) = zero
                        elas = elas + 1
                    endif
                endif
            endif
        endif
!
400  continue
!
!
! ======================================================================
! ---------------- DETERMINATION ETAT ELASTIQUE OU PLASTIQUE -----------
! ======================================================================
    if (elas .eq. 4) then
        etatf = 'ELASTIC'
    else
        etatf = 'PLASTIC'
    endif
!
999  continue
    if (debug) write(6,*)'FIN - VIND=',(vind(23+i),i=1,8)
end subroutine
