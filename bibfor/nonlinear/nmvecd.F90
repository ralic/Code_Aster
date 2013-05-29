subroutine nmvecd(imate, mate, nmat, matcst, loi,&
                  hook, dt, tp, p, np,&
                  beta, nb, ep, rm, dm,&
                  dsgde, dsgdb, dsgdp, drbde, drpde,&
                  rb, rp, drbdb, drbdp, drpdb,&
                  drpdp, etatf, ier)
!-----------------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! TOLE CRP_21
!-----------------------------------------------------------------------
    implicit none
!
    include 'asterc/r8miem.h'
    include 'asterfort/assert.h'
    include 'asterfort/lcdvmi.h'
    include 'asterfort/lceqvn.h'
    include 'asterfort/nmvekx.h'
    include 'asterfort/nmvexi.h'
    include 'asterfort/r8inir.h'
    integer :: imate, nmat, np, nb, ier
    real(kind=8) :: mate(nmat, 2), hook(6, 6)
    real(kind=8) :: p(np), beta(nb), ep(*), rm, dm
    real(kind=8) :: dsgde(nb, nb), dsgdb(nb, nb)
    real(kind=8) :: dsgdp(nb, np), rb(nb), rp(np), drbdb(nb, nb)
    real(kind=8) :: drbdp(nb, np), drpdb(np, nb), drpdp(np, np)
    real(kind=8) :: dt, tp, drbde(nb, nb), drpde(np, nb)
    character(len=3) :: matcst
    character(len=7) :: etatf(3)
    character(len=16) :: loi
!-----------------------------------------------------------------------
!     INTEGRATION DE LA LOI DE COMPORTEMENT VISCO PLASTIQUE DE
!     CHABOCHE AVEC ENDOMAGEMENT
!     METHODE ITERATIVE D'EULER IMPLICITE
!
!     EQUATIONS ET DERIVEES DES RESIDUS: RB ET RP
!-----------------------------------------------------------------------
!-- ARGUMENTS
!------------
!
! IN   MATE    : PARAMETRE MATERIAU A L'INSTANT T
!      IMATE   : ADRESSE DU MATERIAU CODE
!      NMAT    : DIMENSION DE MATE
!      MATCST  : 'OUI' SI MATERIAU CST ENTRE T- ET T
!                'NAP' SI LE PARAMETRE K_D EST UNE NAPPE
!                'NON' SINON
!      HOOK    : OPERATEUR DE HOOK
!      DT      : INCREMENT DE TEMPS
!      P       : INCONNUES ASSOCIEES AUX VARIABLES D'ETAT
!      NP      : NOMBRE D'INCONNUES ASSOCIEES AUX VARIABLES D'ETAT
!      BETA    : INCONNUES ASSOCIEES AUX CONTRAINTES
!      NB      : NOMBRE D'INCONNUES ASSOCIEES AUX CONTRAINTES
!      RM      : VARIABLES INTERNES A T-
!      DM      : VARIABLES INTERNES A T-
!      EP      : DEFORMATIONS TOTALES ET THERMIQUE A T ET
!                VISCOPLASTIQUE A T-
! OUT  DSGDE   : DERIVEES DE LA FONCTION ASSOCIEE PAR E
!      DSGDB   : DERIVEES DE LA FONCTION ASSOCIEE PAR BETA
!      DSGDP   : DERIVEES DE LA FONCTION ASSOCIEE PAR P
!      RB      : RESIDU ASSOCIEE AU VECTEUR CONTRAINTES
!      RP      : RESIDU ASSOCIEE AUX INCONNUS
!      DRBDB   : DERIVEES DE LA FONCTION ASSOCIEE PAR BETA
!      DRBDP   : DERIVEES DE LA FONCTION ASSOCIEE PAR P
!      DRPDB   : DERIVEES DE LA FONCTION ASSOCIEE PAR BETA
!      DRPDP   : DERIVEES DE LA FONCTION ASSOCIEE PAR P
!IN/OUT ETATF   : ETAT MECANIQUE DE L'ELEMENT
!      IER     : CODE DE RETOUR D'ERREUR
!                0=OK
!                1=NOOK
!
! INFO P(1)=RPOINT,  P(2)=DPOINT
!-----------------------------------------------------------------------
    integer :: k, j, i
    real(kind=8) :: dammax, epsi
    parameter  (dammax = 0.99d0)
!
    real(kind=8) :: dkron(6, 6), sc, dscdr, unssc, arg
    real(kind=8) :: ca1, ca0, drpdsc, drpdse, unssem, ec, unmd, ea
    real(kind=8) :: sy, unsm, gk, gr, ga, epsvpm(6), epst(6), epsth(6)
    real(kind=8) :: dsedb(6), dsedb2(6, 6), se, d, r, gn, semsy, crit
    real(kind=8) :: dhede(6, 6), dhedb(6, 6), dhedp(6, 2), xhi, p2, epsvp(6)
    real(kind=8) :: p2dxhi, p2dp2, he(6), dxhidb(6), kxhi, dkxidx
!
    data dkron /1.d0, 0.d0, 0.d0, 0.d0, 0.d0, 0.d0,&
     &            0.d0, 1.d0, 0.d0, 0.d0, 0.d0, 0.d0,&
     &            0.d0, 0.d0, 1.d0, 0.d0, 0.d0, 0.d0,&
     &            0.d0, 0.d0, 0.d0, 1.d0, 0.d0, 0.d0,&
     &            0.d0, 0.d0, 0.d0, 0.d0, 1.d0, 0.d0,&
     &            0.d0, 0.d0, 0.d0, 0.d0, 0.d0, 1.d0/
!-----------------------------------------------------------------------
!-- 1. INITIALISATIONS
!   ===================
!JMP      EPSI   = SQRT( R8PREM() )
!     EPSI   = 1.D-15
    epsi=r8miem()
    ier = 0
    sy = mate(4,2)
    gn = mate(1,2)
    if (gn .le. 0.d0) goto 05001
    unsm = mate(2,2)
    gk = 1.d0/mate(3,2)
    gr = mate(7,2)
    ga = mate(8,2)
    se = 0.d0
    call r8inir(nb, 0.d0, dsedb, 1)
    call r8inir(nb*nb, 0.d0, dsedb2, 1)
    call r8inir(np*np, 0.d0, drpdp, 1)
    call r8inir(np*nb, 0.d0, drpdb, 1)
    call r8inir(nb*nb, 0.d0, drbdb, 1)
    call r8inir(nb*np, 0.d0, drbdp, 1)
    call r8inir(nb, 0.d0, rb, 1)
    call r8inir(np, 0.d0, rp, 1)
    call lceqvn(nb, ep(1), epsth)
    call lceqvn(nb, ep(7), epsvpm)
    call lceqvn(nb, ep(13), epst)
!
!-- 1.1. CRITERE DE VM, DERIVEE PREMIERE ET SECONDE
!   ----------------------------------------------
    call lcdvmi(beta, 0.d0, crit, dsedb, dsedb2,&
                se)
!
!-- 1.2. ETAT COURANT DU SYSTEME
!   ----------------------------
    d = dm+dt*p(2)
    if (d .ge. dammax) then
        p(2) = 0.d0
        d = dammax
        etatf(3)='DAMMAXO'
    endif
    if (d .lt. 0.d0) then
        ier = 10
        goto 9999
    endif
    unmd = 1.d0-d
    r = rm+dt*p(1)
    do 121 i = 1, nb
        epsvp(i)=epsvpm(i)+dt*p(1)/unmd*dsedb(i)
121  end do
!
!-- 1.3. CONTRAINTES ET DERIVEES
!   ----------------------------
    do 131 i = 1, nb
        he(i)=0.d0
        do 131 k = 1, nb
            he(i)=he(i)+unmd*hook(i,k)*(epst(k)-epsth(k)-epsvp(k))
131      continue
!
    do 132 i = 1, nb
        do 1321 j = 1, nb
            dhede(i,j)=0.d0
            dhede(i,j)=dhede(i,j)+unmd*hook(i,j)
1321      continue
!
        do 1322 j = 1, nb
            dhedb(i,j)=0.d0
            do 1322 k = 1, nb
                dhedb(i,j)=dhedb(i,j)-hook(i,k)*dt*p(1)*dsedb2(k,j)
1322          continue
!
        dhedp(i,1)=0.d0
        do 1323 k = 1, nb
            dhedp(i,1)=dhedp(i,1)-hook(i,k)*dt*dsedb(k)
1323      continue
!
        dhedp(i,2)=0.d0
        do 1324 k = 1, nb
            dhedp(i,2)=dhedp(i,2)-dt*hook(i,k)*(epst(k)-epsth(k)-&
            epsvp(k))
1324      continue
!
        dsgdp(i,1)= 0.d0
        dsgdp(i,2)= 0.d0
        do 1325 j = 1, nb
            dsgde(i,j)= 0.d0
            dsgdb(i,j)= dkron(i,j)
1325      continue
!
132  end do
!
!-- 2. EQUATIONS EN BETA ET DERIVEES
!   ================================
    do 21 i = 1, nb
        rb(i)=beta(i)-he(i)
        do 211 j = 1, nb
            drbde(i,j)=-dhede(i,j)
            drbdb(i,j)=-dhedb(i,j)+dkron(i,j)
211      continue
        drbdp(i,1)=-dhedp(i,1)
        drbdp(i,2)=-dhedp(i,2)
21  end do
!
!-- 3. EQUATIONS EN P(1) ET DERIVEES
!   ================================
!--  3.1. SI ELASTIQUE
!    -----------------
    if (etatf(1) .eq. 'ELASTIC' .and. se .le. (sy*unmd)) then
        rp(1)=p(1)-0.d0
        do 311 j = 1, 6
            drpde(1,j)=0.d0
            drpdb(1,j)=0.d0
311      continue
        drpdp(1,1)=1.d0
        drpdp(1,2)=0.d0
!
!--  3.2. SI PLASTIQUE
!    -----------------
    else
        semsy = se/unmd-sy
        if (r .le. epsi) then
            sc = gk*epsi**unsm
            dscdr = 0.d0
        else
            sc = gk*exp(log(r)*unsm)
            dscdr = sc/r*unsm*dt
        endif
        if (r .le. 1.d-5 .and. p(1) .le. 0.d0) then
            gn = gn/(1.d0+gn*unsm)
            sc = gk*dt**unsm
        endif
        if (sc .le. epsi) then
            unssc = 1.d0/epsi
        else
            unssc = 1.d0/sc
        endif
        if (semsy .le. epsi) then
            unssem = 1.d0/epsi
        else
            unssem = 1.d0/semsy
        endif
        arg = gn*(log(semsy)-log(sc))
        if (arg .gt. log(0.1d0/dt)) then
            ca1 = -gn*(0.1d0/dt)**((gn-1.d0)/gn)
            ca0 = (gn-1.d0)*(0.1d0/dt)
            rp(1)= p(1)+ ca1*semsy*unssc + ca0
            drpdsc = -ca1*semsy*unssc*unssc
            drpdse = ca1*unssc
            etatf(2)='TANGENT'
        else
            rp(1)= p(1) - exp(arg)
            drpdsc = exp(arg)*gn*unssc
            drpdse = - gn*unssem*exp(arg)
            etatf(2)='EXPONEN'
        endif
        do 321 j = 1, nb
            drpde(1,j)= 0.d0
            drpdb(1,j)= drpdse/unmd*dsedb(j)
321      continue
        drpdp(1,1)= 1.d0 + drpdsc*dscdr
        drpdp(1,2)= drpdse*se*dt/unmd/unmd
        if (etatf(1) .eq. 'ELASTIC') etatf(1)='PLASTIC'
    endif
!
!-- 4. EQUATIONS EN P(2) ET DERIVEES
!   ================================
    if (loi .eq. 'VENDOCHAB') then
        call nmvexi(beta, se, dsedb, nb, mate,&
                    nmat, xhi, dxhidb)
        kxhi = mate(9,2)
    else if (loi.eq.'VISC_ENDO_LEMA') then
        kxhi = 0.d0
        xhi = 0.d0
    else
        call assert(.false.)
    endif
!
    dkxidx = 0.d0
    if (xhi .le. epsi .or. d .eq. dammax) then
        p2 =0.d0
        p2dp2 =0.d0
        p2dxhi=0.d0
    else
        if (matcst .eq. 'NAP') then
            call nmvekx(imate, tp, xhi, kxhi, dkxidx)
            mate(9,2) = kxhi
        endif
!
        ea = -kxhi*log(unmd)
        ec = gr*log(xhi/ga)
        p2 = exp(ec)*exp(ea)
        rp(2) = p(2)-p2
        p2dp2 = p2*kxhi*dt/(1.d0-d)
        p2dxhi = p2*(gr/xhi-log(1.d0-d)*dkxidx)
    endif
!
    do 422 j = 1, nb
        drpde(2,j)=0.d0
        drpdb(2,j)=-p2dxhi*dxhidb(j)
422  end do
    drpdp(2,1)=0.d0
    drpdp(2,2)=1.d0-p2dp2
!
    goto 09999
!
!-- 5. ERREURS
!   ----------
5001  continue
    ier = 11
!
9999  continue
end subroutine
