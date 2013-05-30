subroutine cjspla(mod, crit, mater, seuili, seuild,&
                  nvi, epsd, deps, sigd, vind,&
                  sigf, vinf, mecani, nivcjs, niter,&
                  ndec, epscon, iret, trac)
    implicit none
!       ================================================================
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
!       ----------------------------------------------------------------
!       INTEGRATION PLASTIQUE DE LA LOI CJS
!       IN  MOD    :  MODELISATION
!           CRIT   :  CRITERES DE CONVERGENCE
!           MATER  :  COEFFICIENTS MATERIAU A T+DT
!           SEUILI :  FONCTION DE CHARGE ISO. CALCULEE AVEC PREDICT ELAS
!           SEUILD :  FONCTION DE CHARGE DEV. CALCULEE AVEC PREDICT ELAS
!           NVI    :  NOMBRE DE VARIABLES INTERNES
!           EPSD   :  DEFORMATIONS A T
!           DEPS   :  INCREMENT DE DEFORMATION
!           SIGD   :  CONTRAINTE  A T
!           VIND   :  VARIABLES INTERNES  A T
!       VAR SIGF   :  CONTRAINTE A T+DT  (IN -> ELAS, OUT -> PLASTI )
!       OUT VINF   :  VARIABLES INTERNES A T+DT
!           MECANI :  MECANISME(S) ACTIVE(S)
!           NITER  :  NOMBRE D ITERATIONS POUR PLASTICITE
!                          (CUMUL DECOUPAGE)
!           NDEC   :  NOMBRE DE DECOUPAGE
!           EPSCON :  EPSILON A CONVERGENCE
!           IRET   :  CODE RETOUR DE  L'INTEGRATION DE LA LOI CJS
!                              IRET=0 => PAS DE PROBLEME
!                              IRET=1 => ECHEC
!       ----------------------------------------------------------------
!
    include 'asterfort/assert.h'
    include 'asterfort/cjsmde.h'
    include 'asterfort/cjsmid.h'
    include 'asterfort/cjsmis.h'
    include 'asterfort/cjssmd.h'
    include 'asterfort/cjssmi.h'
    include 'asterfort/lceqve.h'
    include 'asterfort/lceqvn.h'
    integer :: ndt, ndi, nvi, niter, ndec, iret
    integer :: nvimax
    parameter(nvimax=16)
    real(kind=8) :: epsd(6), deps(6)
    real(kind=8) :: sigd(6), sigf(6), predic(6)
    real(kind=8) :: sigd0(6), deps0(6), predi0(6)
    real(kind=8) :: vind(*), vinf(*), vind0(nvimax), epscon
    real(kind=8) :: mater(14, 2), crit(*)
    real(kind=8) :: i1f
    real(kind=8) :: seuili, seuild, pa, pref, qinit
    real(kind=8) :: zero
    logical :: chgmec, noconv, aredec, stopnc, trac
    character(len=6) :: mecani
    character(len=4) :: nivcjs
!
    character(len=8) :: mod
    parameter     ( zero = 0.d0   )
    integer :: idec
    integer :: i, niter0
!
    common /tdim/   ndt, ndi
!
!
    call assert(nvi.le.nvimax)
!
    pa = mater(12,2)
    qinit = mater(13,2)
    trac = .false.
!
!  SAUVEGARDE DES GRANDEURS D ENTREE INITIALES
    call lceqve(sigf, predi0)
    call lceqve(sigd, sigd0)
    call lceqve(deps, deps0)
    call lceqvn(nvi, vind, vind0)
!
!
!  ARRET OU NON EN NON CONVERGENCE INTERNE
!  -------------------------------------------
!
!
!
    if (int(crit(1)) .lt. 0) then
        stopnc = .true.
    else
        stopnc = .false.
    endif
!
!
!  INITIALISATION DES VARIABLES DE REDECOUPAGE
!  -------------------------------------------
!
! INT(CRIT(5)) = 0  1 OU -1 -> PAS DE REDECOUPAGE DU PAS DE TEMPS
!
!
    if ((int(crit(5)) .eq. 0) .or. (int(crit(5)) .eq. -1) .or. (int(crit(5)) .eq. 1)) then
        ndec = 1
        aredec = .true.
        noconv = .false.
!
!
! INT(CRIT(5)) < -1 -> REDECOUPAGE DU PAS DE TEMPS SI NON CONVERGENCE
!
    else if (int(crit(5)) .lt. -1) then
        ndec = 1
        aredec = .false.
        noconv = .false.
!
!
! INT(CRIT(5)) > 1 -> REDECOUPAGE IMPOSE DU PAS DE TEMPS
!
    else if (int(crit(5)) .gt. 1) then
        ndec = int(crit(5))
        aredec = .true.
        noconv = .false.
    endif
!
!
!
!  POINT DE RETOUR EN CAS DE DECOUPAGE
!  APRES UNE NON CONVERGENCE, POUR  INT(CRIT(5)) < -1
!
500  continue
    if (noconv) then
        ndec = - int(crit(5))
        aredec=.true.
    endif
!
!
!   RESTAURATION DE SIGD VIND DEPS ET PREDIC ELAS SIGF
!   EN TENANT COMPTE DU DECOUPAGE EVENTUEL
!
    call lceqve(sigd0, sigd)
    call lceqvn(nvi, vind0, vind)
!
    do 10 i = 1, ndt
        deps(i) = deps0(i)/ndec
        sigf(i) = sigd0(i)+(predi0(i)-sigd(i))/ndec
10  continue
!
!
!  BOUCLE SUR LES DECOUPAGES
!  -------------------------
    do 400 idec = 1, ndec
!
!
! SAUVEGARDE PREDIC ELASTIQUE POUR EVENTUEL CHANGEMENT
! DE MECANISME
!
        call lceqve(sigf, predic)
!
        i1f = zero
        do 20 i = 1, ndi
            i1f = i1f + sigf(i)
20      continue
!
        if ((i1f+qinit) .eq. 0.d0) then
            i1f = -qinit+1.d-12 * pa
            pref = abs(pa)
        else
            pref = abs(i1f+qinit)
        endif
!
        call cjssmi(mater, sigf, vind, seuili)
        call cjssmd(mater, sigf, vind, seuild)
        seuili = seuili/pref
        seuild = seuild/pref
        chgmec=.false.
        if (seuili .gt. zero .and. seuild .le. zero) then
            mecani='ISOTRO'
        endif
        if (seuili .le. zero .and. seuild .gt. zero) then
            mecani='DEVIAT'
        endif
        if (seuili .gt. zero .and. seuild .gt. zero) then
            mecani='ISODEV'
        endif
!
!
        do 21 i = 1, nvi-1
            vinf(i) = vind(i)
21      continue
!
!
100      continue
!
!--->   RESOLUTION EN FONCTION DES MECANISMES ACTIVES
!
!       MECANISME ISOTROPE SEUL
!       -----------------------
!
        if (mecani .eq. 'ISOTRO') then
            call cjsmis(mod, crit, mater, nvi, epsd,&
                        deps, sigd, sigf, vind, vinf,&
                        noconv, aredec, stopnc, niter0, epscon)
            niter = niter + niter0
            if (noconv .and. (.not.aredec)) goto 500
            if (noconv) then
                iret=1
                goto 9999
            endif
        endif
!
!
!       MECANISME DEVIATOIRE SEUL
!       -------------------------
!
        if (mecani .eq. 'DEVIAT') then
            call cjsmde(mod, crit, mater, nvi, epsd,&
                        deps, sigd, sigf, vind, vinf,&
                        noconv, aredec, stopnc, niter0, epscon,&
                        trac)
            niter = niter + niter0
            if (trac) goto 9999
!
            if (noconv .and. (.not.aredec)) goto 500
            if (noconv) then
                iret=1
                goto 9999
            endif
        endif
!
!       MECANISMES ISOTROPE ET DEVIATOIRE
!       ---------------------------------
!
        if (mecani .eq. 'ISODEV') then
            call cjsmid(mod, crit, mater, nvi, epsd,&
                        deps, sigd, sigf, vind, vinf,&
                        noconv, aredec, stopnc, niter0, epscon)
            niter = niter + niter0
            if (noconv .and. (.not.aredec)) goto 500
            if (noconv) then
                iret=1
                goto 9999
            endif
        endif
!
!
!
!--->   CALCUL DES FONCTIONS DE CHARGES SUR ETAT FINAL
!
        call cjssmi(mater, sigf, vinf, seuili)
        call cjssmd(mater, sigf, vinf, seuild)
!
        i1f = zero
        do 22 i = 1, ndi
            i1f = i1f + sigf(i)
22      continue
        if ((i1f+qinit) .eq. 0.d0) then
            i1f = -qinit + 1.d-12 * pa
            pref = abs(pa)
        else
            pref = abs(i1f+qinit)
        endif
!--->   VERIFICATION DES MECANISMES ACTIVES
!
!
        if ((mecani .eq. 'ISOTRO') .and. (seuild .gt. zero)) then
            mecani='ISODEV'
            chgmec=.true.
        endif
        if ((mecani .eq. 'DEVIAT') .and. (seuili .gt. zero)) then
            mecani='ISODEV'
            chgmec=.true.
        endif
!
!
! - SI ON ACTIVE EN FAIT LES DEUX MECANISMES AU LIEU D'UN SEUL : RETOUR
!   ET SI ON AVAIT CONVERGE
!
        if (chgmec .and. (.not.noconv)) then
            chgmec=.false.
            call lceqve(predic, sigf)
            goto 100
        else
            if (idec .lt. ndec) then
                call lceqve(sigf, sigd)
                do 32 i = 1, nvi-1
                    vind(i) = vinf(i)
32              continue
                do 33 i = 1, ndt
                    sigf(i) = sigd(i)+(predi0(i)-sigd(i))/ndec
33              continue
            endif
        endif
400  continue
!
!
!--->   CALCUL DE LA VARIABLE INTERNE CORRESPONDANT AU MECANISME PLASTIC
!
!
    if (mecani .eq. 'ISOTRO') vinf(nvi) = 1.d0
    if (mecani .eq. 'DEVIAT') vinf(nvi) = 2.d0
    if (mecani .eq. 'ISODEV') vinf(nvi) = 3.d0
!
9999  continue
end subroutine
