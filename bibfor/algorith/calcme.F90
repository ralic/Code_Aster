subroutine calcme(option, compor, thmc, meca, imate,&
                  typmod, crit, instam, instap, tref,&
                  ndim, dimdef, dimcon, nvimec, yate,&
                  addeme, adcome, addete, defgem, congem,&
                  congep, vintm, vintp, addep1, addep2,&
                  dsde, deps, p1, p2, t,&
                  dt, retcom, dp1, dp2, sat,&
                  tbiot, ang2, aniso, phenom)
! aslint: disable=W1504
! ----------------------------------------------------------------------
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
! person_in_charge: sylvie.granet at edf.fr
! **********************************************************************
! ROUTINE CALC_MECA
! CALCULE LES CONTRAINTES GENERALISEES ET LA MATRICE TANGENTE MECANIQUES
! **********************************************************************
!               CRIT    CRITERES  LOCAUX
!                       CRIT(1) = NOMBRE D ITERATIONS MAXI A CONVERGENCE
!                                 (ITER_INTE_MAXI == ITECREL)
!                       CRIT(2) = TYPE DE JACOBIEN A T+DT
!                                 (TYPE_MATR_COMP == MACOMP)
!                                 0 = EN VITESSE     > SYMETRIQUE
!                                 1 = EN INCREMENTAL > NON-SYMETRIQUE
!                       CRIT(3) = VALEUR DE LA TOLERANCE DE CONVERGENCE
!                                 (RESI_INTE_RELA == RESCREL)
!                       CRIT(5) = NOMBRE D'INCREMENTS POUR LE
!                                 REDECOUPAGE LOCAL DU PAS DE TEMPS
!                                 (RESI_INTE_PAS == ITEDEC )
!                                 0 = PAS DE REDECOUPAGE
!                                 N = NOMBRE DE PALIERS
!                OUT RETCOM
! ======================================================================
    implicit none
#include "asterf_types.h"
#include "asterfort/calela.h"
#include "asterfort/dpvplc.h"
#include "asterfort/dsipdp.h"
#include "asterfort/elagon.h"
#include "asterfort/lcdrpr.h"
#include "asterfort/lchbr2.h"
#include "asterfort/lcldsb.h"
#include "asterfort/lcmaza.h"
#include "asterfort/nmbarc.h"
#include "asterfort/nmccam.h"
#include "asterfort/nmcjs.h"
#include "asterfort/rcvalb.h"
#include "asterfort/redece.h"
#include "asterfort/tecael.h"
#include "asterfort/utmess.h"
#include "asterfort/rcvala.h"
#include "asterfort/lcmohr.h"
#include "asterfort/mctgel.h"
#include "asterfort/matini.h"
#include "asterfort/mcbnvi.h"
    aster_logical :: mectru, pre2tr
    integer :: ndim, dimdef, dimcon, nvimec, addeme, addete, addep1
    integer :: addep2, adcome, imate, yate, retcom
    real(kind=8) :: defgem(dimdef), congem(dimcon), congep(dimcon)
    real(kind=8) :: vintm(nvimec), vintp(nvimec)
    real(kind=8) :: dsde(dimcon, dimdef), rac2
    character(len=8) :: typmod(2)
    character(len=16) :: option, compor(*), meca, thmc, phenom
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, j, nelas, nresma, numlc, aniso, nwkin
    real(kind=8) :: deps(6), t, dt, tini, p1, p2, wkin(4)
    real(kind=8) :: young, nu, alpha0, crit(*), instam, instap, tref
    parameter     (nelas = 4  )
    parameter     (nresma = 18)
    real(kind=8) :: elas(nelas)
    character(len=8) :: ncra1(nelas), fami, poum
    integer :: icodre(nresma)
    real(kind=8) :: dsdeme(6, 6), dsdeme12 (6, 12)
    real(kind=8) :: angma1(3), angmas(7), ang2(3), depstr(6)
    real(kind=8) :: d(6, 6), mdal(6), dalal
    character(len=16) :: complg(3)
    aster_logical :: cp, yapre2
    character(len=16) :: nomres(3)
    real(kind=8) :: rprops(3)
! ======================================================================
!    VARIABLES LOCALES POUR L'APPEL AU MODELE DE BARCELONE
    real(kind=8) :: dsidp1(6), dp1, dp2, sat, tbiot(6)
!CCC    SIP NECESSAIRE POUR CALCULER LES CONTRAINTES TOTALES
!CCC    ET ENSUITE CONTRAINTES NETTES DANS LE MODELE DE BARCELONE
    real(kind=8) :: sipm, sipp
! ======================================================================
!    VARIABLES LOCALES POUR L'APPEL AU MODELE DE HOEK_BROWN_TOT
    real(kind=8) :: dsidp2(6), dspdp1, dspdp2
! ======================================================================
    integer :: ndt, ndi, kpg, spt
    common /tdim/   ndt  , ndi
!
    data ncra1 / 'E','NU','ALPHA','RHO' /
! ======================================================================
! --- RECUPERATION DES DONNEES MATERIAU DANS DEFI_MATERIAU -------------
! ======================================================================
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    rac2 = sqrt(2.0d0)
!
    if ((meca.eq.'CJS') .or. (meca.eq.'CAM_CLAY') .or. (meca.eq.'BARCELONE') .or.&
        (meca.eq.'LAIGLE') .or. (meca.eq.'HOEK_BROWN_EFF') .or. (meca.eq.'HOEK_BROWN_TOT')&
        .or. (meca.eq.'MAZARS') .or. (meca.eq.'ENDO_ISOT_BETON')) then
        if (option(10:14) .eq. '_ELAS') then
            call utmess('F', 'ALGORITH_67')
        endif
    endif
    call rcvalb(fami, kpg, spt, poum, imate,&
                ' ', 'ELAS', 1, 'TEMP', [t],&
                3, ncra1(1), elas(1), icodre, 0)
    young = elas(1)
    nu = elas(2)
    alpha0 = elas(3)
! ======================================================================
! --- RECUPERATION OU NON DE LA PRESSION DE GAZ
! ======================================================================
    yapre2 = .false.
    if ((thmc.eq.'GAZ') .or. (thmc.eq.'LIQU_VAPE_GAZ') .or. (thmc.eq.'LIQU_AD_GAZ') .or.&
        (thmc.eq.'LIQU_GAZ') .or. (thmc.eq.'LIQU_AD_GAZ_VAPE')) then
        yapre2 = .true.
    endif
! ======================================================================
! --- CALCUL DES CONTRAINTES -------------------------------------------
! ======================================================================
! --- LOI ELASTIQUE ----------------------------------------------------
! ======================================================================
    if ((meca.eq.'ELAS')) then
!
!   DANS LE CAS ELASTIQUE ON REPASSE AUX CONTRAINTES RELLES POUR APPLIQU
!  LA MATRICE DE ROTATION DANS LE CAS ANISOTROPE
!
        depstr = deps
!
        do i = 4, 6
            depstr(i) = deps(i)*rac2
            congep(adcome+i-1)= congep(adcome+i-1)/rac2
        end do
!
!    CALCUL DE LA MATRICE DE HOOK DANS LE REPERE GLOBAL
!
        call calela(imate, ang2, mdal, dalal, t,&
                    aniso, d, ndim, phenom)
!
        if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
            do i = 1, 3
                do j = 1, 3
                    dsde(adcome-1+i,addeme+ndim-1+j)= dsde(adcome-1+i,&
                    addeme+ndim-1+j)+d(i,j)
                end do
                do j = 4, 6
                    dsde(adcome-1+i,addeme+ndim-1+j)= dsde(adcome-1+i,&
                    addeme+ndim-1+j)+d(i,j)/(0.5*rac2)
                end do
            end do
!
            do i = 4, 6
                do j = 1, 3
                    dsde(adcome-1+i,addeme+ndim-1+j)= dsde(adcome-1+i,&
                    addeme+ndim-1+j)+d(i,j)*rac2
                end do
                do j = 4, 6
                    dsde(adcome-1+i,addeme+ndim-1+j)= dsde(adcome-1+i,&
                    addeme+ndim-1+j)+d(i,j)*2.d0
                end do
            end do
        endif
!
!
        if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
            do i = 1, 6
                do j = 1, 6
                    congep(adcome+i-1)=congep(adcome+i-1)+d(i,j)*&
                    depstr(j)
                end do
            end do
        endif
!
!   ON REVIENT AUX CONTRAINTES * RAC2
!
        do i = 4, 6
            congep(adcome+i-1)= congep(adcome+i-1)*rac2
        end do
!
!
        if (yate .eq. 1) then
            if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
                do i = 1, 6
                    dsde(adcome-1+i,addete)=dsde(adcome-1+i,addete)&
                    -mdal(i)
                end do
            endif
            if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
                do i = 1, 6
                    congep(adcome+i-1)=congep(adcome+i-1)-mdal(i)*dt
                end do
            endif
        endif
!
    endif
! ======================================================================
! --- LOI DE MOHR-COULOMB                                       -------
! ======================================================================
    mectru = .false.
!
    if (meca .eq. 'MOHR_COULOMB') then
!
        call mcbnvi(typmod, ndt, ndi)
!
        mectru = .true.
        tini = t - dt
        call matini(6, 6, 0.d0, dsdeme)
!
        if (option(1:14) .eq. 'RIGI_MECA_TANG') then
!       write(6,'(A)')
!       write(6,'(A)')'> CALCME :: RIGI_MECA -> Elastic Matrix'
!
            nomres(1)= 'ALPHA   '
            nomres(2)= 'E       '
            nomres(3)= 'NU      '
            call rcvala(imate, ' ', 'ELAS', 0, '   ',&
                        [t], 3, nomres, rprops, icodre,&
                        2)
!
            call mctgel(dsdeme, rprops)
!
            retcom = 0
!
        else
!
            call lcmohr(ndim, typmod, imate, option, tini,&
                        deps, congem(adcome), congep(adcome), vintm, vintp,&
                        dsdeme, retcom)
! --OK
        endif
    endif
!
!
! ======================================================================
! --- LOI CJS, LOI LAIGLE, LOI HOEK-BROWN OU LOI DRUCKER_PRAGER -------
! ======================================================================
    if (meca .eq. 'CJS') then
        mectru = .true.
        tini = t - dt
        call nmcjs(typmod, imate, compor, crit, instam,&
                   instap, tini, t, tref, defgem(addeme+ndim),&
                   deps, congem(adcome), vintm, option, congep(adcome),&
                   vintp, dsdeme, retcom)
    endif
!
! ======================================================================
! ------                       LOI DE HUJEUX                      ------
! ======================================================================
    if (meca .eq. 'HUJEUX') then
!
        mectru = .true.
        tini = t - dt
!
        do i = 1, 7
            angmas(i)=0.d0
        end do
!
        complg(1) = 'HUJEUX'
        write (complg(2),'(I16)') nvimec
        complg(3) = compor(3)
        numlc=34
        cp=.false.
        call redece('RIGI', 1, 1, ndim, typmod,&
                    imate, complg, crit, instam, instap,&
                    6, defgem(addeme+ndim), deps, 6, congem(adcome),&
                    vintm, option, angmas, 1, [0.d0],&
                    cp, numlc, tini, t, tref,&
                    congep(adcome), vintp, 36, dsdeme, 1,&
                    [0.d0], retcom)
!
    endif
! --- End
!
    if (meca .eq. 'LAIGLE') then
        complg(1) = 'LAIGLE'
        write (complg(2),'(I16)') nvimec
        complg(3) = compor(3)
        mectru = .true.
        tini = t - dt
        numlc=33
        cp=.false.
        call redece('RIGI', 1, 1, ndim, typmod,&
                    imate, complg, crit, instam, instap,&
                    6, defgem(addeme+ndim), deps, 6, congem(adcome),&
                    vintm, option, angma1, 1, [0.d0],&
                    cp, numlc, tini, t, tref,&
                    congep(adcome), vintp, 36, dsdeme, 1,&
                    [0.d0], retcom)
    endif
    if (meca .eq. 'HOEK_BROWN_EFF') then
        complg(1) = 'HOEK_BROWN_EFF'
        write (complg(2),'(I16)') nvimec
        complg(3) = compor(3)
        mectru = .true.
        tini = t - dt
        numlc=33
        cp=.false.
        call redece('RIGI', 1, 1, ndim, typmod,&
                    imate, complg, crit, instam, instap,&
                    6, defgem(addeme+ndim), deps, 6, congem(adcome),&
                    vintm, option, angma1, 1, [0.d0],&
                    cp, numlc, tini, t, tref,&
                    congep(adcome), vintp, 36, dsdeme, 1,&
                    [0.d0], retcom)
    endif
    if (meca .eq. 'DRUCK_PRAGER' .or. meca .eq. 'DRUCK_PRAG_N_A') then
        mectru = .true.
        tini = t - dt
        call lcdrpr(typmod, option, imate, meca, congem(adcome),&
                    tini, t, tref, deps, vintm,&
                    vintp, congep(adcome), dsdeme, retcom)
    endif
    if (meca .eq. 'LETK') then
        complg(1) = 'LETK'
        write (complg(2),'(I16)') nvimec
        complg(3) = compor(3)
        mectru = .true.
        tini = t - dt
        numlc=35
        cp=.false.
        call redece('RIGI', 1, 1, ndim, typmod,&
                    imate, complg, crit, instam, instap,&
                    6, defgem(addeme+ndim), deps, 6, congem(adcome),&
                    vintm, option, angma1, 1, [0.d0],&
                    cp, numlc, tini, t, tref,&
                    congep(adcome), vintp, 36, dsdeme, 1,&
                    [0.d0], retcom)
    endif

    if (meca .eq. 'MFRONT') then
        complg(1) = 'MFRONT'
        write (complg(2),'(I16)') nvimec
        complg(3) = compor(3)
        mectru = .true.
        tini = t - dt
        numlc=58
        cp=.false.
        nwkin=4
        wkin(1)=-1
        wkin(2)=tini
        wkin(3)=t
        wkin(4)=tref
        call redece('RIGI', 1, 1, ndim, typmod,&
                    imate, complg, crit, instam, instap,&
                    6, defgem(addeme+ndim), deps, 6, congem(adcome),&
                    vintm, option, angma1, nwkin, wkin ,&
                    cp, numlc, tini, t, tref,&
                    congep(adcome), vintp, 36, dsdeme, 1,&
                    [0.d0], retcom)
    endif

    if (mectru) then
        if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
            do i = 1, ndt
                do j = 1, ndt
                    dsde(adcome+i-1,addeme+ndim+j-1)=dsdeme(i,j)
                end do
            end do
! ======================================================================
! --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
! --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
! ======================================================================
            if (yate .eq. 1) then
                do i = 1, 3
                    dsde(adcome-1+i,addete)=-alpha0* (dsde(adcome-1+i,&
                    addeme+ndim-1+1)+ dsde(adcome-1+i,addeme+ndim-1+2)&
                    + dsde(adcome-1+i,addeme+ndim-1+3))/3.d0
                end do
            endif
        endif
    endif
! ======================================================================
! --- LOI VISC_DRUC_PRAG -----------------------------------------------
! ======================================================================
    if (meca .eq. 'VISC_DRUC_PRAG') then
        tini = t - dt
        call dpvplc(typmod, option, imate, crit, instam,&
                    instap, tini, t, tref, deps,&
                    congem(adcome), vintm, congep(adcome), vintp, dsdeme,&
                    retcom)
        if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
            do i = 1, 2*ndim
                do j = 1, 2*ndim
                    dsde(adcome+i-1,addeme+ndim+j-1)=dsdeme(i,j)
                end do
            end do
! ======================================================================
! --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
! --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
! ======================================================================
            if (yate .eq. 1) then
                do i = 1, 3
                    dsde(adcome-1+i,addete)=-alpha0* (dsde(adcome-1+i,&
                    addeme+ndim-1+1)+ dsde(adcome-1+i,addeme+ndim-1+2)&
                    + dsde(adcome-1+i,addeme+ndim-1+3))/3.d0
                end do
            endif
        endif
    endif
! ======================================================================
! --- LOI CAM_CLAY -----------------------------------------------------
! ======================================================================
    if (meca .eq. 'CAM_CLAY') then
        tini = t - dt
        call nmccam(ndim, typmod, imate, compor, crit,&
                    instam, instap, tini, t, tref,&
                    deps, congem(adcome), vintm, option, congep(adcome),&
                    vintp, dsdeme, retcom)
        if ((option(1:16).eq.'RIGI_MECA_TANG') .or. (option(1:9) .eq.'FULL_MECA')) then
            do i = 1, 2*ndim
                do j = 1, 2*ndim
                    dsde(adcome+i-1,addeme+ndim+j-1)=dsdeme(i,j)
                end do
            end do
! ======================================================================
! --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
! --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
! ======================================================================
            if (yate .eq. 1) then
                do i = 1, 3
                    dsde(adcome-1+i,addete)=-alpha0* (dsde(adcome-1+i,&
                    addeme+ndim-1+1)+ dsde(adcome-1+i,addeme+ndim-1+2)&
                    + dsde(adcome-1+i,addeme+ndim-1+3))/3.d0
                end do
            endif
        endif
    endif
! ======================================================================
! --- LOI BARCELONE ----------------------------------------------------
! ======================================================================
    if (meca .eq. 'BARCELONE') then
        tini = t - dt
        sipm=congem(adcome+6)
        sipp=congep(adcome+6)
        call nmbarc(ndim, imate, crit, sat, tbiot(1),&
                    tini, t, deps, congem(adcome), vintm,&
                    option, congep(adcome), vintp, dsdeme, p1,&
                    p2, dp1, dp2, dsidp1, sipm,&
                    sipp, retcom)
        if ((option(1:16).eq.'RIGI_MECA_TANG') .or. (option(1:9) .eq.'FULL_MECA')) then
            do i = 1, 2*ndim
                do j = 1, 2*ndim
                    dsde(adcome+i-1,addeme+ndim+j-1)=dsdeme(i,j)
                end do
            end do
! ======================================================================
! --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
! --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
! ======================================================================
            if (yate .eq. 1) then
                do i = 1, 3
                    dsde(adcome-1+i,addete)=-alpha0* (dsde(adcome-1+i,&
                    addeme+ndim-1+1)+ dsde(adcome-1+i,addeme+ndim-1+2)&
                    + dsde(adcome-1+i,addeme+ndim-1+3))/3.d0
                end do
            endif
        endif
    endif
! ======================================================================
! --- LOI ELAS_GONF ----------------------------------------------------
! ======================================================================
    if (meca .eq. 'ELAS_GONF') then
        tini = t - dt
        sipm=congem(adcome+6)
        sipp=congep(adcome+6)
!
        call elagon(ndim, imate, crit, sat, tbiot(1),&
                    tini, t, alpha0, deps, young,&
                    nu, congem(adcome), option, congep(adcome), dsdeme,&
                    p1, p2, dp1, dsidp1, dsidp2)
!
        if ((option(1:16).eq.'RIGI_MECA_TANG') .or. (option(1:9) .eq.'FULL_MECA')) then
!
            do i = 1, 2*ndim
                dsde(adcome+i-1,addep1) = dsde(adcome+i-1,addep1) +dsidp1(i)
                do j = 1, 2*ndim
                    dsde(adcome+i-1,addeme+ndim+j-1)=dsdeme(i,j)
                end do
            end do
!
            if (yapre2) then
                do i = 1, 2*ndim
                    dsde(adcome+i-1,addep2) = dsde(adcome+i-1,addep2) +dsidp2(i)
                end do
            endif
!
! ======================================================================
! --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
! --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
! ======================================================================
            if (yate .eq. 1) then
                do i = 1, 3
                    dsde(adcome-1+i,addete)=-alpha0* (dsde(adcome-1+i,&
                    addeme+ndim-1+1)+ dsde(adcome-1+i,addeme+ndim-1+2)&
                    + dsde(adcome-1+i,addeme+ndim-1+3))/3.d0
                end do
            endif
        endif
    endif
! ======================================================================
! --- LOI HOEK_BROWN_TOT -----------------------------------------------
! ======================================================================
    if (meca .eq. 'HOEK_BROWN_TOT') then
        tini = t - dt
        sipm=congem(adcome+6)
        sipp=congep(adcome+6)
        dspdp1 = 0.0d0
        dspdp2 = 0.0d0
        call dsipdp(thmc, adcome, addep1, addep2, dimcon,&
                    dimdef, dsde, dspdp1, dspdp2, pre2tr)
!
        call lchbr2(typmod, option, imate, crit, congem(adcome),&
                    defgem( addeme+ndim), tini, t, tref, deps,&
                    vintm, vintp, dspdp1, dspdp2, sipp,&
                    congep(adcome), dsdeme, dsidp1, dsidp2, retcom)
        if ((option(1:16).eq.'RIGI_MECA_TANG') .or. (option(1:9) .eq.'FULL_MECA')) then
            do i = 1, 2*ndim
                if (addep1 .ge. 1) then
                    dsde(adcome+i-1,addep1) = dsidp1(i)
                endif
!
                if (pre2tr) then
                    dsde(adcome+i-1,addep2) = dsidp2(i)
                endif
                do j = 1, 2*ndim
                    dsde(adcome+i-1,addeme+ndim+j-1)=dsdeme(i,j)
                end do
            end do
! ======================================================================
! --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
! --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
! ======================================================================
            if (yate .eq. 1) then
                do i = 1, 3
                    dsde(adcome-1+i,addete)=-alpha0* (dsde(adcome-1+i,&
                    addeme+ndim-1+1)+ dsde(adcome-1+i,addeme+ndim-1+2)&
                    + dsde(adcome-1+i,addeme+ndim-1+3))/3.d0
                end do
            endif
        endif
    endif
! ======================================================================
! --- LOI MAZARS -------------------------------------------------------
! ======================================================================
    if (meca .eq. 'MAZARS') then
        tini = t - dt
        call lcmaza('RIGI', 1, 1, ndim, typmod,&
                    imate, compor, defgem( addeme+ndim), deps, vintm,&
                    tini, t, tref, option, congep(adcome),&
                    vintp, dsdeme)
        if ((option(1:16).eq.'RIGI_MECA_TANG') .or. (option(1:9) .eq.'FULL_MECA')) then
            do i = 1, 2*ndim
                do j = 1, 2*ndim
                    dsde(adcome+i-1,addeme+ndim+j-1)=dsdeme(i,j)
                end do
            end do
! ======================================================================
! --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
! --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
! ======================================================================
            if (yate .eq. 1) then
                do i = 1, 3
                    dsde(adcome-1+i,addete)=-alpha0* (dsde(adcome-1+i,&
                    addeme+ndim-1+1)+ dsde(adcome-1+i,addeme+ndim-1+2)&
                    + dsde(adcome-1+i,addeme+ndim-1+3))/3.d0
                end do
            endif
        endif
    endif
! ======================================================================
! --- LOI ENDO_ISOT_BETON ----------------------------------------------
! ======================================================================
    if (meca .eq. 'ENDO_ISOT_BETON') then
        tini = t - dt
        if (option(6:9) .eq. 'COUP') then
!           on interdit le couplage fluage-eib car dans ce cas dsdeme(6,12)
            call utmess('F', 'ALGORITH_74')
        endif
!       dsdeme12(i,j) ne sert qu'a la compatibilite de l'interface
!       tous les termess i,6+j doive,t etre nuls
        call lcldsb('RIGI', 1, 1, ndim, typmod,&
                    imate, compor, defgem( addeme+ndim), deps, vintm,&
                    tini, t, tref, option, congep(adcome),&
                    vintp, dsdeme12, crit)
        if ((option(1:16).eq.'RIGI_MECA_TANG') .or. (option(1:9) .eq.'FULL_MECA')) then
            do i = 1, 2*ndim
                do j = 1, 2*ndim
                    dsde(adcome+i-1,addeme+ndim+j-1)=dsdeme12(i,j)
                end do
            end do
! ======================================================================
! --- LA DEPENDANCE DES CONTRAINTES / T = -ALPHA0 * DEPENDANCE ---------
! --- PAR RAPPORT A TRACE DE DEPS ( APPROXIMATION) ---------------------
! ======================================================================
            if (yate .eq. 1) then
                do i = 1, 3
                    dsde(adcome-1+i,addete)=-alpha0* (dsde(adcome-1+i,&
                    addeme+ndim-1+1)+ dsde(adcome-1+i,addeme+ndim-1+2)&
                    + dsde(adcome-1+i,addeme+ndim-1+3))/3.d0
                end do
            endif
        endif
    endif
! ======================================================================
end subroutine
