subroutine calcme(option, compor, thmc, meca, imate,&
                  typmod, carcri, instam, instap, &
                  ndim, dimdef, dimcon, nvimec, yate,&
                  addeme, adcome, addete, defgem, congem,&
                  congep, vintm, vintp, addep1, addep2,&
                  dsde, deps, p1, p2, t,&
                  dt, retcom, dp1, dp2, sat,&
                  tbiot, ang2, aniso, phenom)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/calela.h"
#include "asterfort/nmcomp.h"
#include "asterfort/dsipdp.h"
#include "asterfort/elagon.h"
#include "asterfort/lchbr2.h"
#include "asterfort/nmbarc.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/lcidbg.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
! aslint: disable=W1504
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

    aster_logical :: mectru, pre2tr
    integer :: ndim, dimdef, dimcon, nvimec, addeme, addete, addep1
    integer :: addep2, adcome, imate, yate, retcom
    real(kind=8) :: defgem(dimdef), congem(dimcon), congep(dimcon)
    real(kind=8) :: vintm(nvimec), vintp(nvimec)
    real(kind=8) :: dsde(dimcon, dimdef), rac2
    character(len=8) :: typmod(2)
    character(len=16) :: option, compor(*), meca, thmc, phenom, mult_comp
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, j, nelas, nresma, aniso, numlc
    real(kind=8) :: deps(6), t, dt, p1, p2
    real(kind=8) :: young, nu, alpha0, carcri(*), instam, instap
    parameter     (nelas = 4  )
    parameter     (nresma = 18)
    real(kind=8) :: elas(nelas)
    character(len=8) :: ncra1(nelas), fami, poum
    integer :: icodre(nresma)
    real(kind=8) :: dsdeme(6, 6)
    real(kind=8) :: angma1(3), ang2(3), depstr(6)
    real(kind=8) :: d(6, 6), mdal(6), dalal
    character(len=16) :: complg(20)
    aster_logical :: yapre2
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
    integer :: ndt, ndi, kpg, ksp
    common /tdim/   ndt  , ndi
!
    data ncra1 / 'E','NU','ALPHA','RHO' /
! ======================================================================
! --- RECUPERATION DES DONNEES MATERIAU DANS DEFI_MATERIAU -------------
! ======================================================================
    fami='FPG1'
    kpg=1
    ksp=1
    poum='+'
    mult_comp = ' '
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
    call rcvalb(fami, kpg, ksp, poum, imate,&
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
! --- INITIALISATION INDICATEUR RETCOM
! ======================================================================
    retcom = 0
    mectru = .false.
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
    elseif (meca .eq. 'BARCELONE') then
! ======================================================================
! --- LOI BARCELONE ----------------------------------------------------
! ======================================================================
        complg(1) = 'BARCELONE'
        write (complg(2),'(I16)') nvimec
        complg(3) = compor(3)
        sipm=congem(adcome+6)
        sipp=congep(adcome+6)
        call nmbarc(ndim, imate, carcri, sat, tbiot(1),&
                    deps, congem(adcome), vintm,&
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
    elseif (meca .eq. 'ELAS_GONF') then
! ======================================================================
! --- LOI ELAS_GONF ----------------------------------------------------
! ======================================================================
        complg(1) = 'ELAS_GONF'
        write (complg(2),'(I16)') nvimec
        complg(3) = compor(3)
        sipm=congem(adcome+6)
        sipp=congep(adcome+6)
!
        call elagon(ndim, imate, tbiot(1),&
                    alpha0, deps, young, &
                    nu, congem(adcome), option, congep(adcome), dsdeme,&
                    p1, dp1, dsidp1, dsidp2)
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
    elseif (meca .eq. 'HOEK_BROWN_TOT') then
! ======================================================================
! --- LOI HOEK_BROWN_TOT -----------------------------------------------
! ======================================================================
        complg(1) = 'HOEK_BROWN_TOT'
        write (complg(2),'(I16)') nvimec
        complg(3) = compor(3)
        sipm=congem(adcome+6)
        sipp=congep(adcome+6)
        dspdp1 = 0.0d0
        dspdp2 = 0.0d0
        call dsipdp(thmc, adcome, addep1, addep2, dimcon,&
                    dimdef, dsde, dspdp1, dspdp2, pre2tr)
!
        call lchbr2(typmod, option, imate, carcri, congem(adcome),&
                    defgem( addeme+ndim), deps,&
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
        mectru = .false.
    else
        mectru    = .false.
        complg(1) = meca
        write (complg(2),'(I16)') nvimec
        complg(3) = compor(3)
        complg(6) = compor(15)
        read (complg(6),'(I16)') numlc
        if (numlc .ge. 100) then
            call utmess('F', 'THM1_1', sk = meca)
        endif
        if (numlc .ne. 0) then
            mectru    = .true.
            fami      = 'RIGI'
            kpg       = 1
            ksp       = 1
            call nmcomp(fami, kpg, ksp, ndim, typmod,&
                      imate, complg, carcri, instam, instap,&
                      6, defgem(addeme+ndim), deps, 6, congem(adcome),&
                      vintm, option, angma1, 1, [0.d0],&
                      congep(adcome), vintp, 36, dsdeme, 1,&
                      [0.d0], retcom)
        endif
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

!! ======================================================================
!! --- AFFICHAGE DES DONNEES NECESSAIRES POUR REJOUER CALCUL SI ---------
!! --- ECHEC DU MODELE DE COMPORTEMENT - RETCOM.EQ.1 --------------------
!! ======================================================================
    if(retcom .eq. 1) then
        call lcidbg(fami, kpg, ksp, typmod, complg, &
                    carcri, instam, instap, 6, & 
                    defgem(addeme+ndim),deps, 6,&
                    congem(adcome), vintm, option) 
    endif
! ======================================================================
end subroutine
