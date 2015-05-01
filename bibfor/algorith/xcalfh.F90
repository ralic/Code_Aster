subroutine xcalfh(option, thmc, ndim, dimcon, yamec,&
                  addep1, adcp11, addeme, congep, dsde,&
                  grap1, rho11, pesa, tperm, cliq,&
                  viscl, dviscl, dimenr,&
                  adenhy)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: daniele.colombo at ifpen.fr
! ======================================================================
! ROUTINE CALC_FLUX_HYDRO
! CALCULE LES CONTRAINTES GENERALISEES ET LA MATRICE TANGENTE DES FLUX
! HYDRAULIQUES AU POINT DE GAUSS CONSIDERE
! ======================================================================
    implicit none
!
    integer :: ndim, dimcon, yamec
    integer :: addeme, addep1, adcp11, adenhy
    integer :: bdcp11, dimenr
    real(kind=8) :: congep(1:dimcon)
    real(kind=8) :: dsde(1:dimcon, 1:dimenr), grap1(3)
    real(kind=8) :: rho11, pesa(3), tperm(ndim,ndim)
    real(kind=8) :: cliq, viscl, dviscl
    character(len=16) :: option, thmc
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, j, k
    real(kind=8) :: lambd1(3), visco, dvisco
    real(kind=8) :: krel1, dkrel1
    real(kind=8) :: dr11p1
!
! ======================================================================
! --- QUELQUES INITIALISATIONS -----------------------------------------
! ======================================================================
!
    dr11p1 = 0.d0
    bdcp11 = adcp11
!
! ======================================================================
! RECUPERATION DES COEFFICIENTS
! ======================================================================
    if (thmc .eq. 'LIQU_SATU') then
        krel1 = 1.d0
        dkrel1 = 0.d0
        visco = viscl
        dvisco = dviscl
    endif
! ======================================================================
! --- CALCUL DE LAMBDA1 ------------------------------------------------
! ======================================================================
! --- LAMBD1(1) = CONDUC_HYDRO_LIQ -------------------------------------
! --- LAMBD1(2) = D(CONDUC_HYDRO_LIQ)/DEPSV ----------------------------
! --- LAMBD1(3) = D(CONDUC_HYDRO_LIQ)/DP1 ------------------------------
! ======================================================================
    lambd1(1) = krel1/visco
    lambd1(2) = 0.0d0
    lambd1(3) = dkrel1/visco
!
! ======================================================================
! CALCUL DES DERIVEES DES MASSES VOLUMIQUES
!
    if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        if (thmc .eq. 'LIQU_SATU') then
            dr11p1=rho11*cliq
        endif
    endif
!
! ======================================================================
! CALCUL DES FLUX HYDRAULIQUES
!
    if ((option(1:9).eq.'RAPH_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        if (thmc .eq. 'LIQU_SATU') then
            do 102 i = 1, ndim
                        congep(bdcp11+i)=0.d0
                        do 622 j = 1, ndim
                            congep(bdcp11+i)=congep(bdcp11+i)+rho11*&
                            lambd1(1) *tperm(i,j)*(-grap1(j)+rho11*&
                            pesa(j))
622                    continue
102              continue
        endif
    endif
!
    if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9).eq.'FULL_MECA')) then
        if (thmc .eq. 'LIQU_SATU') then
                  do 108 i=1,ndim
                        do 682 j = 1, ndim
                            dsde(bdcp11+i,addep1)=dsde(bdcp11+i,&
                            addep1) +dr11p1*lambd1(1)*tperm(i,j)*&
                            (-grap1(j)+rho11*pesa(j))
!
                            dsde(bdcp11+i,addep1)=dsde(bdcp11+i,&
                            addep1) +rho11*lambd1(3)*tperm(i,j)*&
                            (-grap1(j)+rho11*pesa(j))
!
                            dsde(bdcp11+i,addep1)=dsde(bdcp11+i,&
                            addep1) +rho11*lambd1(1)*tperm(i,j)*(&
                            dr11p1*pesa(j))
!
                            dsde(bdcp11+i,addep1+j)=dsde(bdcp11+i,&
                            addep1+j) -rho11*lambd1(1)*tperm(i,j)
682                    continue
                if (yamec .eq. 1) then
                        do 107 j=1, 3
                                do 672 k = 1, ndim 
                                    dsde(bdcp11+i,addeme+ndim-1+i)=&
                                    dsde(bdcp11+i,addeme+ndim-1+i)&
                                    +rho11*lambd1(2)*tperm(i,k)&
                                    *(-grap1(k)+rho11*pesa(k))
672                            continue
107                    continue
                endif
108            continue
        endif
    endif
! ======================================================================
! CALCUL DES FLUX HYDRAULIQUES POUR XFEM
!
        if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
            if (thmc .eq. 'LIQU_SATU') then
               do 109 i = 1, ndim
                  do 209 j = 1, ndim
                      dsde(bdcp11+i,adenhy)=dsde(bdcp11+i,adenhy)&
                      +dr11p1*lambd1(1)*(-grap1(j)+rho11*pesa(j))*tperm(i,j)
                      dsde(bdcp11+i,adenhy)=dsde(bdcp11+i,adenhy)&
                      +rho11*lambd1(3)*(-grap1(j)+rho11*pesa(j))*tperm(i,j)
                      dsde(bdcp11+i,adenhy)=dsde(bdcp11+i,adenhy)&
                      +rho11*lambd1(1)*(dr11p1*pesa(j))*tperm(i,j)
209               continue
109            continue
            endif
        endif
end subroutine
