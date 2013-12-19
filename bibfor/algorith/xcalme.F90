subroutine xcalme(option, meca, imate, ndim, dimenr,&
                  dimcon, addeme, adcome, congep,&
                  yaenrm, adenme, dsde, deps, t,&
                  idecpg, kpi, ang2, aniso, phenom)
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
! ======================================================================
    implicit none
#   include "asterfort/rcvalb.h"
#   include "asterfort/calela.h"
#   include "asterfort/tecael.h"
    integer :: ndim, dimenr, dimcon, addeme
    integer :: adcome, imate, yaenrm, adenme, idecpg, kpi
    real(kind=8) :: congep(dimcon)
    real(kind=8) :: dsde(dimcon, dimenr), rac2
    character(len=16) :: option, meca, phenom
! ======================================================================
! --- VARIABLES LOCALES ------------------------------------------------
! ======================================================================
    integer :: i, j, nelas, nresma, aniso
    real(kind=8) :: deps(6), t
    real(kind=8) :: young, nu, alpha0
    parameter     (nelas = 4  )
    parameter     (nresma = 18)
    real(kind=8) :: elas(nelas), ang2(3), depstr(6)
    real(kind=8) :: d(6, 6), mdal(6), dalal
    character(len=8) :: ncra1(nelas), fami, poum
    integer :: icodre(nresma)
    integer :: spt, ipi
!
    data ncra1 / 'E','NU','ALPHA','RHO' /
! ======================================================================
! --- RECUPERATION DES DONNEES MATERIAU DANS DEFI_MATERIAU -------------
! ======================================================================
    fami='XFEM'
    ipi=idecpg+kpi
    spt=1
    poum='+'
!
    rac2 = sqrt(2.0d0)
!
    call rcvalb(fami, ipi, spt, poum, imate,&
                ' ', 'ELAS', 1, 'TEMP', [t],&
                3, ncra1(1), elas(1), icodre, 0)
    young = elas(1)
    nu = elas(2)
    alpha0 = elas(3)
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
                    addeme+ndim-1+j) +d(i,j)
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
! ======================================================================
! --- CALCUL DES TERMES POUR XFEM --------------------------------------
! ======================================================================
        if (yaenrm .eq. 1) then
            if ((option(1:9).eq.'RIGI_MECA') .or. (option(1:9) .eq.'FULL_MECA')) then
            do i = 1, 3
                do j = 1, 3
                    dsde(adcome-1+i,adenme+ndim-1+j)= dsde(adcome-1+i,&
                    adenme+ndim-1+j) +d(i,j)
                end do
                do j = 4, 6
                    dsde(adcome-1+i,adenme+ndim-1+j)= dsde(adcome-1+i,&
                    adenme+ndim-1+j)+d(i,j)/(0.5*rac2)
               end do
            end do
!
            do i = 4, 6
                do j = 1, 3
                    dsde(adcome-1+i,adenme+ndim-1+j)= dsde(adcome-1+i,&
                    adenme+ndim-1+j)+d(i,j)*rac2
                 end do
                do j = 4, 6
                    dsde(adcome-1+i,adenme+ndim-1+j)= dsde(adcome-1+i,&
                    adenme+ndim-1+j)+d(i,j)*2.d0
                 end do
            end do
            endif


       endif
    endif
! ======================================================================
end subroutine
