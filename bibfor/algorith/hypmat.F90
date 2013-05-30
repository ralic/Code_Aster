subroutine hypmat(fami, kpg, ksp, poum, imate,&
                  c10, c01, c20, k)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 2005 UCBL LYON1 - T. BARANGER     WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
    include 'asterc/r8prem.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mess.h'
    character(len=*) :: fami, poum
    integer :: kpg, ksp, imate
    real(kind=8) :: c10, c01, c20
    real(kind=8) :: k
!
! ----------------------------------------------------------------------
!
! LOI DE COMPORTEMENT HYPERELASTIQUE DE SIGNORINI
!
! C10 (I1-3) + C01 (I2-3)+ C20 (I1-3)^2 + K/2(J-1)²
!
! RECUPERE DONNEES MATERIAUX
!
! ----------------------------------------------------------------------
!
!
! IN  IMATE   : ADRESSE DU MATERIAU CODE
! IN  FAMI    : FAMILLE DE POINTS DE GAUSS
! IN  KPG     : NUMERO DU POINT DE GAUSS
! IN  KSP     : NUMERO DU SOUS-POINT DE GAUSS
! IN  POUM    : '-' POUR VARIABLES DE COMMANDE
! OUT C10     : PARAMETRE LOI DE COMPORTEMENT
! OUT C02     : PARAMETRE LOI DE COMPORTEMENT
! OUT C20     : PARAMETRE LOI DE COMPORTEMENT
! OUT K       : MODULE DE COMPRESSIBILITE
!
! ----------------------------------------------------------------------
!
    integer :: nbres
    parameter   ( nbres = 3  )
    character(len=8) :: nomres(nbres)
    integer :: codres(nbres)
    real(kind=8) :: valres(nbres)
    real(kind=8) :: nu
    real(kind=8) :: denom
    logical :: cmpk
!
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
!
! --- SOIT ON PREND LE K DONNE PAR DEFI_MATERIAU, SOIT ON LE CALCULE
! --- A PARTIR DU NU
!
    nomres(1) = 'K'
    call rcvalb(fami, kpg, ksp, poum, imate,&
                ' ', 'ELAS_HYPER', 0, ' ', 0.d0,&
                1, nomres, valres, codres, 0)
!
    if (codres(1) .eq. 0) then
        k = valres(1)
        cmpk = .false.
    else
        nomres(1) = 'NU'
        call rcvalb(fami, kpg, ksp, poum, imate,&
                    ' ', 'ELAS_HYPER', 0, ' ', 0.d0,&
                    1, nomres, valres, codres, 2)
        nu = valres(1)
        denom = 3.d0*(1.d0-2.d0*nu)
        if (denom .le. r8prem()) then
            call u2mess('F', 'ELASHYPER_98')
        endif
        cmpk = .true.
    endif
!
! --- RECUPERATION C10, C01 ET C20
!
    nomres(1) = 'C10'
    nomres(2) = 'C01'
    nomres(3) = 'C20'
    call rcvalb(fami, kpg, ksp, poum, imate,&
                ' ', 'ELAS_HYPER', 0, ' ', 0.d0,&
                nbres, nomres, valres, codres, 2)
    c10 = valres(1)
    c01 = valres(2)
    c20 = valres(3)
!
! --- CALCUL DU MODULE DE COMPRESSIBILITE
!
    if (cmpk) then
        k = 4.d0*(c10+c01)*(1.d0+nu)/denom
    endif
!
end subroutine
