subroutine lcmatt(fami, kpg, ksp, mod, imat,&
                  nmat, poum, rela_comp, coefel, coefpl,&
                  typma, ndt, ndi, nr, nvi)
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ROUTINE GENERIQUE DE RECUPERATION DU MATERIAU A T ET T+DT
!     ----------------------------------------------------------------
!     IN  FAMI   :  FAMILLE DE POINT DE GAUSS (RIGI,MASS,...)
!         KPG,KSP:  NUMERO DU (SOUS)POINT DE GAUSS
!         MOD    :  TYPE DE MODELISATION
!         IMAT   :  ADRESSE DU MATERIAU CODE
!         NMAT   :  DIMENSION  DE MATER
!         POUM   :  '+' ou '-'
!     OUT        :  COEFFICIENTS MATERIAU A T- OU T+
!         COEFEL :  CARACTERISTIQUES ELASTIQUES
!         COEFPL :  CARACTERISTIQUES PLASTIQUES
!         TYPMA  :  TYPE DE MATRICE TANGENTE
!         NDT    :  NB TOTAL DE COMPOSANTES TENSEURS
!         NDI    :  NB DE COMPOSANTES DIRECTES  TENSEURS
!         NR     :  NB DE COMPOSANTES SYSTEME NL
!         NVI    :  NB DE VARIABLES INTERNES
!     ----------------------------------------------------------------
#include "asterfort/assert.h"
#include "asterfort/matnor.h"
    integer :: kpg, ksp, nmat, ndt, ndi, nr, nvi, imat
    real(kind=8) :: coefel(nmat), coefpl(nmat)
    character(len=*) :: fami, poum
    character(len=16) :: rela_comp
    character(len=8) :: mod, typma
    character(len=11) :: meting
    common /meti/   meting
!     ----------------------------------------------------------------
!
! -   NB DE COMPOSANTES DES TENSEURS
!
    if (meting(1:11) .eq. 'RUNGE_KUTTA') then
        ndt = 6
    else if (mod(1:2).eq.'3D') then
        ndt = 6
    else if (mod(1:6).eq.'D_PLAN'.or.mod(1:4).eq.'AXIS') then
        ndt = 4
    else if (mod(1:6).eq.'C_PLAN') then
        ndt = 4
    else
        ASSERT(.false.)
    endif
    ndi = 3
    typma = 'COHERENT'
!
    if (rela_comp .eq. 'NORTON') then
        call matnor(fami, kpg, ksp, imat, nmat,&
                    poum, coefel, coefpl, ndt, nvi,&
                    nr)
    else
        ASSERT(.false.)
    endif
!
end subroutine
