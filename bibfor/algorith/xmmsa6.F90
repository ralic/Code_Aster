subroutine xmmsa6(ndim, ipgf, imate, lamb, wsaut, nd,&
                  tau1, tau2, cohes, job, rela,&
                  alpha, dsidep, sigma, p, am, raug)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/lcejex.h"
#include "asterfort/lcecli.h"
#include "asterfort/matini.h"
#include "asterfort/vecini.h"
    integer :: ndim, ipgf, imate
    real(kind=8) :: wsaut(3), lamb(3), am(3), dsidep(6, 6)
    real(kind=8) :: tau1(3), tau2(3), nd(3)
    real(kind=8) :: alpha(3), p(3, 3)
    real(kind=8) :: cohes(3), rela, raug
    character(len=8) :: job
! ----------------------------------------------------------------------
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! ROUTINE CONTACT (METHODE XFEM HPP - CALCUL ELEM.)
!
! --- CALCUL DU SAUT DE DEPLACEMENT EQUIVALENT [[UEG]]
!
! ----------------------------------------------------------------------
!
! IN  NDIM   : DIMENSION DE L'ESPACE
! IN  IPGF   : NUMÉRO DU POINTS DE GAUSS
! IN  IMATE  : ADRESSE DE LA SD MATERIAU
! IN  SAUT   : SAUT DE DEPLACEMENT
! IN  ND     : NORMALE À LA FACETTE ORIENTÉE DE ESCL -> MAIT
!                 AU POINT DE GAUSS
! IN  TAU1   : TANGENTE A LA FACETTE AU POINT DE GAUSS
! IN  TAU2   : TANGENTE A LA FACETTE AU POINT DE GAUSS
! IN  COHES  : VARIABLE INTERNE COHESIVE
! IN  JOB    : 'SAUT_EQ', 'MATRICE' OU 'VECTEUR'
! IN  RELA   : LOI COHESIVE 1:CZM_EXP_REG 2:CZM_LIN_REG
! OUT ALPHA  : SAUT DE DEPLACEMENT EQUIVALENT
! OUT DSIDEP : MATRICE TANGENTE DE CONTACT PENALISE ET DE FISSURATION
! OUT SIGMA  : CONTRAINTE
! OUT PP     : ND X ND
! OUT DNOR   : SAUT DEPLACEMENT NORMAL DANS LA BASE FIXE
! OUT DTANG  : SAUT DEPLACEMENT TANGENTIEL DANS LA BASE FIXE
! OUT P      : MATRICE DE PROJECTION SUR LE PLAN TANGENT
! OUT AM     : SAUT INSTANT - BASE LOCALE : AM(1) = SAUT NORMAL
!                                           AM(2) = SAUT TANGENTIEL
!
!
!
!
    integer :: i
!
    real(kind=8) :: vim(9), vip(9)
    real(kind=8) ::  dsid2d(6, 6), dam(3)
    real(kind=8) :: sigma(6)
!
    character(len=16) :: option
!
! ----------------------------------------------------------------------
!
    call vecini(3, 0.d0, am)
    call vecini(3, 0.d0, dam)
    call matini(3, 3, 0.d0, p)
    call matini(6, 6, 0.d0, dsidep)
    call matini(6, 6, 0.d0, dsid2d)
    call vecini(6, 0.d0, sigma)
    call vecini(9, 0.d0, vim)
    call vecini(9, 0.d0, vip)
!
! CONSTRUCTION DE LA MATRICE DE PASSAGE
!
! avec la nouvelle formulation, le saut est directement dans
! la bonne base
    do 10 i = 1, ndim
        am(i) = wsaut(i)
10  end do
!
    do 7 i = 1, ndim
        p(1,i) = nd(i)
 7  end do
    do 8 i = 1, ndim
        p(2,i) = tau1(i)
 8  end do
    if (ndim .eq. 3) then
        do 9 i = 1, ndim
            p(3,i) = tau2(i)
 9      continue
    endif
!
! --- CALCUL DU SAUT DE DEPLACEMENT AM EN BASE LOCALE
! attention on ne fait plus d inversion de convention
! donc prevu pour fonctionner avec w mais pas avec u
!
!
! --- CALCUL VECTEUR ET MATRICE TANGENTE EN BASE LOCALE
!
    if (job .ne. 'SAUT_LOC') then
        vim(1)=cohes(1)
        if(rela.eq.1.d0) then
            vim(2) = cohes(2)
        else
            if (cohes(2) .le. 0.d0) then
                vim(2)=0.d0
            else
                vim(2)=1.d0
            endif
            vim(3) = abs(cohes(2)) - 1.d0
        endif
!
! PREDICTION: COHES(3)=1 ; CORRECTION: COHES(3)=2
!
        if (cohes(3) .eq. 1.d0) then
            option='RIGI_MECA_TANG'
        else if (cohes(3).eq.2.d0) then
            option='FULL_MECA'
        else
            option='FULL_MECA'
        endif
!
! VIM = VARIABLES INTERNES UTILISEES DANS LCEJEX
!.............VIM(1): SEUIL, PLUS GRANDE NORME DU SAUT
!
        call lcecli('RIGI', ipgf, 1, ndim, imate,&
                    option, lamb, wsaut, sigma, dsidep,&
                    vim, vip, raug)
!
        alpha(1) = vip(1)
        if(rela.eq.1.d0) then
            alpha(2) = vip(2)
        else
            if (vip(2) .eq. 0.d0) then
                alpha(2) = -vip(3)-1.d0
            else if (vip(2).eq.1.d0) then
                alpha(2) = vip(3) + 1.d0
            else
                ASSERT(.false.)
            endif
        endif
! ici on a enleve la securite numerique
!
        if (job .eq. 'ACTU_VI') then
            alpha(3) = 1.d0
        else if (job.eq.'MATRICE') then
            alpha(3) = 2.d0
        endif
!
    endif
end subroutine
