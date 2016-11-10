subroutine xhmsa6(ndim, ipgf, imate, lamb, wsaut, nd,&
                  tau1, tau2, cohes, job, rela,&
                  alpha, dsidep, sigma, p, am, raug,&
                  thmc, meca, hydr, wsautm, dpf, rho110)
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/lcejex.h"
#include "asterfort/lcecli.h"
#include "asterfort/matini.h"
#include "asterfort/thmrcp.h"
#include "asterfort/vecini.h"
    integer :: ndim, ipgf, imate
    real(kind=8) :: wsaut(3), lamb(3), am(3), dsidep(6, 6)
    real(kind=8) :: tau1(3), tau2(3), nd(3), wsautm(3)
    real(kind=8) :: alpha(5), p(3, 3), rho11, rho11m
    real(kind=8) :: cohes(5), rela, raug, dpf, w11, w11m
    character(len=8) :: job
! ======================================================================
! person_in_charge: daniele.colombo at ifpen.fr
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
    integer :: i, ibid, rbid54
    real(kind=8) :: vim(9), vip(9)
    real(kind=8) ::  dsid2d(6, 6), dam(3)
    real(kind=8) :: sigma(6), cliq, varbio
    real(kind=8) :: rbid2, rbid3, rbid4, rbid5, rbid6
    real(kind=8) :: rbid8, rbid9, rbid10, rbid11, rbid12(6), rbid13, rbid14
    real(kind=8) :: rbid15(3), rbid16(3, 3), rbid17, rbid18, rbid19, rbid20
    real(kind=8) :: rbid21, rbid22, rbid23, rbid24, rbid25, rbid26
    real(kind=8) :: rbid29, rbid30, rbid31, rbid32
    real(kind=8) :: rbid33, rbid34, rbid35, rbid36, rbid37(3, 3)
    real(kind=8) :: rbid39, rbid40, rbid41, rbid42, rbid43, rbid44
    real(kind=8) :: rbid45, rbid46, rbid47, rbid48, rbid49, rbid50
    real(kind=8) :: rbid52, rbid53, rbid38(3, 3), rbid51(3, 3)
    real(kind=8) :: r7bid(3), rbid
    real(kind=8) :: rho110, t
    character(len=16) :: option, zkbid, thmc, meca, hydr
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
!   RECUPERATION DES DONNEES HM
!
    zkbid = 'VIDE'
    call thmrcp('INTERMED', imate, thmc, meca, hydr,&
                zkbid, rbid, rbid, rbid, rbid,&
                rbid, t, rbid2, rbid3, rbid4,&
                rbid5, rbid6, rbid8, rbid9, rbid10,&
                rbid11, rbid12, rbid13, rbid53, rbid14,&
                rbid15, rbid16, rbid17, rbid18, rbid19,&
                rbid20, rbid21, rbid22, rbid23, rbid24,&
                rbid25, rbid26, rho110, cliq, rbid29,&
                rbid30, rbid31, rbid32, rbid33, rbid34,&
                rbid35, rbid36, rbid37, rbid38, rbid39,&
                rbid40, rbid41, rbid42, rbid43, rbid44,&
                rbid45, rbid46, rbid47, rbid48, rbid49,&
                rbid50, rbid51, rbid52, ibid,&
                r7bid, rbid54, ndim)
!
    rho11 = cohes(4) + rho110
    rho11m = cohes(4) + rho110
!
    w11 = cohes(5)
    w11m = cohes(5)
!
! CONSTRUCTION DE LA MATRICE DE PASSAGE
!
! avec la nouvelle formulation, le saut est directement dans
! la bonne base
    do i = 1, ndim
        am(i) = wsaut(i)
    end do
!
    do i = 1, ndim
        p(1,i) = nd(i)
    end do
    do i = 1, ndim
        p(2,i) = tau1(i)
    end do
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
! --- VARIABLES INTERNES HYDRAULIQUES
    if (option.eq.'FULL_MECA') then
!   CALCUL DE LA VARIABLE INTERNE : MASSE VOLUMIQUE
       varbio = dpf*cliq
       if (varbio.gt.5.d0) then
          ASSERT(.false.)
       endif
!
       rho11 = rho11m*exp(varbio)
!
!   CALCUL DE LA VARIABLE INTERNE : APPORTS MASSIQUES
!   (SEULEMENT UTILE POUR LE CAS DU SECOND-MEMBRE)
!
       w11 = w11m + rho11*wsaut(1) - rho11m*wsautm(1)
    endif
    alpha(4) = rho11-rho110
    alpha(5) = w11
! ici on a enleve la securite numerique
!
    if (job .eq. 'ACTU_VI') then
        alpha(3) = 1.d0
    else if (job.eq.'MATRICE') then
        alpha(3) = 2.d0
    endif
!
end subroutine
