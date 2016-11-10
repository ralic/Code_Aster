subroutine xvinhm(jmate, thmc, meca, hydr, ndim,&
                  cohes, dpf, saut, sautm, nd, lamb,&
                  w11m, rho11m, alpha, job, t, pf,&
                  rho11, w11, ipgf, rela, dsidep,&
                  delta, r, am)

    implicit none
    
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterc/r8prem.h" 
#include "asterfort/lceitc.h"
#include "asterfort/lceiou.h"
#include "asterfort/thmrcp.h"
#include "asterfort/matini.h" 
#include "asterfort/vecini.h"
   
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
! person_in_charge: daniele.colombo at ifpen.fr
! ======================================================================
!
! ROUTINE MODELE HM-XFEM (CAS DE LA FRACTURE)
!
! CALCUL DES VARIABLES INTERNES (MECANIQUE ET HYDRAULIQUE)
!
! ----------------------------------------------------------------------
!
    integer :: jmate, ndim, i, rbid54, ibid, ipgf
    real(kind=8) :: cliq, vim(2), vip(2), cohes(5), rho11, rho11m
    real(kind=8) :: dsidep(6, 6), delta(6), eps, vim2(9), vip2(9), rela
    real(kind=8) :: w11, w11m, varbio, dpf, psp, psm, saut(3), lamb(3)
    real(kind=8) :: sautm(3), alpha(5), nd(3), t, rho110, r, pf, am(3)
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
    character(len=8)  :: job
    character(len=16) :: thmc, meca, hydr, zkbid, option
!
    call vecini(2, 0.d0, vim)
    call vecini(2, 0.d0, vip)
    call vecini(9, 0.d0, vim2)
    call vecini(9, 0.d0, vip2)
    call matini(6, 6, 0.d0, dsidep)
    call vecini(6, 0.d0, delta)
! 
!   RECUPERATION DES DONNEES HM
!
    zkbid = 'VIDE'
    call thmrcp('INTERMED', jmate, thmc, meca, hydr,&
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
!   INITIALISATION DE LA VARIABLE INTERNE
!
    vim2(4) = cohes(1)
    vim2(2) = cohes(2)
    vim(1) = cohes(4)
    vim(2) = cohes(5)
!
    rho11 = vim(1) + rho110
    rho11m = vim(1) + rho110
! 
    w11 = vim(2)
    w11m = vim(2)
!
!   PREDICTION: COHES(3)=1 ; CORRECTION: COHES(3)=2
!
    if (cohes(3) .eq. 1.d0) then
       option='RIGI_MECA_TANG'
    else if (cohes(3) .eq. 2.d0) then
       option='FULL_MECA'
    else
       option='FULL_MECA'
    endif
    if(job.eq.'MATRICE'.and.option.eq.'FULL_MECA') then
       eps = 100.*r8prem()
       vim2(4)=min(1.d0,vim2(4)*(1+eps))
    endif
!
!   UTILISATION DE LA LOI COHESIVE MIXTE TALON-CURNIER
    if (rela.eq.3.d0) then
       call lceitc('RIGI', ipgf, 1, jmate, option,&
                    lamb, am, delta, dsidep, vim2,&
                    vip2, r, pfluide=pf)
    else if (rela.eq.4.d0) then
       call lceiou('RIGI', ipgf, 1, jmate, option,&
                   lamb, am, delta, dsidep, vim2,&
                   vip2, r, pfluide=pf)
    endif
!
    if (option.eq.'FULL_MECA') then
!   CALCUL DE LA VARIABLE INTERNE : MASSE VOLUMIQUE 
       varbio = dpf*cliq
       if (varbio.gt.5.d0) then 
          ASSERT(.false.)
       endif
!
       vip(1) = - rho110 + (vim(1)+rho110)*exp(varbio)
       rho11 = vip(1) + rho110
       rho11m = vim(1) + rho110
!
!   CALCUL DE LA VARIABLE INTERNE : APPORTS MASSIQUES 
!   (SEULEMENT UTILE POUR LE CAS DU SECOND-MEMBRE)
!
       psp = 0.d0 
       psm = 0.d0
       do i = 1, ndim
          psp = psp - saut(i)*nd(i)
          psm = psm - sautm(i)*nd(i)
       end do 
!
       vip(2) = rho11*psp - rho11m*psm
       w11 = vip(2) + w11m
    endif
!
    alpha(1) = vip2(4)
    alpha(2) = vip2(2)
    alpha(4) = vip(1)
    alpha(5) = w11
!
    if (job.eq.'ACTU_VI') then
       alpha(3) = 1.d0
    else if (job.eq.'MATRICE') then 
       alpha(3) = 2.d0
    endif        
end subroutine
