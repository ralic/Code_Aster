subroutine xmmsa5(ndim, ipgf, imate, saut, lamb,&
                  nd, tau1, tau2, cohes, job,&
                  rela, alpha, dsidep, delta, p,&
                  am, r)
    implicit none
#include "jeveux.h"
#include "asterfort/lceiou.h"
#include "asterfort/lceitc.h"
#include "asterfort/matini.h"
#include "asterfort/prmave.h"
#include "asterfort/vecini.h"
    integer :: ndim, ipgf, imate
    real(kind=8) :: saut(3), am(3), dsidep(6, 6)
    real(kind=8) :: tau1(3), tau2(3), nd(3)
    real(kind=8) :: alpha(3), p(3, 3)
    real(kind=8) :: cohes(3), rela, r
    character(len=8) :: job
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
    integer :: i, ier
!
    real(kind=8) :: vim(9), vip(9), lamb(3)
    real(kind=8) :: delta(6), sqrnor, sqrtan, r8prem, eps
!
    character(len=16) :: option
!
! ----------------------------------------------------------------------
!
! --- INIIALISATIONS
!
    call vecini(3, 0.d0, am)
    call matini(3, 3, 0.d0, p)
    call matini(6, 6, 0.d0, dsidep)
    call vecini(6, 0.d0, delta)
    call vecini(9, 0.d0, vim)
    call vecini(9, 0.d0, vip)
!
! --- ON CONSTRUIT P MATRICE DE PASSAGE BASE FIXE --> BASE COVARIANTE
!
    do 1 i = 1, ndim
        p(1,i) = nd(i)
 1  end do
    do 2 i = 1, ndim
        p(2,i) = tau1(i)
 2  end do
    if (ndim .eq. 3) then
        do 3 i = 1, ndim
            p(3,i) = tau2(i)
 3      continue
    endif
!
! --- CALCUL SAUT DE DEPLACEMENT EN BASE LOCALE {AM}=[P]{SAUT}
! --- ON UTILISE L UTILITAIRE PRMAVE : PRODUIT MATRICE-VECTEUR
!
    call prmave(0, p, 3, ndim, ndim,&
                saut, ndim, am, ndim, ier)
!
! --- INVERSION DE CONVENTIONS ENTRE X-FEM ET ROUTINE COMPORTEMENT
!
    do 4 i = 1, ndim
        am(i) = -am(i)
 4  end do
!
! SI ON VEUT SIMPLEMENT LE SAUT LOCAL, ON S ARRETE ICI
!
    if (job .ne. 'SAUT_LOC') then
!
! --- CALCUL VECTEUR ET MATRICE TANGENTE EN BASE LOCALE
!
        vim(4) = cohes(1)
        vim(2) = cohes(2)
!
! --- PREDICTION: COHES(3)=1, CORRECTION: COHES(3)=2
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
        if (rela .eq. 3.d0) then
            call lceitc('RIGI', ipgf, 1, imate, option,&
                        lamb, am, delta, dsidep, vim,&
                        vip, r)
        else if (rela.eq.4.d0) then
            call lceiou('RIGI', ipgf, 1, imate, option,&
                        lamb, am, delta, dsidep, vim,&
                        vip, r)
!
        endif
!
! VARIABLES INTERNES ACTUALISEES
!
        alpha(1) = vip(4)
        alpha(2) = vip(2)
! SI ACTUALISATION: NOUVEAU PAS DONC PREDICTION EN PERSPECTIVE
! SINON, DESCENTE
!
        if (job .eq. 'ACTU_VI') then
            alpha(3) = 1.d0
        else if (job.eq.'MATRICE') then
            alpha(3) = 2.d0
        endif
!
    endif
end subroutine
