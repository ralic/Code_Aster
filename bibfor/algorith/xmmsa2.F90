subroutine xmmsa2(ndim, ipgf, imate, saut, nd,&
                  tau1, tau2, cohes, job, rela,&
                  alpha, dsidep, sigma, pp, dnor,&
                  dtang, p, am)
    implicit none
    include 'jeveux.h'
    include 'asterfort/lcejex.h'
    include 'asterfort/lcejli.h'
    include 'asterfort/matini.h'
    include 'asterfort/vecini.h'
    include 'asterfort/xmafr1.h'
    integer :: ndim, ipgf, imate
    real(kind=8) :: saut(3), am(3), pp(3, 3), dsidep(6, 6)
    real(kind=8) :: tau1(3), tau2(3), nd(3)
    real(kind=8) :: alpha(3), p(3, 3)
    real(kind=8) :: dtang(3), dnor(3), cohes(3), rela
    character(len=8) :: job
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
    integer :: i, k
!
    real(kind=8) :: vim(9), vip(9), jeu
    real(kind=8) :: am2d(2), dam2d(2), dsid2d(6, 6), dam(3)
    real(kind=8) :: sigma(6), sqrnor, sqrtan, r8prem, eps
!
    character(len=16) :: option
!
! ----------------------------------------------------------------------
!
    call vecini(3, 0.d0, am)
    call vecini(3, 0.d0, dam)
    call matini(3, 3, 0.d0, pp)
    call matini(3, 3, 0.d0, p)
    call matini(6, 6, 0.d0, dsidep)
    call matini(6, 6, 0.d0, dsid2d)
    call vecini(6, 0.d0, sigma)
    call vecini(9, 0.d0, vim)
    call vecini(9, 0.d0, vip)
    call vecini(3, 0.d0, dtang)
    call vecini(3, 0.d0, dnor)
!
    call xmafr1(3, nd, p)
!
! --- CALCUL DU SAUT DE DEPLACEMENT AM EN BASE LOCALE
!
    do 218 i = 1, ndim
        dtang(i) = 0.d0
        dnor(i) = 0.d0
        do 219 k = 1, ndim
            pp(i,k)=nd(i)*nd(k)
            dtang(i)=dtang(i)+p(i,k)*saut(k)
            dnor(i) = dnor(i) +pp(i,k)*saut(k)
219      continue
!
! --- L'INTERPENETRATION CORRESPOND A SAUT<0 DANS LCEJEX
!
        am(1) = am(1) - dnor(i)*nd(i)
        am(2) = am(2) - dtang(i)*tau1(i)
        am(3) = am(3) - dtang(i)*tau2(i)
218  end do
!
! --- CALCUL VECTEUR ET MATRICE TANGENTE EN BASE LOCALE
!
    if (job .ne. 'SAUT_LOC') then
        vim(1)=cohes(1)
        vim(2)=cohes(2)
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
        if (ndim .eq. 2) then
            am2d(1)=am(1)
            am2d(2)=am(2)
            dam2d(1)=dam(1)
            dam2d(2)=dam(2)
            if (rela .eq. 1.d0) then
                call lcejex('RIGI', ipgf, 1, 2, imate,&
                            option, am2d, dam2d, sigma, dsid2d,&
                            vim, vip)
            else if (rela.eq.2.d0) then
                call lcejli('RIGI', ipgf, 1, 2, imate,&
                            option, am2d, dam2d, sigma, dsid2d,&
                            vim, vip)
            endif
            dsidep(1,1)=dsid2d(1,1)
            dsidep(1,2)=dsid2d(1,2)
            dsidep(2,1)=dsid2d(2,1)
            dsidep(2,2)=dsid2d(2,2)
        else if (ndim.eq.3) then
            if (rela .eq. 1.d0) then
                call lcejex('RIGI', ipgf, 1, ndim, imate,&
                            option, am, dam, sigma, dsidep,&
                            vim, vip)
            else if (rela.eq.2.d0) then
                call lcejli('RIGI', ipgf, 1, ndim, imate,&
                            option, am, dam, sigma, dsidep,&
                            vim, vip)
            endif
        endif
!
        alpha(1) = vip(1)
        alpha(2) = vip(2)
        eps = 1.d-4
        if (alpha(1) .le. (cohes(1)*(1.d0+eps))) alpha(2)=0.d0
!        IF(ALPHA(1).GT.1.01*COHES(1)) THEN
!            ALPHA(2)=1.D0
!        ELSE
!            ALPHA(2)=0.D0
!         ENDIF
!
        if (job .eq. 'ACTU_VI') then
            alpha(3) = 1.d0
        else if (job.eq.'MATRICE') then
            alpha(3) = 2.d0
        endif
!
    endif
end subroutine
