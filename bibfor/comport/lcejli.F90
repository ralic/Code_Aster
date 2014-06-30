subroutine lcejli(fami, kpg, ksp, ndim, mate,&
                  option, am, da, sigma, dsidep,&
                  vim, vip)
!
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
! person_in_charge: jerome.laverne at edf.fr
!
    implicit none
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    integer :: mate, ndim, kpg, ksp
    real(kind=8) :: am(ndim), da(ndim), sigma(6), dsidep(6, 6)
    real(kind=8) :: vim(*), vip(*)
    character(len=16) :: option
    character(len=*) :: fami
!
!-----------------------------------------------------------------------
! LOI DE COMPORTEMENT COHESIVE : CZM_LIN_REG
! POUR LES ELEMENTS DE JOINT 2D ET 3D
!
! IN : AM SAUT INSTANT - : AM(1) = SAUT NORMAL, AM(2) = SAUT TANGENTIEL
! IN : DA    INCREMENT DE SAUT
! IN : MATE, OPTION, VIM
! OUT : SIGMA , DSIDEP , VIP
!-----------------------------------------------------------------------
!
    logical(kind=1) :: resi, rigi, elas
    integer :: i, j, diss, cass
    real(kind=8) :: sc, gc, lc, k0, val(4), rtan, zero, un
    real(kind=8) :: a(ndim), na, ka, kap, r0, rc, beta, rk, ra, coef, coef2
    integer :: cod(5)
    character(len=8) :: nom(4)
    character(len=1) :: poum
    parameter  (zero = 0.d0, un = 1.d0)
!
! OPTION CALCUL DU RESIDU OU CALCUL DE LA MATRICE TANGENTE
!
    resi = option(1:9).eq.'FULL_MECA' .or. option.eq.'RAPH_MECA'
    rigi = option(1:9).eq.'FULL_MECA' .or. option(1:9).eq.'RIGI_MECA'
    elas = option.eq.'FULL_MECA_ELAS' .or. option.eq.'RIGI_MECA_ELAS'
!
!
! CALCUL DU SAUT EN T+
!
    call dcopy(ndim, am, 1, a, 1)
    if (resi) call daxpy(ndim, 1.d0, da, 1, a,&
                         1)
!
!
! RECUPERATION DES PARAMETRES PHYSIQUES
!
    nom(1) = 'GC'
    nom(2) = 'SIGM_C'
    nom(3) = 'PENA_ADHERENCE'
    nom(4) = 'PENA_CONTACT'
!
    if (option .eq. 'RIGI_MECA_TANG') then
        poum = '-'
    else
        poum = '+'
    endif
!
    call rcvalb(fami, kpg, ksp, poum, mate,&
                ' ', 'RUPT_FRAG', 0, ' ', [0.d0],&
                4, nom, val, cod, 2)
!
    gc = val(1)
    sc = val(2)
    lc = 2*gc/sc
    k0 = lc*val(3)
    r0 = sc*(1.d0-k0/lc)/k0
    beta = val(4)
!
! INITIALISATION
!
    ka = max(k0,vim(1))
    rtan = 0.d0
    do 10 i = 2, ndim
        rtan=rtan+a(i)**2
10  end do
    na = sqrt( max(zero,a(1))**2 + rtan )
!
    rk = sc*(1.d0-ka/lc)/ka
    rc = rk + beta*(r0-rk)
!
!
! INITIALISATION COMPLEMENTAIRE POUR RIGI_MECA_TANG (SECANTE PENALISEE)
!
    if (.not. resi) then
!
        if (elas) then
            diss = 0
        else
            diss = nint(vim(2))
        endif
!
        cass = nint(vim(3))
!
        goto 5000
    endif
!
! CALCUL DE LA CONTRAINTE
!
    call r8inir(6, 0.d0, sigma, 1)
!
!     CONTRAINTE DE CONTACT PENALISE
    sigma(1) = rc * min(zero,a(1))
!
!     CONTRAINTE DE FISSURATION
    if ((na.ge.lc) .or. (ka.ge.lc)) then
!
        diss = 0
        cass = 2
!
    else
!
        if (na .le. ka) then
!
            diss = 0
            if (ka .gt. k0) then
                cass = 1
            else
                cass = 0
            endif
            sigma(1) = sigma(1) + rk*max(zero,a(1))
            do 20 i = 2, ndim
                sigma(i) = sigma(i) + rk*a(i)
20          continue
!
        else
!
            diss = 1
            cass = 1
            ra = sc*(1.d0 - na/lc)/na
            sigma(1) = sigma(1) + ra*max(zero,a(1))
            do 30 i = 2, ndim
                sigma(i) = sigma(i) + ra*a(i)
30          continue
!
        endif
!
    endif
!
! ACTUALISATION DES VARIABLES INTERNES
!   V1 :  SEUIL, PLUS GRANDE NORME DU SAUT
!   V2 :  INDICATEUR DE DISSIPATION (0 : NON, 1 : OUI)
!   V3 :  INDICATEUR D'ENDOMMAGEMENT  (0 : SAIN, 1: ENDOM, 2: CASSE)
!   V4 :  POURCENTAGE D'ENERGIE DISSIPEE
!   V5 :  VALEUR DE L'ENERGIE DISSIPEE (V4*GC)
!   V6 :  VALEUR DE L'ENERGIE RESIDUELLE COURANTE
!   V7 A V9 : VALEURS DU SAUT
!
    kap = max(ka,na)
    vip(1) = kap
    vip(2) = diss
    vip(3) = cass
    vip(4) = min(un,kap/lc)
    vip(5) = gc*vip(4)
!
    if (cass .ne. 2) then
        vip(6) = 0.5d0*(na**2)*sc*(1.d0 - kap/lc)/kap
    else
        vip(6) = 0.d0
    endif
!
    vip(7) = a(1)
    vip(8) = a(2)
    if (ndim .eq. 3) then
        vip(9) = a(3)
    else
        vip(9) = 0.d0
    endif
!
!
! -- MATRICE TANGENTE
!
5000  continue
    if (.not. rigi) goto 9999
!
    call r8inir(36, 0.d0, dsidep, 1)
!
!    MATRICE TANGENTE DE CONTACT PENALISE
    if (a(1) .le. 0.d0) dsidep(1,1) = dsidep(1,1) + rc
!
! DANS LE CAS OU L'ELEMENT EST TOTALEMENT CASSE ON INTRODUIT UNE
! RIGIDITE ARTIFICIELLE DANS LA MATRICE TANGENTE POUR ASSURER
! LA CONVERGENCE
!
    if (cass .eq. 2) then
        if (a(1) .gt. 0.d0) dsidep(1,1) = dsidep(1,1) - 1.d-8*sc/lc
        do 39 i = 2, ndim
            dsidep(i,i) = dsidep(i,i) - 1.d-8*sc/lc
39      continue
        goto 9999
    endif
!
!    MATRICE TANGENTE DE FISSURATION
    if ((diss.eq.0) .or. elas) then
!
        if (a(1) .gt. 0.d0) dsidep(1,1) = dsidep(1,1) + rk
!
        do 40 i = 2, ndim
            dsidep(i,i) = dsidep(i,i) + rk
40      continue
!
    else
!
        coef = sc*(1.d0/na - 1.d0/lc)
        coef2 = -sc/na**3
!
        if (a(1) .le. 0.d0) then
!
            do 42 i = 2, ndim
                dsidep(i,i) = dsidep(i,i) + coef + coef2*a(i)*a(i)
42          continue
!
            if (ndim .eq. 3) then
                dsidep(2,3) = dsidep(2,3) + coef2*a(2)*a(3)
                dsidep(3,2) = dsidep(3,2) + coef2*a(3)*a(2)
            endif
!
        else
!
            do 44 i = 1, ndim
                dsidep(i,i) = dsidep(i,i) + coef + coef2*a(i)*a(i)
44          continue
!
            do 46 j = 1, ndim-1
                do 47 i = j+1, ndim
                    dsidep(j,i) = dsidep(j,i) + coef2*a(j)*a(i)
                    dsidep(i,j) = dsidep(i,j) + coef2*a(i)*a(j)
47              continue
46          continue
!
!
        endif
!
    endif
9999  continue
!
!
end subroutine
