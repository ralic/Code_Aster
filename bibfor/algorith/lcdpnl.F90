subroutine lcdpnl(fami, kpg, ksp, typmod, ndim,&
                  option, compor, imate, sigm, deps,&
                  vim, vip, sig, dsidep, proj,&
                  iret)
    implicit      none
    include 'asterfort/lcdrpr.h'
    include 'asterfort/lcinma.h'
    include 'asterfort/lcprsm.h'
    include 'asterfort/lcsoma.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/rcvarc.h'
    integer :: ndim, imate, iret, ksp, kpg, iret2
    real(kind=8) :: sigm(6), deps(6, 2), vim(*), vip(*), sig(6), proj(6, 6)
    real(kind=8) :: dsidep(6, 6, 2)
    character(len=*) :: fami
    character(len=8) :: typmod(*)
    character(len=16) :: option, compor(*)
! =====================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! =====================================================================
!
!
! =====================================================================
! --- APPLICATION DE LA LOI DE COMPORTEMENT DE TYPE DRUCKER PRAGER ----
! --- LINEAIRE AVEC PRISE EN COMPTE DES PHENOMENES DE NON LOCALISATION
! =====================================================================
! IN  NDIM    DIMENSION DE L'ESPACE
! IN  OPTION  OPTION DE CALCUL (RAPH_MECA, RIGI_MECA_TANG OU FULL_MECA)
! IN  IMATE   NATURE DU MATERIAU
! IN  EPSM    CHAMP DE DEFORMATION EN T-
! IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION
! IN  VIM     VARIABLES INTERNES EN T-
!               1   : ENDOMMAGEMENT (D)
!               2   : INDICATEUR DISSIPATIF (1) OU ELASTIQUE (0)
! VAR VIP     VARIABLES INTERNES EN T+
!              IN  ESTIMATION (ITERATION PRECEDENTE)
!              OUT CALCULEES
! OUT SIGP    CONTRAINTES EN T+
! OUT DSIDEP  MATRICE TANGENTE
! OUT PROJ    PROJECTEUR DE COUPURE DU TERME DE REGULARISATION
! OUT IRET    CODE RETOUR (0 = OK)
! =====================================================================
    logical :: rigi, resi, elas
    integer :: ndimsi, i, j, ndt, ndi
    real(kind=8) :: kron(6), tre, trer, valres(2), dsdp2(6, 6), tp, tm, tref
    real(kind=8) :: deuxmu, lambda, dsdp1b(6, 6), young, nu
    integer :: icodre(2)
    character(len=8) :: nomres(2)
! =====================================================================
    common /tdim/   ndt, ndi
! =====================================================================
    data kron   /1.d0, 1.d0, 1.d0, 0.d0, 0.d0, 0.d0/
    data nomres /'E','NU'/
! =====================================================================
! --- INITIALISATION --------------------------------------------------
! =====================================================================
    ndimsi = 2*ndim
    rigi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RIGI'
    resi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RAPH'
    elas = option(11:14).eq.'ELAS'
! =====================================================================
! --- CARACTERISTIQUES MATERIAU ---------------------------------------
! =====================================================================
    call rcvalb(fami, 1, 1, '+', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                2, nomres, valres, icodre, 2)
    young = valres(1)
    nu = valres(2)
    deuxmu = young / ( 1.0d0 + nu )
    lambda = young * nu / ( 1.0d0 + nu ) / ( 1.0d0 - 2.d0 * nu )
! =====================================================================
! --- COMPORTEMENT LOCAL ----------------------------------------------
! =====================================================================
!
! APPEL DE RCVARC POUR LE CALCUL DE LA TEMPERATURE
! RAISON: CETTE ROUTINE EST APPELEE POUR LE CALCUL THERMIQUE (CALCME)
    call rcvarc(' ', 'TEMP', '-', fami, kpg,&
                ksp, tm, iret2)
    call rcvarc(' ', 'TEMP', '+', fami, kpg,&
                ksp, tp, iret2)
    call rcvarc(' ', 'TEMP', 'REF', fami, kpg,&
                ksp, tref, iret2)
    call lcdrpr(typmod, option, imate, compor, sigm,&
                tm, tp, tref, deps(1, 2), vim,&
                vip, sig, dsdp2, iret)
! =====================================================================
! --- PROJECTEUR DE COUPURE POUR LA REGULARISATION : DEFAUT------------
! =====================================================================
    call r8inir(36, 0.d0, proj, 1)
    do 5 i = 1, 6
        proj(i,i) = 1.d0
 5  end do
! =====================================================================
! --- CORRECTION NON LOCALE -------------------------------------------
! =====================================================================
    if (resi) then
        tre = deps(1,1)+deps(2,1)+deps(3,1)
        trer = deps(1,2)+deps(2,2)+deps(3,2)
!
        do 100 i = 1, ndimsi
            sig(i) = sig(i) + lambda*(tre - trer)*kron(i) + deuxmu*( deps(i,1)-deps(i,2))
100      continue
    endif
!
    if (rigi) then
!
        call lcinma(0.0d0, dsidep(1, 1, 1))
        call lcinma(0.0d0, dsdp1b)
        call lcinma(0.0d0, dsidep(1, 1, 2))
!
        do 10 i = 1, 3
            do 20 j = 1, 3
                dsidep(i,j,1) = lambda
20          continue
10      continue
!
        do 30 i = 1, ndt
            dsidep(i,i,1) = dsidep(i,i,1) + deuxmu
30      continue
!
        if (.not. elas) then
            call lcprsm(-1.0d0, dsidep(1, 1, 1), dsdp1b)
            call lcsoma(dsdp1b, dsdp2, dsidep(1, 1, 2))
        endif
    endif
! =====================================================================
end subroutine
