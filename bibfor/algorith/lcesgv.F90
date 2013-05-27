subroutine lcesgv(fami, kpg, ksp, neps, typmod,&
                  option, mat, epsm, deps, vim,&
                  itemax, precvg, sig, vip, dsidep,&
                  iret)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    implicit none
    include 'asterfort/lcerma.h'
    include 'asterfort/lcervf.h'
    include 'asterfort/lcesrf.h'
    include 'asterfort/lcesvf.h'
    include 'asterfort/lcgrad.h'
    include 'asterfort/r8inir.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    character(len=8) :: typmod
    character(len=16) :: option
    character(len=*) :: fami
    integer :: neps, mat, iret, kpg, ksp, itemax
    real(kind=8) :: epsm(neps), deps(neps), vim(*), precvg
    real(kind=8) :: vip(*), sig(neps), dsidep(neps, neps)
! ----------------------------------------------------------------------
!           ENDOMMAGEMENT FRAGILE A GRADIENT DE VARIABLE INTERNE :
!                       ENDO_SCALAIRE AVEC GRAD_VARI
! ----------------------------------------------------------------------
! IN  NEPS    DIMENSION DES DEFORMATIONS GENERALISEES
! IN  TYPMOD  TYPE DE MODELISATION
! IN  OPTION  OPTION DE CALCUL
!               RIGI_MECA_TANG, RIGI_MECA_ELAS
!               RAPH_MECA
!               FULL_MECA, FULL_MECA_ELAS
! IN  MAT     NATURE DU MATERIAU
! IN  EPSM    CHAMP DE DEFORMATION EN T- ET PHIM=EPSM(7)
! IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION ET DPHI=DEPS(7)
! IN  VIM     VARIABLES INTERNES EN T-
! IN  NONLOC  INUTILISE
! IN  ITEMAX  NBR MAXI D'ITERATIONS POUR RESOLUTION EQUATION SCALAIRE
! IN  PRECVG  CRITERE DE CVG : A ET A+PRECVG ENCADRENT LA SOLUTION
! OUT VIP     DENSITE DE FISSURATION
! OUT SIG     CONTRAINTE
! OUT DSIDEP  MATRICE TANGENTE
! OUT IRET    CODE RETOUR (0=OK, 1=ECHEC CVG)
! ----------------------------------------------------------------------
    logical :: cplan, rigi, resi, elas
    integer :: ndim, ndimsi, ij, kl, etat
    real(kind=8) :: kron(6)
    real(kind=8) :: phi, lag, apg, grad(3)
    real(kind=8) :: coplan, cor33, vplan(6), eps(6), sigel(6), treps, epsdv(6)
    real(kind=8) :: sigma(6), a, drda, drdae, drdas, gel, gsat, ktg(6, 6, 4)
    real(kind=8) :: ra, fd, d2rda2, dgda, gameps, dgamde(6), coefg
    character(len=1) :: poum
! ----------------------------------------------------------------------
    real(kind=8) :: lambda, deuxmu, troisk, rigmin, pc, pr, epsth
    common /lcee/ lambda,deuxmu,troisk,rigmin,pc,pr,epsth
! ----------------------------------------------------------------------
    real(kind=8) :: pk, pm, pp
    common /lces/ pk,pm,pp
! ----------------------------------------------------------------------
    real(kind=8) :: pct, pch, pcs
    common /lcer/ pch,pct,pcs
! ----------------------------------------------------------------------
    data kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
! ----------------------------------------------------------------------
!
!
! ----------------------------------------------------------------------
!                          INITIALISATIONS
! ----------------------------------------------------------------------
!
!
! -- OPTIONS DE CALCUL
!
    cplan = typmod.eq.'C_PLAN  '
    elas = option(11:14).eq.'ELAS'
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
    resi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RAPH'
    ndim = (neps-2)/3
    ndimsi = 2*ndim
    iret = 0
    poum='-'
    if (resi) poum='+'
!
!
! -- LECTURE DES CARACTERISTIQUES MATERIAU
!
    call lcerma(mat, fami, kpg, ksp, poum)
!
!
! -- DEFORMATIONS COURANTES
!
!    DEFORMATION, ENDOMMAGEMENT, LAGRANGE ET GRADIENT
    call dcopy(ndimsi, epsm, 1, eps, 1)
    call dcopy(ndim, epsm(ndimsi+3), 1, grad, 1)
    apg = epsm(ndimsi+1)
    lag = epsm(ndimsi+2)
!
    if (resi) then
        call daxpy(ndimsi, 1.d0, deps, 1, eps,&
                   1)
        call daxpy(ndim, 1.d0, deps(ndimsi+3), 1, grad,&
                   1)
        apg = apg + deps(ndimsi+1)
        lag = lag + deps(ndimsi+2)
    endif
!
    phi= lag + pr*apg
!
!    DEFORMATIONS MECANIQUES
    eps(1) = eps(1) - epsth
    eps(2) = eps(2) - epsth
    eps(3) = eps(3) - epsth
!
!    CONTRAINTES PLANES
    if (cplan) then
        coplan = -lambda / (lambda + deuxmu)
        eps(3) = coplan*(eps(1)+eps(2))
    endif
!
!
! -- PSEUDO-ENERGIE DE DEFORMATION ET CONTRAINTE ELASTIQUE
!
    call lcervf(0, ndimsi, eps, treps, epsdv,&
                gameps, dgamde)
!
    do 15 ij = 1, ndimsi
        sigel(ij) = lambda*treps*kron(ij) + deuxmu*eps(ij)
15  end do
!
!
! ----------------------------------------------------------------------
!                     CALCUL DE L'ENDOMMAGEMENT
! ----------------------------------------------------------------------
!
    a = vim(1)
    etat = nint(vim(2))
!
    if (.not.resi) goto 5000
!
!
!    ESTIMATION DU CRITERE
    if (etat .eq. 2) goto 2000
!
!
!    PREDICTION ELASTIQUE
!
    drdae = lcesvf(1,a)
    gel = drdae*gameps + pk - phi + pr*a
!
    if (gel .ge. 0) then
        etat = 0
        goto 2000
    endif
!
!
!    PREDICTION SATUREE
!
    drdas = lcesvf(1,1.d0)
    gsat = drdas*gameps + pk - phi + pr
    if (gsat .le. 0) then
        etat = 2
        a = 1.d0
        goto 2000
    endif
!
!
!    RESOLUTION DE L'EQUATION G(A)=0
    etat = 1
    a = lcesrf(a,gameps,pr,pk-phi,precvg,itemax,iret)
    if (iret .ne. 0) goto 9999
!
!    PROJECTION DE A+ ENTRE A- ET 1.D0
    if (a .le. vim(1)) then
        a = vim(1)
    else if (a.gt.1.d0) then
        etat = 2
        a = 1.d0
    endif
!
!    STOCKAGE DES CONTRAINTES ET DES VARIABLES INTERNES
!
2000  continue
!
    ra = lcesvf(0,a)
    do 20 ij = 1, ndimsi
        sigma(ij) = ra*sigel(ij)
20  end do
!
    vip(1) = a
    vip(2) = etat
    vip(3) = 1.d0-ra
!
5000  continue
!
!
! ----------------------------------------------------------------------
!                     CALCUL DES MATRICES TANGENTES
! ----------------------------------------------------------------------
!
    if (.not. rigi) goto 8000
!
    call r8inir(36*4, 0.d0, ktg, 1)
!
!
! -- CONTRIBUTION ELASTIQUE
!
    ra = lcesvf(0,a)
    fd = max(ra, rigmin)
    do 80 ij = 1, 3
        do 90 kl = 1, 3
            ktg(ij,kl,1) = fd*lambda
90      continue
80  end do
    do 100 ij = 1, ndimsi
        ktg(ij,ij,1) = ktg(ij,ij,1) + fd*deuxmu
100  end do
!
!
! -- CORRECTION DISSIPATIVE
    if (etat .eq. 1 .and. .not.elas) then
!
        call lcervf(1, ndimsi, eps, treps, epsdv,&
                    gameps, dgamde)
        drda = lcesvf(1,a)
        d2rda2 = lcesvf(2,a)
        dgda = d2rda2*gameps+pr
        coefg = drda**2 / dgda
!
        do 200 ij = 1, ndimsi
            do 210 kl = 1, ndimsi
                ktg(ij,kl,1) = ktg(ij,kl,1) - coefg*sigel(ij)*dgamde( kl)
210          continue
            ktg(ij,1,2) = drda/dgda * sigel(ij)
            ktg(ij,1,3) = -drda/dgda * dgamde(ij)
200      continue
        ktg(1,1,4) = 1/dgda
!
    endif
!
!
! -- CORRECTION POUR LES CONTRAINTES PLANES
!
    if (cplan) then
!
        cor33 = coplan**2*ktg(3,3,1)
        do 130 ij = 1, ndimsi
            vplan(ij) = coplan * ktg(ij,3,1)
130      continue
        do 140 ij = 1, ndimsi
            ktg(ij,1,1) = ktg(ij,1,1) + vplan(ij)
            ktg(ij,2,1) = ktg(ij,2,1) + vplan(ij)
            ktg(1,ij,1) = ktg(1,ij,1) + vplan(ij)
            ktg(2,ij,1) = ktg(2,ij,1) + vplan(ij)
140      continue
        ktg(1,1,1) = ktg(1,1,1) + cor33
        ktg(1,2,1) = ktg(1,2,1) + cor33
        ktg(2,1,1) = ktg(2,1,1) + cor33
        ktg(2,2,1) = ktg(2,2,1) + cor33
!
        ktg(1,1,2) = ktg(1,1,2) + coplan*ktg(3,1,2)
        ktg(2,1,2) = ktg(2,1,2) + coplan*ktg(3,1,2)
        ktg(1,1,3) = ktg(1,1,3) + coplan*ktg(3,1,3)
        ktg(2,1,3) = ktg(2,1,3) + coplan*ktg(3,1,3)
!
    endif
!
!
! -- PRISE EN CHARGE DES TERMES DU LAGRANGIEN AUGMENTE
!
8000  continue
    call lcgrad(resi, rigi, ndim, ndimsi, neps,&
                sigma, apg, lag, grad, a,&
                pr, pc, ktg, sig, dsidep)
!
!
9999  continue
end subroutine
