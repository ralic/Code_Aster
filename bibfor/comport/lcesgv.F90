subroutine lcesgv(fami, kpg, ksp, neps, typmod,&
                  option, mat, lccrma, lcesga, epsm,&
                  deps, vim, fige, itemax, precvg,&
                  sig, vip, dsidep, iret)
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
#include "asterf_types.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
#include "asterfort/lcesma.h"
#include "asterfort/lcesrf.h"
#include "asterfort/lcesvf.h"
#include "asterfort/lcgrad.h"
#include "asterfort/r8inir.h"
#include "asterc/r8prem.h"
    interface
        subroutine lccrma(mat, fami, kpg, ksp, poum)
            integer, intent(in) :: mat, kpg, ksp
            character(len=1), intent(in) :: poum
            character(len=*), intent(in) :: fami
        end subroutine lccrma
!
        subroutine lcesga(mode, eps, gameps, dgamde, itemax,&
                          precvg, iret)
            integer, intent(in) :: mode, itemax
            real(kind=8), intent(in) :: eps(6), precvg
            integer, intent(out) :: iret
            real(kind=8), intent(out) :: gameps, dgamde(6)
        end subroutine lcesga
    end interface
!
    aster_logical :: fige
    character(len=8) :: typmod(*)
    character(len=16) :: option
    character(len=*) :: fami
    integer :: neps, mat, iret, kpg, ksp, itemax
    real(kind=8) :: epsm(neps), deps(neps), vim(*), precvg
    real(kind=8) :: vip(*), sig(neps), dsidep(neps, neps)
! --------------------------------------------------------------------------------------------------
!           ENDOMMAGEMENT FRAGILE A GRADIENT DE VARIABLE INTERNE :
!                       ENDO_SCALAIRE AVEC GRAD_VARI
! --------------------------------------------------------------------------------------------------
! IN  NEPS    DIMENSION DES DEFORMATIONS GENERALISEES
! IN  TYPMOD  TYPE DE MODELISATION
! IN  OPTION  OPTION DE CALCUL
!               RIGI_MECA_TANG, RIGI_MECA_ELAS
!               RAPH_MECA
!               FULL_MECA, FULL_MECA_ELAS
! IN  MAT     NATURE DU MATERIAU
! IN  LCCRMA  ROUTINE POUR LECTURE MATERIAU SPECIFIQUE AU CRITERE
! IN  LCGVGA  ROUTINE POUR CALCUL GAMMA(EPS) SPECIFIQUE AU CRITERE
! IN  EPSM    CHAMP DE DEFORMATION EN T- ET PHIM=EPSM(7)
! IN  DEPS    INCREMENT DU CHAMP DE DEFORMATION ET DPHI=DEPS(7)
! IN  VIM     VARIABLES INTERNES EN T-
! IN  FIGE    .TRUE. SSI ON FIGE L'ENDOMMAGEMENT (ENDO_FISS_FIGE)
! IN  NONLOC  INUTILISE
! IN  ITEMAX  NBR MAXI D'ITERATIONS POUR RESOLUTION EQUATION SCALAIRE
! IN  PRECVG  CRITERE DE CVG : 10 A 100 FOIS PLUS FIN QUE RESI_REFE_RELA
! OUT VIP     DENSITE DE FISSURATION
! OUT SIG     CONTRAINTE
! OUT DSIDEP  MATRICE TANGENTE
! OUT IRET    CODE RETOUR (0=OK, 1=ECHEC CVG)
! --------------------------------------------------------------------------------------------------
    real(kind=8), dimension(6), parameter :: kr = (/1.d0, 1.d0, 1.d0, 0.d0, 0.d0, 0.d0/)
! --------------------------------------------------------------------------------------------------
    aster_logical :: cplan, rigi, resi, elas
    integer :: ndim, ndimsi, ij, kl, etat
    real(kind=8) :: phi, lag, apg, grad(3)
    real(kind=8) :: coplan, cor33, vplan(6), eps(6), sigel(6), treps
    real(kind=8) :: sigma(6), a, drda, drdae, drdas, gel, gsat, ktg(6, 6, 4)
    real(kind=8) :: ra, fd, d2rda2, dgda, gameps, dgamde(6), coefg
    real(kind=8) :: yng, nrmela, sigref, a0, d1a0, preca, precga
    character(len=1) :: poum
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: lambda, deuxmu, troisk, rigmin, pc, pr, epsth
    common /lcee/ lambda,deuxmu,troisk,rigmin,pc,pr,epsth
! --------------------------------------------------------------------------------------------------
    real(kind=8) :: pk, pm, pp, pq
    common /lces/ pk,pm,pp,pq
! --------------------------------------------------------------------------------------------------
!
!
! --------------------------------------------------------------------------------------------------
!                          INITIALISATIONS
! --------------------------------------------------------------------------------------------------
!
!
! -- OPTIONS DE CALCUL
!
    cplan = typmod(1).eq.'C_PLAN  '
    elas = option(11:14).eq.'ELAS'
    rigi = option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL'
    resi = option(1:4).eq.'FULL' .or. option(1:4).eq.'RAPH'
    ndim = (neps-2)/3
    eps(5) = 0
    eps(6) = 0
    ndimsi = 2*ndim
    iret = 0
    poum = merge('+','-',resi)
!
!
! -- LECTURE DES CARACTERISTIQUES MATERIAU
!
    call lcesma(mat, fami, kpg, ksp, poum,&
                lccrma)
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
!  CONTRAINTE ELASTIQUE
    treps = eps(1)+eps(2)+eps(3)
    sigel = lambda*treps*kr + deuxmu*eps
!
!
!  SEUIL DES CRITERES DE CONVERGENCE
    yng = deuxmu*(3*lambda+deuxmu)/(2*lambda+deuxmu)
    sigref = sqrt(2*yng*pk/pm)
    nrmela = sqrt(dot_product(sigel,sigel))
    a0 = vim(1)
    d1a0 = abs(lcesvf(1,a0))
    preca = precvg*minval((/1.d0, pk/pr, sigref/max(r8prem(),d1a0*nrmela)/))
    precga = pr*preca/max(r8prem(),d1a0)
!     write (6,*) 'preca = ',preca
!
!
!
! -- PSEUDO-ENERGIE DE DEFORMATION ET CONTRAINTE ELASTIQUE
!
!
    call lcesga(0, eps, gameps, dgamde, itemax,&
                precga, iret)
    if (iret .ne. 0) goto 999
!
!
!
! --------------------------------------------------------------------------------------------------
!                     CALCUL DE L'ENDOMMAGEMENT
! --------------------------------------------------------------------------------------------------
!
    a = vim(1)
    etat = nint(vim(2))
!
    if (.not.resi) goto 500
!
!
!    PAS DE CALCUL D'ENDOMMAGEMENT POUR UN POINT SATURE
    if (etat .eq. 2) goto 200
!
!
!    PAS DE CALCUL D'ENDOMMAGEMENT AVEC ENDO_FISS_FIGE
    if (fige) goto 200
!
!
!    ESTIMATION DU CRITERE
!
!    PREDICTION ELASTIQUE
!
    drdae = lcesvf(1,a)
    gel = drdae*gameps + pk - phi + pr*a
!
    if (gel .ge. 0) then
        etat = 0
        goto 200
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
        goto 200
    endif
!
!
!    RESOLUTION DE L'EQUATION G(A)=0
    etat = 1
    a = lcesrf(a,gameps,pr,pk-phi,preca,itemax,iret)
    if (iret .ne. 0) goto 999
!
!    PROJECTION DE A+ ENTRE A- ET 1.D0
    if (a .le. vim(1)) then
        a = vim(1)
    else if (a.gt.1.d0) then
        etat = 2
        a = 1.d0
    endif
!
!
!    STOCKAGE DES CONTRAINTES ET DES VARIABLES INTERNES
!
200 continue
!
    ra = lcesvf(0,a)
    sigma = ra*sigel
!
    vip(1) = a
    vip(2) = etat
    vip(3) = 1.d0-ra
!
500 continue
!
!
! --------------------------------------------------------------------------------------------------
!                     CALCUL DES MATRICES TANGENTES
! --------------------------------------------------------------------------------------------------
!
    if (.not. rigi) goto 800
!
    ktg=0
!
!
! -- CONTRIBUTION ELASTIQUE
!
    ra = lcesvf(0,a)
    fd = max(ra, rigmin)
    do ij = 1, 3
        do kl = 1, 3
            ktg(ij,kl,1) = fd*lambda
        end do
    end do
    do ij = 1, ndimsi
        ktg(ij,ij,1) = ktg(ij,ij,1) + fd*deuxmu
    end do
!
!
! -- CORRECTION DISSIPATIVE
    if (etat .eq. 1 .and. .not.elas .and. .not.fige) then
!
        call lcesga(1, eps, gameps, dgamde, itemax,&
                    precga, iret)
        drda = lcesvf(1,a)
        d2rda2 = lcesvf(2,a)
        dgda = d2rda2*gameps+pr
        coefg = drda**2 / dgda
!
        do ij = 1, ndimsi
            do kl = 1, ndimsi
                ktg(ij,kl,1) = ktg(ij,kl,1) - coefg*sigel(ij)*dgamde( kl)
            end do
            ktg(ij,1,2) = drda/dgda * sigel(ij)
            ktg(ij,1,3) = -drda/dgda * dgamde(ij)
        end do
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
        do ij = 1, ndimsi
            vplan(ij) = coplan * ktg(ij,3,1)
        end do
        do ij = 1, ndimsi
            ktg(ij,1,1) = ktg(ij,1,1) + vplan(ij)
            ktg(ij,2,1) = ktg(ij,2,1) + vplan(ij)
            ktg(1,ij,1) = ktg(1,ij,1) + vplan(ij)
            ktg(2,ij,1) = ktg(2,ij,1) + vplan(ij)
        end do
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
800 continue
    call lcgrad(resi, rigi, ndim, ndimsi, neps,&
                sigma, apg, lag, grad, a,&
                pr, pc, ktg, sig, dsidep)
!
!
999 continue
end subroutine
