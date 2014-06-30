subroutine lceigv(fami, kpg, ksp, neps, imate,&
                  compor, epsm, deps, vim, option,&
                  sig, vip, dsidep)
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
#include "asterfort/diagp3.h"
#include "asterfort/lceib1.h"
#include "asterfort/lcgrad.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/zerop3.h"
#include "blas/ddot.h"
    character(len=16) :: compor(*), option
    character(len=*) :: fami
    integer :: neps, imate, ksp, kpg
    real(kind=8) :: epsm(neps), deps(neps), vim(2)
    real(kind=8) :: sig(neps), vip(2), dsidep(neps, neps)
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT ENDO_ISOT_BETON (NON LOCAL GRAD_VARI)
!
! IN  FAMI    : FAMILLE DE POINT DE GAUSS
! IN  KPG     : POINT DE GAUSS CONSIDERE
! IN  KSP     :
! IN  NEPS    : DIMENSION DES DEFORMATIONS ET DES CONTRAINTES GENERALI.
! IN  IMATE   : NATURE DU MATERIAU
! IN  COMPOR  : COMPORTEMENT :  (1) = TYPE DE RELATION COMPORTEMENT
!                               (2) = NB VARIABLES INTERNES / PG
!                               (3) = HYPOTHESE SUR LES DEFORMATIONS
! IN  EPSM    : DEFORMATION EN T-
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  VIM     : VARIABLES INTERNES EN T-
! IN  OPTION  : OPTION DEMANDEE
!                 RIGI_MECA_TANG ->     DSIDEP
!                 FULL_MECA      -> SIG DSIDEP VIP
!                 RAPH_MECA      -> SIG        VIP
! OUT SIG     : CONTRAINTE
! OUT VIP     : VARIABLES INTERNES
!                 1   -> VALEUR DE L'ENDOMMAGEMENT
!                 2   -> ETAT DE L'ENDOMMAGEMENT
!                        0: non endommage
!                        1: endommage mais < 1
!                        2: ruine endommagement = 1
! OUT DSIDEP  : MATRICE TANGENTE
! ----------------------------------------------------------------------
! LOC EDFRC1  COMMON CARACTERISTIQUES DU MATERIAU (AFFECTE DANS EDFRMA)
    logical(kind=1) :: rigi, resi, elas, coup, secant
    integer :: ndim, ndimsi, k, l, i, j, m, n, t(3, 3), iret, nrac, iok(2)
    real(kind=8) :: eps(6), treps, sigel(6), sigma(6), kron(6)
    real(kind=8) :: rac2
    real(kind=8) :: rigmin, told, fd, d, ener
    real(kind=8) :: tr(6), rtemp2
    real(kind=8) :: epsp(3), vecp(3, 3), dspdep(6, 6)
    real(kind=8) :: deumud(3), lambdd, sigp(3), rtemp, rtemp3, rtemp4
    real(kind=8) :: kdess, bendo, lambda, deuxmu, gamma
    real(kind=8) :: seuil, epsth(2)
    real(kind=8) :: phi, q2, q1, q0, etat, fel, fsat, rac(3)
    real(kind=8) :: coef1, coef2, coef3
    real(kind=8) :: hydrm, hydrp, sechm, sechp, sref
    real(kind=8) :: r, c, grad(3), ktg(6, 6, 4), apg, lag, valnl(2)
    character(len=1) :: poum
    character(len=8) :: nomnl(2)
    parameter  (rigmin = 1.d-5)
    parameter  (told = 1.d-6)
    data        kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
    data nomnl /'C_GRAD_V','PENA_LAG'/
!
! ----------------------------------------------------------------------
!
!
!
! -- OPTION ET MODELISATION
!
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
    coup = (option(6:9).eq.'COUP')
    if (coup) rigi=.true.
    ndim = (neps-2)/3
    ndimsi = 2*ndim
    rac2=sqrt(2.d0)
    secant=.false.
    poum='-'
    if (resi) poum='+'
!
    call rcvarc(' ', 'HYDR', '-', fami, kpg,&
                ksp, hydrm, iret)
    if (iret .ne. 0) hydrm=0.d0
    call rcvarc(' ', 'HYDR', '+', fami, kpg,&
                ksp, hydrp, iret)
    if (iret .ne. 0) hydrp=0.d0
    call rcvarc(' ', 'SECH', '-', fami, kpg,&
                ksp, sechm, iret)
    if (iret .ne. 0) sechm=0.d0
    call rcvarc(' ', 'SECH', '+', fami, kpg,&
                ksp, sechp, iret)
    if (iret .ne. 0) sechp=0.d0
    call rcvarc(' ', 'SECH', 'REF', fami, kpg,&
                ksp, sref, iret)
    if (iret .ne. 0) sref=0.d0
!
!
! -- INITIALISATION
!
    call lceib1(fami, kpg, ksp, imate, compor,&
                ndim, epsm, sref, sechm, hydrm,&
                t, lambda, deuxmu, epsth, kdess,&
                bendo, gamma, seuil)
!
    call rcvalb(fami, kpg, ksp, poum, imate,&
                ' ', 'NON_LOCAL', 0, ' ', [0.d0],&
                2, nomnl, valnl, iok, 2)
    c = valnl(1)
    r = valnl(2)
!
!
! -- MAJ DES DEFORMATIONS ET PASSAGE AUX DEFORMATIONS REELLES 3D
!
    if (resi) then
        do 10 k = 1, ndimsi
            eps(k) = epsm(k) + deps(k) - kron(k) * ( epsth(2) - kdess * (sref-sechp) - bendo * hy&
                     &drp )
10      continue
        apg = epsm(ndimsi+1) + deps(ndimsi+1)
        lag = epsm(ndimsi+2) + deps(ndimsi+2)
        do 11 k = 1, ndim
            grad(k) = epsm(ndimsi+2+k) + deps(ndimsi+2+k)
!
11      continue
    else
        do 40 k = 1, ndimsi
            eps(k) = epsm(k) - ( epsth(1) - kdess * (sref-sechm) - bendo * hydrm ) * kron(k)
40      continue
        apg = epsm(ndimsi+1)
        lag = epsm(ndimsi+2)
        do 41 k = 1, ndim
            grad(k) = epsm(ndimsi+2+k)
41      continue
    endif
!
    do 45 k = 4, ndimsi
        eps(k) = eps(k)/rac2
45  end do
    if (ndimsi .lt. 6) then
        do 46 k = ndimsi+1, 6
            eps(k)=0.d0
46      continue
    endif
!
    phi= lag + r*apg
!
!
! -- DIAGONALISATION DES DEFORMATIONS
!
    tr(1) = eps(1)
    tr(2) = eps(4)
    tr(3) = eps(5)
    tr(4) = eps(2)
    tr(5) = eps(6)
    tr(6) = eps(3)
    call diagp3(tr, vecp, epsp)
!
! -- CALCUL DES CONTRAINTES ELAS POSITIVES ET DE L'ENERGIE POSITIVE
!
    treps = eps(1)+eps(2)+eps(3)
    if (treps .ge. 0.d0) then
        do 60 k = 1, 3
            sigel(k) = lambda*treps
60      continue
    else
        do 61 k = 1, 3
            sigel(k) = 0.d0
61      continue
    endif
    do 15 k = 1, 3
        if (epsp(k) .ge. 0.d0) then
            sigel(k) = sigel(k) + deuxmu*epsp(k)
        endif
15  end do
    ener = 0.5d0 * ddot(3,epsp,1,sigel,1)
!
!
! -- CALCUL (OU RECUPERATION) DE L'ENDOMMAGEMENT
    d = vim(1)
    etat = vim(2)
    elas=.true.
!
    if (.not.resi) goto 5000
!    ESTIMATION DU CRITERE
    if (etat .eq. 2) goto 2000
!
!
!      WRITE(6,*) 'ener=     ',ENER
!      WRITE(6,*) 'phi=      ',PHI
!      WRITE(6,*) 'seuil=    ',SEUIL/(1.D0+GAMMA)**2
!
!
!
    fel = (1+gamma)*ener/(1+gamma*d)**2+phi-r*d-seuil
!
!
!      WRITE(6,*) 'FEL=    ',FEL
!
!
!    CAS ELASTIQUE ?
    if (fel .le. 0) then
        etat = 0
        d = vim(1)
        goto 2000
    endif
!    CAS SATURE ?
!
    fsat = (1+gamma)*ener/(1+gamma)**2+phi-r-seuil
!
    if (fsat .ge. 0) then
        etat = 2
        d = 1.d0
!
        goto 2000
    endif
!
!     ON RESOUD SI NON ELASTIQUE ET NON SATURE
!
!
    elas=.false.
!
    q2 = (2.d0*gamma*r-(phi-seuil)*gamma**2.d0)/r/gamma**2
    q1 = (r-2.d0*gamma*(phi-seuil))/r/gamma**2
    q0 = -((phi-seuil)+(1+gamma)*ener)/r/gamma**2
!
    call zerop3(q2, q1, q0, rac, nrac)
!
    etat = 1
    d = rac(nrac)
    if (d .lt. vim(1)) then
        d = vim(1)
        elas=.true.
    else if (d.gt.(1.d0-told)) then
        d = 1.d0
        elas=.true.
        etat = 2
    endif
!
!      WRITE(6,*) 'deltaD=         ', D-VIM(1)
!
!
!
2000  continue
!
! -- CALCUL DES CONTRAINTES
!
!
!
    fd = (1-d)/(1+gamma*d)
!
    treps=epsp(1)+epsp(2)+epsp(3)
    call r8inir(3, 0.d0, sigp, 1)
!
    if (treps .ge. 0.d0) then
        lambdd=lambda * fd
    else
        lambdd=lambda
    endif
    do 201 i = 1, 3
        if (epsp(i) .ge. 0.d0) then
            deumud(i)=deuxmu*fd
        else
            deumud(i)=deuxmu
        endif
        sigp(i)=lambdd*treps+deumud(i)*epsp(i)
201  end do
!
    call r8inir(6, 0.d0, sigma, 1)
    do 1010 i = 1, 3
        rtemp=sigp(i)
        sigma(1)=sigma(1)+vecp(1,i)*vecp(1,i)*rtemp
        sigma(2)=sigma(2)+vecp(2,i)*vecp(2,i)*rtemp
        sigma(3)=sigma(3)+vecp(3,i)*vecp(3,i)*rtemp
        sigma(4)=sigma(4)+vecp(1,i)*vecp(2,i)*rtemp
        sigma(5)=sigma(5)+vecp(1,i)*vecp(3,i)*rtemp
        sigma(6)=sigma(6)+vecp(2,i)*vecp(3,i)*rtemp
1010  end do
    do 18 k = 4, ndimsi
        sigma(k)=rac2*sigma(k)
18  end do
!
!
    vip(1) = d
    vip(2) = etat
!
5000  continue
!
!
! -- CALCUL DE LA MATRICE TANGENTE
!
    if (.not.rigi) goto 9000
!
    fd=(1-d)/(1+gamma*d)
!
!
    treps=epsp(1)+epsp(2)+epsp(3)
    if (treps .ge. 0.d0) then
        lambdd=lambda * fd
    else
        lambdd=lambda
    endif
    do 203 i = 1, 3
        if (epsp(i) .ge. 0.d0) then
            deumud(i)=deuxmu*fd
        else
            deumud(i)=deuxmu
        endif
203  end do
!
    if (option(11:14) .eq. 'ELAS') secant=.true.
    call r8inir(36, 0.d0, dspdep, 1)
    call r8inir(36*4, 0.d0, ktg, 1)
!
    if (fd .lt. rigmin) then
        if (treps .ge. 0.d0) then
            lambdd=lambda * rigmin
        endif
        do 202 i = 1, 3
            if (epsp(i) .ge. 0.d0) then
                deumud(i)=deuxmu*rigmin
            endif
202      continue
    endif
!
    do 100 k = 1, 3
        do 110 l = 1, 3
            dspdep(k,l) = lambdd
110      continue
100  continue
    do 120 k = 1, 3
        dspdep(k,k) = dspdep(k,k) + deumud(k)
120  continue
    if (epsp(1)*epsp(2) .ge. 0.d0) then
        dspdep(4,4)=deumud(1)
    else
        dspdep(4,4)=(deumud(1)*epsp(1)-deumud(2)*epsp(2)) /(epsp(1)-&
        epsp(2))
    endif
    if (epsp(1)*epsp(3) .ge. 0.d0) then
        dspdep(5,5)=deumud(1)
    else
        dspdep(5,5)=(deumud(1)*epsp(1)-deumud(3)*epsp(3)) /(epsp(1)-&
        epsp(3))
    endif
    if (epsp(3)*epsp(2) .ge. 0.d0) then
        dspdep(6,6)=deumud(3)
    else
        dspdep(6,6)=(deumud(3)*epsp(3)-deumud(2)*epsp(2)) /(epsp(3)-&
        epsp(2))
    endif
!
    do 20 i = 1, 3
        do 21 j = i, 3
            if (i .eq. j) then
                rtemp3=1.d0
            else
                rtemp3=rac2
            endif
            do 22 k = 1, 3
                do 23 l = 1, 3
                    if (t(i,j) .ge. t(k,l)) then
                        if (k .eq. l) then
                            rtemp4=rtemp3
                        else
                            rtemp4=rtemp3/rac2
                        endif
                        rtemp2=0.d0
                        do 24 m = 1, 3
                            do 25 n = 1, 3
                                rtemp2=rtemp2+vecp(k,m)* vecp(i,n)*&
                                vecp(j,n)*vecp(l,m)*dspdep(n,m)
25                          continue
24                      continue
                        rtemp2=rtemp2+vecp(i,1)*vecp(j,2)*vecp(k,1)*&
                        vecp(l,2)*dspdep(4,4)
                        rtemp2=rtemp2+vecp(i,2)*vecp(j,1)*vecp(k,2)*&
                        vecp(l,1)*dspdep(4,4)
                        rtemp2=rtemp2+vecp(i,1)*vecp(j,3)*vecp(k,1)*&
                        vecp(l,3)*dspdep(5,5)
                        rtemp2=rtemp2+vecp(i,3)*vecp(j,1)*vecp(k,3)*&
                        vecp(l,1)*dspdep(5,5)
                        rtemp2=rtemp2+vecp(i,2)*vecp(j,3)*vecp(k,2)*&
                        vecp(l,3)*dspdep(6,6)
                        rtemp2=rtemp2+vecp(i,3)*vecp(j,2)*vecp(k,3)*&
                        vecp(l,2)*dspdep(6,6)
                        ktg(t(i,j),t(k,l),1)=ktg(t(i,j),t(k,l),1)+&
                        rtemp2*rtemp4
                    endif
23              continue
22          continue
21      continue
20  continue
!
    do 26 i = 1, 6
        do 27 j = i+1, 6
            ktg(i,j,1)=ktg(j,i,1)
27      continue
26  continue
!
!
! -- CONTRIBUTION DISSIPATIVE
    if ((.not. elas) .or. (etat.eq.1.d0)) then
!
!
!
!      TREPS = EPS(1)+EPS(2)+EPS(3)
!        DO 960 K=1,3
!          SIGEL(K) = LAMBDA*TREPS
! 960     CONTINUE
!        DO 915 K=1,6
!          SIGEL(K) = SIGEL(K) + DEUXMU*EPS(K)
! 915    CONTINUE
!
        tr(1) = sigel(1)
        tr(2) = sigel(2)
        tr(3) = sigel(3)
        call r8inir(6, 0.d0, sigel, 1)
        do 1020 i = 1, 3
            rtemp=tr(i)
            sigel(1)=sigel(1)+vecp(1,i)*vecp(1,i)*rtemp
            sigel(2)=sigel(2)+vecp(2,i)*vecp(2,i)*rtemp
            sigel(3)=sigel(3)+vecp(3,i)*vecp(3,i)*rtemp
            sigel(4)=sigel(4)+vecp(1,i)*vecp(2,i)*rtemp
            sigel(5)=sigel(5)+vecp(1,i)*vecp(3,i)*rtemp
            sigel(6)=sigel(6)+vecp(2,i)*vecp(3,i)*rtemp
1020      continue
        do 28 k = 4, ndimsi
            sigel(k)=rac2*sigel(k)
28      continue
!
        coef1=(1.d0+gamma)/(1.d0+gamma*d)**2
!
        coef2=(1.d0+gamma)/(r*(1.d0+gamma*d)**2 +2.d0*gamma*(1.d0+&
        gamma)*ener/(1.d0+gamma*d))
        coef3=(1.d0+gamma*d)**3/(r*(1.d0+gamma*d)**3 +2.d0*gamma*(&
        1.d0+gamma)*ener)
!
!
! dans le cas de la matrice secante, on enleve la partie dissipative
! seulement sur la partie meca/meca
        if (.not.secant) then
            do 200 k = 1, ndimsi
                do 210 l = 1, ndimsi
                    ktg(k,l,1)=ktg(k,l,1)-coef1*coef2*sigel(k)*sigel(&
                    l)
210              continue
200          continue
        endif
!
!
! les autres termes ne sont pas annules car ils permettent de faire
! converger sur la regularisation
        do 220 k = 1, ndimsi
            ktg(k,1,3) = coef2*sigel(k)
            ktg(k,1,2) = - ktg(k,1,3)
220      continue
        ktg(1,1,4) = coef3
!
!
    endif
!
!
9000  continue
    call lcgrad(resi, rigi, ndim, ndimsi, neps,&
                sigma, apg, lag, grad, d,&
                r, c, ktg, sig, dsidep)
!
end subroutine
