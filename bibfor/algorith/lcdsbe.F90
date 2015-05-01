subroutine lcdsbe(fami, ndim, typmod, imate, compor,&
                  epstm, depst, vim, option, sig,&
                  vip, dsidpt, proj)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterf_types.h"
#include "asterfort/bptobg.h"
#include "asterfort/diagp3.h"
#include "asterfort/lceib1.h"
#include "asterfort/r8inir.h"
#include "blas/ddot.h"
    character(len=*) :: fami
    character(len=8) :: typmod(2)
    character(len=16) :: option, compor(*)
    integer :: ndim, imate
    real(kind=8) :: epstm(12), depst(12), vim(2)
    real(kind=8) :: sig(6), vip(2), dsidpt(6, 6, 2), proj(6, 6)
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT ELASTIQUE ENDOMMAGEMENT BETON (EN DELOCALISE)
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : NATURE DU MATERIAU
! IN  EPSM    : DEFORMATION EN T-
! IN  EPSRM   : DEFORMATION GENERALISEE EN T-
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  DEPSR   : INCREMENT DE DEFORMATION GENERALISEE
! IN  VIM     : VARIABLES INTERNES EN T-
! IN  OPTION  : OPTION DEMANDEE
!                 RIGI_MECA_TANG ->     DSIDEP
!                 FULL_MECA      -> SIG DSIDEP VIP
!                 RAPH_MECA      -> SIG        VIP
! OUT SIG     : CONTRAINTE
! OUT VIP     : VARIABLES INTERNES
!                 1   -> VALEUR DE L'ENDOMMAGEMENT
! OUT DSIDEP  : MATRICE TANGENTE
! OUT DSIDPR  : MATRICE TANGENTE DEFO GENERALISEE
! OUT PROJ    : PROJECTEUR DE COUPURE DU TERME DE REGULARISATION
! ----------------------------------------------------------------------
! LOC EDFRC1  COMMON CARACTERISTIQUES DU MATERIAU (AFFECTE DANS EDFRMA)
    aster_logical :: rigi, resi, elas
    integer :: ndimsi, k, l, i, j, m, n, t(3, 3)
    real(kind=8) :: eps(6), epsr(6), treps, sigel(6), sigelr(6)
    real(kind=8) :: rac2, coef
    real(kind=8) :: rigmin, fd, d, ener
    real(kind=8) :: tr(6), rtemp2
    real(kind=8) :: epsp(3), vecp(3, 3), dspdep(6, 6)
    real(kind=8) :: deumud(3), lambdd, sigp(3), rtemp3, rtemp4
    real(kind=8) :: epsm(6), epsrm(6), deps(6), depsr(6)
    real(kind=8) :: epsthe(2), kdess, bendo
    real(kind=8) :: lambda, deuxmu, gamma, seuil
    parameter  (rigmin = 1.d-5)
!
! ----------------------------------------------------------------------
! ======================================================================
!                            INITIALISATION
! ======================================================================
! -- OPTION ET MODELISATION
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
    ndimsi = 2*ndim
    rac2=sqrt(2.d0)
    t(1,1)=1
    t(1,2)=4
    t(1,3)=5
    t(2,1)=4
    t(2,2)=2
    t(2,3)=6
    t(3,1)=5
    t(3,2)=6
    t(3,3)=3
!
! -- SEPARATION DE EPSM/EPSRM, DEPS/DEPSR DANS EPSTM,DEPST
!
    do 312 i = 1, ndimsi
        epsm(i)=epstm(i)
        epsrm(i)=epstm(i+6)
        deps(i)=depst(i)
        depsr(i)=depst(i+6)
312 end do
!
!    LECTURE DES CARACTERISTIQUES DU MATERIAU
!
    call lceib1(fami, 1, 1, imate, compor,&
                ndim, epsrm, 0.d0, 0.d0, 0.d0,&
                t, lambda, deuxmu, epsthe, kdess,&
                bendo, gamma, seuil)
!
!
! -- COUPURE ISOTROPE DE LA REGULARISATION SI ENDOMMAGEMENT SATURE
    call r8inir(36, 0.d0, proj, 1)
    if (vim(2) .ne. 2) call r8inir(6, 1.d0, proj, 7)
!
!
!    RECUPERATION DES DEFORMATIONS
!
    if (resi) then
!      MISE A JOUR DES DEFORMATIONS MECANIQUES
        do 10 k = 1, ndimsi
            eps(k) = epsm(k) + deps(k)
            epsr(k) = epsrm(k) + depsr(k)
 10     continue
    else
        do 40 k = 1, ndimsi
            eps(k)=epsm(k)
            epsr(k) = epsrm(k)
 40     continue
        d=vim(1)
        fd = (1 - d) / (1 + gamma*d)
        elas=((nint(vim(2)).eq.0).or.(nint(vim(2)).eq.2))
    endif
! - ON MET DANS EPS LES DEFORMATIONS REELLES
    do 45 k = 4, ndimsi
        eps(k) = eps(k)/rac2
        epsr(k) = epsr(k)/rac2
 45 end do
    if (ndimsi .lt. 6) then
        do 46 k = ndimsi+1, 6
            eps(k)=0.d0
            epsr(k)=0.d0
 46     continue
    endif
!     MATRICE TR = (XX XY XZ YY YZ ZZ)
!
    tr(1) = eps(1)
    tr(2) = eps(4)
    tr(3) = eps(5)
    tr(4) = eps(2)
    tr(5) = eps(6)
    tr(6) = eps(3)
    call diagp3(tr, vecp, epsp)
! -   CALCUL DES CONTRAINTES ELASTIQUES ASSOCIEE AUX DEFO GENERALISEES
    treps = eps(1)+eps(2)+eps(3)
    if (treps .gt. 0.d0) then
        do 600 k = 1, 3
            sigel(k) = lambda*treps
600     continue
    else
        do 610 k = 1, 3
            sigel(k) = 0.d0
610     continue
    endif
    do 150 k = 1, 3
        if (epsp(k) .gt. 0.d0) then
            sigel(k) = sigel(k) + deuxmu*epsp(k)
        endif
150 continue
    tr(1) = epsr(1)
    tr(2) = epsr(4)
    tr(3) = epsr(5)
    tr(4) = epsr(2)
    tr(5) = epsr(6)
    tr(6) = epsr(3)
    call diagp3(tr, vecp, epsp)
! -   CALCUL DES CONTRAINTES ELASTIQUES ASSOCIEE AUX DEFO GENERALISEES
    treps = epsr(1)+epsr(2)+epsr(3)
    if (treps .gt. 0.d0) then
        do 60 k = 1, 3
            sigelr(k) = lambda*treps
 60     continue
    else
        do 61 k = 1, 3
            sigelr(k) = 0.d0
 61     continue
    endif
    do 15 k = 1, 3
        if (epsp(k) .gt. 0.d0) then
            sigelr(k) = sigelr(k) + deuxmu*epsp(k)
        endif
 15 end do
    ener = 0.5d0 * ddot(3,epsp,1,sigelr,1)
!    CALCUL DE L'ETAT D'ENDOMMAGEMENT
    if (resi) then
        elas = .false.
        d = (sqrt((1+gamma)/seuil * ener) - 1) / gamma
        if (d .lt. vim(1)) then
            d = vim(1)
            elas = .true.
        else if (d .gt. 1) then
            d = 1
            elas = .true.
        endif
        fd = (1 - d) / (1 + gamma*d)
        vip(1) = d
        if (elas) then
            vip(2) = 0
            if (fd .le. rigmin) vip(2) = 2
        else
            vip(2) = 1
        endif
    endif
! -   CALCUL DES CONTRAINTES
!     MATRICE TR = (XX XY XZ YY YZ ZZ)
!
    tr(1) = eps(1)
    tr(2) = eps(4)
    tr(3) = eps(5)
    tr(4) = eps(2)
    tr(5) = eps(6)
    tr(6) = eps(3)
    call diagp3(tr, vecp, epsp)
!
    if ((epsp(1)+epsp(2)+epsp(3)) .gt. 0.d0) then
        lambdd=lambda * fd
    else
        lambdd=lambda
    endif
    if (epsp(1) .gt. 0.d0) then
        deumud(1)=deuxmu*fd
    else
        deumud(1)=deuxmu
    endif
    if (epsp(2) .gt. 0.d0) then
        deumud(2)=deuxmu*fd
    else
        deumud(2)=deuxmu
    endif
    if (epsp(3) .gt. 0.d0) then
        deumud(3)=deuxmu*fd
    else
        deumud(3)=deuxmu
    endif
    treps=epsp(1)+epsp(2)+epsp(3)
    sigp(1)=lambdd*treps+deumud(1)*epsp(1)
    sigp(2)=lambdd*treps+deumud(2)*epsp(2)
    sigp(3)=lambdd*treps+deumud(3)*epsp(3)
    if (resi) then
!      ON REPASSE DANS LE REPERE INITIAL LES CONTRAINTES
        tr(1) = sigp(1)
        tr(2) = sigp(2)
        tr(3) = sigp(3)
        tr(4) = 0.d0
        tr(5) = 0.d0
        tr(6) = 0.d0
        call bptobg(tr, sig, vecp)
        do 18 k = 4, ndimsi
            sig(k)=rac2*sig(k)
 18     continue
    endif
!
! - CALCUL DE LA MATRICE TANGENTE
!
    if (rigi) then
        if (option(11:14) .eq. 'ELAS') elas=.true.
        call r8inir(72, 0.d0, dsidpt, 1)
        call r8inir(36, 0.d0, dspdep, 1)
        if (fd .lt. rigmin) then
            if ((epsp(1)+epsp(2)+epsp(3)) .gt. 0.d0) then
                lambdd=lambda * rigmin
            endif
            if (epsp(1) .gt. 0.d0) then
                deumud(1)=deuxmu*rigmin
            endif
            if (epsp(2) .gt. 0.d0) then
                deumud(2)=deuxmu*rigmin
            endif
            if (epsp(3) .gt. 0.d0) then
                deumud(3)=deuxmu*rigmin
            endif
        endif
        tr(1) = sigel(1)
        tr(2) = sigel(2)
        tr(3) = sigel(3)
        tr(4) = 0.d0
        tr(5) = 0.d0
        tr(6) = 0.d0
        call bptobg(tr, sigel, vecp)
        do 28 k = 4, ndimsi
            sigel(k)=rac2*sigel(k)
 28     continue
        tr(1) = sigelr(1)
        tr(2) = sigelr(2)
        tr(3) = sigelr(3)
        tr(4) = 0.d0
        tr(5) = 0.d0
        tr(6) = 0.d0
        call bptobg(tr, sigelr, vecp)
        do 280 k = 4, ndimsi
            sigelr(k)=rac2*sigelr(k)
280     continue
        do 100 k = 1, 3
            do 110 l = 1, 3
                dspdep(k,l) = lambdd
110         continue
100     continue
        do 120 k = 1, 3
            dspdep(k,k) = dspdep(k,k) + deumud(k)
120     continue
        if (epsp(1)*epsp(2) .ge. 0.d0) then
            dspdep(4,4)=deumud(1)
        else
            dspdep(4,4)=(deumud(1)*epsp(1)-deumud(2)*epsp(2)) /(epsp(&
            1)-epsp(2))
        endif
        if (epsp(1)*epsp(3) .ge. 0.d0) then
            dspdep(5,5)=deumud(1)
        else
            dspdep(5,5)=(deumud(1)*epsp(1)-deumud(3)*epsp(3)) /(epsp(&
            1)-epsp(3))
        endif
        if (epsp(3)*epsp(2) .ge. 0.d0) then
            dspdep(6,6)=deumud(3)
        else
            dspdep(6,6)=(deumud(3)*epsp(3)-deumud(2)*epsp(2)) /(epsp(&
            3)-epsp(2))
        endif
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
                                    rtemp2=rtemp2+vecp(k,m)* vecp(i,n)&
                                    *vecp(j,n)*vecp(l,m)*dspdep(n,m)
 25                             continue
 24                         continue
                            rtemp2=rtemp2+vecp(i,1)*vecp(j,2)*vecp(k,&
                            1)*vecp(l,2)*dspdep(4,4)
                            rtemp2=rtemp2+vecp(i,2)*vecp(j,1)*vecp(k,&
                            2)*vecp(l,1)*dspdep(4,4)
                            rtemp2=rtemp2+vecp(i,1)*vecp(j,3)*vecp(k,&
                            1)*vecp(l,3)*dspdep(5,5)
                            rtemp2=rtemp2+vecp(i,3)*vecp(j,1)*vecp(k,&
                            3)*vecp(l,1)*dspdep(5,5)
                            rtemp2=rtemp2+vecp(i,2)*vecp(j,3)*vecp(k,&
                            2)*vecp(l,3)*dspdep(6,6)
                            rtemp2=rtemp2+vecp(i,3)*vecp(j,2)*vecp(k,&
                            3)*vecp(l,2)*dspdep(6,6)
                            dsidpt(t(i,j),t(k,l),1)=dsidpt(t(i,j),t(k,&
                            l),1)+rtemp2*rtemp4
                        endif
 23                 continue
 22             continue
 21         continue
 20     continue
        do 26 i = 1, 6
            do 27 j = i+1, 6
                dsidpt(i,j,1)=dsidpt(j,i,1)
 27         continue
 26     continue
! -- CONTRIBUTION DISSIPATIVE
        if ((.not. elas) .and. (ener.gt.0.d0)) then
            coef = (1+gamma)/(2*gamma*(1+gamma*d)*ener)
            do 200 k = 1, ndimsi
                do 210 l = 1, ndimsi
                    dsidpt(k,l,2) = dsidpt(k,l,2)-coef*sigel(k)* sigelr(l)
210             continue
200         continue
        endif
!
    endif
end subroutine
