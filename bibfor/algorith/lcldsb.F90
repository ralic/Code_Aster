subroutine lcldsb(fami, kpg, ksp, ndim,&
                  imate, compor, epsm, deps, vim,&
                  option, sig,&
                  vip, dsidep)
! ======================================================================
! COPYRIGHT (C) 1991 - 2017  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "jeveux.h"
#include "asterfort/diagp3.h"
#include "asterfort/jevech.h"
#include "asterfort/lceib1.h"
#include "asterfort/r8inir.h"
#include "asterfort/rcvarc.h"
#include "asterfort/get_varc.h"
#include "blas/ddot.h"
    character(len=16) :: compor(*), option
    character(len=*) :: fami
    integer :: ndim, imate, ksp, kpg
    real(kind=8) :: epsm(6), deps(6), vim(*)
    real(kind=8) :: sig(6), vip(*), dsidep(6, 12)
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT ENDO_ISOT_BETON (EN LOCAL)
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : NATURE DU MATERIAU
! IN  EPSM    : DEFORMATION EN T-
! IN  DEPS    : INCREMENT DE DEFORMATION
! IN  VIM     : VARIABLES INTERNES EN T-
! IN  carcri    : CRITERES DE CONVERGENCE
! IN  OPTION  : OPTION DEMANDEE
!                 RIGI_MECA_TANG ->     DSIDEP
!                 FULL_MECA      -> SIG DSIDEP VIP
!                 RAPH_MECA      -> SIG        VIP
! OUT SIG     : CONTRAINTE
! OUT VIP     : VARIABLES INTERNES
!                 1   -> VALEUR DE L'ENDOMMAGEMENT
! OUT DSIDEP  : MATRICE TANGENTE
! ----------------------------------------------------------------------
! LOC EDFRC1  COMMON CARACTERISTIQUES DU MATERIAU (AFFECTE DANS EDFRMA)
    aster_logical :: rigi, resi, elas, coup
    integer :: ndimsi, k, l, i, j, m, n, t(3, 3), iret, iterat
    real(kind=8) :: eps(6), treps, sigel(6), kron(6)
    real(kind=8) :: rac2, coef
    real(kind=8) :: rigmin, fd, d, ener
    real(kind=8) :: tr(6), rtemp2, epsthe(2)
    real(kind=8) :: epsp(3), vecp(3, 3), dspdep(6, 6)
    real(kind=8) :: deumud(3), lambdd, sigp(3), rtemp, rtemp3, rtemp4
    real(kind=8) :: kdess, bendo, lambda, deuxmu, gamma
    real(kind=8) :: seuil
    real(kind=8) :: hydrm, hydrp, sechm, sechp, sref, tp, tm, tref
    parameter  (rigmin = 1.d-5)
    data        kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
! ----------------------------------------------------------------------
!
!
! - Get temperatures
!
    call get_varc(fami , kpg  , ksp , 'T',&
                  tm, tp, tref)
!
! -- OPTION ET MODELISATION
!
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
    coup = (option(6:9).eq.'COUP')
    if (coup) rigi=.true.
    ndimsi = 2*ndim
    rac2=sqrt(2.d0)
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
! -- INITIALISATION
!
    call lceib1(fami, kpg, ksp, imate, compor,&
                ndim, epsm, sref, sechm, hydrm,&
                t, lambda, deuxmu, epsthe, kdess,&
                bendo, gamma, seuil)
!
! -- MAJ DES DEFORMATIONS ET PASSAGE AUX DEFORMATIONS REELLES 3D
!
    if (resi) then
        do k = 1, ndimsi
            eps(k) = epsm(k) + deps(k) - kron(k) * ( epsthe(2) - kdess * (sref-sechp) - bendo * h&
                     &ydrp )
        end do
    else
        do k = 1, ndimsi
            eps(k) = epsm(k) - ( epsthe(1) - kdess * (sref-sechm) - bendo * hydrm ) * kron(k)
        end do
    endif
!
    do k = 4, ndimsi
        eps(k) = eps(k)/rac2
    end do
    if (ndimsi .lt. 6) then
        do k = ndimsi+1, 6
            eps(k)=0.d0
        end do
    endif
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
    if (treps .gt. 0.d0) then
        do k = 1, 3
            sigel(k) = lambda*treps
        end do
    else
        do k = 1, 3
            sigel(k) = 0.d0
        end do
    endif
    do k = 1, 3
        if (epsp(k) .gt. 0.d0) then
            sigel(k) = sigel(k) + deuxmu*epsp(k)
        endif
    end do
    ener = 0.5d0 * ddot(3,epsp,1,sigel,1)
!
! -- CALCUL (OU RECUPERATION) DE L'ENDOMMAGEMENT
!
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
    else
        d=vim(1)
        fd = (1 - d) / (1 + gamma*d)
        elas=((nint(vim(2)).eq.0).or.(nint(vim(2)).eq.2))
    endif
!
!
! -- CALCUL DES CONTRAINTES
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
!
!
    if (resi .and. (.not.coup)) then
        call r8inir(6, 0.d0, sig, 1)
        do i = 1, 3
            rtemp=sigp(i)
            sig(1)=sig(1)+vecp(1,i)*vecp(1,i)*rtemp
            sig(2)=sig(2)+vecp(2,i)*vecp(2,i)*rtemp
            sig(3)=sig(3)+vecp(3,i)*vecp(3,i)*rtemp
            sig(4)=sig(4)+vecp(1,i)*vecp(2,i)*rtemp
            sig(5)=sig(5)+vecp(1,i)*vecp(3,i)*rtemp
            sig(6)=sig(6)+vecp(2,i)*vecp(3,i)*rtemp
        end do
        do k = 4, ndimsi
            sig(k)=rac2*sig(k)
        end do
    endif
!
! -- CALCUL DE LA MATRICE TANGENTE
!
    if (rigi) then
        if (option(11:14) .eq. 'ELAS') elas=.true.
        call r8inir(36, 0.d0, dspdep, 1)
        if (coup) then
            call r8inir(72, 0.d0, dsidep, 1)
        else
            call r8inir(36, 0.d0, dsidep, 1)
        endif
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
        do k = 1, 3
            do l = 1, 3
                dspdep(k,l) = lambdd
            end do
        end do
        do k = 1, 3
            dspdep(k,k) = dspdep(k,k) + deumud(k)
        end do
        if (epsp(1)*epsp(2) .ge. 0.d0) then
            dspdep(4,4)=deumud(1)
        else
            dspdep(4,4)=(deumud(1)*epsp(1)-deumud(2)*epsp(2)) /(epsp(1)-epsp(2))
        endif
        if (epsp(1)*epsp(3) .ge. 0.d0) then
            dspdep(5,5)=deumud(1)
        else
            dspdep(5,5)=(deumud(1)*epsp(1)-deumud(3)*epsp(3)) /(epsp(1)-epsp(3))
        endif
        if (epsp(3)*epsp(2) .ge. 0.d0) then
            dspdep(6,6)=deumud(3)
        else
            dspdep(6,6)=(deumud(3)*epsp(3)-deumud(2)*epsp(2)) /(epsp(3)-epsp(2))
        endif
        do i = 1, 3
            do j = i, 3
                if (i .eq. j) then
                    rtemp3=1.d0
                else
                    rtemp3=rac2
                endif
                do k = 1, 3
                    do l = 1, 3
                        if (t(i,j) .ge. t(k,l)) then
                            if (k .eq. l) then
                                rtemp4=rtemp3
                            else
                                rtemp4=rtemp3/rac2
                            endif
                            rtemp2=0.d0
                            do m = 1, 3
                                do n = 1, 3
                                    rtemp2=rtemp2+vecp(k,m)* vecp(i,n)&
                                    *vecp(j,n)*vecp(l,m)*dspdep(n,m)
                                end do
                            end do
                            rtemp2=rtemp2+vecp(i,1)*vecp(j,2)*vecp(k,1)*vecp(l,2)*dspdep(4,4)
                            rtemp2=rtemp2+vecp(i,2)*vecp(j,1)*vecp(k,2)*vecp(l,1)*dspdep(4,4)
                            rtemp2=rtemp2+vecp(i,1)*vecp(j,3)*vecp(k,1)*vecp(l,3)*dspdep(5,5)
                            rtemp2=rtemp2+vecp(i,3)*vecp(j,1)*vecp(k,3)*vecp(l,1)*dspdep(5,5)
                            rtemp2=rtemp2+vecp(i,2)*vecp(j,3)*vecp(k,2)*vecp(l,3)*dspdep(6,6)
                            rtemp2=rtemp2+vecp(i,3)*vecp(j,2)*vecp(k,3)*vecp(l,2)*dspdep(6,6)
                            dsidep(t(i,j),t(k,l))=dsidep(t(i,j),t(k,l))+rtemp2*rtemp4
                        endif
                    end do
                end do
            end do
        end do
!
        do i = 1, 6
            do j = i+1, 6
                dsidep(i,j)=dsidep(j,i)
            end do
        end do
! -- CONTRIBUTION DISSIPATIVE
        if ((.not. elas) .and. (ener.gt.0.d0)) then
            if (coup) then
                coef = sqrt((1+gamma)/(2*gamma*(1+gamma*d)*ener))
                if ((epsp(1)+epsp(2)+epsp(3)) .gt. 0.d0) then
                    lambdd=lambda * coef
                else
                    lambdd=0.d0
                endif
                if (epsp(1) .gt. 0.d0) then
                    deumud(1)=deuxmu*coef
                else
                    deumud(1)=0.d0
                endif
                if (epsp(2) .gt. 0.d0) then
                    deumud(2)=deuxmu*coef
                else
                    deumud(2)=0.d0
                endif
                if (epsp(3) .gt. 0.d0) then
                    deumud(3)=deuxmu*coef
                else
                    deumud(3)=0.d0
                endif
                do k = 1, 3
                    do l = 1, 3
                        dspdep(k,l) = lambdd
                    end do
                end do
                do k = 1, 3
                    dspdep(k,k) = dspdep(k,k) + deumud(k)
                end do
                if (epsp(1)*epsp(2) .ge. 0.d0) then
                    dspdep(4,4)=deumud(1)
                else
                    dspdep(4,4)=(deumud(1)*epsp(1)-deumud(2)*epsp(2))/(epsp(1)-epsp(2))
                endif
                if (epsp(1)*epsp(3) .ge. 0.d0) then
                    dspdep(5,5)=deumud(1)
                else
                    dspdep(5,5)=(deumud(1)*epsp(1)-deumud(3)*epsp(3))/(epsp(1)-epsp(3))
                endif
                if (epsp(3)*epsp(2) .ge. 0.d0) then
                    dspdep(6,6)=deumud(3)
                else
                    dspdep(6,6)=(deumud(3)*epsp(3)-deumud(2)*epsp(2))/(epsp(3)-epsp(2))
                endif
                do i = 1, 3
                    do j = i, 3
                        if (i .eq. j) then
                            rtemp3=1.d0
                        else
                            rtemp3=rac2
                        endif
                        do k = 1, 3
                            do l = 1, 3
                                if (t(i,j) .ge. t(k,l)) then
                                    if (k .eq. l) then
                                        rtemp4=rtemp3
                                    else
                                        rtemp4=rtemp3/rac2
                                    endif
                                    rtemp2=0.d0
                                    do m = 1, 3
                                        do n = 1, 3
                                            rtemp2=rtemp2+&
                                                   vecp(k,m)*vecp(i,n)*&
                                                   vecp(j,n)*vecp(l,m)*dspdep(n,m)
                                        end do
                                    end do
                                    rtemp2=rtemp2+vecp(i,1)*vecp(j,2)*&
                                    vecp(k,1)*vecp(l,2)*dspdep(4,4)
                                    rtemp2=rtemp2+vecp(i,2)*vecp(j,1)*&
                                    vecp(k,2)*vecp(l,1)*dspdep(4,4)
                                    rtemp2=rtemp2+vecp(i,1)*vecp(j,3)*&
                                    vecp(k,1)*vecp(l,3)*dspdep(5,5)
                                    rtemp2=rtemp2+vecp(i,3)*vecp(j,1)*&
                                    vecp(k,3)*vecp(l,1)*dspdep(5,5)
                                    rtemp2=rtemp2+vecp(i,2)*vecp(j,3)*&
                                    vecp(k,2)*vecp(l,3)*dspdep(6,6)
                                    rtemp2=rtemp2+vecp(i,3)*vecp(j,2)*&
                                    vecp(k,3)*vecp(l,2)*dspdep(6,6)
                                    dsidep(t(i,j),t(k,l)+6)=dsidep(t(&
                                    i,j),t(k,l)+6)+rtemp2*rtemp4
                                endif
                            end do
                        end do
                    end do
                end do
!
                do i = 1, 6
                    do j = i+1, 6
                        dsidep(i,j+6)=dsidep(j,i+6)
                    end do
                end do
!
            else
                tr(1) = sigel(1)
                tr(2) = sigel(2)
                tr(3) = sigel(3)
                call r8inir(6, 0.d0, sigel, 1)
                do i = 1, 3
                    rtemp=tr(i)
                    sigel(1)=sigel(1)+vecp(1,i)*vecp(1,i)*rtemp
                    sigel(2)=sigel(2)+vecp(2,i)*vecp(2,i)*rtemp
                    sigel(3)=sigel(3)+vecp(3,i)*vecp(3,i)*rtemp
                    sigel(4)=sigel(4)+vecp(1,i)*vecp(2,i)*rtemp
                    sigel(5)=sigel(5)+vecp(1,i)*vecp(3,i)*rtemp
                    sigel(6)=sigel(6)+vecp(2,i)*vecp(3,i)*rtemp
                end do
                do k = 4, ndimsi
                    sigel(k)=rac2*sigel(k)
                end do
                coef = (1+gamma)/(2*gamma*(1+gamma*d)*ener)
!
                do k = 1, ndimsi
                    do l = 1, ndimsi
                        dsidep(k,l) = dsidep(k,l) - coef * sigel(k) * sigel(l)
                    end do
                end do
            endif
        endif
    endif
end subroutine
