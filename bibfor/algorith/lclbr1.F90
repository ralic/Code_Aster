subroutine lclbr1(fami, kpg, ksp, ndim, typmod,&
                  imate, compor, epsm, deps, vim,&
                  option, sig, vip, dsidep)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    include 'asterfort/diago2.h'
    include 'asterfort/lclbr2.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/verift.h'
    character(len=*) :: fami
    character(len=8) :: typmod(2)
    character(len=16) :: compor(*), option
    integer :: kpg, ksp, ndim, imate
    real(kind=8) :: epsm(6), deps(6), vim(2)
    real(kind=8) :: sig(6), vip(2), dsidep(6, 12)
! ----------------------------------------------------------------------
!     LOI DE COMPORTEMENT BETON REGLEMENTAIRE 2D
!     ELASTIQUE NON LINEAIRE
!         PIC EN TRACTION
!         EN COMPRESSION : LOI PUISSANCE PUIS PLATEAU
!
! IN  NDIM    : DIMENSION DE L'ESPACE
! IN  TYPMOD  : TYPE DE MODELISATION
! IN  IMATE   : NATURE DU MATERIAU
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
! OUT DSIDEP  : MATRICE TANGENTE
! ----------------------------------------------------------------------
! LOC EDFRC1  COMMON CARACTERISTIQUES DU MATERIAU (AFFECTE DANS EDFRMA)
    logical :: rigi, resi, coup, plan
    integer :: ndimsi, k, l, i, j, m, n, t(3, 3), iret1, iret2
    real(kind=8) :: eps(6), kron(6)
    real(kind=8) :: rac2, e
    real(kind=8) :: tr(6), rtemp2
    real(kind=8) :: epsp(2), vecp(2, 2), dspdep(6, 6)
    real(kind=8) :: deumud(3), sigp(3), rtemp, rtemp3, rtemp4
    real(kind=8) :: gamma
    real(kind=8) :: sigmt, sigmc, epsic, compn, epsthp, epsthm
    data        kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
! ----------------------------------------------------------------------
!
! -- OPTION ET MODELISATION
!
    rigi = (option(1:4).eq.'RIGI' .or. option(1:4).eq.'FULL')
    resi = (option(1:4).eq.'RAPH' .or. option(1:4).eq.'FULL')
    coup = (option(6:9).eq.'COUP')
    plan = ((typmod(1) .eq. 'C_PLAN').or.(typmod(1) .eq. 'D_PLAN'))
    if (.not.plan) then
        call u2mess('F', 'ALGORITH4_62')
    endif
    if (coup) rigi=.true.
    ndimsi = 2*ndim
    rac2=sqrt(2.d0)
!
! -- INITIALISATION
!
    call lclbr2(fami, kpg, ksp, imate, compor,&
                ndim, epsm, t, e, sigmt,&
                sigmc, epsic, compn, gamma)
!
!
! -- MAJ DES DEFORMATIONS ET PASSAGE AUX DEFORMATIONS REELLES 3D
!
    call verift(fami, kpg, ksp, '+', imate,&
                'ELAS', 1, epsthp, iret1)
    call verift(fami, kpg, ksp, '-', imate,&
                'ELAS', 1, epsthm, iret2)
    if (resi) then
        do 10 k = 1, ndimsi
            eps(k) = epsm(k) + deps(k) - epsthp*kron(k)
10      continue
    else
        do 40 k = 1, ndimsi
            eps(k) = epsm(k) - epsthm*kron(k)
40      continue
    endif
!
    do 45 k = 4, ndimsi
        eps(k) = eps(k)/rac2
45  end do
    eps(3)=0.d0
!
! -- DIAGONALISATION DES DEFORMATIONS
!
    tr(1) = eps(1)
    tr(2) = eps(2)
    tr(3) = eps(4)
    call diago2(tr, vecp, epsp)
!
! -- CALCUL DES CONTRAINTES
!
    do 50 i = 1, 2
        if (epsp(i) .le. 0.d0) then
            if (epsp(i) .gt. epsic) then
                sigp(i)=sigmc*(1.d0-(1.d0-epsp(i)/epsic)**compn)
                deumud(i)=sigmc*compn/epsic*(1.d0-epsp(i)/epsic)&
                **(compn-1.d0)
            else
                sigp(i)=sigmc
                deumud(i)=0.d0
            endif
        else
            if (epsp(i) .lt. (sigmt/e)) then
                sigp(i)=e*epsp(i)
                deumud(i)=e
            else if (epsp(i).ge.(1+gamma)*sigmt/e) then
                sigp(i)=0.d0
                deumud(i)=0.d0
            else
                sigp(i)=sigmt-e/gamma*(epsp(i)-sigmt/e)
                deumud(i)=sigp(i)/epsp(i)
            endif
        endif
50  end do
    if ((resi) .and. (.not.coup)) then
        call r8inir(6, 0.d0, sig, 1)
        do 1010 i = 1, 2
            rtemp=sigp(i)
            sig(1)=sig(1)+vecp(1,i)*vecp(1,i)*rtemp
            sig(2)=sig(2)+vecp(2,i)*vecp(2,i)*rtemp
            sig(4)=sig(4)+vecp(1,i)*vecp(2,i)*rtemp
1010      continue
        sig(4)=rac2*sig(4)
        sig(3)=0.d0
        sig(5)=0.d0
        sig(6)=0.d0
    endif
!
! -- CALCUL DE LA MATRICE TANGENTE
!
!
    if (rigi) then
        call r8inir(36, 0.d0, dspdep, 1)
        if (coup) then
            call r8inir(72, 0.d0, dsidep, 1)
        else
            call r8inir(36, 0.d0, dsidep, 1)
        endif
        do 120 k = 1, 2
            dspdep(k,k) = dspdep(k,k) + deumud(k)
120      continue
        dspdep(3,3)=e
        if (epsp(1)*epsp(2) .ge. 0.d0) then
            dspdep(4,4)=deumud(1)
        else
            dspdep(4,4)=(deumud(1)*epsp(1)-deumud(2)*epsp(2)) /(epsp(&
            1)-epsp(2))
        endif
        do 20 i = 1, 2
            do 21 j = i, 2
                if (i .eq. j) then
                    rtemp3=1.d0
                else
                    rtemp3=rac2
                endif
                do 22 k = 1, 2
                    do 23 l = 1, 2
                        if (t(i,j) .ge. t(k,l)) then
                            if (k .eq. l) then
                                rtemp4=rtemp3
                            else
                                rtemp4=rtemp3/rac2
                            endif
                            rtemp2=0.d0
                            do 24 m = 1, 2
                                do 25 n = 1, 2
                                    rtemp2=rtemp2+vecp(k,m)* vecp(i,n)&
                                    *vecp(j,n)*vecp(l,m)*dspdep(n,m)
25                              continue
24                          continue
                            rtemp2=rtemp2+vecp(i,1)*vecp(j,2)*vecp(k,&
                            1)*vecp(l,2)*dspdep(4,4)
                            rtemp2=rtemp2+vecp(i,2)*vecp(j,1)*vecp(k,&
                            2)*vecp(l,1)*dspdep(4,4)
                            dsidep(t(i,j),t(k,l))=dsidep(t(i,j),t(k,l)&
                            )+rtemp2*rtemp4
                        endif
23                  continue
22              continue
21          continue
20      continue
        dsidep(3,3)=dspdep(3,3)
        dsidep(1,2)=dsidep(2,1)
        dsidep(1,4)=dsidep(4,1)
        dsidep(2,4)=dsidep(4,2)
!
    endif
end subroutine
