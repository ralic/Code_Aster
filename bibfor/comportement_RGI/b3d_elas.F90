subroutine b3d_elas(var0,nvari,nvar0,depsv,dgamd6,&
     xk0,xmu0,sigef6,varf,hydra0,hydra1)
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
! person_in_charge: etienne grimal at edf.fr
!=====================================================================
!=====================================================================
!     calcul de l increment de contrainte elastique
!=====================================================================
        implicit none
      integer :: nvari,nvar0
      real(kind=8) :: var0(nvari),varf(nvari)
      real(kind=8) :: depsv,xk0,xmu0,hydra0,hydra1
      real(kind=8) :: sigef6(6),dgamd6(6)
      integer :: i
      real(kind=8) :: dsige,sig0
!     (cf ordre du stockage ds les variables internes dans idva10)
!     rem: castem travaille avec les gamma et non les epsilon (gamma=2*epsilon)
!     pour les termes de ciasaillement
      do i=1,3
       dsige=xk0*depsv+xmu0*dgamd6(i)
!      prise en compte eventuelle degradation chimique
       if (hydra1.lt.hydra0) then
        sig0=var0(nvar0+i)*hydra1/hydra0
       else
        sig0=var0(nvar0+i)
       end if
       sigef6(i)=sig0+dsige
       varf(nvar0+i)=sigef6(i)
      end do
      do i=4,6
       dsige=xmu0*dgamd6(i)
!      prise en compte eventuelle degradation chimique
       if (hydra1.lt.hydra0) then
        sig0=var0(nvar0+i)*hydra1/hydra0
       else
        sig0=var0(nvar0+i)
       end if
       sigef6(i)=sig0+dsige
       varf(nvar0+i)=sigef6(i)
      end do
!      do i=1,6
!       print*,'sigef(',i,')=',sigef6(i)
!      end do
!      read*
end subroutine
