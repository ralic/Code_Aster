subroutine b3d_dc(sigec03,long3,dcf,dc0,e,delta,gf,rc,epic,&
     beta,gama,reg,fr2,suc1,ifour,aleas0,bw1,pw1)
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
!     calcul de lendommagement anisotrope de compression
!     le critere est isotrope mais pour assurer Gfc la loi d evolution
!     depend de la taille ds les 3 directions principale de sst     
!===================================================================
    implicit none
#include "asterfort/indice1.h"
#include "asterfort/b3d_dam.h"
        real(kind=8) :: sigec03(3)
        real(kind=8) :: long3(3)
        real(kind=8) :: dcf
        real(kind=8) :: dc0
        real(kind=8) :: e
        real(kind=8) :: delta
        real(kind=8) :: gf
        real(kind=8) :: rc
        real(kind=8) :: epic
        real(kind=8) :: beta
        real(kind=8) :: gama
        real(kind=8) :: reg
        real(kind=8) :: fr2
        real(kind=8) :: suc1
        integer :: ifour
        real(kind=8) :: aleas0
        real(kind=8) :: bw1
        real(kind=8) :: pw1
!     declarations externes
      real(kind=8) :: dc3(3),d03(3)
!     declarations internes
      real(kind=8) ::xj2,rj2,xi1,xc,xlimax,e2
      integer i,k,l
      real(kind=8) :: sigec3(3),sigind3(3),Gfeff,dpic,dc1
      
!     modification du champ de contrainte de compression pour intergrer
!     l aleas, le critere est evalue dans les zones ou les autocontraintes
!     s additionnent
      do i=1,3
         call indice1(i,k,l)
         sigind3(i)=aleas0*sqrt(sigec03(k)**2+sigec03(l)**2)
         sigec3(i)=sigec03(i)+sigind3(i)
      end do

!     mise a jour de l'endommagement isotrope de compression (Drucker-Prager)
!     calcul des invariants du tenseur des contraintes effectives
      xj2=(sigec3(1)-sigec3(2))**2+(sigec3(1)-sigec3(3))**2&
      +(sigec3(2)-sigec3(3))**2
      rj2=sqrt(xj2/6.d0)
      xi1=(sigec3(1)+sigec3(2)+sigec3(3))/3.d0
!     contrainte equivalente
      xc=(rj2+delta*xi1)
      xc=0.5d0*(xc+abs(xc))
!     actualisation du seuil de compression
      xc=max(xc,suc1)
      suc1=xc
!     calcul de la contrainte de compression uniaxiale equivalente
      xc=xc/((sqrt(3.d0)*(1.d0+aleas0)-delta*(1.d0-2.d0*aleas0))/3.d0)
!     taille des mailles pour ne pas avoir de snap back local
      xlimax=0.2D1*gf/rc/epic/(-0.1D1*beta+gama+gama*beta)
!     bien que le critere soit isotrope il faut utiliser un endo anisotrope pour
!     compenser l anisotropie de la taille de l element
!     en 2d c'est la direction principale 2 en 3d c est la 3
      if (ifour.eq.2) then
       i=3
      else
!      comme la diagonalisation ne porte que sur le carré 2*2 il faut 
!      tester la contrainte maxi dans la direction 3
       if(sigec3(3).lt.sigec3(2))then
         i=3
       else         
         i=2
       end if
      end if
      if(long3(i).gt.xlimax)then
!        print*, 'element de maillage trop grand ds b3d_dc l=',long3(i)
!     # ,' lmax compression=',xlimax
!        read*
!       on augmente gfc pour être a la limite du snap back autorise
!       compte tenu de la taille de mailles
        Gfeff=gf*long3(i)/xlimax
      else
!       on conserve la valeur imposee par l utilisateur
        Gfeff=gf
      end if
!     calcul de l extremite de la branche descendante ds la direction
!     principale de compression
!     print*,'sigec3(',i,')=',sigec3(i)
      e2=(rc*long3(i)*beta*epic+0.2D1*Gfeff)/rc/long3(i)/(0.1D1+beta)


!     calcul de l'endommagement
!     0==nu car pas de couplage directionnel en endo de compression
!     2==fr car 2 paraboles reliees a mi hauteur sur la branche decendante
      dpic=1.d0-rc/(e*epic)
      call b3d_dam(xc,e,epic,reg,rc,0.d0,dc1,e2,fr2,beta,dpic)
!     end do
!     print*      
!     verif de la non decroissance de l'endo de compression
!     call dmin_3d(d03,dc3)      
      dcf=min(max(dc1,dc0),0.99999)
end subroutine
