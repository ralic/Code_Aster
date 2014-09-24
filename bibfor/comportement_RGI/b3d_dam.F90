subroutine b3d_dam(s,e,epic,reg,r,nu,dam,e2,fr,beta,dpic)
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
!     A.Sellier juin 2009 : maj sam. 28 août 2010 14:13:52 CEST
!     calcul de l'endommagement en fonction de la contrainte equivalente
!     loi de comportement definie par :
!     - une loi de Weibull jusqu au pic de traction de 0 a epic
!     - deux polynomes de la forme sigma=a.eps**2+b.eps+ de epic a e2
!     le changement d equation de polynome a lieu a epic+(e2-epic)/fr
!     le polynome devient convexe a cette valeur de deformation
!     -une resistance residuelle beta.R apres e2
!     s contrainte equivalente
!     R resistance
!     E module sain
!     epic def au pic
!     e2 fin de la branche descendante
!===================================================================
    implicit none
        real(kind=8) :: s
        real(kind=8) :: e
        real(kind=8) :: epic
        real(kind=8) :: reg
        real(kind=8) :: r
        real(kind=8) :: nu
        real(kind=8) :: dam
        real(kind=8) :: e2
        real(kind=8) :: fr
        real(kind=8) :: beta
        real(kind=8) :: dpic
!     declaration locale
      real(kind=8) :: eps,xm
      real(kind=8):: t2,t5,t8,t9,t10,t11,t13,t15,t17,t20,t28,t4
      real(kind=8):: t24,t31,t35,t40,t55,sre,sef2,sef,su,slim,t23,t36,t41
!     deformation equivalente
      sef2=E*e2
      sef=max(s,0.d0)
      eps=max(s/E,0.d0)
      su=R/(1.d0-dpic)
      if(sef.le.su)then
!      branche ascandante : 2 equations possibles
       if(su.ge.(reg*R))then
!       branche ascendante approche par une loi de type Weibull
!        xm=-1.d0/log(R/E/epic)
!        su=R/exp(-1.d0/xm)
!        su=(E*epic*nu-E*epic+(2.d0*nu**2)*R)/(nu-1.d0+2.d0*nu**2)
!        su=E*epic
!       rem  pas de couplage directionnel des endo ==nu=0
!       ds la formule ci-dessus, donc b3d_dam appele avec nu =0
!       pour l endo de compression ou l endo loc de traction
        xm=-1.d0/dlog(R/su)
        dam=1.d0-dexp(-1./xm*(sef/su)**xm)
       else
!       approximation bilineaire
        if(sef.le.R)then
         dam=0.d0
        else
         dam=1.d0-R/sef
        end if
       end if
!       print*,'dam0',dam
      else
!      on est sur la branche descendante, le choix entre concave et convexe
!      depend si on est avant ou apres epslim
       slim=su+(sef2-su)/fr
        if(sef.le.slim)then
!        branche descendantes polynomiale
      t2 = -dpic + dpic * beta + 0.1D1 - beta
      t5 = sef2 ** 2
      t8 = 0.2D1 * sef2 * dpic * R
      t9 = R ** 2
      t10 = dpic ** 2
      t11 = t5 * t10
      t13 = 0.2D1 * sef2 * R
      t15 = 0.2D1 * t5 * dpic
      t17 = 0.1D1 / (t5 + t8 + t9 + t11 - t13 - t15)
      t20 = sef ** 2
      t28 = t9 * fr
      dam = 0.1D1 - (t2 * R * fr * t17 * (-0.1D1 + dpic) * t20 + 0.2D1 *&
      t2 * t9 * fr * t17 * sef + R * (t28 * beta + t11 + t8 - t13 - t15&
      - t28 + t9 + t5) * t17) / sef
        else
         if (sef.lt.sef2)then
      t4 = dpic ** 2
      t8 = R * fr * (-0.1D1 - 0.2D1 * dpic * beta + t4 * beta + 0.2D1 *&
     dpic - t4 + beta)
      t9 = sef2 ** 2
      t13 = R ** 2
      t23 = 0.1D1 / (t9 + 0.2D1 * sef2 * dpic * R + t13 + t9 * t4 - 0.2D1&
     * sef2 * R - 0.2D1 * t9 * dpic) / (-0.1D1 + fr)
      t24 = sef ** 2
      t31 = t9 * fr
      t35 = beta * t9
      t36 = beta * sef2
      t40 = beta * R
      t41 = fr * sef2
      t55 = t31 - 0.2D1 * t31 * dpic + t31 * t4 - t35 - 0.2D1 * t36 * dpic&
     * R + 0.2D1 * t40 * t41 * dpic - beta * t13 + t13 * fr * beta -&
      t35 * t4 + 0.2D1 * t36 * R - 0.2D1 * t40 * t41 + 0.2D1 * t35 * dpic
      dam = 0.1D1 - (-t8 * t23 * t24 + 0.2D1 * t8 * t23 * sef2 * sef + R&
      * t55 * t23) / sef
         else
!         residu beta.R atteint
          sre=beta*R
          dam=1.d0-sre/s
         end if
        end if
      end if
end subroutine
