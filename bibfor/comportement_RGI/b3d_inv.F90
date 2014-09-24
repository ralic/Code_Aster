subroutine b3d_inv(rapp, r, e, epic, reg,&
                   beta, gf, li, fr, nu,&
                   s)
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF r&D                  WWW.CODE-ASTER.ORG
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
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF r&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!=====================================================================
!     recherche de la contrainte effective s associee  au niveau 
!     de resistance residuelle rapp
!     si s<su rapp doit être donne egal à su(1-d) de façon à situer
!     la contrainte seuil / a celle au pic
!=====================================================================
    implicit none
    real(kind=8) :: rapp
    real(kind=8) :: r
    real(kind=8) :: e
    real(kind=8) :: epic
    real(kind=8) :: reg
    real(kind=8) :: beta
    real(kind=8) :: gf
    real(kind=8) :: li
    real(kind=8) :: fr
    real(kind=8) :: nu
    real(kind=8) :: s, dpic, su, dam, xm, sef2, s12, a1, b1, c1, delta1, a2, b2, c2, delta2
!     calcul d'un seuil equivalent pour une loi d'endommagement a vitesse
!     d ecrouissage different
!     endo au pic 
    dpic=max(1.d0-r/e/epic,0.d0)
!     resistance effective au pic
!     su=(e*epic*nu-e*epic+(2.d0*nu**2)*r)/(nu-1.d0+2.d0*nu**2)
    su=e*epic
!     position de la resistance apparente par rapport a la resistance au pic
!     par convention rapp=su(1-d) avant le pic et rapp=ss*(1-d) apres
    if (rapp .ge. r) then
!      on est avant le pic
        dam=1.d0-rapp/su
        if (epic .gt. (reg*r/e)) then
!       branche ascendante approchee par une loi de type Weibull
            xm=-1.d0/dlog(r/su)
            s=su*(-xm*dlog(1.d0-dam))**(1./xm)
        else
!       approximation bilineaire
            if (dam .eq. 0.) then
                s=0.d0
            else
                s=r/(1.d0-dam)
            end if
        end if
    else
!      calcul de lextremite de la seconde parabole
        sef2 =((4.d0*r*li*beta*su*fr+2.d0*fr*r*li*su+& 
      6.d0*Gf*e*fr-3.d0*r*li*epic*e*fr-2.d0*r*li*beta*&
      su+2.d0*r*li*su)/r/li/(2.d0*beta*fr+fr-beta+1.d0))&
      /0.2D1
!      on est sur la branche descendante
        if (rapp .gt. (beta*r)) then
!       on est sur une des branches paraboliques 
!       limite entre les deux paraboles
            s12=su+(sef2-su)/fr
!       1ere partie de la branche parabolique
            a1 =fr*r*(-1.d0+beta)/(-2.d0*sef2*su+su**2+sef2**2)
            b1 =-2.d0*fr*r*(-1.d0+beta)/(-2.d0*sef2*su+su**2+sef2**2)*su
            c1 =r*(-su**2*fr+beta*su**2*fr-2.d0*sef2*su+su**2+sef2**2)&
       /(-2.d0*sef2*su+su**2+sef2**2)
            c1=c1-rapp
            delta1=b1**2-4.d0*a1*c1
            s=0.5d0*(-b1-dsqrt(delta1))/a1
!        s=s1
            if (s .gt. s12) then
!        2eme partie branche parabolique
                a2 =-fr*r*(-1.d0+beta)/(-2.d0*su*fr*sef2+su**2& 
        *fr+fr*sef2**2-sef2**2+2.d0*sef2*su-su**2)
                b2=2.d0*fr*r*(-1.d0+beta)/(-2.d0*su*fr*sef2+su**2&
        *fr+fr*sef2**2-sef2**2+2.d0*sef2*su-su**2)*sef2
                c2 =r*(fr*sef2**2-2.d0*beta*su*fr*sef2+beta& 
        *su**2*fr-beta*sef2**2+2.d0*beta*sef2*su-beta*& 
        su**2)/(-2.d0*su*fr*sef2+su**2*fr+fr*sef2**2-& 
        sef2**2+2.d0*sef2*su-su**2)
                c2=c2-rapp
                delta2=b2**2-4.d0*a2*c2
                s=0.5d0*(-b2-dsqrt(delta2))/a2
!         s=s2
                if (s .gt. sef2) then
                    s=sef2
                end if
            end if
        else
!       residu beta.r atteint
            s=sef2
        end if
    end if
end subroutine
