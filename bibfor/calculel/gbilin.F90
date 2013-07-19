subroutine gbilin(fami, kp, imate, dudm, dvdm,&
                  dtdm, dfdm, tgdm, poids, c1,&
                  c2, c3, cs, th, coef,&
                  rho, puls, axi, g)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
!
! IN  FAMI   : SCHEMA D'INTEGRATION DE GAUSS
! IN  KP     : NUMERO DU POINTS DE GAUSS
! IN  IMATE  :
! IN  DUDM   :
! IN  DVDM   :
! IN  DTDM   :
! IN  DFDM   :
! IN  TGDM   :
! IN  POIDS  :
! IN  C1     : COEFFICIENT MATERIAUX (VOIR DOC R)
! IN  C2     : COEFFICIENT MATERIAUX (VOIR DOC R)
! IN  C3     : COEFFICIENT MATERIAUX (VOIR DOC R)
! IN  CS     : COEFFICNET DE SINGULARITÉ POUR LES FORCES VOLUMIQUES
!              (CS=1 SI CALCUL DE G, CS=0.5 SI CALCUL DE K1 OU K2)
! IN  TH     : COEFFICIENT DU TERME CLASSIQUE DU A LA THERMIQUE
! IN  COEF   : COEFFICIENT PERMETTANT DE DOUBLER LES TERMES
!              THERMIQUES POUR LE CALCUL DE G
!              (COEF=2 SI CALCUL DE G, COEF=1 SI CALCUL DE K1 OU K2)
! IN  RHO    : MASSE VOLUMIQUE
! IN  PULS   : PULSATION
! IN  AXI    : INDIQUE SI ON EST EN AXI
!
! OUT G      :
!
!
#include "jeveux.h"
#include "asterfort/rcvalb.h"
#include "asterfort/verift.h"
    character(len=*) :: fami
    integer :: kp, imate
    real(kind=8) :: dudm(3, 4), dvdm(3, 4), dtdm(3, 4), dfdm(3, 4), tgdm(2)
    real(kind=8) :: c1, c2, c3, cs, th, poids, g, bil(3, 3, 3, 3), coef
    logical :: axi
!
! ----------------------------------------------------------------------
!     CALCUL DU TAUX DE RESTITUTION D'ENERGIE G SOUS LA FORME
!     BILINEAIRE SYMETRIQUE G(U,V) EN ELATICITE LINEAIRE EN 2D
!     (DEFORMATIONS OU CONTRAINTES PLANES)
! ----------------------------------------------------------------------
!
    integer :: i, j, k, p, l, m, iret
!
    integer :: icodre(3)
    character(len=8) :: nomres(3), materi
    real(kind=8) :: valres(3)
    real(kind=8) :: vect(7), s11, s12, s13, s21, s22, s23, s1, s2, puls, rho
    real(kind=8) :: tcla, tfor, tthe, tdyn, divt, divv, s1th, s2th, prod, epsthe
    real(kind=8) :: e, nu, alpha
!
!
! DEB-------------------------------------------------------------------
!
!
    nomres(1) = 'E'
    nomres(2) = 'NU'
    nomres(3) = 'ALPHA'
    materi = ' '
!
    call verift(fami, kp, 1, '+', imate,&
                materi, 'ELAS', 1, epsthe, iret)
    call rcvalb(fami, kp, 1, '+', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                2, nomres(1), valres(1), icodre(1), 1)
    call rcvalb(fami, kp, 1, '+', imate,&
                ' ', 'ELAS', 0, ' ', 0.d0,&
                1, nomres(3), valres(3), icodre(3), 0)
    e = valres(1)
    nu = valres(2)
    alpha = valres(3)
    divt = dtdm(1,1)+dtdm(2,2)
    if (axi) divt = divt+dtdm(3,3)
    divv = th*(dvdm(1,1)+dvdm(2,2))
    if (axi) divv = divv+dvdm(3,3)
!
! - TERME CLASSIQUE
!
    if (.not. axi) then
        vect(1)= 0.5d0*(dvdm(1,1)*dudm(2,2)+dudm(1,1)*dvdm(2,2))
        vect(2)= 0.5d0*(dvdm(1,1)*dudm(1,2)+dudm(1,1)*dvdm(1,2))
        vect(3)= 0.5d0*(dvdm(1,1)*dudm(2,1)+dudm(1,1)*dvdm(2,1))
        vect(4)= 0.5d0*(dvdm(2,2)*dudm(1,2)+dudm(2,2)*dvdm(1,2))
        vect(5)= 0.5d0*(dvdm(2,2)*dudm(2,1)+dudm(2,2)*dvdm(2,1))
        vect(6)= 0.5d0*(dvdm(1,2)*dudm(2,1)+dudm(1,2)*dvdm(2,1))
!
        s11 = dudm(1,1)*dvdm(1,1) + dudm(2,2)*dvdm(2,2)
        s12 = dudm(1,1)*dvdm(2,2) + dudm(2,2)*dvdm(1,1)
        s13 = (dudm(1,2)+dudm(2,1))*(dvdm(1,2)+dvdm(2,1))
!
        s21 = dudm(1,1)*dvdm(1,1)*dtdm(1,1) + dudm(2,2)*dvdm(2,2)* dtdm(2,2) + vect(5)*dtdm(1,2) &
              &+ vect(2)*dtdm(2,1)
!
        s22 = vect(1)*(dtdm(1,1)+dtdm(2,2)) +vect(3)* dtdm(1,2) +vect(4)* dtdm(2,1)
!
        s23 = (&
              vect(6)+dudm(2, 1)*dvdm(2, 1))*dtdm(1, 1) +(vect(6)+dudm( 1, 2)*dvdm(1, 2))*dtdm(2,&
              2) +(vect(2)+vect(3))*dtdm(1, 2) +(vect(4)+vect(5))*dtdm(2, 1&
              )
!
    else
! Cas AXI
        s11 = dudm(1,1)*dvdm(1,1) + dudm(2,2)*dvdm(2,2) + dudm(3,3)* dvdm(3,3)
        s12 = dudm(1,1)*dvdm(2,2) + dudm(2,2)*dvdm(1,1) + dudm(1,1)* dvdm(3,3) + dudm(3,3)*dvdm(1&
              &,1) + dudm(2,2)*dvdm(3,3) + dudm( 3,3)*dvdm(2,2)
        s13 = (&
              dudm(1,2)+dudm(2,1))*(dvdm(1,2)+dvdm(2,1)) + (dudm(2,3) +dudm(3,2))*(dvdm(2,3)+dvdm&
              &(3,2)) + (dudm(3,1)+dudm(1,3))*( dvdm(3,1)+dvdm(1,3)&
              )
! Calcul de S2
        do 100 i = 1, 3
            do 101 j = 1, 3
                do 102 k = 1, 3
                    do 103 l = 1, 3
                        bil(i,j,k,l) = 0.5d0 * ( dudm(i,j)*dvdm(k,l)+ dudm(k,l)*dvdm(i,j) )
103                  continue
102              continue
101          continue
100      continue
!
        s21 = 0.d0
        do 110 k = 1, 3
            do 120 p = 1, 3
                s21 = s21 + bil(k,k,k,p)*dtdm(p,k)
120          continue
110      continue
!
        s22 = 0.d0
        do 300 k = 1, 3
            do 301 l = 1, 3
                if (l .ne. k) then
                    do 302 p = 1, 3
                        s22 = s22 + bil(l,l,k,p)*dtdm(p,k)
302                  continue
                endif
301          continue
300      continue
!
        s23 = 0.d0
        do 400 k = 1, 3
            do 401 l = 1, 3
                if (l .ne. k) then
                    do 402 m = 1, 3
                        if (m .ne. k .and. m .ne. l) then
                            do 403 p = 1, 3
                                s23 = s23 + bil(l,m,l,p)*dtdm(p,m)
                                s23 = s23 + bil(l,m,m,p)*dtdm(p,l)
403                          continue
                        endif
402                  continue
                endif
401          continue
400      continue
!
!
    endif
!
    s1 = c1*s11 + c2*s12 + c3*s13
    s2 = c1*s21 + c2*s22 + c3*s23
!
!--------------------------AUTRE MANIERE DE CALCUL POUR S2----------
!
!  TERME CLASSIQUE DU A LA THERMIQUE
!
    s1th = coef*epsthe*divv*e/(1.d0-2.d0*nu)
    prod = 0.d0
    do 20 i = 1, 2
        do 10 j = 1, 2
            prod = prod + dvdm(i,j)*dtdm(j,i)
10      continue
20  end do
    if (axi) prod = prod+dvdm(3,3)*dtdm(3,3)
    s2th = 0.5d0*th*coef*epsthe*prod*e/(1.d0-2.d0*nu)
!
    tcla = (-divt/2.d0*(s1-s1th)+ (s2-s2th))*poids
!
! - TERME THERMIQUE
!
    prod = 0.d0
    if (iret .eq. 0) then
        do 50 i = 1, 2
            prod = prod + tgdm(i)*dtdm(i,4)
50      continue
        tthe = poids*prod*divv*coef*alpha*e/(2.d0*(1.d0-2.d0*nu))
    else
        tthe = 0.d0
    endif
!
! - TERME FORCE VOLUMIQUE
!
    tfor=0.d0
    do 80 i = 1, 2
        prod=0.d0
        do 70 j = 1, 2
            prod = prod + dfdm(i,j)*dtdm(j,4)
70      continue
        tfor = tfor + cs*dvdm(i,4)*(prod+dfdm(i,4)*divt)*poids
80  end do
!
! - TERME DYNAMIQUE
!
    prod=0.d0
    do 200 i = 1, 2
        do 210 j = 1, 2
            prod = prod + dudm(i,j)*dtdm(j,4)*dvdm(i,4)+ dvdm(i,j)* dtdm(j,4)*dudm(i,4)
210      continue
200  end do
    tdyn = -0.5d0*rho*(puls**2)*prod*poids
!
    g = tcla+tthe+tfor+tdyn
!
end subroutine
