subroutine gbil3d(dudm, dvdm, dtdm, dfudm, dfvdm,&
                  tgudm, tgvdm, ttrgu, ttrgv, poids,sigin,&
                  dsigin,epsref,&
                  c1, c2, c3, k3a, alpha, coef, rho,&
                  puls, g)
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
!
#include "asterfort/assert.h"
    real(kind=8) :: dudm(3, 4), dvdm(3, 4), dtdm(3, 4)
    real(kind=8) :: dfudm(3, 4), dfvdm(3, 4), tgudm(3), tgvdm(3)
    real(kind=8) :: c1, c2, c3, poids, g, k3a, rho, puls, ttrgu, ttrgv, gcla
    real(kind=8) :: alpha, coef, enethu(2), enethv(2)
!
! ----------------------------------------------------------------------
!     CALCUL DU TAUX DE RESTITUTION D'ENERGIE G SOUS LA FORME
!     BILINEAIRE SYMETRIQUE G(U,V) EN ELASTICITE LINEAIRE EN 3D
! ----------------------------------------------------------------------
!
    real(kind=8) :: vect(33), s11, s12, s13, s21, s22, s23, s1, s2, sa21, sa22
    real(kind=8) :: sa23,rac2,temp1(6),temp2(6),epsref(6),epsu(6),epsv(6)
    real(kind=8) :: divt, bil(3, 3, 3, 3), sigin(6), dsigin(6,3), epsthe
    real(kind=8) :: divv, divu, ttt1u, ttt1v, tini1, tini2, tini3
    real(kind=8) :: somm1u, somm2u, somm1v, somm2v, prod,siginv(3,3)
    real(kind=8) :: s2th1u, s2th2u, s2th3u, s2th1v, s2th2v, s2th3v
    real(kind=8) :: s1th, s2th, tther, tfor, tdyn
    integer :: i, j, k, p, l, m
!
    divt = dtdm(1,1) + dtdm(2,2) + dtdm(3,3)
!
    rac2 = sqrt(2.d0)
! INITIALISATION DES TENSEURS DE DEFORMATION TOTALE
    epsu(1)=dudm(1,1)
    epsu(2)=dudm(2,2)
    epsu(3)=dudm(3,3)
    epsu(4)=0.5d0*(dudm(1,2)+dudm(2,1))*rac2
    epsu(5)=0.5d0*(dudm(1,3)+dudm(3,1))*rac2
    epsu(6)=0.5d0*(dudm(3,2)+dudm(2,3))*rac2

    epsv(1)=dvdm(1,1)
    epsv(2)=dvdm(2,2)
    epsv(3)=dvdm(3,3)
    epsv(4)=0.5d0*(dvdm(1,2)+dvdm(2,1))*rac2
    epsv(5)=0.5d0*(dvdm(1,3)+dvdm(3,1))*rac2
    epsv(6)=0.5d0*(dvdm(3,2)+dvdm(2,3))*rac2

! ECRITURE DE SIGIN SOUS FORME DE TENSEUR (les racines de 2 sont incluses dans sigin)
    siginv(1,1) = sigin(1)
    siginv(2,2) = sigin(2)
    siginv(3,3) = sigin(3)
    siginv(1,2) = sigin(4)/rac2
    siginv(1,3) = sigin(5)/rac2
    siginv(2,3) = sigin(6)/rac2
    siginv(2,1) = sigin(4)/rac2
    siginv(3,1) = sigin(5)/rac2
    siginv(3,2) = sigin(6)/rac2




!INITIALISATION DE TEMP1 et TEMP2
    do 60 i=1,6
      temp1(i)=0.d0
      temp2(i)=0.d0
60  continue
!
! - TERME CLASSIQUE S1 + S2
!
    vect(1) = 0.5d0 * (dvdm(1,1)*dudm(2,2)+dudm(1,1)*dvdm(2,2))
    vect(2) = 0.5d0 * (dvdm(1,1)*dudm(1,2)+dudm(1,1)*dvdm(1,2))
    vect(3) = 0.5d0 * (dvdm(1,1)*dudm(2,1)+dudm(1,1)*dvdm(2,1))
    vect(4) = 0.5d0 * (dvdm(2,2)*dudm(1,2)+dudm(2,2)*dvdm(1,2))
    vect(5) = 0.5d0 * (dvdm(2,2)*dudm(2,1)+dudm(2,2)*dvdm(2,1))
    vect(6) = 0.5d0 * (dvdm(1,2)*dudm(2,1)+dudm(1,2)*dvdm(2,1))
    vect(7) = 0.5d0 * (dvdm(3,3)*dudm(3,1)+dudm(3,3)*dvdm(3,1))
    vect(8) = 0.5d0 * (dvdm(1,1)*dudm(1,3)+dudm(1,1)*dvdm(1,3))
    vect(9) = 0.5d0 * (dvdm(3,3)*dudm(3,2)+dudm(3,3)*dvdm(3,2))
    vect(10) = 0.5d0 * (dvdm(2,2)*dudm(2,3)+dudm(2,2)*dvdm(2,3))
    vect(11) = 0.5d0 * (dvdm(1,1)*dudm(3,3)+dudm(1,1)*dvdm(3,3))
    vect(12) = 0.5d0 * (dvdm(2,2)*dudm(3,3)+dudm(2,2)*dvdm(3,3))
    vect(13) = 0.5d0 * (dvdm(3,3)*dudm(1,2)+dudm(3,3)*dvdm(1,2))
    vect(14) = 0.5d0 * (dvdm(3,3)*dudm(2,1)+dudm(3,3)*dvdm(2,1))
    vect(15) = 0.5d0 * (dvdm(1,1)*dudm(3,1)+dudm(1,1)*dvdm(3,1))
    vect(16) = 0.5d0 * (dvdm(2,2)*dudm(3,1)+dudm(2,2)*dvdm(3,1))
    vect(17) = 0.5d0 * (dvdm(1,1)*dudm(3,2)+dudm(1,1)*dvdm(3,2))
    vect(18) = 0.5d0 * (dvdm(2,2)*dudm(3,2)+dudm(2,2)*dvdm(3,2))
    vect(19) = 0.5d0 * (dvdm(1,1)*dudm(2,3)+dudm(1,1)*dvdm(2,3))
    vect(20) = 0.5d0 * (dvdm(3,3)*dudm(2,3)+dudm(3,3)*dvdm(2,3))
    vect(21) = 0.5d0 * (dvdm(2,2)*dudm(1,3)+dudm(2,2)*dvdm(1,3))
    vect(22) = 0.5d0 * (dvdm(3,3)*dudm(1,3)+dudm(3,3)*dvdm(1,3))
    vect(23) = 0.5d0 * (dvdm(1,3)*dudm(3,1)+dudm(1,3)*dvdm(3,1))
    vect(24) = 0.5d0 * (dvdm(2,3)*dudm(3,2)+dudm(2,3)*dvdm(3,2))
    vect(25) = 0.5d0 * (dvdm(2,3)*dudm(3,1)+dudm(2,3)*dvdm(3,1))
    vect(26) = 0.5d0 * (dvdm(3,2)*dudm(3,1)+dudm(3,2)*dvdm(3,1))
    vect(27) = 0.5d0 * (dvdm(1,3)*dudm(3,2)+dudm(1,3)*dvdm(3,2))
    vect(28) = 0.5d0 * (dvdm(1,2)*dudm(1,3)+dudm(1,2)*dvdm(1,3))
    vect(29) = 0.5d0 * (dvdm(2,1)*dudm(1,3)+dudm(2,1)*dvdm(1,3))
    vect(30) = 0.5d0 * (dvdm(1,2)*dudm(3,1)+dudm(1,2)*dvdm(3,1))
    vect(31) = 0.5d0 * (dvdm(2,1)*dudm(2,3)+dudm(2,1)*dvdm(2,3))
    vect(32) = 0.5d0 * (dvdm(2,1)*dudm(3,2)+dudm(2,1)*dvdm(3,2))
    vect(33) = 0.5d0 * (dvdm(2,3)*dudm(1,2)+dudm(2,3)*dvdm(1,2))
!
!
    s11 = dudm(1,1)*dvdm(1,1) + dudm(2,2)*dvdm(2,2) + dudm(3,3)*dvdm(3,3)
    s12 = dudm(1,1)*dvdm(2,2) + dudm(2,2)*dvdm(1,1) + dudm(1,1)*dvdm(3,3) + dudm(3,3)*dvdm(1,1) +&
          & dudm(2,2)*dvdm(3,3) + dudm(3,3)*dvdm(2,2)
    s13 = (&
          dudm(1,2)+dudm(2,1))*(dvdm(1,2)+dvdm(2,1)) + (dudm(2,3)+dudm(3,2))*(dvdm(2,3)+dvdm(3,2)&
          &) + (dudm(3,1)+dudm(1,3))*(dvdm(3,1)+dvdm(1,3)&
          )
!
    s21 = dudm(1,1)*dvdm(1,1)*dtdm(1,1) + dudm(2,2)*dvdm(2,2)*dtdm(2,2) + dudm(3,3)*dvdm(3,3)*dtd&
          &m(3,3) + vect(5)*dtdm(1,2) + vect(2)*dtdm(2,1) + vect(7)*dtdm(1,3) + vect(8)*dtdm(3,1)&
          & + vect(9)*dtdm(2,3) + vect(10)*dtdm(3,2)
!
    s22 = vect(1)*(dtdm(1,1)+dtdm(2,2)) + vect(11)*(dtdm(1,1)+dtdm(3,3)) + vect(12)*(dtdm(2,2)+dt&
          &dm(3,3)) + (vect(3)+vect(14))*dtdm(1,2) + (vect(4)+vect(13))*dtdm(2,1) + (vect(15)+vec&
          &t(16))*dtdm(1,3) + (vect(17)+vect(18))*dtdm(2,3) + (vect(19)+vect(20))*dtdm(3,2) + (ve&
          &ct(21)+vect(22))*dtdm(3,1)
!
    s23 = (&
          vect(6)+dudm(2, 1)*dvdm(2, 1))*dtdm(1, 1) + (vect(23)+dudm(3, 1)*dvdm(3, 1))*dtdm(1,&
          1) + (vect(6)+dudm(1, 2)*dvdm(1, 2))*dtdm(2,&
          2) + (vect(24)+dudm(3, 2)*dvdm(3, 2))*dtdm(2,&
          2) + (vect(24)+dudm(2, 3)*dvdm(2, 3))*dtdm(3,&
          3) + (vect(23)+dudm(1, 3)*dvdm(1, 3))*dtdm(3, 3) + (vect(2)+vect(3))*dtdm(1,&
          2) + (vect(25)+vect(26))*dtdm(1, 2) + (vect(4)+vect(5))*dtdm(2,&
          1) + (vect(26)+vect(27))*dtdm(2, 1) + (vect(28)+vect(29))*dtdm(3,&
          2) + (vect(9)+vect(20))*dtdm(3, 2) + (vect(10)+vect(18))*dtdm(2,&
          3) + (vect(28)+vect(30))*dtdm(2, 3) + (vect(8)+vect(15))*dtdm(1,&
          3) + (vect(31)+vect(32))*dtdm(1, 3) + (vect(31)+vect(33))*dtdm(3,&
          1) + (vect(22)+vect(7))*dtdm(3, 1&
          )
!
    s1 = c1*s11 + c2*s12 + c3*s13
    s2 = c1*s21 + c2*s22 + c3*s23
!
!--------------------------AUTRE MANIERE DE CALCUL POUR S2----------
    do 100 i = 1, 3
        do 101 j = 1, 3
            do 102 k = 1, 3
                do 103 l = 1, 3
                    bil(i,j,k,l) = 0.5d0 * ( dudm(i,j)*dvdm(k,l)+dudm( k,l)*dvdm(i,j))
103              continue
102          continue
101      continue
100  continue
!
    sa21 = 0.d0
    do 10 k = 1, 3
        do 20 p = 1, 3
            sa21 = sa21 + bil(k,k,k,p)*dtdm(p,k)
20      continue
10  continue
!
    sa22 = 0.d0
    do 300 k = 1, 3
        do 301 l = 1, 3
            if (l .ne. k) then
                do 302 p = 1, 3
                    sa22 = sa22 + bil(l,l,k,p)*dtdm(p,k)
302              continue
            endif
301      continue
300  continue
!
    sa23 = 0.d0
    do 400 k = 1, 3
        do 401 l = 1, 3
            if (l .ne. k) then
                do 402 m = 1, 3
                    if (m .ne. k .and. m .ne. l) then
                        do 403 p = 1, 3
                            sa23 = sa23 + bil(l,m,l,p)*dtdm(p,m)
                            sa23 = sa23 + bil(l,m,m,p)*dtdm(p,l)
403                      continue
                    endif
402              continue
            endif
401      continue
400  continue
!
    ASSERT(abs(sa21-s21) .le. max(1.d-8, abs(sa21)*1.d-8))
    ASSERT(abs(sa22-s22) .le. max(1.d-8, abs(sa22)*1.d-8))
    ASSERT(abs(sa23-s23) .le. max(1.d-8, abs(sa23)*1.d-8))
!
!
!
! - TERME CLASSIQUE DU A LA THERMIQUE S1TH + S2TH
!
    divv = dvdm(1,1) + dvdm(2,2) + dvdm(3,3)
    divu = dudm(1,1) + dudm(2,2) + dudm(3,3)
    enethv(1) = k3a * ttrgu * divv
    enethu(1) = k3a * ttrgv * divu
!
!   POUR CALCULER G : coef = 2.0
!   POUR CALCULER K : coef = 1.0
    s1th = coef*enethv(1)

!! AJOUTER LE TERME CONSTANT POUR COMPARER AVEC CALC_G
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    s1th = s1th - 3*k3a*alpha*ttrgu*ttrgv

    
    
    s2th1v = dvdm(1,1) * dtdm(1,1) + dvdm(1,2) * dtdm(2,1) + dvdm(1,3) * dtdm(3,1)
    s2th2v = dvdm(2,1) * dtdm(1,2) + dvdm(2,2) * dtdm(2,2) + dvdm(2,3) * dtdm(3,2)
    s2th3v = dvdm(3,1) * dtdm(1,3) + dvdm(3,2) * dtdm(2,3) + dvdm(3,3) * dtdm(3,3)
!
    s2th1u = dudm(1,1) * dtdm(1,1) + dudm(1,2) * dtdm(2,1) + dudm(1,3) * dtdm(3,1)
    s2th2u = dudm(2,1) * dtdm(1,2) + dudm(2,2) * dtdm(2,2) + dudm(2,3) * dtdm(3,2)
    s2th3u = dudm(3,1) * dtdm(1,3) + dudm(3,2) * dtdm(2,3) + dudm(3,3) * dtdm(3,3)
!
    s2th = 0.5d0 *coef* (&
           k3a * ttrgu * (s2th1v + s2th2v + s2th3v))
!
    gcla = (s2 - s2th - 0.5d0 * (s1 - s1th) * divt) * poids

!
! - TERME FORCE VOLUMIQUE
!
    somm1u = 0.0d0
    do 500 i = 1, 3
        do 550 j = 1, 3
            somm1u = somm1u + 0.5d0 * dfudm(i,j) * dtdm(j,4) * dvdm ( i,4)
550      continue
500  continue
!
    somm1v = 0.0d0
    do 1000 i = 1, 3
        do 1010 j = 1, 3
            somm1v = somm1v + 0.5d0 * dfvdm(i,j) * dtdm(j,4) * dudm ( i,4)
1010      continue
1000  continue
!
    somm2u = 0.0d0
    do 600 i = 1, 3
        somm2u = somm2u + 0.5d0 * dfudm(i,4) * dvdm(i,4) * divt
600  continue
!
    somm2v = 0.0d0
    do 164 i = 1, 3
        somm2v = somm2v + 0.5d0 * dfvdm(i,4) * dudm(i,4) * divt
164 continue
!
!
    tfor = (somm1u + somm1v + somm2u + somm2v) * poids
!
!  - TERME THERMIQUE
!
    ttt1u = 0.0d0
    do 700 j = 1, 3
        ttt1u = ttt1u + 0.5d0 * tgudm(j) * dtdm(j,4)
700  continue
!
    ttt1v = 0.0d0
    do 174 j = 1, 3
        ttt1v = ttt1v + 0.5d0 * tgvdm(j) * dtdm(j,4)
174 continue
!
    enethv(2) = -1.d0 * k3a * divv
    enethu(2) = -1.d0 * k3a * divu
!
    tther = -coef*(ttt1u * enethv(2))*poids

! AJOUTER LE TERME CONSTANT POUR COMPARER AVEC CALC_G
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!! ATTENTION AUX FACTEUR 0.5 DANS tttlu   
    tther = tther -3.d0*k3a*alpha*(ttrgv*ttt1u*coef)*poids
!  - TERME DYNAMIQUE
!
    prod = 0.d0
    do 800 i = 1, 3
        do 810 j = 1, 3
            prod = prod + dudm(i,j) * dtdm(j,4) * dvdm(i,4) + dvdm(i, j) * dtdm(j,4) * dudm(i,4)
810      continue
800  continue
!
    tdyn = -0.5d0*rho*puls*puls*prod*poids
!
!
!
! - TERMES LIES A LA CONTRAINTE INITIALE
!
!   TINI1 : TERME DE CONTRAINTES INITIALES SEULES -(EPS-EPSTHE*Id2-EPSREF):GRAD(SIGIN).THETA
!   Deformation thermique
    epsthe = alpha*ttrgu

!si coef =2, on calcule G
!si coef = 1, on calcule K
    tini1 =0
    do 601 j=1,6
       do 602 i=1,3
          temp1(j)=temp1(j)+dsigin(j,i)*dtdm(i,4)
602    continue
601 continue

    if (coef.gt.1.9) then
       
       do 501 i=1,3
          tini1 =tini1-(epsu(i)-epsthe-epsref(i))*temp1(i)
501    continue
! Le terme thermique n'apparait pas hors diagonale
          tini1 =tini1-(epsu(4)-epsref(4))*temp1(4)
          tini1 =tini1-(epsu(5)-epsref(5))*temp1(5)
          tini1 =tini1-(epsu(6)-epsref(6))*temp1(6)
    else if (coef.lt.1.1) then
       do 503 i=1,6
             tini1 =tini1-0.5d0*(epsv(i))*temp1(i)
503    continue
    end if
    tini1=tini1*poids
!
!   TINI2 : TERME DU A LA MODIFICATION DE LA CONTRAINTE SIGIN: GRAD(U).GRAD(THETA)
    tini2 =0
! ce terme ne s'exprime pas en notation de Voigt, on doit le calculer terme a terme (trop cool)
    if (coef.gt.1.9) then
!        temp2(1)=dudm(1,1)*dtdm(1,1)+dudm(1,2)*dtdm(2,1)+dudm(1,3)*dtdm(3,1)
!        temp2(2)=dudm(2,1)*dtdm(1,2)+dudm(2,2)*dtdm(2,2)+dudm(2,3)*dtdm(3,2)
!        temp2(3)=dudm(3,1)*dtdm(1,3)+dudm(3,2)*dtdm(2,3)+dudm(3,3)*dtdm(3,3)

!        temp2(4)=(dudm(1,1)*dtdm(1,2)+dudm(1,2)*dtdm(2,2)+dudm(1,3)*dtdm(3,2)+&
!        dudm(2,1)*dtdm(1,1)+dudm(2,2)*dtdm(2,1)+dudm(2,3)*dtdm(3,1))/rac2


!        temp2(5)=(dudm(1,1)*dtdm(1,3)+dudm(1,2)*dtdm(2,3)+dudm(1,3)*dtdm(3,3)+&
!        dudm(3,1)*dtdm(1,1)+dudm(3,2)*dtdm(2,1)+dudm(3,3)*dtdm(3,1))/rac2
       
!        temp2(6)=(dudm(2,1)*dtdm(1,3)+dudm(2,2)*dtdm(2,3)+dudm(2,3)*dtdm(3,3)+&
!        dudm(3,1)*dtdm(1,2)+dudm(3,2)*dtdm(2,2)+dudm(3,3)*dtdm(3,2))/rac2 
        do 551 i = 1, 3
            do 540 j = 1, 3
                do 530 k = 1, 3
                    do 520 m = 1, 3
                        tini2 = tini2 + siginv(j,k)*dudm(i,m)*dtdm(m, k)
520                  continue
530              continue
540          continue
551      continue        
    else if (coef.lt.1.1) then
        temp2(1)=0.5d0*(dvdm(1,1)*dtdm(1,1)+dvdm(1,2)*dtdm(2,1)+dvdm(1,3)*dtdm(3,1))
        temp2(2)=0.5d0*(dvdm(2,1)*dtdm(1,2)+dvdm(2,2)*dtdm(2,2)+dvdm(2,3)*dtdm(3,2))
        temp2(3)=0.5d0*(dvdm(3,1)*dtdm(1,3)+dvdm(3,2)*dtdm(2,3)+dvdm(3,3)*dtdm(3,3))

        temp2(4)=0.5d0*(dvdm(1,1)*dtdm(1,2)+dvdm(1,2)*dtdm(2,2)+dvdm(1,3)*dtdm(3,2)+&
        dvdm(2,1)*dtdm(1,1)+dvdm(2,2)*dtdm(2,1)+dvdm(2,3)*dtdm(3,1))/rac2


        temp2(5)=0.5d0*(dvdm(1,1)*dtdm(1,3)+dvdm(1,2)*dtdm(2,3)+dvdm(1,3)*dtdm(3,3)+&
        dvdm(3,1)*dtdm(1,1)+dvdm(3,2)*dtdm(2,1)+dvdm(3,3)*dtdm(3,1))/rac2
       
        temp2(6)=0.5d0*(dvdm(2,1)*dtdm(1,3)+dvdm(2,2)*dtdm(2,3)+dvdm(2,3)*dtdm(3,3)+&
        dvdm(3,1)*dtdm(1,2)+dvdm(3,2)*dtdm(2,2)+dvdm(3,3)*dtdm(3,2))/rac2 

       do 504 i=1,6
        tini2=tini2+sigin(i)*temp2(i)
504    continue 
!        do 552 i = 1, 3
!            do 542 j = 1, 3
!                do 532 k = 1, 3
!                    do 522 m = 1, 3
!                        tini2 = tini2 + 0.5d0*siginv(j,k)*dvdm(i,m)*dtdm(m, k)
!522                  continue
!532              continue
!542          continue
!552      continue  


    endif

    tini2=tini2*poids
!
!   TINI3:TERME DU A LA MODIFICATION DE L'ENERGIE LIBRE:-1/2*(2*(EPS-EPSTH)-EPSREF):SIGIN divTheta
    tini3 =0
    if (coef.gt.1.9) then
       do 505 i=1,3
             tini3=tini3-(epsu(i)-epsthe-0.5d0*epsref(i))*sigin(i)*divt
505    continue   
       tini3=tini3-(epsu(4)-0.5d0*epsref(4))*sigin(4)*divt
       tini3=tini3-(epsu(5)-0.5d0*epsref(5))*sigin(5)*divt
       tini3=tini3-(epsu(6)-0.5d0*epsref(6))*sigin(6)*divt
       
    else if (coef.lt.1.1)  then
       do 506 i=1,6
             tini3=tini3-0.5d0*epsv(i)*sigin(i)*divt
506    continue   
    endif
    tini3=tini3*poids 
    
!   SOMME FINALE DE CALCUL DE G  
    g = gcla + tfor + tther + tdyn + tini1 + tini2 + tini3
end subroutine
