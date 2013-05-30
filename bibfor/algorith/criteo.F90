subroutine criteo(epsp, epsd, eta, ba, d,&
                  lambda, mu, alpha, ecrob, ecrod,&
                  seuil, crit, critp)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2004  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
    implicit none
    include 'asterfort/dfbde.h'
    include 'asterfort/dfdde.h'
    include 'asterfort/diago3.h'
    include 'asterfort/r8inir.h'
    include 'blas/ddot.h'
    real(kind=8) :: epsp(6), epsd(6), eta
    real(kind=8) :: ba(6), d
    real(kind=8) :: lambda, mu, alpha, seuil, ecrob, ecrod
    real(kind=8) :: crit, critp
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (PILOTAGE - PRED_ELAS - ENDO_ORTH_BETON)
!
! CALCUL DU CRITERE DE F(ETA) ET DE SA DERIVEE
!
! ----------------------------------------------------------------------
!
!
! IN  EPSP   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES FIXES
! IN  EPSD   : CORRECTION DE DEFORMATIONS DUES AUX CHARGES PILOTEES
! IN  ETA    : PARAMETRE DE PILOTAGE
! IN  LAMBDA : COEFFICIENT DE LAME
! IN  DEUXMU : COEFFICIENT DE LAME
! IN  ALPHA  : PARAMETRE DE LA LOI
! IN  ECROB  : PARAMETRE DE LA LOI
! IN  ECROD  : PARAMETRE DE LA LOI
! IN  D      : VARIABLE D'ENDOMMAGEMENT
! IN  BA     : TENSEUR D'ENDOMMAGEMENT
! IN  SEUIL  : SEUIL DU CRITERE
! OUT CRIT   : VALEUR DU CRITERE POUR ETA DONNEE EN ENTREE
! OUT CRITP  : VALEUR DE LA DERIVEE DU CRITERE POUR ETA DONNEE EN ENTREE
!
! ----------------------------------------------------------------------
!
    integer :: k, i, j, l, t(3, 3)
    real(kind=8) :: epsa(6), eps(6), epsdr(6), b(6)
    real(kind=8) :: fb(6), fbr(6), fbm(6), fd, rec(6)
    real(kind=8) :: cc(6), vecc(3, 3), valcc(3), ccp(6), cpe(6), valb(3)
    real(kind=8) :: vecb(3, 3)
    real(kind=8) :: valfb(3), vecfb(3, 3)
    real(kind=8) :: tdfbde(6, 6), tdfdde(6), dfde(6)
    real(kind=8) :: rtemp, treb, treps, trem, dcoefd, ene, coupl
    real(kind=8) :: tole, rac2, kron(6)
    data  kron/1.d0,1.d0,1.d0,0.d0,0.d0,0.d0/
!
! ----------------------------------------------------------------------
!
! TOLE: TOLERANCE POUR ARRET EVOLUTION DE L ENDOMMAGEMENT
    tole = 1.d-2
!
    rac2 = sqrt(2.d0)
!
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
!
!
!
    epsa(1)=epsp(1)+eta*epsd(1)
    epsa(2)=epsp(2)+eta*epsd(2)
    epsa(3)=epsp(3)+eta*epsd(3)
    epsa(4)=epsp(4)+eta*epsd(4)
    epsa(5)=epsp(5)+eta*epsd(5)
    epsa(6)=epsp(6)+eta*epsd(6)
!
!
!
!--ON TRAVAILLE DANS ESPACE PROPRE ENDO
!
    call r8inir(6, 1.d0, rec, 1)
    call r8inir(6, 0.d0, b, 1)
    call r8inir(6, 0.d0, eps, 1)
    call r8inir(6, 0.d0, epsdr, 1)
!
    call diago3(ba, vecb, valb)
    do 201 i = 1, 3
        b(i)=valb(i)
201  end do
!
    if (abs(valb(1)) .lt. tole) then
        rec(1)=0.d0
        rec(4)=0.d0
        rec(5)=0.d0
    endif
    if (abs(valb(2)) .lt. tole) then
        rec(2)=0.d0
        rec(4)=0.d0
        rec(6)=0.d0
    endif
    if (abs(valb(3)) .lt. tole) then
        rec(3)=0.d0
        rec(5)=0.d0
        rec(6)=0.d0
    endif
!
!
!
    do 202 i = 1, 3
        do 203 j = i, 3
            do 204 k = 1, 3
                do 205 l = 1, 3
                    eps(t(i,j))=eps(t(i,j))+vecb(k,i)*epsa(t(k,l))*&
                    vecb(l,j)
                    epsdr(t(i,j))=epsdr(t(i,j))+vecb(k,i)*epsd(t(k,l))&
                    *vecb(l,j)
205              continue
204          continue
203      continue
202  end do
!
!
!
!--------------------------------------------------------------------
!---CALCUL DU CRITERE------------------------------------------------
!--------------------------------------------------------------------
!
    call r8inir(6, 0.d0, cc, 1)
!
    do 9 i = 1, 3
        do 10 j = i, 3
            do 11 k = 1, 3
                cc(t(i,j))=cc(t(i,j))+b(t(i,k))*eps(t(k,j))+ b(t(j,k))&
                *eps(t(k,i))
11          continue
10      continue
 9  end do
    call diago3(cc, vecc, valcc)
    call r8inir(6, 0.d0, ccp, 1)
    call r8inir(6, 0.d0, cpe, 1)
    do 12 i = 1, 3
        if (valcc(i) .lt. 0.d0) then
            valcc(i)=0.d0
        endif
12  end do
    do 13 i = 1, 3
        do 14 j = i, 3
            do 15 k = 1, 3
                ccp(t(i,j))=ccp(t(i,j))+vecc(i,k)*valcc(k)*vecc(j,k)
15          continue
14      continue
13  end do
    do 16 i = 1, 3
        do 17 j = i, 3
            do 18 k = 1, 3
                cpe(t(i,j))=cpe(t(i,j))+ ccp(t(i,k))*eps(t(k,j))+&
                ccp(t(j,k))*eps(t(k,i))
18          continue
17      continue
16  end do
!
    call r8inir(6, 0.d0, fb, 1)
    treb=0.d0
    do 301 i = 1, 3
        treb=treb+cc(i)/2
301  end do
    if (treb .gt. 0.d0) then
        do 19 i = 1, 6
            fb(i)=-lambda*treb*eps(i)
19      continue
    endif
    do 20 i = 1, 6
        fb(i)=fb(i)-mu/2.d0*cpe(i)+ecrob*(kron(i)-b(i))
20  end do
!
!
    do 100 i = 1, 6
        fbr(i)=fb(i)*rec(i)
100  end do
!
!
    call diago3(fbr, vecfb, valfb)
    rtemp=0.d0
    do 29 i = 1, 3
        if (valfb(i) .gt. 0.d0) then
            valfb(i)=0.d0
        endif
        rtemp=rtemp+valfb(i)*valfb(i)
29  end do
!
    call r8inir(6, 0.d0, fbm, 1)
    do 26 i = 1, 3
        do 27 j = i, 3
            do 28 k = 1, 3
                fbm(t(i,j))=fbm(t(i,j))+vecfb(i,k)*valfb(k)*vecfb(j,k)
28          continue
27      continue
26  end do
!
!
    treps=eps(1)+eps(2)+eps(3)
!
!
!
    call diago3(eps, vecc, valcc)
    do 22 i = 1, 3
        if (valcc(i) .gt. 0.d0) then
            valcc(i)=0.d0
        endif
22  end do
!
    call r8inir(6, 0.d0, ccp, 1)
!
    do 23 i = 1, 3
        do 24 j = i, 3
            do 25 k = 1, 3
                ccp(t(i,j))=ccp(t(i,j))+vecc(i,k)*valcc(k)*vecc(j,k)
25          continue
24      continue
23  end do
!
    trem=valcc(1)**2+valcc(2)**2+valcc(3)**2
    if (treps .gt. 0.d0) then
        treps=0.d0
    endif
    dcoefd=2.d0*(1.d0-d)
    ene=lambda/2*treps**2+mu*trem
    fd=dcoefd*ene-2.d0*d*ecrod
    if (fd .lt. 0.d0) then
        fd=0.d0
    endif
!
    coupl=sqrt(alpha*rtemp+(1-alpha)*fd**2)
    crit=coupl-seuil
!
!
!
!----------------------------------------------------------------
!----CALCUL DE LA DERIVEE DU CRITERE-----------------------------
!----------------------------------------------------------------
!
    fbm(4)=rac2*fbm(4)
    fbm(5)=rac2*fbm(5)
    fbm(6)=rac2*fbm(6)
!
    call dfbde(3, b, eps, 2.d0*mu, lambda,&
               tdfbde)
    call dfdde(eps, d, 3, lambda, mu,&
               tdfdde)
!
!
    call r8inir(6, 0.d0, dfde, 1)
!
    if (coupl .gt. 1.d-20) then
        do 101 i = 1, 6
            do 102 j = 1, 6
                dfde(i)=dfde(i)+alpha/coupl*fbm(j)*tdfbde(j,i)*rec(j)
102          continue
            dfde(i)=dfde(i)+(1.d0-alpha)*fd/coupl*tdfdde(i)
101      end do
    endif
!
    do 52 i = 4, 6
        epsdr(i)=epsdr(i)*rac2
52  end do
!
    critp=ddot(6,dfde,1,epsdr,1)
!
!
end subroutine
