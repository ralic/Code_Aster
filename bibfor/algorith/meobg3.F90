subroutine meobg3(eps, epsg, b, d, deltab,&
                  deltad, mult, lambda, mu, ecrob,&
                  ecrod, alpha, k1, k2, bdim,&
                  dsidep)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
    include 'asterfort/ceobfb.h'
    include 'asterfort/ceobfd.h'
    include 'asterfort/dfbdb.h'
    include 'asterfort/dfbde.h'
    include 'asterfort/dfddd.h'
    include 'asterfort/dfdde.h'
    include 'asterfort/dfmdf.h'
    include 'asterfort/mgauss.h'
    include 'asterfort/r8inir.h'
    real(kind=8) :: eps(6), epsg(6), b(6), d, dsidep(6, 6)
    real(kind=8) :: deltab(6), deltad, mult
    real(kind=8) :: lambda, mu, alpha, k1, k2, ecrob, ecrod
    integer :: bdim
!
!--CALCUL DE LA MATRICE TANGENTE POUR LA LOI ENDO_ORTHO_BETON
!  VERSION NON LOCALE
!-------------------------------------------------------------
!
    integer :: i, j, k, iret
    real(kind=8) :: rac2, nofbm, un, deux
    real(kind=8) :: det, fb(6), fbm(6)
    real(kind=8) :: trepsg, fd
    real(kind=8) :: dfbmdf(6, 6), tdfbdb(6, 6), tdfbde(6, 6)
    real(kind=8) :: tdfdde(6), tdfddd
    real(kind=8) :: interd(6), intert(6), interg(6)
    real(kind=8) :: psi(6, 6), ksi(6, 6), iksi(6, 6)
    real(kind=8) :: matb(6, 6), matd(6)
    real(kind=8) :: dsigb(6, 6), dsigd(6)
    real(kind=8) :: coupl, dcrit(6)
!
    deux=2.d0
    rac2=sqrt(deux)
    un=1.d0
!
    call r8inir(36, 0.d0, dsidep, 1)
!
!-------------------------------------------------------
!-------------------------------------------------------
!----CALCUL DE FB: FORCE THERMO ASSOCIEE A
!-------------------ENDOMMAGEMENT ANISOTROPE DE TRACTION
!
    call ceobfb(b, epsg, lambda, mu, ecrob,&
                bdim, fb, nofbm, fbm)
!
!----CALCUL DE FD: PARTIE POSITIVE DE LA FORCE THERMO ASSOCIEE A
!-------------------ENDOMMAGEMENT ISOTROPE DE COMPRESSION
!
    call ceobfd(d, epsg, lambda, mu, ecrod,&
                fd)
!
!---CALCUL DE DERIVEES UTILES----------------------------------
!
    call dfmdf(6, fb, dfbmdf)
    call dfbdb(3, b, epsg, deux*mu, lambda,&
               ecrob, tdfbdb)
    call dfbde(3, b, epsg, deux*mu, lambda,&
               tdfbde)
    call dfbde(3, b, eps, deux*mu, lambda,&
               dsigb)
!
    call dfdde(epsg, d, 3, lambda, mu,&
               tdfdde)
    call dfdde(eps, d, 3, lambda, mu,&
               dsigd)
    call dfddd(epsg, d, 3, lambda, mu,&
               ecrod, tdfddd)
!
!
    trepsg=epsg(1)+epsg(2)+epsg(3)
    if (trepsg .gt. 0.d0) then
        trepsg=0.d0
    endif
    dcrit(1)=-k1*(-trepsg/k2/(un+(-trepsg/k2)**deux)&
     &           +atan2(-trepsg/k2,un))
    dcrit(2)=-k1*(-trepsg/k2/(un+(-trepsg/k2)**deux)&
     &           +atan2(-trepsg/k2,un))
    dcrit(3)=-k1*(-trepsg/k2/(un+(-trepsg/k2)**deux)&
     &           +atan2(-trepsg/k2,un))
    dcrit(4)=0.d0
    dcrit(5)=0.d0
    dcrit(6)=0.d0
!
    do 101 i = 4, 6
        fbm(i)=rac2*fbm(i)
        deltab(i)=deltab(i)*rac2
101  end do
!
    nofbm=fbm(1)**2+fbm(2)**2+fbm(3)**2+fbm(4)**2&
     &        +fbm(5)**2+fbm(6)**2
!
    coupl=sqrt(alpha*nofbm+(un-alpha)*fd**deux)
!
    if ((fd.ne.0.d0) .and. (nofbm.ne.0.d0)) then
!---CALCUL DE DBDE ET DDDE-------------------------------------
!
!---CALCUL DE KSI ET PSI
!
        call r8inir(6, 0.d0, interd, 1)
        call r8inir(6, 0.d0, interg, 1)
        call r8inir(6, 0.d0, intert, 1)
        call r8inir(36, 0.d0, psi, 1)
        call r8inir(36, 0.d0, ksi, 1)
!
        do 110 i = 1, 6
            interg(i)=deltab(i)/fd-alpha*fbm(i)/(un-alpha)/fd/tdfddd
            intert(i)=(un-alpha)*fd*tdfdde(i)-coupl*dcrit(i)
            do 111 j = 1, 6
                do 112 k = 1, 6
                    ksi(i,j)=ksi(i,j)+alpha*deltad*dfbmdf(i,k)*tdfbdb(&
                    k,j)
                    interd(i)=interd(i)+alpha*fbm(k)*dfbmdf(k,j)*&
                    tdfbdb(j,i)
                    psi(i,j)=psi(i,j)-alpha*deltad*dfbmdf(i,k)*tdfbde(&
                    k,j)
                    intert(i)=intert(i)+alpha*fbm(k)*dfbmdf(k,j)*&
                    tdfbde(j,i)
112              continue
111          continue
110      end do
!
        do 120 i = 1, 6
            ksi(i,i)=ksi(i,i)-(un-alpha)*fd
120      end do
!
        do 130 i = 1, 6
            do 131 j = 1, 6
                ksi(i,j)=ksi(i,j)+interg(i)*interd(j)
                psi(i,j)=psi(i,j)-interg(i)*intert(j) +(un-alpha)*&
                deltab(i)*tdfdde(j)
131          continue
130      end do
!
        call r8inir(36, 0.d0, iksi, 1)
        do 140 i = 1, 6
            iksi(i,i)=1.d0
140      end do
!
        call mgauss('NCVP', ksi, iksi, 6, 6,&
                    6, det, iret)
!
        if (iret .ne. 0) goto 999
!
!-- ! ksi n est plus disponible
!
        call r8inir(36, 0.d0, matb, 1)
        call r8inir(6, 0.d0, matd, 1)
!
        do 150 i = 1, 6
            matd(i)=-intert(i)/(un-alpha)/fd/tdfddd
            do 151 j = 1, 6
                do 152 k = 1, 6
                    matb(i,j)=matb(i,j)+iksi(i,k)*psi(k,j)
                    matd(i)=matd(i)-interd(j)*iksi(j,k)*psi(k,i)&
                    /(un-alpha)/fd/tdfddd
152              continue
151          continue
150      continue
!
        do 201 i = 1, 6
            do 202 j = 1, 6
                dsidep(i,j)=-dsigd(i)*matd(j)
                do 203 k = 1, 6
                    dsidep(i,j)=dsidep(i,j)-dsigb(k,i)*matb(k,j)
203              continue
202          continue
201      continue
!
    else if ((fd.eq.0.d0).and.(nofbm.ne.0.d0)) then
!
        call r8inir(36, 0.d0, ksi, 1)
        call r8inir(36, 0.d0, psi, 1)
!
        do 500 i = 1, 6
            do 501 j = 1, 6
                ksi(i,j)=-fbm(i)*fbm(j)/nofbm
                psi(i,j)=psi(i,j)-fbm(i)*alpha*mult/coupl*dcrit(j)
                do 502 k = 1, 6
                    ksi(i,j)=ksi(i,j)-alpha*mult*dfbmdf(i,k)*tdfbdb(k,&
                    j)
                    psi(i,j)=psi(i,j)+alpha*mult*dfbmdf(i,k)*tdfbde(k,&
                    j)
502              continue
501          continue
500      continue
!
        do 504 i = 1, 6
            ksi(i,i)=ksi(i,i)+1
504      continue
!
        call r8inir(36, 0.d0, iksi, 1)
        do 505 i = 1, 6
            iksi(i,i)=1.d0
505      continue
!
        call mgauss('NFVP', ksi, iksi, 6, 6,&
                    6, det, iret)
!
        call r8inir(36, 0.d0, matb, 1)
!
        do 550 i = 1, 6
            do 551 j = 1, 6
                do 552 k = 1, 6
                    matb(i,j)=matb(i,j)+iksi(i,k)*psi(k,j)
552              continue
551          continue
550      continue
!
        do 561 i = 1, 6
            do 562 j = 1, 6
                do 563 k = 1, 6
                    dsidep(i,j)=dsidep(i,j)-dsigb(k,i)*matb(k,j)
563              continue
562          continue
561      continue
!
    else if ((fd.ne.0.d0).and.(nofbm.eq.0.d0)) then
!
        do 661 i = 1, 6
            do 662 j = 1, 6
                dsidep(i,j)= -dsigd(i)*(-tdfdde(j)+coupl/(un-alpha)&
                *dcrit(j)/fd)/tdfddd
662          continue
661      continue
!
    endif
!
999  continue
!
end subroutine
