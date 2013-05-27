subroutine meobg2(eps, epsg, b, d, deltab,&
                  deltad, mult, lambda, mu, ecrob,&
                  ecrod, alpha, k1, k2, bdim,&
                  dsidep)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
    real(kind=8) :: fb(6), trepsg, fd
    real(kind=8) :: dfmf(3, 3), tdfbdb(6, 6), tdfbde(6, 6)
    real(kind=8) :: tdfdde(6), tdfddd
    real(kind=8) :: interd(3), intert(6), interg(3)
    real(kind=8) :: psi(3, 6), ksi(3, 3), iksi(3, 3)
    real(kind=8) :: matb(3, 6), matd(6)
    real(kind=8) :: fbs(3), deltas(3)
    real(kind=8) :: fbsm(6), sdfbdb(3, 3), sdfbde(3, 6)
    real(kind=8) :: dsigb(6, 6), dsigd(6), dib(3, 6)
    real(kind=8) :: coupl, dcrit(6), det
!
    deux=2.d0
    rac2=sqrt(deux)
    un=1.d0
!
!-------------------------------------------------------
!-------------------------------------------------------
!----CALCUL DE FB: FORCE THERMO ASSOCIEE A
!-------------------ENDOMMAGEMENT ANISOTROPE DE TRACTION
!
    call ceobfb(b, epsg, lambda, mu, ecrob,&
                bdim, fb, nofbm, fbsm)
!
    fbs(1)=fb(1)
    fbs(2)=fb(2)
    fbs(3)=fb(4)
!
    deltas(1)=deltab(1)
    deltas(2)=deltab(2)
    deltas(3)=deltab(4)
!
!----CALCUL DE FD: PARTIE POSITIVE DE LA FORCE THERMO ASSOCIEE A
!-------------------ENDOMMAGEMENT ISOTROPE DE COMPRESSION
!
    call ceobfd(d, epsg, lambda, mu, ecrod,&
                fd)
!
!---CALCUL DE DERIVEES UTILES----------------------------------
!
    call dfmdf(3, fbs, dfmf)
!
!----CALCUL DE LA DERIVEE DU SEUIL---------------------
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
    call dfbdb(3, b, epsg, deux*mu, lambda,&
               ecrob, tdfbdb)
    call dfbde(3, b, epsg, deux*mu, lambda,&
               tdfbde)
    call dfbde(3, b, eps, deux*mu, lambda,&
               dsigb)
!
    sdfbdb(1,1)=tdfbdb(1,1)
    sdfbdb(1,2)=tdfbdb(1,2)
    sdfbdb(1,3)=tdfbdb(1,4)
    sdfbdb(2,1)=tdfbdb(2,1)
    sdfbdb(2,2)=tdfbdb(2,2)
    sdfbdb(2,3)=tdfbdb(2,4)
    sdfbdb(3,1)=tdfbdb(4,1)
    sdfbdb(3,2)=tdfbdb(4,2)
    sdfbdb(3,3)=tdfbdb(4,4)
!
    do 381 i = 1, 6
        sdfbde(1,i)=tdfbde(1,i)
        sdfbde(2,i)=tdfbde(2,i)
        sdfbde(3,i)=tdfbde(4,i)
        dib(1,i)=dsigb(1,i)
        dib(2,i)=dsigb(2,i)
        dib(3,i)=dsigb(4,i)
381  end do
!
    fbsm(3)=rac2*fbsm(3)
    deltas(3)=deltas(3)*rac2
!
    call dfdde(epsg, d, 3, lambda, mu,&
               tdfdde)
    call dfdde(eps, d, 3, lambda, mu,&
               dsigd)
    call dfddd(epsg, d, 3, lambda, mu,&
               ecrod, tdfddd)
!
    nofbm=fbsm(1)**2+fbsm(2)**2+fbsm(3)**2
!
    coupl=sqrt(alpha*nofbm+(un-alpha)*fd**deux)
!
    call r8inir(36, 0.d0, dsidep, 1)
!
    if ((fd.ne.0.d0) .and. (nofbm.ne.0.d0)) then
!
!---CALCUL DE DBDE ET DDDE-------------------------------------
!
!---CALCUL DE KSI ET PSI
!
        call r8inir(3, 0.d0, interd, 1)
        call r8inir(3, 0.d0, interg, 1)
        call r8inir(6, 0.d0, intert, 1)
        call r8inir(18, 0.d0, psi, 1)
        call r8inir(9, 0.d0, ksi, 1)
!
        do 110 i = 1, 6
            intert(i)=(un-alpha)*fd*tdfdde(i)-coupl*dcrit(i)
            do 111 j = 1, 3
                do 112 k = 1, 3
                    intert(i)=intert(i)+alpha*fbsm(k)*dfmf(k,j)*&
                    sdfbde(j,i)
112              continue
111          continue
110      end do
!
        do 310 i = 1, 3
            interg(i)=deltas(i)/fd-alpha*fbsm(i)/(un-alpha)/fd/tdfddd
            do 311 j = 1, 3
                do 312 k = 1, 3
                    ksi(i,j)=ksi(i,j)+alpha*deltad*dfmf(i,k)*sdfbdb(k,&
                    j)
                    interd(i)=interd(i)+alpha*fbsm(k)*dfmf(k,j)*&
                    sdfbdb(j,i)
312              continue
311          continue
            do 313 j = 1, 6
                do 314 k = 1, 3
                    psi(i,j)=psi(i,j)-alpha*deltad*dfmf(i,k)*sdfbde(k,&
                    j)
314              continue
313          continue
310      end do
!
        do 120 i = 1, 3
            ksi(i,i)=ksi(i,i)-(un-alpha)*fd
120      end do
!
        do 130 i = 1, 3
            do 131 j = 1, 3
                ksi(i,j)=ksi(i,j)+interg(i)*interd(j)
131          continue
            do 331 j = 1, 6
                psi(i,j)=psi(i,j)-interg(i)*intert(j) +(un-alpha)*&
                deltas(i)*tdfdde(j)
331          continue
130      end do
!
        call r8inir(9, 0.d0, iksi, 1)
        do 140 i = 1, 3
            iksi(i,i)=1.d0
140      end do
!
        call mgauss('NFVP', ksi, iksi, 3, 3,&
                    3, det, iret)
!
!-- ! ksi n est plus disponible
!
        call r8inir(18, 0.d0, matb, 1)
        call r8inir(6, 0.d0, matd, 1)
!
        do 150 i = 1, 6
            matd(i)=-intert(i)/(un-alpha)/fd/tdfddd
            do 151 j = 1, 3
                do 152 k = 1, 3
                    matb(j,i)=matb(j,i)+iksi(j,k)*psi(k,i)
                    matd(i)=matd(i)-interd(j)*iksi(j,k)*psi(k,i)&
                    /(un-alpha)/fd/tdfddd
152              continue
!            WRITE(6,*) 'MB(',J,',',I,')=',MATB(J,I),';'
151          continue
150      continue
!
        do 201 i = 1, 6
            do 202 j = 1, 6
                dsidep(i,j)=-dsigd(i)*matd(j)
!         WRITE(6,*) 'DID(',I,',',J,')=', DSIDEP(I,J),';'
                do 203 k = 1, 3
                    dsidep(i,j)=dsidep(i,j)-dib(k,i)*matb(k,j)
203              continue
202          continue
201      continue
!
    else if ((fd.eq.0.d0).and.(nofbm.ne.0.d0)) then
!
        call r8inir(9, 0.d0, ksi, 1)
        call r8inir(18, 0.d0, psi, 1)
!
        do 500 i = 1, 3
            do 501 j = 1, 3
                ksi(i,j)=-fbsm(i)*fbsm(j)/nofbm
                do 502 k = 1, 3
                    ksi(i,j)=ksi(i,j)-alpha*mult*dfmf(i,k)*sdfbdb(k,j)
502              continue
501          continue
            do 581 j = 1, 6
                psi(i,j)=psi(i,j)-fbsm(i)*alpha*mult/coupl*dcrit(j)
                do 582 k = 1, 3
                    psi(i,j)=psi(i,j)+alpha*mult*dfmf(i,k)*sdfbde(k,j)
582              continue
581          continue
500      continue
!
        do 504 i = 1, 3
            ksi(i,i)=ksi(i,i)+1
504      continue
!
        call r8inir(9, 0.d0, iksi, 1)
        do 505 i = 1, 3
            iksi(i,i)=1.d0
505      continue
!
        call mgauss('NFVP', ksi, iksi, 3, 3,&
                    3, det, iret)
!
        call r8inir(18, 0.d0, matb, 1)
!
        do 550 i = 1, 3
            do 551 j = 1, 6
                do 552 k = 1, 3
                    matb(i,j)=matb(i,j)+iksi(i,k)*psi(k,j)
552              continue
551          continue
550      continue
!
        do 561 i = 1, 6
            do 562 j = 1, 6
                do 563 k = 1, 3
                    dsidep(i,j)=dsidep(i,j)-dib(k,i)*matb(k,j)
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
end subroutine
