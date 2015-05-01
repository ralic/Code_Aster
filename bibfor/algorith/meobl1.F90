subroutine meobl1(eps, b, d, deltab, deltad,&
                  mult, lambda, mu, ecrob, ecrod,&
                  alpha, k1, k2, bdim, dsidep)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
!
#include "asterfort/ceobfb.h"
#include "asterfort/ceobfd.h"
#include "asterfort/dfbdb.h"
#include "asterfort/dfbde.h"
#include "asterfort/dfddd.h"
#include "asterfort/dfdde.h"
#include "asterfort/r8inir.h"
#include "asterfort/utmess.h"
    real(kind=8) :: eps(6), b(6), d, dsidep(6, 6)
    real(kind=8) :: deltab(6), deltad, mult
    real(kind=8) :: lambda, mu, alpha, k1, k2, ecrob, ecrod
    integer :: bdim
!
!--CALCUL DE LA MATRICE TANGENTE POUR LA LOI ENDO_ORTHO_BETON
!-------------------------------------------------------------
!
    integer :: i, j
    real(kind=8) :: un, deux
    real(kind=8) :: fb(6), treps, fd, fbm(6)
    real(kind=8) :: dfmf, tdfbdb(6, 6), tdfbde(6, 6)
    real(kind=8) :: tdfdde(6), tdfddd
    real(kind=8) :: interd, intert(6), interg
    real(kind=8) :: psi(6), ksi, iksi
    real(kind=8) :: matb(6), matd(6)
    real(kind=8) :: fbs, deltas
    real(kind=8) :: fbsm, sdfbdb, sdfbde(6)
    real(kind=8) :: coupl, dcrit(6), nofbm
!
    un=1.d0
    deux=2.d0
    call r8inir(36, 0.d0, dsidep, 1)
!-------------------------------------------------------
!-------------------------------------------------------
!----CALCUL DE FB: FORCE THERMO ASSOCIEE A
!-------------------ENDOMMAGEMENT ANISOTROPE DE TRACTION
!
    call ceobfb(b, eps, lambda, mu, ecrob,&
                bdim, fb, nofbm, fbm)
!
    fbs=fb(1)
    fbsm=fbm(1)
    deltas=deltab(1)
!
!----CALCUL DE FD: PARTIE POSITIVE DE LA FORCE THERMO ASSOCIEE A
!-------------------ENDOMMAGEMENT ISOTROPE DE COMPRESSION
!
    call ceobfd(d, eps, lambda, mu, ecrod,&
                fd)
!
!---CALCUL DE DERIVEES UTILES----------------------------------
!
    if (fbs .le. 0.d0) then
        dfmf=1.d0
    else
        dfmf=0.d0
    endif
!
    treps=eps(1)+eps(2)+eps(3)
    if (treps .gt. 0.d0) then
        treps=0.d0
    endif
    dcrit(1)=-k1*(-treps/k2/(un+(-treps/k2)**deux)&
     &           +atan2(-treps/k2,un))
    dcrit(2)=-k1*(-treps/k2/(un+(-treps/k2)**deux)&
     &           +atan2(-treps/k2,un))
    dcrit(3)=-k1*(-treps/k2/(un+(-treps/k2)**deux)&
     &           +atan2(-treps/k2,un))
    dcrit(4)=0.d0
    dcrit(5)=0.d0
    dcrit(6)=0.d0
!
    call dfbdb(3, b, eps, deux*mu, lambda,&
               ecrob, tdfbdb)
    call dfbde(3, b, eps, deux*mu, lambda,&
               tdfbde)
!
    sdfbdb=tdfbdb(1,1)
!
    call dfdde(eps, d, 3, lambda, mu,&
               tdfdde)
    call dfddd(eps, d, 3, lambda, mu,&
               ecrod, tdfddd)
!
    do 381 i = 1, 6
        sdfbde(i)=tdfbde(1,i)
381  end do
!
    coupl=sqrt(alpha*fbsm**deux+(un-alpha)*fd**deux)
    if ((fd.ne.0.d0) .and. (fbsm.ne.0.d0)) then
!
!---CALCUL DE DBDE ET DDDE-------------------------------------
!
!---CALCUL DE KSI ET PSI
!
        interd=0.d0
        interg=0.d0
        call r8inir(6, 0.d0, intert, 1)
        call r8inir(6, 0.d0, psi, 1)
        ksi=0.d0
!
        do 110 i = 1, 6
            intert(i)=(un-alpha)*fd*tdfdde(i)+alpha*fbsm*dfmf*sdfbde(&
            i) -coupl*dcrit(i)
110      end do
!
        interg=deltas/fd-alpha*fbsm/(un-alpha)/fd/tdfddd
        interd=alpha*fbsm*dfmf*sdfbdb
        do 313 j = 1, 6
            psi(j)=-alpha*deltad*dfmf*sdfbde(j)-interg*intert(j)&
            +(un-alpha)*deltas*tdfdde(j)
313      end do
!
        ksi=alpha*deltad*dfmf*sdfbdb-(un-alpha)*fd +interg*interd
!
        if (ksi .ne. 0.d0) then
            iksi=un/ksi
        else
            call utmess('F', 'ALGORITH4_54')
        endif
!
!-- ! ksi n est plus disponible
!
        call r8inir(6, 0.d0, matb, 1)
        call r8inir(6, 0.d0, matd, 1)
!
        do 150 i = 1, 6
            matd(i)=-intert(i)/(un-alpha)/fd/tdfddd -interd*iksi*psi(&
            i)/(un-alpha)/fd/tdfddd
            matb(i)=matb(i)+iksi*psi(i)
150      continue
!
        do 201 i = 1, 6
            do 202 j = 1, 6
                dsidep(i,j)=-tdfdde(i)*matd(j)-sdfbde(i)*matb(j)
202          continue
201      continue
!
    else if ((fd.eq.0.d0).and.(fbsm.ne.0.d0)) then
!
        call r8inir(6, 0.d0, psi, 1)
        ksi=-alpha*mult*dfmf*sdfbdb
        do 581 j = 1, 6
            psi(j)=alpha*mult*dfmf*sdfbde(j) -fbsm*alpha*mult/coupl*&
            dcrit(j)
581      continue
!
        if (ksi .ne. 0.d0) then
            iksi=un/ksi
        else
            call utmess('F', 'ALGORITH5_79')
        endif
!
        call r8inir(6, 0.d0, matb, 1)
!
        do 551 j = 1, 6
            matb(j)=iksi*psi(j)
551      continue
!
        do 561 i = 1, 6
            do 562 j = 1, 6
                dsidep(i,j)=dsidep(i,j)-sdfbde(i)*matb(j)
562          continue
561      continue
!
    else if ((fd.ne.0.d0).and.(fbsm.eq.0.d0)) then
!
        do 661 i = 1, 6
            do 662 j = 1, 6
                dsidep(i,j)= -tdfdde(i)*(-tdfdde(j)+coupl/(un-alpha)&
                *dcrit(j)/fd)/tdfddd
662          continue
661      continue
!
    endif
!
end subroutine
