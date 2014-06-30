subroutine fsurf(option, nomte, xi, nb1, vecl,&
                 vectpt)
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
#include "jeveux.h"
#include "asterfort/fointe.h"
#include "asterfort/forsrg.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/r8inir.h"
#include "asterfort/utpvlg.h"
#include "asterfort/vectci.h"
#include "asterfort/vexpan.h"
    character(len=16) :: option, nomte
!
!
    integer :: nb1
    real(kind=8) :: rnormc, f1, pr
    real(kind=8) :: xi(3, *), vectpt(9, 3, 3)
!     REAL*8       VECTC(3),VECPTX(3,3)
    real(kind=8) :: vecl(51), vecl1(42)
    real(kind=8) :: chgsrg(6, 8), chgsrl(6), chg(6)
    real(kind=8) :: kijkm1(40, 2), pgl(3, 3)
    logical(kind=1) :: global, locapr
    real(kind=8) :: valpar(4)
    character(len=8) :: nompar(4)
!
!
!-----------------------------------------------------------------------
    integer :: i, i1, ier, intsn, ip, itemps, j
    integer :: jp, jpres, k, lzi, lzr, nb2, npgsn
!
!-----------------------------------------------------------------------
    call jevete('&INEL.'//nomte(1:8)//'.DESI', ' ', lzi)
    nb1  =zi(lzi-1+1)
    nb2  =zi(lzi-1+2)
    npgsn=zi(lzi-1+4)
!
    call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
    call r8inir(42, 0.d0, vecl1, 1)
!
!
! --- CAS DES CHARGEMENTS DE FORME REEL
    if (option .eq. 'CHAR_MECA_FRCO3D') then
        call jevech('PFRCO3D', 'L', jpres)
        global=abs(zr(jpres-1+7)).lt.1.d-3
        if (global) then
            do 20 j = 1, nb1
                do 10 i = 1, 6
                    chgsrg(i,j)=zr(jpres-1+7*(j-1)+i)
10              continue
20          continue
        else
            do 70 j = 1, nb1
                do 30 i = 1, 5
                    chgsrl(i)=zr(jpres-1+7*(j-1)+i)
30              continue
                chgsrl(i)=0.d0
                do 50 jp = 1, 3
                    do 40 ip = 1, 3
                        pgl(jp,ip)=vectpt(j,jp,ip)
40                  continue
50              continue
                call utpvlg(1, 6, pgl, chgsrl, chg)
                do 60 i = 1, 6
                    chgsrg(i,j)=chg(i)
60              continue
70          continue
        endif
!
!
! --- CAS DES CHARGEMENTS DE FORME FONCTION
    else if (option .eq. 'CHAR_MECA_FFCO3D') then
        call jevech('PFFCO3D', 'L', jpres)
        call jevech('PTEMPSR', 'L', itemps)
        valpar(4) = zr(itemps)
        nompar(4) = 'INST'
        nompar(1) = 'X'
        nompar(2) = 'Y'
        nompar(3) = 'Z'
        global = zk8(jpres+6) .eq. 'GLOBAL'
        locapr = zk8(jpres+6) .eq. 'LOCAL_PR'
!
        if (global) then
! --        LECTURE DES INTERPOLATIONS DE FX, FY, FZ, MX, MY, MZ
            do 100 j = 1, nb1
                valpar(1) = xi(1,j)
                valpar(2) = xi(2,j)
                valpar(3) = xi(3,j)
                call fointe('FM', zk8(jpres ), 4, nompar, valpar,&
                            chgsrg(1, j), ier)
                call fointe('FM', zk8(jpres+1), 4, nompar, valpar,&
                            chgsrg( 2, j), ier)
                call fointe('FM', zk8(jpres+2), 4, nompar, valpar,&
                            chgsrg( 3, j), ier)
                call fointe('FM', zk8(jpres+3), 4, nompar, valpar,&
                            chgsrg( 4, j), ier)
                call fointe('FM', zk8(jpres+4), 4, nompar, valpar,&
                            chgsrg( 5, j), ier)
                call fointe('FM', zk8(jpres+5), 4, nompar, valpar,&
                            chgsrg( 6, j), ier)
100          continue
!
        else if (locapr) then
! --        BASE LOCALE - CAS D UNE PRESSION
! --        LECTURE DES INTERPOLATIONS DE LA PRESSION PRES
            do 140 j = 1, nb1
                valpar(1) = xi(1,j)
                valpar(2) = xi(2,j)
                valpar(3) = xi(3,j)
                call fointe('FM', zk8(jpres+2), 4, nompar, valpar,&
                            pr, ier)
                chgsrl(3) = -1 * pr
                chgsrl(1) = 0.d0
                chgsrl(2) = 0.d0
                chgsrl(4) = 0.d0
                chgsrl(5) = 0.d0
                chgsrl(6) = 0.d0
! --           CHANGEMENT DE BASE LOCAL --> GLOBAL
                do 120 jp = 1, 3
                    do 110 ip = 1, 3
                        pgl(jp,ip)=vectpt(j,jp,ip)
110                  continue
120              continue
                call utpvlg(1, 6, pgl, chgsrl, chg)
                do 130 i = 1, 6
                    chgsrg(i,j)=chg(i)
130              continue
140          continue
        else
!
! --        BASE LOCALE - CAS DE F1, F2, F3, MF1, MF2
! --        LECTURE DES INTERPOLATIONS DE F1, F2, F3, MF1, MF2
            do 180 j = 1, nb1
                valpar(1) = xi(1,j)
                valpar(2) = xi(2,j)
                valpar(3) = xi(3,j)
                call fointe('FM', zk8(jpres ), 4, nompar, valpar,&
                            chgsrl(1), ier)
                call fointe('FM', zk8(jpres+1), 4, nompar, valpar,&
                            chgsrl( 2), ier)
                call fointe('FM', zk8(jpres+2), 4, nompar, valpar,&
                            chgsrl( 3), ier)
                call fointe('FM', zk8(jpres+3), 4, nompar, valpar,&
                            chgsrl( 4), ier)
                call fointe('FM', zk8(jpres+4), 4, nompar, valpar,&
                            chgsrl( 5), ier)
                chgsrl(6) = 0.d0
! --           CHANGEMENT DE BASE LOCAL --> GLOBAL
                do 160 jp = 1, 3
                    do 150 ip = 1, 3
                        pgl(jp,ip)=vectpt(j,jp,ip)
150                  continue
160              continue
                call utpvlg(1, 6, pgl, chgsrl, chg)
                do 170 i = 1, 6
                    chgsrg(i,j)=chg(i)
170              continue
180          continue
        endif
!
    endif
!
!
    do 200 intsn = 1, npgsn
        call vectci(intsn, nb1, xi, zr(lzr), rnormc)
!
        call forsrg(intsn, nb1, nb2, zr(lzr), chgsrg,&
                    rnormc, vectpt, vecl1)
200  end do
!
!     RESTITUTION DE KIJKM1 POUR CONDENSER LES FORCES
!     ATTENTION LA ROUTINE N'EST PAS UTILISEE DANS LE CAS DES
!     EFFORTS SUIVANTS (MOMENTS SURFACIQUES)
    i1=5*nb1
    do 220 j = 1, 2
        do 210 i = 1, i1
            k=(j-1)*i1+i
            kijkm1(i,j)=zr(lzr-1+1000+k)
210      end do
220  end do
!
    do 240 i = 1, i1
        f1=0.d0
        do 230 k = 1, 2
            f1=f1+kijkm1(i,k)*vecl1(i1+k)
230      continue
        vecl1(i)=vecl1(i)-f1
240  end do
!
!     EXPANSION DU VECTEUR VECL1 : DUE A L'AJOUT DE LA ROTATION FICTIVE
!
    call vexpan(nb1, vecl1, vecl)
    do 90 i = 1, 3
        vecl(6*nb1+i)=0.d0
90  end do
!
end subroutine
