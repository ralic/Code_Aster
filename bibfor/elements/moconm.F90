subroutine moconm(dir, sigb, siga, hh, nlit,&
                  om, rr, nufsup, nufinf, nufsd1,&
                  nufid1, nufsd2, nufid2, prec)
! aslint: disable=W1306
    implicit none
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: sebastien.fayolle at edf.fr
!
#include "jeveux.h"
#include "asterfort/lsqpol.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nufsup, nufinf, nufsd1, nufid1, nufsd2, nufid2
    character :: dir
    integer :: nlit
    real(kind=8) :: sigb, siga(nlit), hh, om(nlit), rr(nlit), prec, e1, sigma
    integer :: ptmax, ordlu
    parameter (ptmax=50)
    parameter (ordlu=2)
    real(kind=8) :: nn(nlit*ptmax), mm(nlit*ptmax), eta, rhol(nlit+2)
    real(kind=8) :: xi(nlit+2), omm(nlit+2), nn0, mm0, poly(ordlu+1), xx
    integer :: i, j, k, ii, ilit, deb, npt, tri(nlit+2)
    integer :: ordok, jvale, jfon, jprol
!
! --- TRI SELON LA POSITION DANS L'EPAISSEUR
!    POUR AVOIR: RR(TRI(I))<=RR(TRI(I+1))
!    --> DU PLUS PETIT AU PLUS GRAND
!
    do 10, i = 1,nlit
    rhol(i+1) = rr(i)
    omm(i+1) = om(i)
    10 end do
    rhol(1) = -1.0d0
    rhol(nlit+2) = 1.0d0
    omm(1) = hh
    omm(nlit+2) = hh
!
    do 20, i=1,nlit+2
    tri(i)=i
    20 end do
!
    if (nlit .gt. 1) then
        do 40, j=1,nlit-1
        do 30, i=2,nlit+1-j
        if (rr(tri(i)-1) .gt. rr(tri(i+1)-1)) then
            ii=tri(i)
            tri(i)=tri(i+1)
            tri(i+1)=ii
        endif
30      continue
40      continue
    endif
!
! --- POSITIVE BENDING
!
! LE CAS DES POINTS SUPERPOSES EST TRAITE: OM=0 OU RR(I)=RR(I-1)
!    (PAR EX LINER SI RR=-1)
!
    do 45, i = 1,nlit+2
    xi(i) = -1.0d0
    45 end do
    ii=0
    do 50, ilit=0,nlit
    i = nlit-ilit+2
    xi(tri(i)) = 1.0d0
    nn0=0.d0
    mm0=0.d0
    do 60, j=1,nlit
    nn0=nn0+xi(j+1)*om(j)*siga(j)
    mm0=mm0+xi(j+1)*om(j)*siga(j)*rhol(j+1)*hh/2.0d0
60  continue
    if (omm(tri(i)) .lt. 1.d-8*omm(1)) then
        deb=1
    else
        deb=0
    endif
    npt = int(abs(rhol(tri(i-1))-rhol(tri(i)))/2.d0*ptmax)-1
    npt = max(npt,0)
    do 70, k=deb,npt
    if (npt .eq. 0) then
        eta=rhol(tri(i))
    else
        eta=rhol(tri(i))+k*(rhol(tri(i-1))-rhol(tri(i)))/npt
    endif
    ii=ii+1
    nn(ii)=nn0+sigb*hh*(1+eta)/2.0d0-prec
    mm(ii)=mm0-sigb*hh*hh*(1-eta*eta)/8.0d0
70  continue
    50 end do
!
! --- AJOUT DE LA FONCTION
    e1=0.d0
    npt = ii
    call lsqpol(ordlu, e1, npt, nn, mm,&
                ordok, poly, sigma)
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NUFSUP.VALE ---
    call wkvect(nufsup//'           .VALE', 'G V R', 2*npt, jvale)
    jfon = jvale + npt
    do 72 i = 0, npt-1
        xx = nn(1) + (nn(npt)-nn(1))*i/(npt-1)
        zr(jvale+i) = xx
        zr(jfon +i) = 0.0d0
        do 73, j = 0,ordok
        zr(jfon +i) = zr(jfon +i) + poly(j+1)*(xx**j)
73      continue
72  end do
!
    call wkvect(nufsd1//'           .VALE', 'G V R', 2*npt, jvale)
    jfon = jvale + npt
    do 74 i = 0, npt-1
        xx = nn(1) + (nn(npt)-nn(1))*i/(npt-1)
        zr(jvale+i) = xx
        zr(jfon +i) = 0.0d0
        do 75, j = 1,ordok
        zr(jfon +i) = zr(jfon +i) + j*poly(j+1)*(xx**(j-1))
75      continue
74  end do
!
    call wkvect(nufsd2//'           .VALE', 'G V R', 2*npt, jvale)
    jfon = jvale + npt
    do 76 i = 0, npt-1
        xx = nn(1) + (nn(npt)-nn(1))*i/(npt-1)
        zr(jvale+i) = xx
        zr(jfon +i) = 0.0d0
        do 77, j = 2,ordok-1
        zr(jfon +i) = zr(jfon +i)+ j*(j-1)*poly(j+1)*(xx**(j-2))
77      continue
76  end do
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NUFSUP.PROL ---
    call wkvect(nufsup//'           .PROL', 'G V K24', 6, jprol)
    zk24(jprol) = 'FONCTION                '
    zk24(jprol+1) = 'LIN LIN                 '
    zk24(jprol+2) = 'X                       '
    zk24(jprol+3) = 'FME'//dir//'1                   '
    zk24(jprol+4) = 'LL                      '
!
    call wkvect(nufsd1//'           .PROL', 'G V K24', 6, jprol)
    zk24(jprol) = 'FONCTION                '
    zk24(jprol+1) = 'LIN LIN                 '
    zk24(jprol+2) = 'X                       '
    zk24(jprol+3) = 'DFME'//dir//'1                  '
    zk24(jprol+4) = 'CC                      '
!
    call wkvect(nufsd2//'           .PROL', 'G V K24', 6, jprol)
    zk24(jprol) = 'FONCTION                '
    zk24(jprol+1) = 'LIN LIN                 '
    zk24(jprol+2) = 'X                       '
    zk24(jprol+3) = 'DDFME'//dir//'1                 '
    zk24(jprol+4) = 'CC                      '
!
!--- NEGATIVE BENDING
!
    ii=0
    do 80, i=1,nlit+1
    nn0=0.d0
    mm0=0.d0
    do 90, j=1,nlit
    nn0=nn0-xi(j+1)*om(j)*siga(j)
    mm0=mm0-xi(j+1)*om(j)*siga(j)*rhol(j+1)*hh/2.0d0
90  continue
    if (omm(tri(i)) .lt. 1.d-8*omm(1)) then
        deb=1
    else
        deb=0
    endif
    npt = int(abs(rhol(tri(i+1))-rhol(tri(i)))/2.d0*ptmax)-1
    npt = max(npt,0)
    do 100, k=deb,npt
    if (npt .eq. 0) then
        eta=rhol(tri(i))
    else
        eta=rhol(tri(i))+k*(rhol(tri(i+1))-rhol(tri(i)))/npt
    endif
    ii=ii+1
    nn(ii)=nn0+sigb*hh*(1-eta)/2.0d0-prec
    mm(ii)=mm0+sigb*hh*hh*(1-eta*eta)/8.0d0
100  continue
    xi(tri(i+1))=-1
    80 end do
!
!--- AJOUT DE LA FONCTION
    e1=0.d0
    npt = ii
    call lsqpol(ordlu, e1, npt, nn, mm,&
                ordok, poly, sigma)
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NUFINF.VALE ---
    call wkvect(nufinf//'           .VALE', 'G V R', 2*npt, jvale)
    jfon = jvale + npt
    do 102 i = 0, npt-1
        xx = nn(1) + (nn(npt)-nn(1))*i/(npt-1)
        zr(jvale+i) = xx
        zr(jfon +i) = 0.0d0
        do 103, j = 0,ordok
        zr(jfon +i) = zr(jfon +i) + poly(j+1)*(xx**j)
103      continue
102  end do
!
    call wkvect(nufid1//'           .VALE', 'G V R', 2*npt, jvale)
    jfon = jvale + npt
    do 104 i = 0, npt-1
        xx = nn(1) + (nn(npt)-nn(1))*i/(npt-1)
        zr(jvale+i) = xx
        zr(jfon +i) = 0.0d0
        do 105, j = 1,ordok
        zr(jfon +i) = zr(jfon +i) + j*poly(j+1)*(xx**(j-1))
105      continue
104  end do
!
    call wkvect(nufid2//'           .VALE', 'G V R', 2*npt, jvale)
    jfon = jvale + npt
    do 106 i = 0, npt-1
        xx = nn(1) + (nn(npt)-nn(1))*i/(npt-1)
        zr(jvale+i) = xx
        zr(jfon +i) = 0.0d0
        do 107, j = 2,ordok-1
        zr(jfon +i) = zr(jfon +i)+ j*(j-1)*poly(j+1)*(xx**(j-2))
107      continue
106  end do
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NUFSUP.PROL ---
    call wkvect(nufinf//'           .PROL', 'G V K24', 6, jprol)
    zk24(jprol) = 'FONCTION                '
    zk24(jprol+1) = 'LIN LIN                 '
    zk24(jprol+2) = 'X                       '
    zk24(jprol+3) = 'FME'//dir//'2                   '
    zk24(jprol+4) = 'LL                      '
!
    call wkvect(nufid1//'           .PROL', 'G V K24', 6, jprol)
    zk24(jprol) = 'FONCTION                '
    zk24(jprol+1) = 'LIN LIN                 '
    zk24(jprol+2) = 'X                       '
    zk24(jprol+3) = 'DFME'//dir//'2                  '
    zk24(jprol+4) = 'CC                      '
!
    call wkvect(nufid2//'           .PROL', 'G V K24', 6, jprol)
    zk24(jprol) = 'FONCTION                '
    zk24(jprol+1) = 'LIN LIN                 '
    zk24(jprol+2) = 'X                       '
    zk24(jprol+3) = 'DDFME'//dir//'2                 '
    zk24(jprol+4) = 'CC                      '
!
end subroutine
