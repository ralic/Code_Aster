subroutine mocon2(dir, sigb, siga, hh, nlit,&
                  om, rr, nufsup, nufinf, nufsd1,&
                  nufid1, nufsd2, nufid2, prec)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: sebastien.fayolle at edf.fr
!
#include "jeveux.h"
!
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lsqpol.h"
#include "asterfort/wkvect.h"
    character(len=8) :: nufsup, nufinf, nufsd1, nufid1, nufsd2, nufid2
    character(len=1) :: dir
    integer :: nlit
    real(kind=8) :: sigb, siga(nlit), hh, om(nlit), rr(nlit), prec, e1, sigma
    integer :: ordlu
    parameter (ordlu=2)
    real(kind=8) :: poly(ordlu+1), xx
    integer :: i, j, npt
    integer :: ordok, jvale, jfon, jprol, jtab, lmax
!
! --- POSITIVE BENDING
    call jeveuo(nufsup//'           .VALE', 'L', jtab)
    call jelira(nufsup//'           .VALE', 'LONMAX', lmax)
!
!--- INTERPOLATION DE LA FONCTION ET CALCUL DES DERIVEES
    e1=0.d0
    npt = lmax/2
    call lsqpol(ordlu, e1, npt, zr(jtab), zr(jtab+npt),&
                ordok, poly, sigma)
!
    call wkvect(nufsd1//'           .VALE', 'G V R', 2*npt, jvale)
    jfon = jvale + npt
    do 74 i = 0, npt-1
        xx = zr(jtab) + (zr(jtab-1+npt)-zr(jtab))*i/(npt-1)
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
        xx = zr(jtab) + (zr(jtab-1+npt)-zr(jtab))*i/(npt-1)
        zr(jvale+i) = xx
        zr(jfon +i) = 0.0d0
        do 77, j = 2,ordok-1
        zr(jfon +i) = zr(jfon +i)+ j*(j-1)*poly(j+1)*(xx**(j-2))
77      continue
76  end do
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NUFSUP.PROL ---
    call jeveuo(nufsup//'           .PROL', 'L', jprol)
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
    call jeveuo(nufinf//'           .VALE', 'L', jtab)
    call jelira(nufinf//'           .VALE', 'LONMAX', lmax)
!
!--- INTERPOLATION DE LA FONCTION ET CALCUL DES DERIVEES
    e1=0.d0
    npt = lmax/2
    call lsqpol(ordlu, e1, npt, zr(jtab), zr(jtab+npt),&
                ordok, poly, sigma)
!
    call wkvect(nufid1//'           .VALE', 'G V R', 2*npt, jvale)
    jfon = jvale + npt
    do 104 i = 0, npt-1
        xx = zr(jtab) + (zr(jtab-1+npt)-zr(jtab))*i/(npt-1)
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
        xx = zr(jtab) + (zr(jtab-1+npt)-zr(jtab))*i/(npt-1)
        zr(jvale+i) = xx
        zr(jfon +i) = 0.0d0
        do 107, j = 2,ordok-1
        zr(jfon +i) = zr(jfon +i)+ j*(j-1)*poly(j+1)*(xx**(j-2))
107      continue
106  end do
!
!     --- CREATION ET REMPLISSAGE DE L'OBJET NUFSUP.PROL ---
    call jeveuo(nufinf//'           .PROL', 'L', jprol)
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
