subroutine jacopo(long, tpscaz, iad1, iad2)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
    include 'jeveux.h'
    include 'asterfort/assert.h'
    integer :: long, iad1, iad2
    character(len=*) :: tpscaz
! ----------------------------------------------------------------------
!     IN:
!      LONG: LONGUEUR A RECOPIER
!     TPSCAZ : 'R', 'I', 'K8/16/24/32/80','C'
!      LONG: LONGUEUR A RECOPIER
!      IAD1: ADRESSE DANS LE COMMON ZI (OU ZR...) DE L'OBJET A COPIER.
!      IAD2: ADRESSE DANS LE COMMON ZI (OU ZR...) DE L'OBJET RECOPIE.
!
    character(len=3) :: t
    character(len=8) :: typsca
! DEB-------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    typsca = tpscaz
!
    t = typsca(1:3)
!
!     -- QUELQUES VERIFICATIONS:
!
    call assert(long.ge.0)
!
!     -- RECOPIE SELON LE TYPE DE SCALAIRE:
!
    if (t .eq. 'I  ') then
!CDIR$ IVDEP
        do 10,i = 1,long
        zi(iad2-1+i) = zi(iad1-1+i)
10      continue
    else if (t.eq.'R  ') then
!CDIR$ IVDEP
        do 20,i = 1,long
        zr(iad2-1+i) = zr(iad1-1+i)
20      continue
    else if (t.eq.'C  ') then
!CDIR$ IVDEP
        do 30,i = 1,long
        zc(iad2-1+i) = zc(iad1-1+i)
30      continue
    else if (t.eq.'L  ') then
!CDIR$ IVDEP
        do 40,i = 1,long
        zl(iad2-1+i) = zl(iad1-1+i)
40      continue
    else if (t.eq.'K8 ') then
!CDIR$ IVDEP
        do 50,i = 1,long
        zk8(iad2-1+i) = zk8(iad1-1+i)
50      continue
    else if (t.eq.'K16') then
!CDIR$ IVDEP
        do 60,i = 1,long
        zk16(iad2-1+i) = zk16(iad1-1+i)
60      continue
    else if (t.eq.'K24') then
!CDIR$ IVDEP
        do 70,i = 1,long
        zk24(iad2-1+i) = zk24(iad1-1+i)
70      continue
    else if (t.eq.'K32') then
!CDIR$ IVDEP
        do 80,i = 1,long
        zk32(iad2-1+i) = zk32(iad1-1+i)
80      continue
    else if (t.eq.'K80') then
!CDIR$ IVDEP
        do 90,i = 1,long
        zk80(iad2-1+i) = zk80(iad1-1+i)
90      continue
    else
        call assert(.false.)
    endif
!
!
end subroutine
