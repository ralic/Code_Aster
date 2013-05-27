subroutine mecumu(scal, ncmp, iad1, iad2, nec,&
                  dg1, dg2)
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
    implicit none
!
! ----------------------------------------------------------------------
!     "CUMULE" LES CMPS D'1 GRANDEUR AU SENS DE:
!     '   ' + ' 2' = ' 2 '
!     ' 3 ' + ' 2' = ' 3 '
!
! ----------------------------------------------------------------------
!
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/exisdg.h'
    include 'asterfort/u2mesk.h'
    character(len=8) :: scal
    integer :: ncmp, iad1, iad2, nec, dg1(nec), dg2(nec)
! ----------------------------------------------------------------------
!     ENTREES:
!       SCAL : TYPE SCALAIRE : 'R  ', 'C  ', 'I  ', 'K8 ' ,'K16 '
!       NCMP : NOMBRE DE COMPOSANTES A ACCUMULER.
!       IAD1 : ADRESSE DANS ZI,ZR,... DU SEGMENT A CUMULER
!       IAD2 : ADRESSE DANS ZI,ZR,... DU SEGMENT OU ON CUMULE
!
!       NEC  : NOMBRE D'ENTIERS CODES
!       DG1  : DESCRIPTEUR DE LA GRANDEUR A CUMULER.
!       DG2  : DESCRIPTEUR  DE LA GRANDEUR OU ON CUMULE.
!
!     SORTIES:
!       LES OBJETS SONT MODIFIES.
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
!
! DEB-------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: i, ico
!-----------------------------------------------------------------------
    ico = 0
    if (scal(1:1) .eq. 'I') then
!
!        -- CAS D'1 SEGMENT ENTIER:
!        --------------------------
        do 1,i = 1,ncmp
        if (exisdg(dg1,i)) then
            ico = ico + 1
            zi(iad2-1+i) = zi(iad1-1+ico)
        endif
 1      continue
    else if (scal(1:1).eq.'R') then
!
!        -- CAS D'1 SEGMENT REEL  :
!        --------------------------
        do 2,i = 1,ncmp
        if (exisdg(dg1,i)) then
            ico = ico + 1
            zr(iad2-1+i) = zr(iad1-1+ico)
        endif
 2      continue
!
!        -- CAS D'1 SEGMENT COMPLEX:
!        --------------------------
    else if (scal(1:1).eq.'C') then
        do 3,i = 1,ncmp
        if (exisdg(dg1,i)) then
            ico = ico + 1
            zc(iad2-1+i) = zc(iad1-1+ico)
        endif
 3      continue
!
!        -- CAS D'1 SEGMENT DE CARACTERES (K8):
!        ---------------------------------
    else if (scal(1:3).eq.'K8 ') then
        do 4,i = 1,ncmp
        if (exisdg(dg1,i)) then
            ico = ico + 1
            zk8(iad2-1+i) = zk8(iad1-1+ico)
        endif
 4      continue
!
!        -- CAS D'1 SEGMENT DE CARACTERES (K16):
!        ---------------------------------
    else if (scal(1:3).eq.'K16') then
        do 5,i = 1,ncmp
        if (exisdg(dg1,i)) then
            ico = ico + 1
            zk16(iad2-1+i) = zk16(iad1-1+ico)
        endif
 5      continue
!
!        -- CAS D'1 SEGMENT DE CARACTERES (K24):
!        ---------------------------------
    else if (scal(1:3).eq.'K24') then
        do 6,i = 1,ncmp
        if (exisdg(dg1,i)) then
            ico = ico + 1
            zk24(iad2-1+i) = zk24(iad1-1+ico)
        endif
 6      continue
    else
        call u2mesk('F', 'CALCULEL3_38', 1, scal(1:4))
    endif
!
    do 10,i = 1,nec
    dg2(i) = ior(dg2(i),dg1(i))
    10 end do
!
end subroutine
