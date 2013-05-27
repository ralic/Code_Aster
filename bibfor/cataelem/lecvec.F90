subroutine lecvec(iad, long, type, unite)
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
!
!
    include 'jeveux.h'
    include 'asterfort/assert.h'
    character(len=3) :: type
    integer :: unite
!
!
!-----------------------------------------------------------------------
    integer :: iad, k, long
!-----------------------------------------------------------------------
    if (type .eq. 'R') then
        do 11, k=1,long
        read(unite,'(1E12.5)') zr(iad-1+k)
11      continue
!
    else if (type.eq.'I') then
        do 21, k=1,long
        read(unite,'(I12)') zi(iad-1+k)
21      continue
!
    else if (type.eq.'K8') then
        do 31, k=1,long
        read(unite,'(A8)') zk8(iad-1+k)
31      continue
!
    else if (type.eq.'K16') then
        do 32, k=1,long
        read(unite,'(A16)') zk16(iad-1+k)
32      continue
!
    else if (type.eq.'K24') then
        do 33, k=1,long
        read(unite,'(A24)') zk24(iad-1+k)
33      continue
!
    else if (type.eq.'K32') then
        do 34, k=1,long
        read(unite,'(A32)') zk32(iad-1+k)
34      continue
!
    else if (type.eq.'K80') then
        do 35, k=1,long
        read(unite,'(A80)') zk80(iad-1+k)
35      continue
    else
        call assert(.false.)
    endif
!
!
end subroutine
