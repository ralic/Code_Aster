subroutine tran63(c66, c3333, icas)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!=======================================================================
! ICAS  = 1 ---> PASSAGE D'UN TENSEUR D'ORDRE 4 A UN TENSEUR D'ORDRE 2
!       = 2 ---> PASSAGE D'UN TENSEUR D'ORDRE 2 A UN TENSEUR D'ORDRE 4
!
    implicit none
    integer :: icas, i1(6), j1(6), ij, kl, ij1(3, 3), i, j, k, l
    real(kind=8) :: c3333(3, 3, 3, 3), c66(6, 6)
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    if (icas .eq. 1) then
        do 10 k = 1, 3
            i1(k)=k
            j1(k)=k
10      continue
!
        i1(4)=1
        j1(4)=2
        i1(5)=2
        j1(5)=3
        i1(6)=1
        j1(6)=3
!
        do 30 ij = 1, 6
            do 20 kl = 1, 6
                c66(ij,kl)=c3333(i1(ij),j1(ij),i1(kl),j1(kl))
20          continue
30      continue
    endif
!
    if (icas .eq. 2) then
        do 40 k = 1, 3
            ij1(k,k)=k
40      continue
!
        ij1(1,2)=4
        ij1(2,1)=4
        ij1(2,3)=5
        ij1(3,2)=5
        ij1(1,3)=6
        ij1(3,1)=6
!
        do 80 i = 1, 3
            do 70 j = 1, 3
                do 60 k = 1, 3
                    do 50 l = 1, 3
                        c3333(i,j,k,l)=c66(ij1(i,j),ij1(k,l))
50                  continue
60              continue
70          continue
80      continue
    endif
!
end subroutine
