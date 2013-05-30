subroutine btdmsn(ind, nb1, intsn, npgsr, xr,&
                  btdm, btdf, btds, btild)
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
!
    implicit none
!
    integer :: nb1, intsn, npgsr
    real(kind=8) :: xr(*), btdm1, btds1
    real(kind=8) :: btdm(4, 3, 42), btds(4, 2, 42), btdf(3, 42), btild(5, 42)
!
!-----------------------------------------------------------------------
    integer :: i, i1, ind, j, k, l
!-----------------------------------------------------------------------
    l=702
!
!     CALCUL DE BTILDMN, BTILDSN AUX PTS DE GAUSS NORMAL
!            M=MEMBRANE, S=CISAILLEMENT, N=NORMAL
!
!        BTILDMN = SOMME MKBARRE BTILDMR   OU  K=1,NPGSR  R=REDUIT
!        BTILDSN = SOMME MKBARRE BTILDSR   OU  K=1,NPGSR  R=REDUIT
!        (AUX PTS DE GAUSS NORMAL)
!
!     MKBARRE = FONCTIONS DE FORME ASSOCIEES AUX PTS DE GAUSS REDUITS
!
    i1=l+4*(intsn-1)
!
    if (ind .eq. 0) then
!
!     INTEGRATION UNIFORME (ICI REDUITE)
!     BTILD =  BTDF + BTDM  : BTDF, BTDM , BTDS
!              BTDS           OBTENUES PAR INTEGRATION REDUITE
!
        do 14 i = 1, 3
            do 15 j = 1, 5*nb1+2
                btild(i,j)=btdf(i,j)+btdm(intsn,i,j)
15          end do
14      end do
!
        do 16 i = 1, 2
            do 17 j = 1, 5*nb1+2
                btild(3+i,j)=btds(intsn,i,j)
17          end do
16      end do
!
    else if (ind.eq.1) then
!
!     INTEGRATION SELECTIVE
!     BTILD =  BTDF + BTDM  : BTDF, BTDM , BTDS
!              BTDS           BTDM, BTDS INTEGRATION REDUITE
!
!
        do 10 i = 1, 3
            do 20 j = 1, 5*nb1+2
                btdm1=0.d0
                if (i .le. 2) btds1=0.d0
                do 30 k = 1, npgsr
                    btdm1=btdm1+xr(i1+k)*btdm(k,i,j)
                    if (i .le. 2) btds1=btds1+xr(i1+k)*btds(k,i,j)
30              end do
!
!                               BTILDMN + BTILDFN
!     CONSTRUCTION DE BTILD =
!                               BTILDSN
!
!
                btild(i,j)=btdm1+btdf(i,j)
                if (i .le. 2) btild(i+3,j)=btds1
20          end do
10      end do
!
    endif
!
end subroutine
