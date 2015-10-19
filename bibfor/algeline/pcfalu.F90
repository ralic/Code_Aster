subroutine pcfalu(n, inc, ipc, inx, ipx,&
                  index, imp)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W1304
    implicit none
    integer :: n
    integer :: inc(n), index(*)
    integer(kind=4) :: ipc(*), ipx(*)
    integer :: inx(0:n)
!-----------------------------------------------------------------------
! FONCTION  INC;IPC ------------>INX;IPX
!           SYMETRIQUE           PLEINE FORMAT LU
!-----------------------------------------------------------------------
    integer :: i, ii, ii1, ii2, imp, j, kkk
!-----------------------------------------------------------------------
!
!
!  CALCUL DU NOMBRE DE COEFS PAR LIGNE COMPLETE
!        SANS LA DIAGONALE
! ----------------------------------------------
!
!  DEMI-LIGNE
    index(1) = 0
    do 10 i = 2, n
        index(i) = inc(i) - inc(i-1) - 1
10  end do
!  SYMETRIQUES
    do 30 i = 2, n
        ii1 = inc(i-1) + 1
        ii2 = inc(i)
        do 20 ii = ii1, ii2 - 1
            j = ipc(ii)
            index(j) = index(j) + 1
20      continue
30  end do
!
    if (imp .eq. 1) then
        ii1 = index(1)
        ii2 = index(1)
        do 40 i = 2, n
            ii1 = min(ii1,index(i))
            ii2 = max(ii2,index(i))
40      continue
    endif
!
! CALCUL NOUVEAU INX POINTEUR DEBUT DE LIGNE
!                             QUI SE DECALE ENSUITE
! --------------------------------------------------
!
    inx(0) = 0
    inx(1) = 1
    do 50 i = 2, n
        inx(i) = inx(i-1) + index(i-1)
50  end do

!
! CALCUL NOUVEAUX IPX
!    INX(I)= DEBUT LIGNE I DE L
!    INDEX(I)=POINTEUR DERNIER COEF LIGNE I DE U
! ------------------------------------------------
!
    index(1) = 0
    do 70 i = 2, n
        ii1 = inc(i-1) + 1
        ii2 = inc(i)
        kkk = inx(i) - 1
        do 60 ii = ii1, ii2 - 1
            kkk = kkk + 1
            ipx(kkk) = ipc(ii)
            j = ipc(ii)
            index(j) = index(j) + 1
            ipx(index(j)) = int(i, 4)
60      continue
        index(i) = kkk
70  end do


!  TRANSFORMATION DE INX DE DEBUT EN FIN DE LIGNE DE LU
!  ----------------------------------------------------
    inx(0) = 0
    do 80 i = 1, n - 1
        inx(i) = inx(i+1) - 1
80  end do
    inx(n) = index(n)
!
    if (imp .eq. 1) then
        do 90 i = 1, 5
            ii1 = inx(i-1) + 1
            ii2 = inx(i)
90      continue
    endif
end subroutine
