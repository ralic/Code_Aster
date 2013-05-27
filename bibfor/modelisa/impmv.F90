subroutine impmv(ifm, txt, mv, n, isym)
    implicit   none
    integer :: ifm, n, isym
    real(kind=8) :: mv(n)
    character(len=8) :: txt
! --- -------------------------------------------------------------
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
! --- -------------------------------------------------------------
!       IMPRESSION MATRICE STOCKEE COLONNE
!       IN     MV    =  MATRICE STOCKE COLONNE
!              N     =  LONGEUR DU VECTEUR MATRICE NON-SYMETRIQUE
!              TXT   =  TEXTE
!              IFM   =  UNITE D'IMPRESSION
!              ISYM  =  (1) SYMETRIQUE   (2) NON-SYMETRIQUE
! --- -------------------------------------------------------------
    integer :: i, j, k, m
    real(kind=8) :: mp(12, 12)
!
    if (n .eq. 0) goto 9999
!
    if (n .eq. 4) m = 2
    if (n .eq. 9) m = 3
    if (n .eq. 16) m = 4
    if (n .eq. 36) m = 6
    if (n .eq. 144) m = 12
!
    if (isym .eq. 1) then
! -- --  SYMETRIQUE PAR COLONNE
        k = 0
        do 10 j = 1, m
            do 20 i = 1, j
                k = k + 1
                mp(i,j) = mv(k)
                mp(j,i) = mv(k)
20          continue
10      continue
        write(ifm,100) txt,'SYMETRIQUE'
    else
! -- --  NON SYMETRIQUE PAR COLONNE
        k = 0
        do 15 j = 1, m
            do 25 i = 1, m
                k = k + 1
                mp(i,j) = mv(k)
25          continue
15      continue
        write(ifm,100) txt,'NON SYMETRIQUE'
    endif
!
    do 30 i = 1, m
        write(ifm,201) (mp(i,j),j=1,m)
30  end do
    write(ifm,*)' '
!
    100 format(/,3x,a8,3x,a20)
    201 format(12(2x,1pd10.3))
!
9999  continue
end subroutine
