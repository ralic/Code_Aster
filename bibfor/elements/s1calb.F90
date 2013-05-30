subroutine s1calb(bksip, xnoe, b, ajac)
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
!*
!*     -----------------------------------------------------------------
!*      CALCUL DE LA MATRICE B AU POINT D INTEGRATION DONNE
!*
!*                                                    V-D.TRINH     2006
!*     -----------------------------------------------------------------
!*
!*  ENTREES :
!*     XNOE(45)     : COORDONNEES DES NOEUDS
!*     IPINT        : NO DU POINT D INTEGRATION
!*     MOT          : NOM DE L ELEMENT FINI
!*  SORTIE
!*     B(NBLIB,NBN) : MATRICE B
!*     AJAC         : JACOBIEN
!*
    implicit none
!*
    include 'asterfort/matini.h'
    include 'asterfort/matinv.h'
    real(kind=8) :: xnoe(45), bksip(3, 15)
    real(kind=8) :: b(3, 15), ajac
!*
!*----   VARIABLES LOCALES
!*
    real(kind=8) :: dj(3, 3), uj(3, 3)
    integer :: i, j, k
!*
!*---   DJ = BKSIP * TRANSPOSE(XNOE)
!*
    call matini(3, 3, 0.d0, dj)
!
    do 50 i = 1, 3
        do 40 j = 1, 3
            do 30 k = 1, 15
                dj(j,i)=dj(j,i)+bksip(j,k)*xnoe((k-1)*3+i)
30          continue
40      continue
50  end do
!
!-----   UJ(J,I)  MATRICE INVERSE DE DJ(J,I)
!
    call matinv('S', 3, dj, uj, ajac)
! TEST SI ELEMENT TROP DEFORME: CROISEMENT
    ajac=abs(ajac)
!*
!*-----   MATRICE ( B ) = UJ * BKSIP
!*
    call matini(3, 15, 0.d0, b)
!
    do 100 k = 1, 3
        do 90 j = 1, 3
            do 80 i = 1, 15
                b(j,i)=b(j,i)+uj(j,k)*bksip(k,i)
80          continue
90      continue
100  end do
!*
end subroutine
