subroutine shbpkc(sigpk, sigca, dusdx, npg)
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
!
!--------------------------------------------------------
! ELEMENT SHB8-PS A.COMBESCURE, S.BAGUET INSA LYON 2003 /
!-------------------------------------------------------
    implicit none
    include 'asterfort/mulmat.h'
    real(kind=8) :: sigpk(*), sigca(*), sig(6), sig33(3, 3)
    real(kind=8) :: sigtmp(120)
    real(kind=8) :: dusdx(*), f(3, 3), tmptab(3, 3), ft(3, 3)
    real(kind=8) :: cc1, cc2, cc3, detf
    integer :: ip, i, j, npg, nsig
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
!              TRANSFORMATION DES CONTRAINTES DE PIOLA-KIRCHHOFF
!              EN CONTRAINTES DE CAUCHY AUX 5 PTS D'INTEGRATION DU SHB8
!
!      SIG(CA) = 1/DET(F) * (F) * SIG(PK) * TRANSP(F)
!
!  ENTREE
!
!      DUSDX   : D(1)=DU1/DX1 ;  D(2)=DU2/DX1 ; D(3)=DU3/DX1 ;
!              : D(4)=DU1/DX2 ;  D(5)=DU2/DX2 ; D(6)=DU3/DX2 ;
!              : D(7)=DU1/DX3 ;  D(8)=DU2/DX3 ; D(9)=DU3/DX3
!
!      SIGPK   : CONTRAINTES AUX 5 PTS D'INTEGRATIONS
!               [SIG] = [S_11, S_22, S_33, S_12, S_23, S_13]
!
!  SORTIE
!
!      SIGCA
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    nsig = npg*6
    do 100 ip = 1, npg
!CCCCCCCCCCCCCC CALCUL DE F: PASSAGE AU PAS FINAL
        do 20 i = 1, 3
            do 10 j = 1, 3
                f(i,j) = dusdx((i-1)*3+j+ (ip-1)*9)
10          continue
20      continue
        f(1,1) = f(1,1) + 1.d0
        f(2,2) = f(2,2) + 1.d0
        f(3,3) = f(3,3) + 1.d0
!CCCCCCCCCCCCCC CALCUL DE F^T
        do 40 i = 1, 3
            do 30 j = 1, 3
                ft(i,j) = f(j,i)
30          continue
40      continue
!CCCCCCCCCCCCCC EXTRACTION DE LA CONTRAINTE DE PK DU POINT IP
        do 50 i = 1, 6
            sig(i) = sigpk((ip-1)*6+i)
50      continue
        do 70 i = 1, 3
            do 60 j = 1, 3
                sig33(i,j) = 0.d0
60          continue
70      continue
        sig33(1,1) = sig(1)
        sig33(2,2) = sig(2)
        sig33(3,3) = sig(3)
        sig33(1,2) = sig(4)
        sig33(2,3) = sig(5)
        sig33(1,3) = sig(6)
!CCCCCCCCCCCCCC MULTIPLICATION POUR AVOIR LA CONTRAINTE DE CAUCHY
        call mulmat(3, 3, 3, sig33, ft,&
                    tmptab)
        call mulmat(3, 3, 3, f, tmptab,&
                    sig33)
!CCCCCCCCCCCCCC CALCUL DE DETERMINANT(F)
        cc1 = f(2,2)*f(3,3) - f(3,2)*f(2,3)
        cc2 = -f(1,2)*f(3,3) + f(3,2)*f(1,3)
        cc3 = f(1,2)*f(2,3) - f(2,2)*f(1,3)
        detf = f(1,1)*cc1 + f(2,1)*cc2 + f(3,1)*cc3
!
        do 90 i = 1, 3
            do 80 j = 1, 3
                sig33(i,j) = sig33(i,j)/detf
80          continue
90      continue
!CCCCCCCCCCCCCC SORTIE DE LA CONTRAINTE DE CAUCHY
        sigtmp(1+ (ip-1)*6) = sig33(1,1)
        sigtmp(2+ (ip-1)*6) = sig33(2,2)
        sigtmp(3+ (ip-1)*6) = sig33(3,3)
        sigtmp(4+ (ip-1)*6) = sig33(1,2)
        sigtmp(5+ (ip-1)*6) = sig33(2,3)
        sigtmp(6+ (ip-1)*6) = sig33(1,3)
100  end do
!CCCCCCCCCCCCCC ON SORT LES RESULTATS
    do 110 i = 1, nsig
        sigca(i) = sigtmp(i)
110  end do
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
end subroutine
