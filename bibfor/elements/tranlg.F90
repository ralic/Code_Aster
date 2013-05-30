subroutine tranlg(nb1, nddlx, nddlet, plg, matloc,&
                  xr)
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
    integer :: nb1, nddlet
    real(kind=8) :: xr(*)
    real(kind=8) :: matloc(nddlx, nddlx), plg(9, 3, 3)
    real(kind=8) :: matxp(48, 48), matx(51, 51)
    real(kind=8) :: kb12pt(48, 3), kb21pg(3, 48), kb22pt(3, 3)
!
!-----------------------------------------------------------------------
    integer :: i, i1, i2, ib, j, j1, j2
    integer :: jb, k, k1, kompt, nb2, nddle, nddlx
!
!-----------------------------------------------------------------------
    nddle=6*nb1
!
!     CONSTRUCTION DE LA MATRICE GLOBALE K = TLAMBDA * KB * LAMBDA
!
!             KB11 KB12                 P   0
!       KB =                  LAMBDA =
!             KB21 KB22                 0   P9
!
!       KB11(NDDLE,NDDLE) , KB12(NDDLE,3) , KB21(3,NDDLE) , KB22(3,3)
!          P(NDDLE,NDDLE) ,   P9(3,3)
!
!          P = PLG (IB,3,3) IB=1,NB1
!
!     CALCULS DU BLOC PRINCIPAL TP * KB11 * P
!
!     CONSTRUCTION DE LA MATRICE MATXP = MATLOC *  PLG  : (NDDLE,NDDLE)
!
    do 30 i1 = 1, nddle
        do 40 jb = 1, nb1
            do 50 j = 1, 3
                j1=3*(2*jb-2)+j
                matxp(i1,j1)=matloc(i1,j1)
!
                j2=3*(2*jb-1)+j
                matxp(i1,j2)=0.d0
                do 60 k = 1, 3
                    k1=3*(2*jb-1)+k
                    matxp(i1,j2)=matxp(i1,j2)+matloc(i1,k1)*plg(jb,k,&
                    j)
60              end do
50          end do
40      end do
30  end do
!
!   CONSTRUCTION DE LA MATRICE MATX = PLGT * MATLOC * PLG (NDDLET,NDDLE)
!
!   CONSTRUCTION EN PREMIER DE PT * KB11 * P (NDDLE,NDDLE)
!
    do 100 ib = 1, nb1
        do 110 i = 1, 3
            i1=3*(2*ib-2)+i
!
            i2=3*(2*ib-1)+i
            do 120 j1 = 1, nddle
                matx(i1,j1)=matxp(i1,j1)
!
                matx(i2,j1)=0.d0
                do 130 k = 1, 3
                    k1=3*(2*ib-1)+k
!CC      MATX(I2,J1)=MATX(I2,J1)+PLGT(IB,I,K)*MATXP(K1,J1)
                    matx(i2,j1)=matx(i2,j1)+plg (ib,k,i)*matxp(k1,j1)
130              end do
120          end do
110      end do
100  end do
!
    nb2=nb1+1
!
!     CALCULS DES SOUS MATRICES POUR FORMER LA MATRICE COMPLETE K
!
!     CALCULS DE KB * LAMBDA
!
    do 200 i = 1, nddle
        do 210 j = 1, 3
            kb12pt(i,j)=0.d0
            do 220 k = 1, 3
                k1=nddle+k
                kb12pt(i,j)=kb12pt(i,j)+matloc(i,k1)*plg(nb2,k,j)
220          end do
210      end do
200  end do
!
    do 230 i = 1, 3
        i1=nddle+i
        do 240 jb = 1, nb1
            do 250 j = 1, 3
                j1=3*(2*jb-2)+j
                kb21pg(i,j1)=matloc(i1,j1)
!
                j2=3*(2*jb-1)+j
                kb21pg(i,j2)=0.d0
                do 260 k = 1, 3
                    k1=3*(2*jb-1)+k
                    kb21pg(i,j2)=kb21pg(i,j2)+matloc(i1,k1)*plg(jb,k,&
                    j)
260              end do
250          end do
240      end do
230  end do
!
    do 270 i = 1, 3
        i1=nddle+i
        do 280 j = 1, 3
            kb22pt(i,j)=0.d0
            do 290 k = 1, 3
                k1=nddle+k
                kb22pt(i,j)=kb22pt(i,j)+matloc(i1,k1)*plg(nb2,k,j)
290          end do
280      end do
270  end do
!
!     CALCULS DE K = TLAMBDA * KB * LAMBDA
!
    do 300 ib = 1, nb1
        do 310 i = 1, 3
            i1=3*(2*ib-2)+i
!
            i2=3*(2*ib-1)+i
            do 320 j = 1, 3
                j1=nddle+j
                matx(i1,j1)=kb12pt(i1,j)
!
                matx(i2,j1)=0.d0
                do 330 k = 1, 3
                    k1=3*(2*ib-1)+k
                    matx(i2,j1)=matx(i2,j1)+plg(ib,k,i)*kb12pt(k1,j)
330              end do
320          end do
310      end do
300  end do
!
    do 340 i = 1, 3
        i1=nddle+i
        do 350 j = 1, nddle
            matx(i1,j)=0.d0
            do 360 k = 1, 3
                matx(i1,j)=matx(i1,j)+plg(nb2,k,i)*kb21pg(k,j)
360          end do
350      end do
340  end do
!
    do 370 i = 1, 3
        i1=nddle+i
        do 380 j = 1, 3
            j1=nddle+j
            matx(i1,j1)=0.d0
            do 390 k = 1, 3
                matx(i1,j1)=matx(i1,j1)+plg(nb2,k,i)*kb22pt(k,j)
390          end do
380      end do
370  end do
!
!     STOCKAGE DE LA PARTIE TRIANGULAIRE SUPERIEURE DANS LE TABLEAU XR
!
    kompt=0
    do 140 j = 1, nddlet
        do 150 i = 1, j
            kompt=kompt+1
            xr(kompt)=matx(i,j)
!        XR(KOMPT)=MATLOC(I,J)
150      end do
140  end do
!
end subroutine
