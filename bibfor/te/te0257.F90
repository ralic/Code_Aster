subroutine te0257(option, nomte)
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
!.......................................................................
    implicit none
!
!     BUT: CALCUL DES MATRICES DE MASSE  ELEMENTAIRES EN MECANIQUE
!          ELEMENTS 1D DE COUPLAGE ACOUSTICO-MECANIQUE
!
!          OPTION : 'MASS_MECA '
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
    include 'jeveux.h'
!
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/vff2dn.h'
    integer :: icodre
    character(len=8) :: fami, poum
    character(len=16) :: nomte, option
    real(kind=8) :: a(3, 3, 3, 3), nx, ny, rho, norm(2), poids
    integer :: igeom, imate, i, j, k, l, ik, ijkl, ldec, kco, ino, jno
    integer :: nno, npg, kp, ndim, nnos, jgano
    integer :: ipoids, ivf, idfde, imatuu, kpg, spt
    logical :: laxi
!
!
!-----------------------------------------------------------------------
    real(kind=8) :: r, r8b
!-----------------------------------------------------------------------
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    laxi = .false.
    if (lteatt(' ','AXIS','OUI')) laxi = .true.
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATUUR', 'E', imatuu)
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
    call rcvalb(fami, kpg, spt, poum, zi(imate),&
                ' ', 'FLUIDE', 0, ' ', r8b,&
                1, 'RHO', rho, icodre, 1)
!
!     INITIALISATION DE LA MATRICE
!
    do 40 k = 1, 3
        do 30 l = 1, 3
            do 20 i = 1, nno
                do 10 j = 1, i
                    a(k,l,i,j) = 0.d0
10              continue
20          continue
30      continue
40  end do
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 90 kp = 1, npg
        ldec = (kp-1)*nno
!
        call vff2dn(ndim, nno, kp, ipoids, idfde,&
                    zr(igeom), nx, ny, poids)
!
        norm(1) = nx
        norm(2) = ny
!
        if (laxi) then
            r = 0.d0
            do 50 i = 1, nno
                r = r + zr(igeom+2* (i-1))*zr(ivf+ldec+i-1)
50          continue
            poids = poids*r
        endif
!
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!       CALCUL DU TERME PHI*(U.N DS)       C
!CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!
        do 80 ino = 1, nno
            do 70 jno = 1, ino
                do 60 kco = 1, 2
!
                    a(kco,3,ino,jno) = a(kco,3,ino,jno) + poids*norm( kco)*rho*zr(ivf+ldec+ino-1)&
                                       &* zr(ivf+ldec+jno-1)
!
!
!
!
60              continue
70          continue
80      continue
!
90  end do
!
    do 120 ino = 1, nno
        do 110 jno = 1, ino
            do 100 kco = 1, 2
                a(3,kco,ino,jno) = a(kco,3,ino,jno)
100          continue
110      continue
120  end do
!
!
! PASSAGE DU STOCKAGE RECTANGULAIRE (A) AU STOCKAGE TRIANGULAIRE (ZR)
!
    ijkl = 0
    ik = 0
    do 160 k = 1, 3
        do 150 l = 1, 3
            do 140 i = 1, nno
                ik = ((3*i+k-4)* (3*i+k-3))/2
                do 130 j = 1, i
                    ijkl = ik + 3* (j-1) + l
                    zr(imatuu+ijkl-1) = a(k,l,i,j)
!
!
!
!
130              continue
140          continue
150      continue
160  end do
!
end subroutine
