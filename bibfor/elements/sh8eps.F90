subroutine sh8eps(xetemp, xidepp, deploc, propel)
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
!               ELEMENT SHB8
!
    implicit none
    include 'jeveux.h'
    include 'asterfort/chrp3d.h'
    include 'asterfort/dsdx3d.h'
    include 'asterfort/mulmat.h'
    include 'asterfort/rloshb.h'
    include 'asterfort/shbksi.h'
    include 'asterfort/shcalb.h'
    integer :: lag
    real(kind=8) :: xe(24), xidepp(*)
    real(kind=8) :: xxg5(5), xcoq(3, 4), bksip(3, 8, 5), b(3, 8)
    real(kind=8) :: xcent(3), ppp(3, 3)
    real(kind=8) :: xl(3, 4), xxx(3), yyy(3)
    real(kind=8) :: deps(6), ue(3, 8)
    real(kind=8) :: depslo(6), deploc(*), propel(*), rr2(3, 3)
    real(kind=8) :: xetemp(*), rr12(3, 3), dusx(9), pppt(3, 3)
!
!
! ON DEFINIT LES POINTS DE GAUSS ET LES POIDS
!
!-----------------------------------------------------------------------
    integer :: i, ip, j
    real(kind=8) :: ajac, rbid, zeta, zlamb
!-----------------------------------------------------------------------
    xxg5(1) = -0.906179845938664D0
    xxg5(2) = -0.538469310105683D0
    xxg5(3) = 0.d0
    xxg5(4) = 0.538469310105683D0
    xxg5(5) = 0.906179845938664D0
!
! -----------------------------------------------------
! ON VERIFIE QUE LA CONNECTIVITE DONNE UN REPERE DIRECT
! SI CE N EST PAS LE CAS ON PERMUTE LES NOEUDS
! -----------------------------------------------------
!
!     ON FAIT UNE COPIE DE XETEMP DANS XE
    do 10 i = 1, 24
        xe(i) = xetemp(i)
10  end do
!
! UE: INCREMENT DE DEPLACEMENT NODAL, REPERE GLOBAL
!
! XE: DEBUT DU PAS
    do 30 j = 1, 8
        do 20 i = 1, 3
            ue(i,j) = xidepp((j-1)*3+i)
20      continue
30  end do
!
! CALCUL DE BKSIP(3,8,IP) DANS REPERE DE REFERENCE
!      BKSIP(1,*,IP) = VECTEUR BX AU POINT GAUSS IP
!      BKSIP(2,*,IP) = VECTEUR BY AU POINT GAUSS IP
!      BKSIP(3,*,IP) = VECTEUR BZ AU POINT GAUSS IP
!
    call shbksi(5, xxg5, bksip)
!
    do 120 ip = 1, 5
!
! DEFINITION DES 4 POINTS  COQUES
!
        zeta = xxg5(ip)
        zlamb = 0.5d0*(1.d0-zeta)
        do 50 i = 1, 4
            do 40 j = 1, 3
                xcoq(j,i) = zlamb*xe((i-1)*3+j) + (1.d0-zlamb)*xe((i- 1+4)*3+j)
40          continue
50      continue
!
! CALCUL DE PPP 3*3 PASSAGE DE GLOBAL A LOCAL,COQUE
! XCENT : COORD GLOBAL DU CENTRE DE L'ELEMENT
!
        call rloshb(xcoq, xcent, ppp, xl, xxx,&
                    yyy, rbid)
!
! CALCUL DE B : U_GLOBAL ---> EPS_GLOBAL
!
        call shcalb(bksip(1, 1, ip), xe, b, ajac)
!
! CALCUL DE EPS DANS LE REPERE GLOBAL: 1 POUR DEFORMATIONS LINEAIRES
!                                     2 POUR TERMES CARRES EN PLUS
        do 60 i = 1, 6
            deps(i) = 0.d0
60      continue
        lag = 0
        if (lag .eq. 1) then
! ON AJOUTE LA PARTIE NON-LINEAIRE DE EPS
            call dsdx3d(2, b, ue, deps, dusx,&
                        8)
        else
            call dsdx3d(1, b, ue, deps, dusx,&
                        8)
        endif
        do 80 i = 1, 3
            do 70 j = 1, 3
                pppt(j,i) = ppp(i,j)
70          continue
80      continue
        rr12(1,1) = dusx(1)
        rr12(1,2) = dusx(2)
        rr12(1,3) = dusx(3)
        rr12(2,1) = dusx(4)
        rr12(2,2) = dusx(5)
        rr12(2,3) = dusx(6)
        rr12(3,1) = dusx(7)
        rr12(3,2) = dusx(8)
        rr12(3,3) = dusx(9)
        call mulmat(3, 3, 3, pppt, rr12,&
                    rr2)
        call mulmat(3, 3, 3, rr2, ppp,&
                    rr12)
        dusx(1) = rr12(1,1)
        dusx(2) = rr12(1,2)
        dusx(3) = rr12(1,3)
        dusx(4) = rr12(2,1)
        dusx(5) = rr12(2,2)
        dusx(6) = rr12(2,3)
        dusx(7) = rr12(3,1)
        dusx(8) = rr12(3,2)
        dusx(9) = rr12(3,3)
        do 90 i = 1, 9
            propel(i+ (ip-1)*9) = dusx(i)
90      continue
!
        do 100 i = 1, 6
            depslo(i) = 0.d0
100      continue
        call chrp3d(ppp, deps, depslo, 2)
!
! ATTENTION !!! DEFORMATION ECRITES SOUS LA FORME:
!               [DEPS] = [E_XX, E_YY, E_ZZ, 2*E_XY, 2*E_YZ, 2*E_XZ]
        do 110 i = 1, 6
!
! ON LAISSE LES DEFORMATIONS DANS LE REPERE LOCAL POUR LA PLASTICITE
!
            deploc((ip-1)*6+i) = depslo(i)
110      continue
120  end do
!
end subroutine
