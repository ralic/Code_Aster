subroutine brfluo(sut, sut6, xmt, sige6, eps0,&
                  tau0, dt, evp06, evp16, devpt6,&
                  evpmax, bgpgmx, eragmx)
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
    implicit none
    include 'asterfort/brvp33.h'
    include 'asterfort/matini.h'
    include 'asterfort/transp.h'
    include 'asterfort/utbtab.h'
    real(kind=8) :: sige6(6), sige3(3), siged6(6)
    real(kind=8) :: evp06(6), evp16(6), evpd06(6)
    real(kind=8) :: sige33(3, 3), vse33(3, 3), vse33t(3, 3)
    real(kind=8) :: x33(3, 3), y33(3, 3), sut33(3, 3)
    real(kind=8) :: sut3(3), vdt33(3, 3), vdt33t(3, 3), dt3(3), sut6(6)
    real(kind=8) :: drag3(3), trav(3, 3)
    real(kind=8) :: evpl3f(3), devpt6(6), devpt3(3)
    real(kind=8) :: evpmax, aa, bgpgmx, eragmx
    integer :: i, j
    real(kind=8) :: xmt
    real(kind=8) :: a, dt, eps0, somme1, sut, tau0, x
    real(kind=8) :: y, zz
!
!        CLASSEMENT DES CONTRAINTES ET DES DEFORMATION:
!      I=1,3 => SIGMA II
!      I=4   => SIGMA 12
!      I=5   => SIGMA 13
!      I=6   => SIGMA 23
!        PARTITION DES CONTRAINTES EN PARTIE POSITIVE ET PARTIE NéGATIVE
!        CONTRAINTES EFFECTIVES PRINCIPALES ET MATRICE DE
!        PASSAGE à LA BASE PRINCIPALE
!        RANGEMENT DES CONTRAINTES EFFECTIVES EN TABLEAU 3*3
!
!-----------------------------------------------------------------------
    zz = 0.d0
!
    sige33(1,1)=sige6(1)
    sige33(2,2)=sige6(2)
    sige33(3,3)=sige6(3)
    sige33(1,2)=sige6(4)
    sige33(1,3)=sige6(5)
    sige33(2,3)=sige6(6)
    sige33(2,1)=sige33(1,2)
    sige33(3,1)=sige33(1,3)
    sige33(3,2)=sige33(2,3)
    do 10 i = 1, 3
        do 20 j = 1, 3
            x33(i,j)=sige33(i,j)
20      continue
10  end do
!        DIAGONALISATION ET VALEURS PROPRES PAR LA METHODE DE JACOBI
    call brvp33(x33, sige3, vse33)
!        TRANSPOSITION DE DE LA MATRICE DE PASSAGE
    call transp(vse33, 3, 3, 3, vse33t,&
                3)
!        PARTIE POSITIVE ET NEGATIVES DES CONTRAINTES EFFECTIVES
    do 30 i = 1, 3
!      SIGET3(I)=0.5D0*(SIGE3(I)+ABS(SIGE3(I)))
!      SIGEC3(I)=0.5D0*(SIGE3(I)-ABS(SIGE3(I)))
30  end do
!     DEFORMATION VISCOPLASTIQUE POSITIVES LIMITES ASSOCIéES AUX
!     CONTRAINTES EFFECTIVES ACTUELLES
!     DIRECTIONS PRINCIPALE DE ENDOMMAGEMENT
    sut33(1,1)=sut6(1)
    sut33(2,2)=sut6(2)
    sut33(3,3)=sut6(3)
    sut33(1,2)=sut6(4)
    sut33(1,3)=sut6(5)
    sut33(2,3)=sut6(6)
    sut33(2,1)=sut33(1,2)
    sut33(3,1)=sut33(1,3)
    sut33(3,2)=sut33(2,3)
!       DIRECTIONS PRINCIPALES DES ENDOMMAGEMENTS DE TRACTION
!       CONTRAINTES EFFECTIVES PRINCIPALES ET MATRICE DE PASSAGE à
!       LA BASE PRINCIPALE
    do 40 i = 1, 3
        do 50 j = 1, 3
            x33(i,j)=sut33(i,j)
50      continue
40  end do
!        DIAGONALISATION ET VALEURS PROPRES PAR LA METHODE DE JACOBI
    call brvp33(x33, sut3, vdt33)
    call transp(vdt33, 3, 3, 3, vdt33t,&
                3)
!
!      CALCUL DES ENDOMMAGEMENTS PRINCIPAUX
!       DRG3MX=ERAGMX/(3*EPS0+ERAGMX)
!
    do 60 i = 1, 3
        if (sut3(i) .lt. 0.0d0) then
            sut3(i)=0.0d0
        endif
        dt3(i)=1.d0-exp(-(1.d0/xmt)*((sut3(i)/sut)**xmt))
        drag3(i)=1.d0-exp(-(1.d0/xmt)*(bgpgmx/sut)**xmt)
        drag3(i)=min(drag3(i),dt3(i))
60  continue
    do 70 i = 1, 3
        x=(1.d0-drag3(i))
        if (x .le. 0.0d0) then
            if (eps0 .eq. 0) then
                y=0.0d0
            else
                y=evpmax/eps0
            endif
        else
            y=(1.d0-x)/x
            y=min(y,evpmax/eps0)
        endif
        evpl3f(i)=eps0*y
70  end do
!     PASSAGE DES DEFORMATION VDTM DANS LA
!       BASE PRINCIPALE D'ENDOMMAGEMENT
    call matini(3, 3, zz, x33)
    do 80 i = 1, 3
        x33(i,i)=evp06(i)
80  end do
    x33(1,2)=evp06(4)/2.d0
    x33(1,3)=evp06(5)/2.d0
    x33(2,3)=evp06(6)/2.d0
    x33(2,1)=x33(1,2)
    x33(3,1)=x33(1,3)
    x33(3,2)=x33(2,3)
!
    call utbtab('ZERO', 3, 3, x33, vdt33,&
                trav, y33)
    evpd06(1)=y33(1,1)
    evpd06(2)=y33(2,2)
    evpd06(3)=y33(3,3)
    evpd06(4)=y33(1,2)
    evpd06(5)=y33(1,3)
    evpd06(6)=y33(2,3)
!
!     PASSAGE DES CONTRAINTES EFFECTIVES DANS LA
!       BASE PRINCIPALE D'ENDOMMAGEMENT
    call matini(3, 3, zz, x33)
    do 90 i = 1, 3
        x33(i,i)=sige6(i)
90  end do
    x33(1,2)=sige6(4)
    x33(1,3)=sige6(5)
    x33(2,3)=sige6(6)
    x33(2,1)=x33(1,2)
    x33(3,1)=x33(1,3)
    x33(3,2)=x33(2,3)
!
    call utbtab('ZERO', 3, 3, x33, vdt33,&
                trav, y33)
    siged6(1)=y33(1,1)
    siged6(2)=y33(2,2)
    siged6(3)=y33(3,3)
    siged6(4)=y33(1,2)
    siged6(5)=y33(1,3)
    siged6(6)=y33(2,3)
!
    somme1=0.d0
    do 100 i = 1, 3
!       VITESSE DE FLUAGE
        a=(evpl3f(i)-evpd06(i))*(1.d0-exp(-dt/tau0))/dt
        if (a .lt. 0.d0) then
            a=0
        endif
        aa=a*siged6(i)
        somme1=somme1+aa
!
        devpt3(i)=a
100  end do
    if (somme1 .lt. 0.d0) then
        do 110 i = 1, 3
            devpt3(i)=0.d0
110      continue
    endif
!
! REPASSAGE DES VITESSES DANS LA BASE FIXE
    call matini(3, 3, zz, x33)
    do 120 i = 1, 3
        x33(i,i)=devpt3(i)
120  end do
!
!
    call utbtab('ZERO', 3, 3, x33, vdt33t,&
                trav, y33)
    devpt6(1)=y33(1,1)
    devpt6(2)=y33(2,2)
    devpt6(3)=y33(3,3)
    devpt6(4)=y33(1,2)*2.d0
    devpt6(5)=y33(1,3)*2.d0
    devpt6(6)=y33(2,3)*2.d0
!
end subroutine
