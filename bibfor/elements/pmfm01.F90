subroutine pmfm01(kanl, xl, cars, m)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: kanl
    real(kind=8) :: cars(6), xl
    real(kind=8) :: m(*)
!     ------------------------------------------------------------------
!     CALCUL DE LA MATRICE DE MASSE DES ELEMENTS DE POUTRE MULTIFIBRES
!     PAR LA METHODE
!          - DES MASSES CONCENTREES
!          - DES MASSES EQUIVALENTES
!     ------------------------------------------------------------------
! IN  KANL       - TYPE DE MODELISATION DES MASSES
! IN               KANL = 0 MASSES CONCENTREES FORMULATION S.D.R.C.
! IN               KANL = 1 MASSES COHERENTE
! IN R*8  ! CARS   !     6   ! CARACTERISTIQUES INTEGREES DE LA SECTION
!              INT(VAR DS) = INTEGRALE DE VAR SUR LA SECTION
!              RHO : MASSE VOLUMIQUE
!              Y,Z : COORDONNEES DE LA FIBRE
!
! IN R*8  ! CARS(1)!     -   ! RA   = INT(RHO. DS)
! IN R*8  ! CARS(2)!     -   ! RSZ  = INT(RHO.Y DS)
! IN R*8  ! CARS(3)!     -   ! RSY  = INT(RHO.Z DS)
! IN R*8  ! CARS(4)!     -   ! RIZ  = INT(RHO.Y.Y DS)
! IN R*8  ! CARS(5)!     -   ! RIY  = INT(RHO.Z.Z DS)
! IN R*8  ! CARS(6)!     -   ! RPYZ = INT(RHO.Y.Z DS)
! OUT M          -(78) MATRICE DE MASSE ELEMENT
!
!
    real(kind=8) :: zero, deux, trois, cinq, six, sept, neuf, dix, onze, douze
    real(kind=8) :: treize
    real(kind=8) :: quinze, vingt, trente, v35, v70
    real(kind=8) :: v105, v140, v210, v420
    real(kind=8) :: v48
!
    parameter (zero=0.0d+0,deux=2.0d+0,trois=3.0d+0,cinq=5.0d+0,&
     &          six=6.0d+0,sept=7.0d+0,neuf=9.0d+0,dix=1.0d+1,&
     &          onze=1.1d+1,douze=1.2d+1,treize=1.3d+1,quinze=1.5d+1,&
     &          vingt=2.0d+1,v35=3.5d+1,v48=4.8d+1,v70=7.0d+1,&
     &          trente=3.0d+1,v105=1.05d+2,v140=1.4d+2,v210=2.1d+2,&
     &          v420=4.2d+2)
    real(kind=8) :: co13, co112, co1335, co65, co720, c11210, co970, co320
    real(kind=8) :: c13420
    real(kind=8) :: co120, co1302, co130, co1105, co215, co1140
    real(kind=8) :: ms11, ms22, ms33, ms44, ms55, ms66, ms15, ms16, ms24, ms34
    real(kind=8) :: ms56
    real(kind=8) :: zaire, zinex, c
    integer :: ip(12), i
    data ip/0,1,3,6,10,15,21,28,36,45,55,66/
!
!
    if (kanl .eq. 0) then
!     ------------------------------------------------------------------
!               MASSES CONCENTREES FORMULATION S.D.R.C. KANL =0
!     ------------------------------------------------------------------
!     INITIALISATION
        do 10 i = 1, 78
            m(i) = zero
10      continue
        zaire = cars(1)*xl/deux
        zinex = xl* (cars(4)+cars(5))/deux
        c = deux*zaire*xl
        c = min(c*xl/v105,c/v48)
        m(ip(1)+1) = zaire
        m(ip(2)+2) = zaire
        m(ip(3)+3) = zaire
        m(ip(4)+4) = zinex
        m(ip(5)+5) = c + deux*cars(5)*xl/quinze
        m(ip(6)+6) = c + deux*cars(4)*xl/quinze
        m(ip(7)+7) = zaire
        m(ip(8)+8) = zaire
        m(ip(9)+9) = zaire
        m(ip(10)+10) = zinex
        m(ip(11)+11) = m(ip(5)+5)
        m(ip(12)+12) = m(ip(6)+6)
    else
!     ------------------------------------------------------------------
!                       MASSES COHERENTES   KANL = 1
!     ------------------------------------------------------------------
        ms11 = cars(1)
        ms22 = cars(1)
        ms33 = cars(1)
        ms44 = cars(4) + cars(5)
        ms55 = cars(5)
        ms66 = cars(4)
        ms15 = cars(3)
        ms16 = -cars(2)
        ms24 = -ms15
        ms34 = -ms16
        ms56 = -cars(6)
!
        co13 = xl/trois
        co112 = xl/douze
!
        co1335 = treize*xl/v35
        co65 = six/ (cinq*xl)
        co720 = sept*xl/vingt
        c11210 = onze*xl*xl/v210
        co970 = neuf*xl/v70
        co320 = trois*xl/vingt
        c13420 = treize*xl*xl/v420
!
        co120 = xl*xl/vingt
        co1302 = xl*xl/trente
!
        co130 = xl/trente
        co1105 = xl*xl*xl/v105
        co215 = deux*xl/quinze
        co1140 = xl*xl*xl/v140
!
! COLONNE 1
        m(ip(1)+1) = ms11*co13
        m(ip(2)+1) = -ms16/deux
        m(ip(3)+1) = ms15/deux
        m(ip(4)+1) = zero
        m(ip(5)+1) = ms15*co112
        m(ip(6)+1) = ms16*co112
        m(ip(7)+1) = m(ip(1)+1)/deux
        m(ip(8)+1) = -m(ip(2)+1)
        m(ip(9)+1) = -m(ip(3)+1)
        m(ip(10)+1) = zero
        m(ip(11)+1) = -m(ip(5)+1)
        m(ip(12)+1) = -m(ip(6)+1)
!
! COLONNE 2
        m(ip(2)+2) = co1335*ms22 + co65*ms66
        m(ip(3)+2) = -co65*ms56
        m(ip(4)+2) = co720*ms24
        m(ip(5)+2) = ms56/dix
        m(ip(6)+2) = c11210*ms22 + ms66/dix
        m(ip(7)+2) = m(ip(2)+1)
        m(ip(8)+2) = co970*ms22 - co65*ms66
        m(ip(9)+2) = co65*ms56
        m(ip(10)+2) = co320*ms24
        m(ip(11)+2) = m(ip(5)+2)
        m(ip(12)+2) = -c13420*ms22 + ms66/dix
! COLONNE 3
        m(ip(3)+3) = co1335*ms33 + co65*ms55
        m(ip(4)+3) = co720*ms34
        m(ip(5)+3) = -c11210*ms33 - ms55/dix
        m(ip(6)+3) = -m(ip(5)+2)
        m(ip(7)+3) = m(ip(3)+1)
        m(ip(8)+3) = m(ip(9)+2)
        m(ip(9)+3) = co970*ms33 - co65*ms55
        m(ip(10)+3) = co320*ms34
        m(ip(11)+3) = c13420*ms33 - ms55/dix
        m(ip(12)+3) = m(ip(6)+3)
! COLONNE 4
        m(ip(4)+4) = co13*ms44
        m(ip(5)+4) = -co120*ms34
        m(ip(6)+4) = co120*ms24
        m(ip(7)+4) = zero
        m(ip(8)+4) = co320*ms24
        m(ip(9)+4) = co320*ms34
        m(ip(10)+4) = m(ip(4)+4)/deux
        m(ip(11)+4) = co1302*ms34
        m(ip(12)+4) = -co1302*ms24
! COLONNE 5
        m(ip(5)+5) = co1105*ms33 + co215*ms55
        m(ip(6)+5) = co215*ms56
        m(ip(7)+5) = m(ip(11)+1)
        m(ip(8)+5) = m(ip(6)+3)
        m(ip(9)+5) = -m(ip(11)+3)
        m(ip(10)+5) = -m(ip(11)+4)
        m(ip(11)+5) = -co1140*ms33 - co130*ms55
        m(ip(12)+5) = -co130*ms56
! COLONNE 6
        m(ip(6)+6) = co1105*ms22 + co215*ms66
        m(ip(7)+6) = m(ip(12)+1)
        m(ip(8)+6) = -m(ip(12)+2)
        m(ip(9)+6) = -m(ip(12)+3)
        m(ip(10)+6) = -m(ip(12)+4)
        m(ip(11)+6) = m(ip(12)+5)
        m(ip(12)+6) = -co1140*ms22 - co130*ms66
! COLONNE 7
        m(ip(7)+7) = m(ip(1)+1)
        m(ip(8)+7) = m(ip(8)+1)
        m(ip(9)+7) = m(ip(9)+1)
        m(ip(10)+7) = zero
        m(ip(11)+7) = -m(ip(11)+1)
        m(ip(12)+7) = -m(ip(12)+1)
! COLONNE 8
        m(ip(8)+8) = m(ip(2)+2)
        m(ip(9)+8) = m(ip(3)+2)
        m(ip(10)+8) = m(ip(4)+2)
        m(ip(11)+8) = -m(ip(5)+2)
        m(ip(12)+8) = -m(ip(6)+2)
! COLONNE 9
        m(ip(9)+9) = m(ip(3)+3)
        m(ip(10)+9) = m(ip(4)+3)
        m(ip(11)+9) = -m(ip(5)+3)
        m(ip(12)+9) = -m(ip(6)+3)
! COLONNE 10
        m(ip(10)+10) = m(ip(4)+4)
        m(ip(11)+10) = -m(ip(5)+4)
        m(ip(12)+10) = -m(ip(6)+4)
! COLONNE 11
        m(ip(11)+11) = m(ip(5)+5)
        m(ip(12)+11) = m(ip(6)+5)
! COLONNE 12
        m(ip(12)+12) = m(ip(6)+6)
    endif
!
end subroutine
