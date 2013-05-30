subroutine lcmmsg(nomfam, nbsys, nusys, pgl2, mus,&
                  ng, mg, ir, q)
    implicit none
! person_in_charge: jean-michel.proix at edf.fr
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
! ======================================================================
!       IN  FAMSYS  :  NOM FAMILLE SYS GLIS
!           NUSYS   :  NUMERO SYS GLIS (FACULTATIF)
!           PGL2     :  MATRICE DE PASSAGE REPERE GLOBAL REPERE LOCAL
!           IR      :  =0 pas de rotation de reseau ; =1 : rotation
!           Q       :  matrice de rotation de reseau
!     OUT:
!           NBSYS    : NOMBRE DE SYS GLIS
!           MUS       : TENSEUR MUS POUR LE SYS GLIS NUMERO NUSYS
!
    include 'asterfort/assert.h'
    include 'asterfort/lcmmjs.h'
    include 'asterfort/pmavec.h'
    include 'asterfort/utpvlg.h'
    include 'asterfort/vecini.h'
    include 'blas/dcopy.h'
    character(len=16) :: nomfam
    real(kind=8) :: mus(6), n(30, 3), m(30, 3), pgl2(3, 3), ng(3), nl(3), mg(3)
    real(kind=8) :: ml(3)
    real(kind=8) :: sqrt2, sqrt3, q(3, 3), ngr(3), mgr(3), tbsys(30, 6), norn
    real(kind=8) :: norm
    integer :: nbsys, nusys, k, i, j, ir
!     ----------------------------------------------------------------
! TOLE CRP_20
!
    if (nomfam(1:4) .eq. 'UTIL') then
        call lcmmjs(nomfam, nbsys, tbsys)
    endif
!
    if (nusys .eq. 0) then
        if (nomfam .eq. 'BCC24') then
            nbsys=24
            goto 150
!
        else if (nomfam.eq.'OCTAEDRIQUE') then
            nbsys=12
            goto 150
!
        else if (nomfam.eq.'CUBIQUE1') then
            nbsys=12
            goto 150
!
        else if (nomfam.eq.'CUBIQUE2') then
            nbsys=12
            goto 150
!
        else if (nomfam.eq.'ZIRCONIUM') then
            nbsys=30
            goto 150
!
        else if (nomfam.eq.'UNIAXIAL') then
            nbsys=1
            goto 150
!
        else if (nomfam(1:4).eq.'UTIL') then
            goto 150
!
        else
            call assert(.false.)
        endif
    endif
!
    sqrt2=sqrt(2.d0)
    sqrt3=sqrt(3.d0)
    call vecini(6, 0.d0, mus)
    if (nomfam .eq. 'ZIRCONIUM') then
!  prism1
        n(1,1)=1.0d0
        n(1,2)=0.0d0
        n(1,3)=0.0d0
        n(2,1)=-0.5d0
        n(2,2)=0.866025403784D0
        n(2,3)=0.0d0
        n(3,1)=-0.5d0
        n(3,2)=-0.866025403784D0
        n(3,3)=0.0d0
        m(1,1)=0.0d0
        m(1,2)=-1.0d0
        m(1,3)=0.0d0
        m(2,1)=0.866025403784D0
        m(2,2)=0.5d0
        m(2,3)=0.0d0
        m(3,1)=-0.866025403784D0
        m(3,2)=0.5d0
        m(3,3)=0.0d0
!
!  basal1
        n(4,1)=1.0d0
        n(4,2)=0.0d0
        n(4,3)=0.0d0
        n(5,1)=-0.5d0
        n(5,2)=0.866025403784D0
        n(5,3)=0.0d0
        n(6,1)=-0.5d0
        n(6,2)=-0.866025403784D0
        n(6,3)=0.0d0
        m(4,1)=0.0d0
        m(4,2)=0.0d0
        m(4,3)=1.0d0
        m(5,1)=0.0d0
        m(5,2)=0.0d0
        m(5,3)=1.0d0
        m(6,1)=0.0d0
        m(6,2)=0.0d0
        m(6,3)=1.0d0
!
!  pyr_a1
        n(7,1)=1.0d0
        n(7,2)=0.0d0
        n(7,3)=0.0d0
        n(8,1)=1.0d0
        n(8,2)=0.0d0
        n(8,3)=0.0d0
        n(9,1)=-0.5d0
        n(9,2)=0.866025403784D0
        n(9,3)=0.0d0
        n(10,1)=-0.5d0
        n(10,2)=0.866025403784D0
        n(10,3)=0.0d0
        n(11,1)=-0.5d0
        n(11,2)=-0.866025403784D0
        n(11,3)=0.0d0
        n(12,1)=-0.5d0
        n(12,2)=-0.866025403784D0
        n(12,3)=0.0d0
        m(7,1)=0.0d0
        m(7,2)=-0.87856329157D0
        m(7,3)=0.477625944339D0
        m(8,1)=0.0d0
        m(8,2)=0.87856329157D0
        m(8,3)=0.477625944339D0
        m(9,1)=-0.760858129332D0
        m(9,2)=-0.439281645785D0
        m(9,3)=0.477625944339D0
        m(10,1)=0.760858129332D0
        m(10,2)=0.439281645785D0
        m(10,3)=0.477625944339D0
        m(11,1)=-0.760858129332D0
        m(11,2)=0.439281645785D0
        m(11,3)=0.477625944339D0
        m(12,1)=0.760858129332D0
        m(12,2)=-0.439281645785D0
        m(12,3)=0.477625944339D0
!
!  pyr_c_a1
        n(13,1)=0.531670580449D0
        n(13,2)=0.0d0
        n(13,3)=-0.846951234656D0
        n(14,1)=0.265835290225D0
        n(14,2)=0.460440229114D0
        n(14,3)=-0.846951234656D0
        n(15,1)=0.265835290225D0
        n(15,2)=0.460440229114D0
        n(15,3)=-0.846951234656D0
        n(16,1)=-0.265835290225D0
        n(16,2)=0.460440229114D0
        n(16,3)=-0.846951234656D0
        n(17,1)=-0.265835290225D0
        n(17,2)=0.460440229114D0
        n(17,3)=-0.846951234656D0
        n(18,1)=-0.531670580449D0
        n(18,2)=0.0d0
        n(18,3)=-0.846951234656D0
        n(19,1)=-0.531670580449D0
        n(19,2)=0.0d0
        n(19,3)=-0.846951234656D0
        n(20,1)=-0.265835290225D0
        n(20,2)=-0.460440229114D0
        n(20,3)=-0.846951234656D0
        n(21,1)=-0.265835290225D0
        n(21,2)=-0.460440229114D0
        n(21,3)=-0.846951234656D0
        n(22,1)=0.265835290225D0
        n(22,2)=-0.460440229114D0
        n(22,3)=-0.846951234656D0
        n(23,1)=0.265835290225D0
        n(23,2)=-0.460440229114D0
        n(23,3)=-0.846951234656D0
        n(24,1)=0.531670580449D0
        n(24,2)=0.0d0
        n(24,3)=-0.846951234656D0
        m(13,1)=0.760858129332D0
        m(13,2)=0.439281645785D0
        m(13,3)=0.477625944339D0
        m(14,1)=0.760858129332D0
        m(14,2)=0.439281645785D0
        m(14,3)=0.477625944339D0
        m(15,1)=0.0d0
        m(15,2)=0.87856329157D0
        m(15,3)=0.477625944339D0
        m(16,1)=0.0d0
        m(16,2)=0.87856329157D0
        m(16,3)=0.477625944339D0
        m(17,1)=-0.760858129332D0
        m(17,2)=0.439281645785D0
        m(17,3)=0.477625944339D0
        m(18,1)=-0.760858129332D0
        m(18,2)=0.439281645785D0
        m(18,3)=0.477625944339D0
        m(19,1)=-0.760858129332D0
        m(19,2)=-0.439281645785D0
        m(19,3)=0.477625944339D0
        m(20,1)=-0.760858129332D0
        m(20,2)=-0.439281645785D0
        m(20,3)=0.477625944339D0
        m(21,1)=0.0d0
        m(21,2)=-0.87856329157D0
        m(21,3)=0.477625944339D0
        m(22,1)=0.0d0
        m(22,2)=-0.87856329157D0
        m(22,3)=0.477625944339D0
        m(23,1)=0.760858129332D0
        m(23,2)=-0.439281645785D0
        m(23,3)=0.477625944339D0
        m(24,1)=0.760858129332D0
        m(24,2)=-0.439281645785D0
        m(24,3)=0.477625944339D0
!
!  pyr2_c_a1
        n(25,1)=-0.265835290225D0
        n(25,2)=-0.460440229114D0
        n(25,3)=0.846951234656D0
        n(26,1)=0.531670580449D0
        n(26,2)=0.0d0
        n(26,3)=0.846951234656D0
        n(27,1)=-0.265835290225D0
        n(27,2)=0.460440229114D0
        n(27,3)=0.846951234656D0
        n(28,1)=0.265835290225D0
        n(28,2)=0.460440229114D0
        n(28,3)=0.846951234656D0
        n(29,1)=-0.531670580449D0
        n(29,2)=0.0d0
        n(29,3)=0.846951234656D0
        n(30,1)=0.265835290225D0
        n(30,2)=-0.460440229114D0
        n(30,3)=0.846951234656D0
        m(25,1)=0.423475617328D0
        m(25,2)=0.733481284978D0
        m(25,3)=0.531670580449D0
        m(26,1)=-0.846951234656D0
        m(26,2)=0.0d0
        m(26,3)=0.531670580449D0
        m(27,1)=0.423475617328D0
        m(27,2)=-0.733481284978D0
        m(27,3)=0.531670580449D0
        m(28,1)=-0.423475617328D0
        m(28,2)=-0.733481284978D0
        m(28,3)=0.531670580449D0
        m(29,1)=0.846951234656D0
        m(29,2)=0.0d0
        m(29,3)=0.531670580449D0
        m(30,1)=-0.423475617328D0
        m(30,2)=0.733481284978D0
        m(30,3)=0.531670580449D0
!
!        N ET L SONT UNITAIRES
    else if (nomfam.eq.'OCTAEDRIQUE') then
! FCC LATTICE
        n(1,1)=1.d0
        n(1,2)=1.d0
        n(1,3)=1.d0
        n(2,1)=1.d0
        n(2,2)=1.d0
        n(2,3)=1.d0
        n(3,1)=1.d0
        n(3,2)=1.d0
        n(3,3)=1.d0
        n(4,1)=1.d0
        n(4,2)=-1.d0
        n(4,3)=1.d0
        n(5,1)=1.d0
        n(5,2)=-1.d0
        n(5,3)=1.d0
        n(6,1)=1.d0
        n(6,2)=-1.d0
        n(6,3)=1.d0
        n(7,1)=-1.d0
        n(7,2)=1.d0
        n(7,3)=1.d0
        n(8,1)=-1.d0
        n(8,2)=1.d0
        n(8,3)=1.d0
        n(9,1)=-1.d0
        n(9,2)=1.d0
        n(9,3)=1.d0
        n(10,1)=-1.d0
        n(10,2)=-1.d0
        n(10,3)=1.d0
        n(11,1)=-1.d0
        n(11,2)=-1.d0
        n(11,3)=1.d0
        n(12,1)=-1.d0
        n(12,2)=-1.d0
        n(12,3)=1.d0
        m(1,1)=-1.d0
        m(1,2)=0.d0
        m(1,3)=1.d0
        m(2,1)=0.d0
        m(2,2)=-1.d0
        m(2,3)=1.d0
        m(3,1)=-1.d0
        m(3,2)=1.d0
        m(3,3)=0.d0
        m(4,1)=-1.d0
        m(4,2)=0.d0
        m(4,3)=1.d0
        m(5,1)=0.d0
        m(5,2)=1.d0
        m(5,3)=1.d0
        m(6,1)=1.d0
        m(6,2)=1.d0
        m(6,3)=0.d0
        m(7,1)=0.d0
        m(7,2)=-1.d0
        m(7,3)=1.d0
        m(8,1)=1.d0
        m(8,2)=1.d0
        m(8,3)=0.d0
        m(9,1)=1.d0
        m(9,2)=0.d0
        m(9,3)=1.d0
        m(10,1)=-1.d0
        m(10,2)=1.d0
        m(10,3)=0.d0
        m(11,1)=1.d0
        m(11,2)=0.d0
        m(11,3)=1.d0
        m(12,1)=0.d0
        m(12,2)=1.d0
        m(12,3)=1.d0
!        N ET L DOIVENT ETRE UNITAIRES
        do 20 j = 1, 12
            do 10 k = 1, 3
                m(j,k)=m(j,k)/sqrt2
                n(j,k)=n(j,k)/sqrt3
10          continue
20      continue
    else if (nomfam.eq.'CUBIQUE1') then
!        BCC LATTICE, {110} SLIP
        n(1,1)=1.d0
        n(1,2)=1.d0
        n(1,3)=0.d0
        n(2,1)=-1.d0
        n(2,2)=0.d0
        n(2,3)=1.d0
        n(3,1)=0.d0
        n(3,2)=-1.d0
        n(3,3)=-1.d0
        n(4,1)=0.d0
        n(4,2)=-1.d0
        n(4,3)=1.d0
        n(5,1)=1.d0
        n(5,2)=0.d0
        n(5,3)=-1.d0
        n(6,1)=-1.d0
        n(6,2)=1.d0
        n(6,3)=0.d0
        n(7,1)=-1.d0
        n(7,2)=-1.d0
        n(7,3)=0.d0
        n(8,1)=1.d0
        n(8,2)=0.d0
        n(8,3)=1.d0
        n(9,1)=0.d0
        n(9,2)=1.d0
        n(9,3)=-1.d0
        n(10,1)=1.d0
        n(10,2)=-1.d0
        n(10,3)=0.d0
        n(11,1)=-1.d0
        n(11,2)=0.d0
        n(11,3)=-1.d0
        n(12,1)=0.d0
        n(12,2)=1.d0
        n(12,3)=1.d0
!
        m(1,1)=1.d0
        m(1,2)=-1.d0
        m(1,3)=1.d0
        m(2,1)=1.d0
        m(2,2)=-1.d0
        m(2,3)=1.d0
        m(3,1)=1.d0
        m(3,2)=-1.d0
        m(3,3)=1.d0
        m(4,1)=1.d0
        m(4,2)=1.d0
        m(4,3)=1.d0
        m(5,1)=1.d0
        m(5,2)=1.d0
        m(5,3)=1.d0
        m(6,1)=1.d0
        m(6,2)=1.d0
        m(6,3)=1.d0
        m(7,1)=-1.d0
        m(7,2)=1.d0
        m(7,3)=1.d0
        m(8,1)=-1.d0
        m(8,2)=1.d0
        m(8,3)=1.d0
        m(9,1)=-1.d0
        m(9,2)=1.d0
        m(9,3)=1.d0
        m(10,1)=1.d0
        m(10,2)=1.d0
        m(10,3)=-1.d0
        m(11,1)=1.d0
        m(11,2)=1.d0
        m(11,3)=-1.d0
        m(12,1)=1.d0
        m(12,2)=1.d0
        m(12,3)=-1.d0
!
!        N ET L DOIVENT ETRE UNITAIRES
        do 40 j = 1, 12
            do 30 k = 1, 3
                m(j,k)=m(j,k)/sqrt3
                n(j,k)=n(j,k)/sqrt2
30          continue
40      continue
    else if (nomfam.eq.'CUBIQUE2') then
!        BCC LATTICE, {211} SLIP
        n(1,1)=2.d0
        n(1,2)=-1.d0
        n(1,3)=1.d0
        n(2,1)=1.d0
        n(2,2)=-2.d0
        n(2,3)=-1.d0
        n(3,1)=1.d0
        n(3,2)=1.d0
        n(3,3)=2.d0
        n(4,1)=2.d0
        n(4,2)=1.d0
        n(4,3)=1.d0
        n(5,1)=1.d0
        n(5,2)=2.d0
        n(5,3)=-1.d0
        n(6,1)=1.d0
        n(6,2)=-1.d0
        n(6,3)=2.d0
        n(7,1)=2.d0
        n(7,2)=1.d0
        n(7,3)=-1.d0
        n(8,1)=1.d0
        n(8,2)=2.d0
        n(8,3)=1.d0
        n(9,1)=1.d0
        n(9,2)=-1.d0
        n(9,3)=-2.d0
        n(10,1)=2.d0
        n(10,2)=-1.d0
        n(10,3)=-1.d0
        n(11,1)=1.d0
        n(11,2)=-2.d0
        n(11,3)=1.d0
        n(12,1)=1.d0
        n(12,2)=1.d0
        n(12,3)=-2.d0
        m(1,1)=1.d0
        m(1,2)=1.d0
        m(1,3)=-1.d0
        m(2,1)=1.d0
        m(2,2)=1.d0
        m(2,3)=-1.d0
        m(3,1)=1.d0
        m(3,2)=1.d0
        m(3,3)=-1.d0
        m(4,1)=1.d0
        m(4,2)=-1.d0
        m(4,3)=-1.d0
        m(5,1)=1.d0
        m(5,2)=-1.d0
        m(5,3)=-1.d0
        m(6,1)=1.d0
        m(6,2)=-1.d0
        m(6,3)=-1.d0
        m(7,1)=1.d0
        m(7,2)=-1.d0
        m(7,3)=1.d0
        m(8,1)=1.d0
        m(8,2)=-1.d0
        m(8,3)=1.d0
        m(9,1)=1.d0
        m(9,2)=-1.d0
        m(9,3)=1.d0
        m(10,1)=1.d0
        m(10,2)=1.d0
        m(10,3)=1.d0
        m(11,1)=1.d0
        m(11,2)=1.d0
        m(11,3)=1.d0
        m(12,1)=1.d0
        m(12,2)=1.d0
        m(12,3)=1.d0
!        N ET L DOIVENT ETRE UNITAIRES
        do 60 j = 1, 12
            do 50 k = 1, 3
                m(j,k)=m(j,k)/sqrt3
                n(j,k)=n(j,k)/sqrt(6.d0)
50          continue
60      continue
    else if (nomfam.eq.'BCC24') then
!        BCC LATTICE, {110} SLIP
        n(1,1)=0.d0
        n(1,2)=1.d0
        n(1,3)=1.d0
        n(2,1)=1.d0
        n(2,2)=0.d0
        n(2,3)=1.d0
        n(3,1)=1.d0
        n(3,2)=-1.d0
        n(3,3)=0.d0
        n(4,1)=0.d0
        n(4,2)=1.d0
        n(4,3)=-1.d0
        n(5,1)=1.d0
        n(5,2)=0.d0
        n(5,3)=1.d0
        n(6,1)=1.d0
        n(6,2)=1.d0
        n(6,3)=0.d0
        n(7,1)=0.d0
        n(7,2)=1.d0
        n(7,3)=1.d0
        n(8,1)=1.d0
        n(8,2)=0.d0
        n(8,3)=-1.d0
        n(9,1)=1.d0
        n(9,2)=1.d0
        n(9,3)=0.d0
        n(10,1)=0.d0
        n(10,2)=1.d0
        n(10,3)=-1.d0
        n(11,1)=1.d0
        n(11,2)=0.d0
        n(11,3)=-1.d0
        n(12,1)=1.d0
        n(12,2)=-1.d0
        n(12,3)=0.d0
        m(1,1)=1.d0
        m(1,2)=1.d0
        m(1,3)=-1.d0
        m(2,1)=1.d0
        m(2,2)=1.d0
        m(2,3)=-1.d0
        m(3,1)=1.d0
        m(3,2)=1.d0
        m(3,3)=-1.d0
        m(4,1)=1.d0
        m(4,2)=-1.d0
        m(4,3)=-1.d0
        m(5,1)=1.d0
        m(5,2)=-1.d0
        m(5,3)=-1.d0
        m(6,1)=1.d0
        m(6,2)=-1.d0
        m(6,3)=-1.d0
        m(7,1)=1.d0
        m(7,2)=-1.d0
        m(7,3)=1.d0
        m(8,1)=1.d0
        m(8,2)=-1.d0
        m(8,3)=1.d0
        m(9,1)=1.d0
        m(9,2)=-1.d0
        m(9,3)=1.d0
        m(10,1)=1.d0
        m(10,2)=1.d0
        m(10,3)=1.d0
        m(11,1)=1.d0
        m(11,2)=1.d0
        m(11,3)=1.d0
        m(12,1)=1.d0
        m(12,2)=1.d0
        m(12,3)=1.d0
        do 80 j = 1, 12
            do 70 k = 1, 3
                m(j,k)=m(j,k)/sqrt3
                n(j,k)=n(j,k)/sqrt2
70          continue
80      continue
!        BCC LATTICE, {211} SLIP
        n(13,1)=2.d0
        n(13,2)=-1.d0
        n(13,3)=1.d0
        n(14,1)=1.d0
        n(14,2)=-2.d0
        n(14,3)=-1.d0
        n(15,1)=1.d0
        n(15,2)=1.d0
        n(15,3)=2.d0
        n(16,1)=2.d0
        n(16,2)=1.d0
        n(16,3)=1.d0
        n(17,1)=1.d0
        n(17,2)=2.d0
        n(17,3)=-1.d0
        n(18,1)=1.d0
        n(18,2)=-1.d0
        n(18,3)=2.d0
        n(19,1)=2.d0
        n(19,2)=1.d0
        n(19,3)=-1.d0
        n(20,1)=1.d0
        n(20,2)=2.d0
        n(20,3)=1.d0
        n(21,1)=1.d0
        n(21,2)=-1.d0
        n(21,3)=-2.d0
        n(22,1)=2.d0
        n(22,2)=-1.d0
        n(22,3)=-1.d0
        n(23,1)=1.d0
        n(23,2)=-2.d0
        n(23,3)=1.d0
        n(24,1)=1.d0
        n(24,2)=1.d0
        n(24,3)=-2.d0
!
        m(13,1)=1.d0
        m(13,2)=1.d0
        m(13,3)=-1.d0
        m(14,1)=1.d0
        m(14,2)=1.d0
        m(14,3)=-1.d0
        m(15,1)=1.d0
        m(15,2)=1.d0
        m(15,3)=-1.d0
        m(16,1)=1.d0
        m(16,2)=-1.d0
        m(16,3)=-1.d0
        m(17,1)=1.d0
        m(17,2)=-1.d0
        m(17,3)=-1.d0
        m(18,1)=1.d0
        m(18,2)=-1.d0
        m(18,3)=-1.d0
        m(19,1)=1.d0
        m(19,2)=-1.d0
        m(19,3)=1.d0
        m(20,1)=1.d0
        m(20,2)=-1.d0
        m(20,3)=1.d0
        m(21,1)=1.d0
        m(21,2)=-1.d0
        m(21,3)=1.d0
        m(22,1)=1.d0
        m(22,2)=1.d0
        m(22,3)=1.d0
        m(23,1)=1.d0
        m(23,2)=1.d0
        m(23,3)=1.d0
        m(24,1)=1.d0
        m(24,2)=1.d0
        m(24,3)=1.d0
!        N ET L DOIVENT ETRE UNITAIRES
        do 100 j = 13, 24
            do 90 k = 1, 3
                m(j,k)=m(j,k)/sqrt3
                n(j,k)=n(j,k)/sqrt(6.d0)
90          continue
100      continue
    else if (nomfam.eq.'UNIAXIAL') then
        n(1,1)=1.d0
        n(1,2)=0.d0
        n(1,3)=0.d0
        m(1,1)=1.d0
        m(1,2)=0.d0
        m(1,3)=0.d0
    else if (nomfam(1:4).eq.'UTIL') then
        do 120 i = 1, nbsys
            norn=sqrt(tbsys(i,1)**2+tbsys(i,2)**2+tbsys(i,3)**2)
            norm=sqrt(tbsys(i,4)**2+tbsys(i,5)**2+tbsys(i,6)**2)
            do 110 j = 1, 3
                n(i,j)=tbsys(i,j)/norn
                m(i,j)=tbsys(i,j+3)/norm
110          continue
120      continue
!
    endif
!     POUR LE SYSTEME K, EXPRESSION DE N ET L DANS REPERE GLOBAL
    k=nusys
    do 130 j = 1, 3
        nl(j)=n(k,j)
        ml(j)=m(k,j)
130  end do
    call utpvlg(1, 3, pgl2, nl, ng)
    call utpvlg(1, 3, pgl2, ml, mg)
!     rotation de reseau
    if (ir .eq. 1) then
        call dcopy(3, ng, 1, ngr, 1)
        call dcopy(3, mg, 1, mgr, 1)
        call pmavec('ZERO', 3, q, ngr, ng)
        call pmavec('ZERO', 3, q, mgr, mg)
    else
        call assert(ir.eq.0)
    endif
    do 140 j = 1, 3
        mus(j)=ng(j)*mg(j)
140  end do
!     SQRT(2) PAR HOMOGENEITE AVEC NMPL3D.
    mus(4)=0.5d0*(ng(1)*mg(2)+ng(2)*mg(1))*sqrt2
    mus(5)=0.5d0*(ng(1)*mg(3)+ng(3)*mg(1))*sqrt2
    mus(6)=0.5d0*(ng(2)*mg(3)+ng(3)*mg(2))*sqrt2
!
150  continue
end subroutine
