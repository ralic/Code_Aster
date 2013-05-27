subroutine elraca(elrefz, ndim, nno, nnos, nbfpg,&
                  fapg, nbpg, x, vol)
    implicit none
    include 'asterfort/u2mesk.h'
    integer :: nbfamx
    parameter (nbfamx=20)
    integer :: ndim, nno, nnos, nbfpg, nbpg(nbfamx)
    real(kind=8) :: x(*), vol
    character(len=8) :: fapg(*)
    character(len=*) :: elrefz
! ----------------------------------------------------------------------
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
! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
! TOLE CRP_20
! BUT :  RETOURNE LES CARACTERISTIQUES DE L'ELREFA
! ----------------------------------------------------------------------
!   IN   ELREFZ : NOM DE L'ELREFA (K8)
!   OUT  NDIM   : DIMENSION TOPOLOGIQUE : 0/1/2/3
!        NNO    : NOMBRE DE NOEUDS
!        NNOS   : NOMBRE DE NOEUDS SOMMETS
!        NBFPG  : NOMBRE DE FAMILLE DE POINTS DE GAUSS
!        FAPG(*): NOMS DES FAMILLES DE POINTS DE GAUSS
!        NBPG(*): NOMBRE DE POINTS DES FAMILLES DE POINTS DE GAUSS
!        X(*)   : COORDONNEES DES NOEUDS DE L'ELREFA
!        VOL    : VOLUME DE L'ELREFA
! COMMENTAIRE POUR LES ROUTINES ELRAGA, INMAT4, INMAT5, INMAT6, JNI002
! ----------------------------------------------------------------------
!        NBPGMX : NOMBRE DE POINTS DE GAUSS MAX DE L'ELEMENT
!                 NBPGMX=1000 DU A XFEM
!        NBNOMX : NOMBRE DE NOEUDS MAX DE L'ELEMENT
!        NBFAMX : NOMBRE DE FAMILLES DE POINTS DE GAUSS MAX DE L'ELEMENT
!
!   -------------------------------------------------------------------
    character(len=8) :: elrefa
    integer :: i, j
    real(kind=8) :: xin(27), yin(27), zin(27)
! DEB ------------------------------------------------------------------
!
    elrefa = elrefz
!     ------------------------------------------------------------------
! LES ELEMENTS 3D
!     ------------------------------------------------------------------
!
    if (elrefa .eq. 'HE8') then
        ndim = 3
        nno = 8
        nnos = 8
        vol = 8.d0
!
        nbfpg = 7
!
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 8
        nbpg(5) = 27
        nbpg(6) = 5
        nbpg(7) = 16
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG8'
        fapg(5) = 'FPG27'
        fapg(6) = 'SHB5'
        fapg(7) = 'FPG8NOS'
!
        do 10 i = 1, 8
            xin(i) = -1.d0
            yin(i) = -1.d0
            zin(i) = -1.d0
10      continue
        do 12 i = 1, 4
            zin(i+4) = +1.d0
12      continue
        do 14 i = 1, 2
            xin(i+1) = +1.d0
            xin(i+5) = +1.d0
            yin(i+2) = +1.d0
            yin(i+6) = +1.d0
14      continue
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'H20') then
        ndim = 3
        nno = 20
        nnos = 8
        vol = 8.d0
!
        nbfpg = 7
!
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 8
        nbpg(5) = 27
        nbpg(6) = 16
        nbpg(7) = 20
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG8'
        fapg(5) = 'FPG27'
        fapg(6) = 'FPG8NOS'
        fapg(7) = 'SHB20'
!
        do 20 i = 1, 20
            xin(i) = -1.d0
            yin(i) = -1.d0
            zin(i) = -1.d0
20      continue
!
        xin(2) = +1.d0
        xin(3) = +1.d0
        yin(3) = +1.d0
        yin(4) = +1.d0
!
        zin(5) = +1.d0
        xin(6) = +1.d0
        zin(6) = +1.d0
        xin(7) = +1.d0
        yin(7) = +1.d0
        zin(7) = +1.d0
        yin(8) = +1.d0
        zin(8) = +1.d0
!
        xin(9) = 0.d0
        xin(10) = +1.d0
        yin(10) = 0.d0
        xin(11) = 0.d0
        yin(11) = +1.d0
        yin(12) = 0.d0
!
        zin(13) = 0.d0
        xin(14) = +1.d0
        zin(14) = 0.d0
        xin(15) = +1.d0
        yin(15) = +1.d0
        zin(15) = 0.d0
        yin(16) = +1.d0
        zin(16) = 0.d0
!
        xin(17) = 0.d0
        zin(17) = +1.d0
        xin(18) = +1.d0
        yin(18) = 0.d0
        zin(18) = +1.d0
        xin(19) = 0.d0
        yin(19) = +1.d0
        zin(19) = +1.d0
        yin(20) = 0.d0
        zin(20) = +1.d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'H27') then
        ndim = 3
        nno = 27
        nnos = 8
        vol = 8.d0
!
        nbfpg = 5
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 8
        nbpg(5) = 27
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG8'
        fapg(5) = 'FPG27'
!
        do 30 i = 1, 20
            xin(i) = -1.d0
            yin(i) = -1.d0
            zin(i) = -1.d0
30      continue
!
!   NOEUDS SOMMETS
!
        xin(2) = +1.d0
        xin(3) = +1.d0
        yin(3) = +1.d0
        yin(4) = +1.d0
!
        zin(5) = +1.d0
        xin(6) = +1.d0
        zin(6) = +1.d0
        xin(7) = +1.d0
        yin(7) = +1.d0
        zin(7) = +1.d0
        yin(8) = +1.d0
        zin(8) = +1.d0
!
!   NOEUDS MILIEUX DES ARETES
!
        xin(9) = 0.d0
        xin(10) = +1.d0
        yin(10) = 0.d0
        xin(11) = 0.d0
        yin(11) = +1.d0
        yin(12) = 0.d0
!
        zin(13) = 0.d0
        xin(14) = +1.d0
        zin(14) = 0.d0
        xin(15) = +1.d0
        yin(15) = +1.d0
        zin(15) = 0.d0
        yin(16) = +1.d0
        zin(16) = 0.d0
!
        xin(17) = 0.d0
        zin(17) = +1.d0
        xin(18) = +1.d0
        yin(18) = 0.d0
        zin(18) = +1.d0
        xin(19) = 0.d0
        yin(19) = +1.d0
        zin(19) = +1.d0
        yin(20) = 0.d0
        zin(20) = +1.d0
!
!    NOEUDS MILIEUX DES FACES ET BARYCENTRE
!
        do 32 i = 21, 27
            xin(i) = 0.d0
            yin(i) = 0.d0
            zin(i) = 0.d0
32      continue
        zin(21) = -1.d0
        yin(22) = -1.d0
        xin(23) = +1.d0
        yin(24) = +1.d0
        xin(25) = -1.d0
        zin(26) = +1.d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'TE4') then
        ndim = 3
        nno = 4
        nnos = 4
        vol = 1.d0/6.d0
!
        nbfpg = 7
!
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 4
        nbpg(5) = 5
        nbpg(6) = 15
        nbpg(7) = 8
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG4'
        fapg(5) = 'FPG5'
        fapg(6) = 'FPG15'
        fapg(7) = 'FPG4NOS'
!
        do 40 i = 1, 4
            xin(i) = 0.d0
            yin(i) = 0.d0
            zin(i) = 0.d0
40      continue
        yin(1) = +1.d0
        zin(2) = +1.d0
        xin(4) = +1.d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'T10') then
        ndim = 3
        nno = 10
        nnos = 4
        vol = 1.d0/6.d0
!
        nbfpg = 7
!
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 4
        nbpg(5) = 5
        nbpg(6) = 15
        nbpg(7) = 8
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG4'
        fapg(5) = 'FPG5'
        fapg(6) = 'FPG15'
        fapg(7) = 'FPG4NOS'
!
        do 50 i = 1, 10
            xin(i) = 0.d0
            yin(i) = 0.d0
            zin(i) = 0.d0
50      continue
        xin(4) = +1.d0
        yin(1) = +1.d0
        zin(2) = +1.d0
        xin(8) = +0.5d0
        xin(9) = +0.5d0
        xin(10) = +0.5d0
        yin(5) = +0.5d0
        yin(7) = +0.5d0
        yin(8) = +0.5d0
        zin(5) = +0.5d0
        zin(6) = +0.5d0
        zin(9) = +0.5d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'PE6') then
        ndim = 3
        nno = 6
        nnos = 6
        vol = 1.d0
!
        nbfpg = 9
!
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 6
        nbpg(5) = 6
        nbpg(6) = 8
        nbpg(7) = 21
        nbpg(8) = 12
        nbpg(9) = 5
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG6'
        fapg(5) = 'FPG6B'
        fapg(6) = 'FPG8'
        fapg(7) = 'FPG21'
        fapg(8) = 'FPG6NOS'
        fapg(9) = 'SHB6'
!
        do 60 i = 1, 6
            yin(i) = 0.d0
            zin(i) = 0.d0
60      continue
        do 62 i = 1, 3
            xin(i ) = -1.d0
            xin(i+3) = 1.d0
62      continue
        yin(1) = +1.d0
        yin(4) = +1.d0
        zin(2) = +1.d0
        zin(5) = +1.d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'P15') then
        ndim = 3
        nno = 15
        nnos = 6
        vol = 1.d0
!
        nbfpg = 8
!
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 6
        nbpg(5) = 8
        nbpg(6) = 21
        nbpg(7) = 12
        nbpg(8) = 15
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG6'
        fapg(5) = 'FPG8'
        fapg(6) = 'FPG21'
        fapg(7) = 'FPG6NOS'
        fapg(8) = 'SHB15'
!
        do 70 i = 1, 15
            xin(i) = 0.d0
            yin(i) = 0.d0
            zin(i) = 0.d0
70      continue
        do 72 i = 1, 3
            xin(i) = -1.d0
            xin(i+6) = -1.d0
            xin(i+3) = +1.d0
            xin(i+12) = +1.d0
72      continue
        yin(1) = +1.d0
        yin(4) = +1.d0
        yin(10) = +1.d0
        zin(2) = +1.d0
        zin(5) = +1.d0
        zin(11) = +1.d0
        do 74 i = 1, 2
            do 76 j = 1, 2
                yin(6*i+2*j-1) = +0.5d0
                zin(6*i+j) = +0.5d0
76          continue
74      continue
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'P18') then
        ndim = 3
        nno = 18
        nnos = 6
        vol = 1.d0
!
        nbfpg = 7
!
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 6
        nbpg(5) = 8
        nbpg(6) = 21
        nbpg(7) = 12
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG6'
        fapg(5) = 'FPG8'
        fapg(6) = 'FPG21'
        fapg(7) = 'FPG6NOS'
!
        do 80 i = 1, 18
            xin(i) = 0.d0
            yin(i) = 0.d0
            zin(i) = 0.d0
80      continue
        do 82 i = 1, 3
            xin(i) = -1.d0
            xin(i+6) = -1.d0
            xin(i+3) = +1.d0
            xin(i+12) = +1.d0
82      continue
        yin(1) = +1.d0
        yin(4) = +1.d0
        yin(10) = +1.d0
        zin(2) = +1.d0
        zin(5) = +1.d0
        zin(11) = +1.d0
        do 84 i = 1, 2
            do 86 j = 1, 2
                yin(6*i+2*j-1) = +0.5d0
                zin(6*i+j) = +0.5d0
86          continue
84      continue
        do 87 j = 1, 2
            yin(15+2*j-1) = +0.5d0
            zin(15+j) = +0.5d0
87      continue
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'PY5') then
        ndim = 3
        nno = 5
        nnos = 5
        vol = 2.d0/3.d0
!
        nbfpg = 7
!
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 5
        nbpg(5) = 6
        nbpg(6) = 27
        nbpg(7) = 10
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG5'
        fapg(5) = 'FPG6'
        fapg(6) = 'FPG27'
        fapg(7) = 'FPG5NOS'
!
        xin(1) = +1.d0
        yin(1) = 0.d0
        zin(1) = 0.d0
!
        xin(2) = 0.d0
        yin(2) = +1.d0
        zin(2) = 0.d0
!
        xin(3) = -1.d0
        yin(3) = 0.d0
        zin(3) = 0.d0
!
        xin(4) = 0.d0
        yin(4) = -1.d0
        zin(4) = 0.d0
!
        xin(5) = 0.d0
        yin(5) = 0.d0
        zin(5) = +1.d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'P13') then
        ndim = 3
        nno = 13
        nnos = 5
        vol = 2.d0/3.d0
!
        nbfpg = 7
!
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 5
        nbpg(5) = 6
        nbpg(6) = 27
        nbpg(7) = 10
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG5'
        fapg(5) = 'FPG6'
        fapg(6) = 'FPG27'
        fapg(7) = 'FPG5NOS'
!
        xin(1) = +1.d0
        yin(1) = 0.d0
        zin(1) = 0.d0
!
        xin(2) = 0.d0
        yin(2) = +1.d0
        zin(2) = 0.d0
!
        xin(3) = -1.d0
        yin(3) = 0.d0
        zin(3) = 0.d0
!
        xin(4) = 0.d0
        yin(4) = -1.d0
        zin(4) = 0.d0
!
        xin(5) = 0.d0
        yin(5) = 0.d0
        zin(5) = +1.d0
!
        xin(6) = +0.5d0
        yin(6) = +0.5d0
        zin(6) = 0.d0
!
        xin(7) = -0.5d0
        yin(7) = +0.5d0
        zin(7) = 0.d0
!
        xin(8) = -0.5d0
        yin(8) = -0.5d0
        zin(8) = 0.d0
!
        xin(9) = +0.5d0
        yin(9) = -0.5d0
        zin(9) = 0.d0
!
        xin(10) = +0.5d0
        yin(10) = 0.d0
        zin(10) = +0.5d0
!
        xin(11) = 0.d0
        yin(11) = +0.5d0
        zin(11) = +0.5d0
!
        xin(12) = -0.5d0
        yin(12) = 0.d0
        zin(12) = +0.5d0
!
        xin(13) = 0.d0
        yin(13) = -0.5d0
        zin(13) = +0.5d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'TR3') then
        ndim = 2
        nno = 3
        nnos = 3
        vol = 1.d0/2.d0
!
        nbfpg = 13
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 3
        nbpg(5) = 4
        nbpg(6) = 6
        nbpg(7) = 7
        nbpg(8) = 12
        nbpg(9) = 3
        nbpg(10) = 6
        nbpg(11) = 13
        nbpg(12) = 16
        nbpg(13) = 6
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG3'
        fapg(5) = 'FPG4'
        fapg(6) = 'FPG6'
        fapg(7) = 'FPG7'
        fapg(8) = 'FPG12'
        fapg(9) = 'COT3'
        fapg(10) = 'FPG3NOS'
        fapg(11) = 'FPG13'
        fapg(12) = 'FPG16'
        fapg(13) = 'SIMP'
!
        xin(1) = 0.d0
        yin(1) = 0.d0
!
        xin(2) = +1.d0
        yin(2) = 0.d0
!
        xin(3) = 0.d0
        yin(3) = +1.d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'TR6') then
        ndim = 2
        nno = 6
        nnos = 3
        vol = 1.d0/2.d0
!
        nbfpg = 11
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 3
        nbpg(5) = 4
        nbpg(6) = 6
        nbpg(7) = 7
        nbpg(8) = 12
        nbpg(9) = 6
        nbpg(10) = 13
        nbpg(11) = 16
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG3'
        fapg(5) = 'FPG4'
        fapg(6) = 'FPG6'
        fapg(7) = 'FPG7'
        fapg(8) = 'FPG12'
        fapg(9) = 'FPG3NOS'
        fapg(10) = 'FPG13'
        fapg(11) = 'FPG16'
!
        xin(1) = 0.d0
        yin(1) = 0.d0
!
        xin(2) = +1.d0
        yin(2) = 0.d0
!
        xin(3) = 0.d0
        yin(3) = +1.d0
!
        xin(4) = 0.5d0
        yin(4) = 0.d0
!
        xin(5) = 0.5d0
        yin(5) = 0.5d0
!
        xin(6) = 0.d0
        yin(6) = 0.5d0
!
!     -------------------------------------------------------
    else if (elrefa.eq.'TR7') then
        ndim = 2
        nno = 7
        nnos = 3
        vol = 1.d0/2.d0
!
        nbfpg = 10
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 3
        nbpg(5) = 4
        nbpg(6) = 6
        nbpg(7) = 7
        nbpg(8) = 12
        nbpg(9) = 13
        nbpg(10) = 16
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG3'
        fapg(5) = 'FPG4'
        fapg(6) = 'FPG6'
        fapg(7) = 'FPG7'
        fapg(8) = 'FPG12'
        fapg(9) = 'FPG13'
        fapg(10) = 'FPG16'
!
        xin(1) = 0.d0
        yin(1) = 0.d0
!
        xin(2) = +1.d0
        yin(2) = 0.d0
!
        xin(3) = 0.d0
        yin(3) = +1.d0
!
        xin(4) = 0.5d0
        yin(4) = 0.d0
!
        xin(5) = 0.5d0
        yin(5) = 0.5d0
!
        xin(6) = 0.d0
        yin(6) = 0.5d0
!
        xin(7) = 1.0d0/3.0d0
        yin(7) = 1.0d0/3.0d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'QU4') then
        ndim = 2
        nno = 4
        nnos = 4
        vol = 4.d0
!
        nbfpg = 8
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 4
        nbpg(5) = 9
        nbpg(6) = 16
        nbpg(7) = 2
        nbpg(8) = 8
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG4'
        fapg(5) = 'FPG9'
        fapg(6) = 'FPG16'
        fapg(7) = 'FIS2'
        fapg(8) = 'FPG4NOS'
!
        xin(1) = -1.d0
        yin(1) = -1.d0
!
        xin(2) = +1.d0
        yin(2) = -1.d0
!
        xin(3) = +1.d0
        yin(3) = +1.d0
!
        xin(4) = -1.d0
        yin(4) = +1.d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'QU8') then
        ndim = 2
        nno = 8
        nnos = 4
        vol = 4.d0
!
        nbfpg = 6
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 4
        nbpg(5) = 9
        nbpg(6) = 8
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG4'
        fapg(5) = 'FPG9'
        fapg(6) = 'FPG4NOS'
!
        xin(1) = -1.d0
        yin(1) = -1.d0
!
        xin(2) = +1.d0
        yin(2) = -1.d0
!
        xin(3) = +1.d0
        yin(3) = +1.d0
!
        xin(4) = -1.d0
        yin(4) = +1.d0
!
        xin(5) = 0.d0
        yin(5) = -1.d0
!
        xin(6) = +1.d0
        yin(6) = +0.d0
!
        xin(7) = 0.d0
        yin(7) = +1.d0
!
        xin(8) = -1.d0
        yin(8) = 0.d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'QU9') then
        ndim = 2
        nno = 9
        nnos = 4
        vol = 4.d0
!
        nbfpg = 5
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 4
        nbpg(5) = 9
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG4'
        fapg(5) = 'FPG9'
!
        xin(1) = -1.d0
        yin(1) = -1.d0
!
        xin(2) = +1.d0
        yin(2) = -1.d0
!
        xin(3) = +1.d0
        yin(3) = +1.d0
!
        xin(4) = -1.d0
        yin(4) = +1.d0
!
        xin(5) = 0.d0
        yin(5) = -1.d0
!
        xin(6) = +1.d0
        yin(6) = +0.d0
!
        xin(7) = 0.d0
        yin(7) = +1.d0
!
        xin(8) = -1.d0
        yin(8) = +0.d0
!
        xin(9) = 0.d0
        yin(9) = 0.d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'PO1') then
        ndim = 0
        nno = 1
        nnos = 1
        vol = 1.d0
        xin(1) = 0.d0
!
        nbfpg = 3
        nbpg(1) = 1
        nbpg(2) = 1
        nbpg(3) = 1
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'SE2') then
        ndim = 1
        nno = 2
        nnos = 2
        vol = 2.d0
!
        nbfpg = 13
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 2
        nbpg(5) = 3
        nbpg(6) = 4
        nbpg(7) = 3
        nbpg(8) = 5
        nbpg(9) = 4
        nbpg(10) = 5
        nbpg(11) = 10
        nbpg(12) = nnos + 2
        nbpg(13) = nnos+3
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG2'
        fapg(5) = 'FPG3'
        fapg(6) = 'FPG4'
        fapg(7) = 'SIMP'
        fapg(8) = 'SIMP1'
        fapg(9) = 'COTES'
        fapg(10) = 'COTES1'
        fapg(11) = 'COTES2'
        fapg(12) = 'FPG2NOS'
        fapg(13) = 'FPG3NOS'
!
        xin(1) = -1.d0
        xin(2) = +1.d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'SE3') then
        ndim = 1
        nno = 3
        nnos = 2
        vol = 2.d0
!
        nbfpg = 10
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 2
        nbpg(5) = 3
        nbpg(6) = 4
        nbpg(7) = 3
        nbpg(8) = 4
        nbpg(9) = nnos+2
        nbpg(10) = nnos+3
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG2'
        fapg(5) = 'FPG3'
        fapg(6) = 'FPG4'
        fapg(7) = 'SIMP'
        fapg(8) = 'COTES'
        fapg(9) = 'FPG2NOS'
        fapg(10) = 'FPG3NOS'
!
        xin(1) = -1.d0
        xin(2) = +1.d0
        xin(3) = 0.d0
!
!     ------------------------------------------------------------------
    else if (elrefa.eq.'SE4') then
        ndim = 1
        nno = 4
        nnos = 2
        vol = 2.d0
!
        nbfpg = 8
        nbpg(1) = nno
        nbpg(2) = nnos
        nbpg(3) = 1
        nbpg(4) = 2
        nbpg(5) = 3
        nbpg(6) = 4
        nbpg(7) = 3
        nbpg(8) = 4
!
        fapg(1) = 'NOEU'
        fapg(2) = 'NOEU_S'
        fapg(3) = 'FPG1'
        fapg(4) = 'FPG2'
        fapg(5) = 'FPG3'
        fapg(6) = 'FPG4'
        fapg(7) = 'SIMP'
        fapg(8) = 'COTES'
!
        xin(1) = -1.d0
        xin(2) = +1.d0
        xin(3) = -1.d0/3.d0
        xin(4) = +1.d0/3.d0
!
!
!     ------------------------------------------------------------------
    else
        call u2mesk('F', 'ELEMENTS_55', 1, elrefa)
    endif
!
    do 200 i = 0, nno - 1
        if (ndim .ge. 1) x(ndim*i+1) = xin(i+1)
        if (ndim .ge. 2) x(ndim*i+2) = yin(i+1)
        if (ndim .eq. 3) x(ndim*i+3) = zin(i+1)
200  end do
end subroutine
