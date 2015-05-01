subroutine acedat(motfac, in, npara, sec, exp,&
                  tab, car)
    implicit none
    integer :: in, npara(*)
    character(len=*) :: motfac, sec(*), exp(*), tab(*), car(*)
! ----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!     AFFE_CARA_ELEM
!     INITIALISATION DES PARAMETRES ET DES DATAS
! ----------------------------------------------------------------------
!
!     --- MOT CLE FACTEUR "POUTRE"-------------------------------------
!-----------------------------------------------------------------------
    integer :: i, k, nbobar, nbopou, ncerba, ncerpo, ngenba
    integer :: ngenpo, nrecba, nrecpo, nsecba, nsecpo, ntseba, ntsepo
!
!-----------------------------------------------------------------------
    parameter ( ngenpo = 15 , nrecpo = 6  , ncerpo = 2  )
    parameter ( nsecpo = 2  , ntsepo = 3  , nbopou = 44 )
    character(len=8) :: exppou(nbopou), tabpou(nbopou)
    character(len=8) :: carpou(ngenpo*(nsecpo+1), ntsepo)
    character(len=16) :: secpou(ntsepo)
!
!     --- MOT CLE FACTEUR "BARRE" -------------------------------------
    parameter ( ngenba = 1  , nrecba = 6  , ncerba = 2  )
    parameter ( nsecba = 1  , ntseba = 3  , nbobar = 8  )
    character(len=8) :: expbar(nbobar), tabbar(nbobar), carbar(nrecba, ntseba)
    character(len=16) :: secbar(ntseba)
!
!     --- POUTRE -------------------------------------------------------
    data tabpou /'A1      ','IY1     ','IZ1     ','AY1     ',&
     &             'AZ1     ','EY1     ','EZ1     ','JX1     ',&
     &             'RY1     ','RZ1     ','RT1     ',&
     &             'A2      ','IY2     ','IZ2     ','AY2     ',&
     &             'AZ2     ','EY2     ','EZ2     ','JX2     ',&
     &             'RY2     ','RZ2     ','RT2     ','TVAR    ',&
     &             'HY1     ','HZ1     ','EPY1    ','EPZ1    ',&
     &             'HY2     ','HZ2     ','EPY2    ','EPZ2    ',&
     &             'R1      ','EP1     ','R2      ','EP2     ',&
     &             'TSEC    ','AI1     ','AI2     ','JG1     ',&
     &             'JG2     ','IYR21   ','IYR22   ','IZR21   ',&
     &             'IZR22   '/
    data exppou /'A       ','IY      ','IZ      ','AY      ',&
     &             'AZ      ','EY      ','EZ      ','JX      ',&
     &             'RY      ','RZ      ','RT      ',&
     &             'A       ','IY      ','IZ      ','AY      ',&
     &             'AZ      ','EY      ','EZ      ','JX      ',&
     &             'RY      ','RZ      ','RT      ','TVAR    ',&
     &             'HY      ','HZ      ','EPY     ','EPZ     ',&
     &             'HY      ','HZ      ','EPY     ','EPZ     ',&
     &             'R       ','EP      ','R       ','EP      ',&
     &             'TSEC    ','AI      ','AI      ','JG      ',&
     &             'JG      ','IYR2    ','IYR2    ','IZR2    ',&
     &             'IZR2    '/
    data secpou(1)                     /'GENERALE  '/
    data (carpou(i,1),i=1,ngenpo*(nsecpo+1))/&
     &             'A       ','IY      ','IZ      ','AY      ',&
     &             'AZ      ','EY      ','EZ      ','JX      ',&
     &             'RY      ','RZ      ','RT      ','AI      ',&
     &             'JG      ','IYR2    ','IZR2    ',&
     &             'A1      ','IY1     ','IZ1     ','AY1     ',&
     &             'AZ1     ','EY1     ','EZ1     ','JX1     ',&
     &             'RY1     ','RZ1     ','RT1     ','AI1     ',&
     &             'JG1     ','IYR21   ','IZR21   ',&
     &             'A2      ','IY2     ','IZ2     ','AY2     ',&
     &             'AZ2     ','EY2     ','EZ2     ','JX2     ',&
     &             'RY2     ','RZ2     ','RT2     ','AI2     ',&
     &             'JG2     ','IYR22   ','IZR22   '/
    data secpou(2)                     /'RECTANGLE'/
    data (carpou(i,2),i=1,nrecpo*(nsecpo+1))/&
     &             'H       ','HY      ','HZ      ',&
     &             'EP      ','EPY     ','EPZ     ',&
     &             'H1      ','HY1     ','HZ1     ',&
     &             'EP1     ','EPY1    ','EPZ1    ',&
     &             'H2      ','HY2     ','HZ2     ',&
     &             'EP2     ','EPY2    ','EPZ2    '/
    data secpou(3)                     /'CERCLE    '/
    data (carpou(i,3),i=1,ncerpo*(nsecpo+1))/&
     &             'R       ','EP      ',&
     &             'R1      ','EP1     ',&
     &             'R2      ','EP2     '/
!
!     --- BARRE --------------------------------------------------------
    data  expbar /'A' ,'HY' ,'HZ' ,'EPY' ,'EPZ' ,'R' ,'EP' ,'TSEC'/
    data  tabbar /'A1','HY1','HZ1','EPY1','EPZ1','R1','EP1','TSEC'/
    data  secbar(1)               /'GENERALE'/
    data (carbar(i,1),i=1,ngenba) /'A'/
    data  secbar(2)               /'RECTANGLE'/
    data (carbar(i,2),i=1,nrecba) /'H','HY','HZ','EP','EPY','EPZ'/
    data  secbar(3)               /'CERCLE '/
    data (carbar(i,3),i=1,ncerba) /'R','EP'/
!     ------------------------------------------------------------------
    if (motfac .eq. 'POUTRE') then
        npara(1) = nsecpo
        npara(2) = ntsepo
        npara(3) = nbopou
        npara(4) = nsecpo * ngenpo
        npara(5) = nsecpo * ngenpo
        npara(6) = ngenpo
        npara(7) = nrecpo
        npara(8) = ncerpo
        if (in .eq. 0) goto 9999
        do 10 i = 1, ntsepo
            sec(i) = secpou(i)
10      continue
        do 12 i = 1, nbopou
            exp(i) = exppou(i)
            tab(i) = tabpou(i)
12      continue
        k = 0
        do 14 i = 1, ngenpo*(nsecpo+1)
            k = k + 1
            car(k) = carpou(i,1)
14      continue
        do 16 i = 1, nrecpo*(nsecpo+1)
            k = k + 1
            car(k) = carpou(i,2)
16      continue
        k = 2*ngenpo*(nsecpo+1)
        do 18 i = 1, ncerpo*(nsecpo+1)
            k = k + 1
            car(k) = carpou(i,3)
18      continue
    else if (motfac.eq. 'BARRE') then
        npara(1) = nsecba
        npara(2) = ntseba
        npara(3) = nbobar
        npara(4) = nsecba * nrecba
        npara(5) = nsecba * nrecba
        npara(6) = ngenba
        npara(7) = nrecba
        npara(8) = ncerba
        if (in .eq. 0) goto 9999
        do 20 i = 1, ntseba
            sec(i) = secbar(i)
20      continue
        do 22 i = 1, nbobar
            exp(i) = expbar(i)
            tab(i) = tabbar(i)
22      continue
        k = 0
        do 24 i = 1, ngenba
            k = k + 1
            car(k) = carbar(i,1)
24      continue
        k = nrecba
        do 26 i = 1, nrecba
            k = k + 1
            car(k) = carbar(i,2)
26      continue
        k = 2*nrecba
        do 28 i = 1, ncerba
            k = k + 1
            car(k) = carbar(i,3)
28      continue
    endif
!
9999  continue
end subroutine
