subroutine permea(imate, hydr, phi, t, sat,&
                  ncon, cond)
!
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
! ARGUMENTS DE SORTIE
!   COND(1) : KINT
!   COND(2) : PERMLIQ
!   COND(3) : D(PERMLIQ)/DSAT
!   COND(4) : PERMGAZ
!   COND(5) : D(PERMGAZ)/DSAT
!   COND(6) : D(PERMGAZ)/DPGAZ
!   COND(7) : FICK
!   COND(8) : D(FICK)/DTEMP
!   COND(9) : D(FICK)/DPGAZ
!   COND(10) : CONDSOLI = CONDUCTIVITE THERMIQUE SOLIDE
!   COND(11) : D(CONDSOLI)/DTEMP
!   COND(12) : CONDLIQ= CONDUCTIVITE THERMIQUE LIQUIDE
!   COND(13) : D(CONDLIQ)/DTEMP
!   COND(14) : CONDGAZ= CONDUCTIVITE THERMIQUE GAZ
!   COND(15) : D(CONDGAZ)/DTEMP
!
    implicit none
!
    include 'asterfort/rcvala.h'
    include 'asterfort/u2mess.h'
    integer :: imate, ncon, nc
    parameter     ( nc = 1)
    real(kind=8) :: t, phi, valpar(2), sat
    character(len=16) :: hydr
    character(len=8) :: nompar(2)
    real(kind=8) :: cond(ncon), conred(nc)
!
    integer :: icodre(nc)
    character(len=8) :: ncra1(nc), ncra2(nc), ncra3(nc), ncra4(nc)
    data ncra1 / 'PERM_IN' /
!     LAMBDA SOLIDE :
    data ncra2 / 'LAMBDA' /
!     LAMBDA LIQUIDE :
    data ncra3 / 'LAMBDA' /
!     LAMBDA GAZ :
    data ncra4 / 'LAMBDA' /
!
    if (hydr .eq. 'HYDR') then
!
        call u2mess('F', 'ALGORITH9_80')
!
        nompar(1)='PORO'
        nompar(2)='TEMP'
        valpar(1)=phi
        valpar(2)=t
        call rcvala(imate, ' ', 'THM_DIFFU', 2, nompar,&
                    valpar, nc, ncra1, conred, icodre,&
                    1)
        cond(1)=conred(1)
        cond(2)=sat
        cond(3)=1.d0
        cond(4)=1.d0-sat
        cond(5)=-1.d0
        cond(6)=0.d0
        cond(7)=1.d-7
        cond(8)=0.d0
        cond(9)=0.d0
        call rcvala(imate, ' ', 'THM_DIFFU', 2, nompar,&
                    valpar, nc, ncra2, conred, icodre,&
                    1)
        cond(10)=conred(1)
        cond(11)=0.d0
        call rcvala(imate, ' ', 'THM_LIQU', 2, nompar,&
                    valpar, nc, ncra3, conred, icodre,&
                    1)
        cond(12)=conred(1)
        cond(13)=0.d0
        call rcvala(imate, ' ', 'THM_GAZ', 2, nompar,&
                    valpar, nc, ncra4, conred, icodre,&
                    1)
        cond(14)=conred(1)
        cond(15)=0.d0
    endif
!
end subroutine
