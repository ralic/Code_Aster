subroutine permea(imate, hydr, phi, t, sat,&
                  ncon, cond, aniso)
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
!   COND(1) :  KINT
!   COND(2) :  KINT_L (DANS LE CAS ISOTROPE TRANSVERSE)
!   COND(3) :  KINT_N (DANS LE CAS ISOTROPE TRANSVERSE)
!   COND(4) :  PERMLIQ
!   COND(5) :  D(PERMLIQ)/DSAT
!   COND(6) :  PERMGAZ
!   COND(7) :  D(PERMGAZ)/DSAT
!   COND(8) :  D(PERMGAZ)/DPGAZ
!   COND(9) :  FICK
!   COND(10) : D(FICK)/DTEMP
!   COND(11) : D(FICK)/DPGAZ
!   COND(12) : CONDSOLI = CONDUCTIVITE THERMIQUE SOLIDE
!   COND(13) : D(CONDSOLI)/DTEMP
!   COND(14) : CONDLIQ= CONDUCTIVITE THERMIQUE LIQUIDE
!   COND(15) : D(CONDLIQ)/DTEMP
!   COND(16) : CONDGAZ= CONDUCTIVITE THERMIQUE GAZ
!   COND(17) : D(CONDGAZ)/DTEMP
!
    implicit none
!
#include "asterfort/rcvala.h"
#include "asterfort/utmess.h"
    integer :: imate, ncon, nc, nc1
    integer :: aniso
    parameter      (nc = 1)
    parameter      (nc1= 3)
    real(kind=8) :: t, phi, valpar(2), sat
    character(len=16) :: hydr
    character(len=8) :: nompar(2)
    real(kind=8) :: cond(ncon), conred(nc), conred1(nc1)
!
    integer :: icodre(nc), icodre1(nc1)
    character(len=8) :: ncra1(nc1), ncra2(nc), ncra3(nc), ncra4(nc)
    data ncra1 / 'PERM_IN','PERMIN_L','PERMIN_N' /
!     LAMBDA SOLIDE :
    data ncra2 / 'LAMBDA' /
!     LAMBDA LIQUIDE :
    data ncra3 / 'LAMBDA' /
!     LAMBDA GAZ :
    data ncra4 / 'LAMBDA' /
!
    if (hydr .eq. 'HYDR') then
!
        cond(1) = 0.d0
        cond(2) = 0.d0
        cond(3) = 0.d0
!
        call utmess('F', 'ALGORITH9_80')
!
        nompar(1)='PORO'
        nompar(2)='TEMP'
        valpar(1)=phi
        valpar(2)=t
        call rcvala(imate, ' ', 'THM_DIFFU', 2, nompar,&
                    valpar, 1, ncra1, conred1, icodre1,&
                    1)
        if (icodre1(1) .eq. 1) then
            aniso=1
            call rcvala(imate, ' ', 'THM_DIFFU', 2, nompar,&
                        valpar, 2, ncra1(2), conred1(2), icodre1,&
                        1)
        else if (icodre1(1).eq.0) then
            aniso=0
        endif
        cond(1)=conred1(1)
        cond(2)=conred1(2)
        cond(3)=conred1(3)
        cond(4)=sat
        cond(5)=1.d0
        cond(6)=1.d0-sat
        cond(7)=-1.d0
        cond(8)=0.d0
        cond(9)=1.d-7
        cond(10)=0.d0
        cond(11)=0.d0
        call rcvala(imate, ' ', 'THM_DIFFU', 2, nompar,&
                    valpar, nc, ncra2, conred, icodre,&
                    1)
        cond(12)=conred(1)
        cond(13)=0.d0
        call rcvala(imate, ' ', 'THM_LIQU', 2, nompar,&
                    valpar, nc, ncra3, conred, icodre,&
                    1)
        cond(14)=conred(1)
        cond(15)=0.d0
        call rcvala(imate, ' ', 'THM_GAZ', 2, nompar,&
                    valpar, nc, ncra4, conred, icodre,&
                    1)
        cond(16)=conred(1)
        cond(17)=0.d0
    endif
!
end subroutine
