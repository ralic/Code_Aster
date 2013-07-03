subroutine cjsncv(roucjs, nitimp, iter, ndt, nvi,&
                  umess, erimp, epsd, deps, sigd,&
                  vind)
!       ================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!       ----------------------------------------------------------------
!
!  DUMP EN CAS NON CONVERGENCE ITE INTERNES CJS
!
    implicit none
#include "asterfort/u2mess.h"
    character(len=*) :: roucjs
    integer :: nitimp, iter, ndt, nvi, umess
    real(kind=8) :: erimp(nitimp, 3)
    real(kind=8) :: epsd(ndt), deps(ndt), sigd(ndt), vind(nvi)
!
    integer :: i
    write(umess,2001)
    2001   format(&
     &       t3,' ITER',t10,' ERR1=DDY',&
     &       t30,'ERR2=DY',t50,'ERR=DDY/DY')
    do 300 i = 1, min(nitimp, iter)
        write(umess,1000) i,erimp(i,1),erimp(i,2),erimp(i,3)
300  continue
    1000   format(&
     &       t3,i4,t10,e12.5,&
     &       t30,e12.5,t50,e12.5)
    call u2mess('F', 'ALGORITH2_18')
    write(6,1002) (i,epsd(i),i = 1 , ndt)
    write(umess,*) ' DEPS '
    write(6,1002) (i,deps(i),i = 1 , ndt)
    write(umess,*) ' SIGD '
    write(6,1002) (i,sigd(i),i = 1 , ndt)
    write(umess,*) ' VIND '
    write(6,1002) (i,vind(i),i = 1 , nvi)
    1002 format(2x,i5,2x,e12.5)
end subroutine
