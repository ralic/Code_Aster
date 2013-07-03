subroutine brdsde(e0, nu0, dsidep, vim, sigm)
!
!    ROUTINE ANCIENNEMENT NOMMEE DSIDEPRG
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2009  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/brchre.h"
#include "asterfort/brksec.h"
#include "asterfort/brvp33.h"
#include "asterfort/transp.h"
#include "asterfort/utbtab.h"
    real(kind=8) :: e0, nu0
    real(kind=8) :: vim(65), sigm(6)
    real(kind=8) :: dsidep(6, 6), h66(6, 6)
    real(kind=8) :: s33(3, 3), sn3(3)
    real(kind=8) :: x33(3, 3), vbt33(3, 3), vbt33t(3, 3)
    real(kind=8) :: bt33(3, 3), bc0, bt3(3), trav(3, 3)
    integer :: i
!
!
!      PRINT *,'JE PASSE DANS MAT TANG'
!       RANGEMENT DES ENDOMMAGEMENTS EN TABLEAU 3*3
!
    s33(1,1)=sigm(1)
    s33(2,2)=sigm(2)
    s33(3,3)=sigm(3)
    s33(1,2)=sigm(4)
    s33(1,3)=sigm(5)
    s33(2,3)=sigm(6)
    s33(2,1)=s33(1,2)
    s33(3,1)=s33(1,3)
    s33(3,2)=s33(2,3)
    bt33(1,1)=vim(58)
    bt33(2,2)=vim(59)
    bt33(3,3)=vim(60)
    bt33(1,2)=vim(61)
    bt33(1,3)=vim(62)
    bt33(2,3)=vim(63)
    bt33(2,1)=bt33(1,2)
    bt33(3,1)=bt33(1,3)
    bt33(3,2)=bt33(2,3)
    bc0=vim(64)
!
    call brvp33(bt33, bt3, vbt33)
    call transp(vbt33, 3, 3, 3, vbt33t,&
                3)
! PASSAGE DES CONTRAINTES DANS LA BASE PRINCIPALE D'ENDOMMAGEMENT
    call utbtab('ZERO', 3, 3, s33, vbt33,&
                trav, x33)
!
    do 10 i = 1, 3
        sn3(i)=s33(i,i)
10  end do
!
    call brksec(h66, bt3, bc0, nu0, e0,&
                sn3)
!
! PASSAGE H66 EN BASE FIXE
    call brchre(h66, vbt33t, vbt33, dsidep)
!      CALL AFFICHE66(DSIDEP)
!      DO I=1,6
!       DO J=1,6
!      PRINT*,'DSIDEP (',I,',',J,')',DSIDEP(I,J)
!       ENDDO
!       ENDDO
end subroutine
