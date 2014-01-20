subroutine xcedge(ndime, pinref, pi1, pi2, pmiref, m12, crit)
!
    implicit none
!
#include "asterfort/xnormv.h"
#include "blas/ddot.h"
!
    real(kind=8) :: pinref(*), pmiref(*), crit 
    integer :: pi1, pi2, m12, ndime
!
!
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
! FONCTION REALISEE:  CALCUL D UN CRITERE DE COURBURE SUR UNE ARETE DANS 
!                          L ELEMENT DE REFERENCE
!   CRIT=0 SI ARETE DROITE
!       >0 SI ARETE COURBE
!
    real(kind=8) :: pipk(ndime), pimik(ndime), rbid, cosi
    integer :: i
!
    crit=0.d0
    do i = 1,ndime
       pipk(i)=pinref(ndime*(pi2-1)+i)-pinref(ndime*(pi1-1)+i)
       pimik(i)=pmiref(ndime*(m12-1)+i)-pinref(ndime*(pi1-1)+i)
    enddo
    call xnormv(ndime, pipk, rbid)
    call xnormv(ndime, pimik, rbid)
    cosi=ddot(ndime,pipk,1,pimik,1)
!    write(6,*)'xcedge: cosi=', cosi
    if (cosi .lt. 1.d0) crit=dacos(cosi)
!
end subroutine
