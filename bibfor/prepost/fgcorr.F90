subroutine fgcorr(nbcycl, sigmin, sigmax, method, su,&
                  rcorr)
    implicit none
#include "jeveux.h"
#include "asterfort/utmess.h"
    character(len=*) :: method
    real(kind=8) :: sigmin(*), sigmax(*), su, rcorr(*)
    integer :: nbcycl
!     ------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     -----------------------------------------------------------------
!     CORRECTION DE HAIGH : GOODMAN OU GERBER
!     ------------------------------------------------------------------
! IN  NBCYCL : I   : NOMBRE DE CYCLES
! IN  SIGMIN : R   : CONTRAINTES MINIMALES DES CYLES
! IN  SIGMAX : R   : CONTRAINTES MAXIMALES DES CYCLES
! IN  METHOD : K   : METHODE DE CORRECTION GOODMAN OU GERBER
! IN  SU     : R   :
! OUT RCORR  : R   : CORRECTION DE HAIGH POUR CHAQUE CYCLE
!     ------------------------------------------------------------------
!
    real(kind=8) :: valmoy
!
!-----------------------------------------------------------------------
    integer :: i
!-----------------------------------------------------------------------
    do 10 i = 1, nbcycl
        valmoy = (sigmax(i)+sigmin(i))/2.d0
        if (method .eq. 'GOODMAN') then
            if (valmoy .lt. su) then
                rcorr(i) = 1.d0 - (valmoy/su)
            else
                call utmess('F', 'FATIGUE1_4')
            endif
        else if (method.eq.'GERBER') then
            if (valmoy .lt. su) then
                rcorr(i) = 1.d0 - (valmoy/su)**2
            else
                call utmess('F', 'FATIGUE1_5')
            endif
        endif
10  end do
!
end subroutine
