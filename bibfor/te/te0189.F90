subroutine te0189(option, nomte)
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
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES NIVEAUX DE PRESSION ACOUSTIQUE
    implicit none
!                          OPTION : 'PRAC_ELNO'
!    - ARGUMENTS:
!        ENTREES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
    character(len=16) :: option, nomte
    integer :: idino, ino, nno, nnos, ndim, jgano, npg1
    integer :: ipdeb, ipres, ipoids, ivf, idfde
!
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    call assert(option.eq.'PRAC_ELNO')
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg1, ipoids, ivf, idfde, jgano)
!
    call jevech('PPRESSC', 'L', ipres)
    call jevech('PPRAC_R', 'E', ipdeb)
!
    do 101 ino = 1, nno
        idino = ipdeb +3*(ino - 1)
        zr(idino-1+1) = dble(zc(ipres +ino-1))
        zr(idino-1+2) = dimag(zc(ipres +ino-1))
        zr(idino-1+3) = 20.d0*log10(abs(zc(ipres +ino-1))/2.d-5)
101  end do
!
end subroutine
