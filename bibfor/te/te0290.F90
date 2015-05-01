subroutine te0290(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CALC_NOEU_BORD  '
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    real(kind=8) :: coor(8), dx(4), dy(4), nx(9), ny(9), sens
!
!
!-----------------------------------------------------------------------
    integer :: i, idfde, igeom, ipoids, ivectu, ivf, jgano
    integer :: ndim, nno, npg, nsom
!-----------------------------------------------------------------------
    call jemarq()
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nsom,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PVECTUR', 'E', ivectu)
    do 1 i = 1, nsom
        coor(2*i-1) = zr(igeom+2*(i-1))
        coor(2*i) = zr(igeom+2*i-1)
 1  end do
    do 2 i = 1, nsom-1
        dx(i) = coor(2*i+1)-coor(2*i-1)
        dy(i) = coor(2*i+2)-coor(2*i)
 2  end do
    dx(nsom) = coor(1)-coor(2*nsom-1)
    dy(nsom) = coor(2)-coor(2*nsom)
!
!   INITIALISATION A 0.
!
    do 3 i = 1, nno
        zr(ivectu+2*i-2) = 0.d0
        zr(ivectu+2*i-1) = 0.d0
        nx(i) = 0.d0
        ny(i) = 0.d0
 3  end do
    nx(1) = (dy(nsom)+dy(1))
    ny(1) = -(dx(nsom)+dx(1))
    do 4 i = 2, nsom
        nx(i) = (dy(i-1)+dy(i))
        ny(i) = -(dx(i-1)+dx(i))
 4  end do
    if (nno .ne. nsom) then
        do 6 i = nsom+1, 2*nsom
            nx(i) = dy(i-nsom)
            ny(i) = -dx(i-nsom)
 6      continue
    endif
!
!   VERIFICATION DU SENS DE L'ELEMENT
!
    sens = dy(1)*dx(nsom)-dx(1)*dy(nsom)
    if (sens .eq. 0.d0) then
        call utmess('F', 'ELEMENTS3_67')
    else if (sens.lt.0.d0) then
        do 7 i = 1, nno
            nx(i) = -nx(i)
            ny(i) = -ny(i)
 7      continue
    endif
!
    do 5 i = 1, nno
        zr(ivectu+2*i-2) = nx(i)
        zr(ivectu+2*i-1) = ny(i)
 5  end do
    call jedema()
end subroutine
