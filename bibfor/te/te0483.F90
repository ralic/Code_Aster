subroutine te0483(option, nomte)
    implicit   none
#include "jeveux.h"
!
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/nmholi.h"
    character(len=16) :: option, nomte
! ......................................................................
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
!
!    - FONCTION REALISEE:  CALCUL DE LA CHARGE LIMITE POUR
!                          DES ELEMENTS INCOMPRESSIBLES PLAN OU AXI
!                          OPTION : 'CHAR_LIMITE'
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
! VECTEURS DIMENSIONNES POUR  NNO = 8
!
!
    logical :: axi
    integer :: ndim, nno, npg
    integer :: ipoi, ivf, jgano, idfde, nnos
    integer :: igeom, imate, idepl, itemps, iechli
!
!
! ......................................................................
!
!
    axi = .false.
    ndim = 3
!
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoi,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PDEPLAR', 'L', idepl)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PECHLI', 'E', iechli)
!
!
! - CALCUL DE LA CONTRIBUTION ELEMENTAIRE A LA CHARGE LIMITE
    call nmholi(ndim, axi, nno, npg, ipoi,&
                ivf, idfde, zi(imate), zr(itemps), zr(igeom),&
                zr(idepl), zr(iechli))
!
end subroutine
