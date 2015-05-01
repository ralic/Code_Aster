subroutine te0379(option, nomte)
    implicit none
#include "jeveux.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
!
    character(len=16) :: option, nomte
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
! ......................................................................
!    - FONCTION REALISEE: EXTENSION DU CHAM_ELEM ERREUR AUX NOEUDS
!                         OPTIONS : 'ERME_ELNO'  ET 'ERTH_ELNO'
!             (POUR PERMETTRE D'UTILISER POST_RELEVE_T)
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE
! ......................................................................
!
!
!
!
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfde, jgano, nbcmp
    integer :: i, j, itab(3), ierr, ierrn, iret
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call tecach('OOO', 'PERREUR', 'L', iret, nval=3,&
                itab=itab)
    call jevech('PERRENO', 'E', ierrn)
    ierr=itab(1)
    nbcmp=itab(2)
!
    do 10 i = 1, nno
        do 20 j = 1, nbcmp
            zr(ierrn+nbcmp*(i-1)+j-1) = zr(ierr-1+j)
20      continue
10  end do
!
end subroutine
