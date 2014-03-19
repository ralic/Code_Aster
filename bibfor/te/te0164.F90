subroutine te0164(option, nomte)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
#include "asterfort/biline.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/matvec.h"
#include "asterfort/tecach.h"
#include "asterfort/terefe.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES FORCES NODALES DE MECABL2
!                          OPTION : 'FORC_NODA', 'REFE_FORC_NODA'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    real(kind=8) :: coef, jacobi, nx, ytywpq(9), w(9), forref
    integer :: nno, kp, i, ipoids, ivf, igeom, nc, nordre, iret, k
    integer :: ivectu, ino, ndim, nnos, npg
    integer :: idfdk, jgano, iyty, idepla, ideplp, lsigma, jefint
! ----------------------------------------------------------------------
!
    if (option .eq. 'REFE_FORC_NODA') then
        nno = 2
        nc = 3
        call terefe('EFFORT_REFE', 'MECA_BARRE', forref)
        call jevech('PVECTUR', 'E', ivectu)
        do 101 ino = 1, nno
            do 102 i = 1, nc
                zr(ivectu+(ino-1)*nc+i-1)=forref
102          continue
101      continue
!
    else if (option.eq.'FORC_NODA') then
        call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdk,jgano=jgano)
        call jevete('&INEL.CABPOU.YTY', 'L', iyty)
        nordre = 3*nno
!        PARAMETRES EN ENTREE
        call jevech('PGEOMER', 'L', igeom)
!
        call jevech('PDEPLMR', 'L', idepla)
        call tecach('ONN', 'PDEPLPR', 'L', iret, iad=ideplp)
        call jevech('PCONTMR', 'L', lsigma)
!        PARAMETRES EN SORTIE
        call jevech('PVECTUR', 'E', jefint)
!
        if (ideplp .eq. 0) then
            do 10 i = 1, 3*nno
                w(i) = zr(idepla-1+i)
10          continue
        else
            do 20 i = 1, 3*nno
                w(i) = zr(idepla-1+i) + zr(ideplp-1+i)
20          continue
        endif
        do 40 kp = 1, npg
            k = (kp-1)*nordre*nordre
            jacobi=sqrt(biline(nordre,zr(igeom),zr(iyty+k),zr(igeom)))
            nx = zr(lsigma-1+kp)
            call matvec(nordre, zr(iyty+k), 2, zr(igeom), w,&
                        ytywpq)
            coef = nx*zr(ipoids-1+kp)/jacobi
            do 30 i = 1, nordre
                zr(jefint-1+i) = zr(jefint-1+i) + coef*ytywpq(i)
30          continue
40      continue
    endif
end subroutine
