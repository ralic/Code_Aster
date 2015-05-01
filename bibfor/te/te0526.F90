subroutine te0526(option, nomte)
!
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
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/elrefe_info.h"
#include "asterfort/foderi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jevech.h"
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES
!                          OPTION : 'CHAR_THER_FLUTNL'
!                          ELEMENTS DE FACE 3D
!                            -  PROBLEME  DE  TRANSPORT  -
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
    real(kind=8) :: nx, ny, nz, sx(9, 9), sy(9, 9), sz(9, 9), jac
    real(kind=8) :: tpg, alpha, alphap
    integer :: ndim, nno, npg1, ipoids, ivf, idfdx, idfdy
    integer :: igeom, iflux, itemp, itemps, ino, jno
    integer :: itempi, iveres
    integer :: i, j, kp, kdec, ldec, idec, jdec, nnos, jgano
    character(len=8) :: coef
!
    call jemarq()
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfdx,jgano=jgano)
    idfdy = idfdx + 1
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPER', 'L', itemp)
    call jevech('PTEMPEI', 'L', itempi)
    call jevech('PFLUXNL', 'L', iflux)
    call jevech('PRESIDU', 'E', iveres)
!
    coef = zk8(iflux)
    if (coef(1:7) .eq. '&FOZERO') goto 999
!
!
!    CALCUL DES PRODUITS VECTORIELS OMI   OMJ
!
    do 1 ino = 1, nno
        i = igeom + 3*(ino-1) -1
        do 2 jno = 1, nno
            j = igeom + 3*(jno-1) -1
            sx(ino,jno) = zr(i+2) * zr(j+3) - zr(i+3) * zr(j+2)
            sy(ino,jno) = zr(i+3) * zr(j+1) - zr(i+1) * zr(j+3)
            sz(ino,jno) = zr(i+1) * zr(j+2) - zr(i+2) * zr(j+1)
 2      continue
 1  end do
!
    do 101 kp = 1, npg1
        kdec = (kp-1)*nno*ndim
        ldec = (kp-1)*nno
        nx = 0.0d0
        ny = 0.0d0
        nz = 0.0d0
!
!   CALCUL DE LA NORMALE AU POINT DE GAUSS KP
!
        do 102 i = 1, nno
            idec = (i-1)*ndim
            do 102 j = 1, nno
                jdec = (j-1)*ndim
                nx = nx+ zr(idfdx+kdec+idec)* zr(idfdy+kdec+jdec)* sx( i,j)
                ny = ny+ zr(idfdx+kdec+idec)* zr(idfdy+kdec+jdec)* sy( i,j)
                nz = nz+ zr(idfdx+kdec+idec)* zr(idfdy+kdec+jdec)* sz( i,j)
102          continue
!
!   CALCUL DU JACOBIEN AU POINT DE GAUSS KP
!
        jac = sqrt(nx*nx + ny*ny + nz*nz)
!
        tpg = 0.d0
        do 103 i = 1, nno
            tpg = tpg + zr(itempi+i-1) * zr(ivf+ldec+i-1)
103      continue
        call foderi(coef, tpg, alpha, alphap)
!
        do 104 i = 1, nno
            zr(iveres+i-1) = zr(iveres+i-1) + zr(ipoids+kp-1)*jac* (alpha - alphap*tpg)*zr(ivf+ld&
                             &ec+i-1)
104      continue
101  end do
999  continue
! FIN ------------------------------------------------------------------
    call jedema()
end subroutine
