subroutine te0404(option, nomte)
!
implicit none
!
#include "jeveux.h"
#include "asterfort/dxmate.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
#include "asterfort/teattr.h"
#include "asterfort/get_elas_type.h"
#include "asterfort/utmess.h"
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! aslint: disable=W0104
!
!
! ----------------------------------------------------------------------
! FONCTION REALISEE:  CALCUL DU PAS DE TEMPS DE COURANT POUR L'ELEMENT
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
!
!

!
    character(len=16) :: option, nomte
!
    character(len=4) :: fami
    integer :: icodre(1)
    integer :: codres(2)
    character(len=8) :: nomres(2)
    character(len=8) :: cnd
    integer :: icour, imate, igeom, nd, ndim, nno, nnos, npg
    integer :: i, j, ipoids, ivf, idfde, jgano, ier, iret
    integer :: jcoqu, multic, idfd2, icoopg
    real(kind=8) :: dmin, distij, xi, yi, zii, xj, yj, zj
    real(kind=8) :: e, nu, rho(1), vitmat, epais
    real(kind=8) :: df(3, 3), dm(3, 3), dmf(3, 3), dc(2, 2), dci(2, 2)
    real(kind=8) :: dmc(3, 2), dfc(3, 2)
    real(kind=8) :: pgl(3, 3), t2iu(4), t2ui(4), t1ve(9), valres(2)
    logical(kind=1) :: coupmf
    integer :: elas_type
    character(len=16) :: elas_keyword
! DEB ------------------------------------------------------------------
!
    call jevech('PCOURAN', 'E', icour)
!
!     RECUPERATION DES COORDONNEES DES NOEUDS
    call teattr('S', 'DIM_COOR_MODELI', cnd, ier)
    read(cnd,'(I8)')  nd
    call jevech('PGEOMER', 'L', igeom)
    fami = 'RIGI'
    call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
!     CALCUL DE LA PLUS PETITE DISTANCE ENTRE LES NOEUDS SOMMETS
    dmin = sqrt(&
           (&
           zr(&
           igeom-1+nd*(2-1)+1)-zr(igeom-1+1))**2 +(zr(igeom-1+nd*(2-1)+2)-zr(igeom-1+2))**2 +(zr(&
           &igeom-1+nd*(2-1)+3)-zr(igeom-1+3&
           )&
           )**2&
           )
!
    do i = 1, nnos-1
        do j = i+1, nnos
!
            xi = zr(igeom-1+nd*(i-1)+1)
            yi = zr(igeom-1+nd*(i-1)+2)
!
            xj = zr(igeom-1+nd*(j-1)+1)
            yj = zr(igeom-1+nd*(j-1)+2)
!
            if (nd .eq. 3) then
                zii = zr(igeom-1+nd*(i-1)+3)
                zj = zr(igeom-1+nd*(j-1)+3)
            else
                zii = 0.d0
                zj = 0.d0
            endif
!
            distij = sqrt((xj-xi)**2+(yj-yi)**2+(zj-zii)**2)
            if ((distij.le.dmin) .and. (distij.ne.0)) dmin = distij
!
        end do
    end do
!
!     RECUPERATION DU MODULE D'YOUNG ET DE LA MASSE VOLUMIQUE
    call jevech('PMATERC', 'L', imate)

    call get_elas_type(zi(imate), elas_type, elas_keyword)

    if (elas_type.eq.1) then
        if (elas_keyword .eq. 'ELAS') then
            nomres(1) = 'E'
            nomres(2) = 'NU'
            call rcvalb(fami, 1, 1, '+', zi(imate),&
                        ' ', elas_keyword, 0, ' ', [0.d0],&
                        2, nomres, valres, codres, 1)
            e = valres(1)
            nu = valres(2)
        else if (elas_keyword .eq. 'ELAS_GLRC') then
            nomres(1) = 'E_M'
            nomres(2) = 'NU_M'
            call rcvalb(fami, 1, 1, '+', zi(imate),&
                        ' ', elas_keyword, 0, ' ', [0.d0],&
                        2, nomres, valres, codres, 1)
            e = valres(1)
            nu = valres(2)
        else if (elas_keyword.eq.'ELAS_COQUE' .or. elas_keyword.eq.'ELAS_DHRC') then
            call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,&
                    npg=npg,jpoids=ipoids,jcoopg=icoopg,jvf=ivf,jdfde=idfde,&
                   jdfd2=idfd2,jgano=jgano)
            call jevech('PCACOQU', 'L', jcoqu)
            epais = zr(jcoqu)
            if (nno .eq. 3) then
                call dxtpgl(zr(igeom), pgl)
            else if (nno.eq.4) then
                call dxqpgl(zr(igeom), pgl, 'S', iret)
            endif
!
            call dxmate(fami, df, dm, dmf, dc,&
                        dci, dmc, dfc, nno, pgl,&
                        multic, coupmf, t2iu, t2ui, t1ve)
            nu = dm(1,2)/dm(1,1)
            e = (1.d0-nu**2)*dm(1,1)/epais
        endif
    else
        call utmess('F','DYNAMIQUE_32')
    endif
!
    call rcvalb(fami, 1, 1, '+', zi(imate),&
                ' ', elas_keyword, 0, ' ', [0.d0],&
                1, 'RHO', rho, icodre(1), 1)
!
!     CALCUL DE LA CELERITE DES ONDES DANS LE MATERIAU
!
    vitmat = sqrt(e/rho(1))
!
!     CALCUL DU PAS DE TEMPS DE LA CONDITION DE COURANT
!
    zr(icour) = dmin/vitmat
! FIN ------------------------------------------------------------------
end subroutine
