subroutine te0051(option, nomte)
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
!.......................................................................
    implicit none
!
!     BUT: CALCUL DES MATRICES DE RIGIDITE ELEMENTAIRES EN THERMIQUE
!          ELEMENTS ISOPARAMETRIQUES 3D
!
!          OPTION : 'RIGI_THER'
!
!     ENTREES  ---> OPTION : OPTION DE CALCUL
!          ---> NOMTE  : NOM DU TYPE ELEMENT
!.......................................................................
!
!
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/matrot.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
#include "asterfort/utpvgl.h"
#include "asterfort/utpvlg.h"
#include "asterfort/utrcyl.h"
!
!-----------------------------------------------------------------------
    integer :: icamas, ij, l, n1, n2, nbres, ndim
    integer :: nnos, nuno
    real(kind=8) :: alpha, beta
!-----------------------------------------------------------------------
    parameter (nbres=3)
    character(len=8) :: nomres(nbres)
    integer :: icodre(nbres)
    character(len=8) :: fami, poum
    character(len=16) :: nomte, option, phenom
    real(kind=8) :: valres(nbres), lambda, theta, fluloc(3), fluglo(3)
    real(kind=8) :: valpar(nbres), lambor(3), orig(3), dire(3)
    real(kind=8) :: p(3, 3), dfdx(27), dfdy(27), dfdz(27), poids
    real(kind=8) :: point(3), angl(3)
    integer :: ipoids, ivf, idfde, igeom, imate, kpg, spt
    integer :: jgano, nno, kp, npg1, i, j, imattt, itemps
    logical(kind=1) :: aniso, global
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PMATTTR', 'E', imattt)
    call jevech('PTEMPSR', 'L', itemps)
    theta = zr(itemps+2)
!
    valpar(1) = zr(itemps)
    call rccoma(zi(imate), 'THER', 1, phenom, icodre(1))
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    if (phenom .eq. 'THER') then
        nomres(1) = 'LAMBDA'
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', phenom, 1, 'INST', [valpar],&
                    1, nomres, valres, icodre, 1)
        lambda = valres(1)
        aniso = .false.
    else if (phenom.eq.'THER_ORTH') then
        nomres(1) = 'LAMBDA_L'
        nomres(2) = 'LAMBDA_T'
        nomres(3) = 'LAMBDA_N'
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', phenom, 1, 'INST', [valpar],&
                    3, nomres, valres, icodre, 1)
        lambor(1) = valres(1)
        lambor(2) = valres(2)
        lambor(3) = valres(3)
        aniso = .true.
    else
        call utmess('F', 'ELEMENTS2_63')
    endif
!
    global = .false.
    if (aniso) then
        call jevech('PCAMASS', 'L', icamas)
        if (zr(icamas) .gt. 0.d0) then
            global = .true.
            angl(1) = zr(icamas+1)*r8dgrd()
            angl(2) = zr(icamas+2)*r8dgrd()
            angl(3) = zr(icamas+3)*r8dgrd()
            call matrot(angl, p)
        else
            alpha = zr(icamas+1)*r8dgrd()
            beta = zr(icamas+2)*r8dgrd()
            dire(1) = cos(alpha)*cos(beta)
            dire(2) = sin(alpha)*cos(beta)
            dire(3) = -sin(beta)
            orig(1) = zr(icamas+4)
            orig(2) = zr(icamas+5)
            orig(3) = zr(icamas+6)
        endif
    endif
!
!    BOUCLE SUR LES POINTS DE GAUSS
!
    do 50 kp = 1, npg1
!
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy, dfdz)
!
        if (.not.global .and. aniso) then
            point(1) = 0.d0
            point(2) = 0.d0
            point(3) = 0.d0
            do 20 nuno = 1, nno
                point(1) = point(1) + zr(ivf+l+nuno-1)*zr(igeom+3* nuno-3)
                point(2) = point(2) + zr(ivf+l+nuno-1)*zr(igeom+3* nuno-2)
                point(3) = point(3) + zr(ivf+l+nuno-1)*zr(igeom+3* nuno-1)
20          continue
            call utrcyl(point, dire, orig, p)
        endif
!
        do 40 i = 1, nno
            if (.not.aniso) then
                fluglo(1) = lambda*dfdx(i)
                fluglo(2) = lambda*dfdy(i)
                fluglo(3) = lambda*dfdz(i)
            else
                fluglo(1) = dfdx(i)
                fluglo(2) = dfdy(i)
                fluglo(3) = dfdz(i)
                n1 = 1
                n2 = 3
                call utpvgl(n1, n2, p, fluglo, fluloc)
                fluloc(1) = lambor(1)*fluloc(1)
                fluloc(2) = lambor(2)*fluloc(2)
                fluloc(3) = lambor(3)*fluloc(3)
                n1 = 1
                n2 = 3
                call utpvlg(n1, n2, p, fluloc, fluglo)
            endif
!
            do 30 j = 1, i
                ij = (i-1)*i/2 + j
                zr(imattt+ij-1) = zr(imattt+ij-1) + theta*poids* ( fluglo(1)*dfdx(j)+ fluglo(2)*d&
                                  &fdy(j)+fluglo(3)*dfdz(j) )
30          continue
40      continue
!
50  end do
!
end subroutine
