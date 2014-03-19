subroutine te0062(option, nomte)
    implicit none
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
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
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
!
!     BUT:
!       CALCUL DES FLUX DE TEMPERATURE AUX POINTS DE GAUSS
!       ELEMENTS 3D
!       OPTION : 'FLUX_ELGA'
!
! ---------------------------------------------------------------------
!
!
!
    integer :: icodre(3)
    integer :: jgano, ipoids, ivf, idfde, igeom, imate, nno, kp
    integer :: npg1, i, iflux, itemps, itempe, l, n1, n2, ndim
    integer :: icamas, nuno, nnos, nbcmp
!
    real(kind=8) :: valres(3), lambda(1), fluxx, fluxy, fluxz, tpg, dfdx(27)
    real(kind=8) :: dfdy(27)
    real(kind=8) :: dfdz(27), poids, lambor(3), fluglo(3), fluloc(3), p(3, 3)
    real(kind=8) :: dire(3), orig(3), point(3), angl(3)
    real(kind=8) :: alpha, beta
!
    character(len=8) :: nomres(3)
    character(len=16) :: phenom
!
    logical :: aniso, global
!
! ----------------------------------------------------------------------
!
!====
! 1.1 PREALABLES: RECUPERATION ALDRESSES FONCTIONS DE FORMES...
!====
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg1,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    nbcmp = 3
!
!====
! 1.2 PREALABLES LIES AUX RECHERCHES DE DONNEES GENERALES
!====
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PTEMPER', 'L', itempe)
    call jevech('PFLUXPG', 'E', iflux)
    call rccoma(zi(imate), 'THER', 1, phenom, icodre(1))
!
!====
! 1.3 PREALABLES LIES A LA RECUPERATION DES DONNEES MATERIAUX EN
!     THERMIQUE LINEAIRE ISOTROPE OU ORTHOTROPE
!====
    if (phenom .eq. 'THER') then
        nomres(1) = 'LAMBDA'
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', phenom, 1, 'INST', [zr(itemps)],&
                    1, nomres, lambda, icodre, 1)
        aniso = .false.
    else if (phenom.eq.'THER_ORTH') then
        nomres(1) = 'LAMBDA_L'
        nomres(2) = 'LAMBDA_T'
        nomres(3) = 'LAMBDA_N'
        call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                    ' ', phenom, 1, 'INST', [zr(itemps)],&
                    3, nomres, valres, icodre, 1)
        lambor(1) = valres(1)
        lambor(2) = valres(2)
        lambor(3) = valres(3)
        aniso = .true.
    else if (phenom.eq.'THER_NL') then
        aniso = .false.
    else
        call utmess('F', 'ELEMENTS2_63')
    endif
!
!====
! 1.4 PREALABLES LIES A L'ANISOTROPIE
!====
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
!====
! 2. CALCULS TERMES DE FLUX
!====
    do 40 kp = 1, npg1
        l = (kp-1)*nno
        call dfdm3d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy, dfdz)
!
!     CALCUL DE T ET DE GRAD(T) AUX POINTS DE GAUSS
        tpg = 0.0d0
        fluxx = 0.0d0
        fluxy = 0.0d0
        fluxz = 0.0d0
        if (.not.global .and. aniso) then
            point(1) = 0.d0
            point(2) = 0.d0
            point(3) = 0.d0
            do 10 nuno = 1, nno
                point(1) = point(1) + zr(ivf+l+nuno-1)*zr(igeom+3* nuno-3)
                point(2) = point(2) + zr(ivf+l+nuno-1)*zr(igeom+3* nuno-2)
                point(3) = point(3) + zr(ivf+l+nuno-1)*zr(igeom+3* nuno-1)
10          continue
            call utrcyl(point, dire, orig, p)
        endif
!
        do 20 i = 1, nno
            tpg = tpg + zr(itempe-1+i)*zr(ivf+l+i-1)
            fluxx = fluxx + zr(itempe-1+i)*dfdx(i)
            fluxy = fluxy + zr(itempe-1+i)*dfdy(i)
            fluxz = fluxz + zr(itempe-1+i)*dfdz(i)
20      continue
!
        if (phenom .eq. 'THER_NL') then
            call rcvalb('FPG1', 1, 1, '+', zi(imate),&
                        ' ', phenom, 1, 'TEMP', [tpg],&
                        1, 'LAMBDA', lambda, icodre, 1)
        endif
!
        if (.not.aniso) then
            fluglo(1) = lambda(1)*fluxx
            fluglo(2) = lambda(1)*fluxy
            fluglo(3) = lambda(1)*fluxz
        else
            fluglo(1) = fluxx
            fluglo(2) = fluxy
            fluglo(3) = fluxz
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
        zr(iflux+(kp-1)*nbcmp-1+1) = -fluglo(1)
        zr(iflux+(kp-1)*nbcmp-1+2) = -fluglo(2)
        zr(iflux+(kp-1)*nbcmp-1+3) = -fluglo(3)
40  end do
! FIN ------------------------------------------------------------------
end subroutine
