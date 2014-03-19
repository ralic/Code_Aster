subroutine te0421(option, nomte)
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
#include "asterfort/dfdm2d.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "asterfort/rccoma.h"
#include "asterfort/rcvalb.h"
#include "asterfort/rcvarc.h"
#include "asterfort/tecach.h"
#include "asterfort/utmess.h"
!
    character(len=16) :: option, nomte
! ......................................................................
!    - FONCTION REALISEE:  CALCUL DES VECTEURS ELEMENTAIRES EN 2D
!                      OPTION : 'CHAR_MECA_EPSA_R  '
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!-----------------------------------------------------------------------
    integer :: jgano, nbpar, nbres, ndim, nnos
    real(kind=8) :: e1, e2, e3
!-----------------------------------------------------------------------
    parameter        ( nbres=10 )
    character(len=16) :: phenom
    character(len=8) :: nomres(nbres), nompar, blan8
    character(len=4) :: fami
    integer :: icodre(nbres)
    real(kind=8) :: valres(nbres), valpar, zero
    real(kind=8) :: dfdx(9), dfdy(9), poids, r, exx, eyy, exy, ezz
    real(kind=8) :: a11, a22, a33, a12, a13, a23, delta, c1
    real(kind=8) :: nu12, nu21, nu13, nu31, nu23, nu32, g12
    integer :: nno, kp, k, npg, i, itemps, ivectu, iret
    integer :: ipoids, ivf, idfde, igeom, imate
!
!
    data  zero / 0.d0 /
!
    blan8='        '
    fami = 'RIGI'
    call elrefe_info(fami=fami,ndim=ndim,nno=nno,nnos=nnos,&
  npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfde,jgano=jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call rccoma(zi(imate), 'ELAS', 1, phenom, icodre(1))
!
    call tecach('ONN', 'PTEMPSR', 'L', iret, iad=itemps)
    if (itemps .eq. 0) then
        nbpar = 0
        nompar=' '
    endif
!
    do 10 i = 1, nbres
        nomres(i)=blan8
10  end do
    if (phenom .eq. 'ELAS') then
        nomres(1) = 'E'
        nomres(2) = 'NU'
    else if (phenom.eq.'ELAS_ORTH') then
        nomres(1) = 'E_X'
        nomres(2) = 'E_Y'
        nomres(3) = 'E_Z'
        nomres(4) = 'NU_XY'
        nomres(5) = 'NU_XZ'
        nomres(6) = 'NU_YZ'
        nomres(7) = 'G_XY'
    else if (phenom.eq.'ELAS_GITR') then
        nomres(1) = 'E_XY'
        nomres(2) = 'E_Z'
        nomres(3) = 'NU_XY'
        nomres(4) = 'NU_Z'
    else
        call utmess('F', 'ELEMENTS_50')
    endif
    call jevech('PVECTUR', 'E', ivectu)
!
    do 101 kp = 1, npg
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    poids, dfdx, dfdy)
        r = zero
        call rcvarc(' ', 'EPSAXX', '+', 'RIGI', kp,&
                    1, exx, iret)
        if (iret .eq. 1) exx=0.d0
        call rcvarc(' ', 'EPSAYY', '+', 'RIGI', kp,&
                    1, eyy, iret)
        if (iret .eq. 1) eyy=0.d0
        call rcvarc(' ', 'EPSAZZ', '+', 'RIGI', kp,&
                    1, ezz, iret)
        if (iret .eq. 1) ezz=0.d0
        call rcvarc(' ', 'EPSAXY', '+', 'RIGI', kp,&
                    1, exy, iret)
        if (iret .eq. 1) exy=0.d0
!
        nbpar = 1
        nompar = 'INST'
        valpar = zr(itemps)
!
        do 102 i = 1, nno
            r = r + zr(igeom+2*i-2)*zr(ivf+k+i-1)
102      continue
!
!
        if (phenom .eq. 'ELAS') then
!CC --- CAS ISOTROPE
            call rcvalb(fami, kp, 1, '+', zi(imate),&
                        ' ', phenom, nbpar, nompar, [valpar],&
                        2, nomres, valres, icodre, 1)
            call rcvalb(fami, kp, 1, '+', zi(imate),&
                        ' ', phenom, nbpar, nompar, [valpar],&
                        1, nomres(3), valres(3), icodre(3), 0)
            if (icodre(3) .ne. 0) valres(3) = 0.d0
!
            c1 = valres(1)/(1.d0 + valres(2))
            a11 = c1*(1.d0 - valres(2))/(1.d0 - 2.d0*valres(2))
            a12 = c1* valres(2) /(1.d0 - 2.d0*valres(2))
            a13 = a12
            a22 = a11
            a23 = a12
            a33 = a11
            g12 = c1/2.d0
!
        else if (phenom.eq.'ELAS_ORTH') then
!CC --- CAS ORTHOTROPE
            call rcvalb(fami, kp, 1, '+', zi(imate),&
                        ' ', phenom, nbpar, nompar, [valpar],&
                        7, nomres, valres, icodre, 1)
!
            e1 = valres(1)
            e2 = valres(2)
            e3 = valres(3)
            nu12 = valres(4)
            nu13 = valres(5)
            nu23 = valres(6)
            nu21 = e1*nu12/e2
            nu31 = e1*nu13/e3
            nu32 = e2*nu23/e3
            delta = 1.d0-nu23*nu32-nu31*nu13-nu12*nu21-2.d0*nu23*nu31* nu12
            a11 = (1.d0 - nu23*nu32)*e1/delta
            a12 = (nu12 + nu13*nu32)*e1/delta
            a13 = (nu13 + nu12*nu23)*e1/delta
            a22 = (1.d0 - nu13*nu31)*e2/delta
            a23 = (nu23 + nu13*nu21)*e2/delta
            a33 = (1.d0 - nu12*nu21)*e3/delta
            g12 = valres(7)
!
!
        else if (phenom.eq.'ELAS_GITR') then
!CC     CAS ISOTROPE_TRANSVERSE
            call rcvalb(fami, kp, 1, '+', zi(imate),&
                        ' ', phenom, nbpar, nompar, [valpar],&
                        4, nomres, valres, icodre, 1)
!
            e1 = valres(1)
            e3 = valres(2)
            nu12 = valres(3)
            nu13 = valres(4)
            c1 = e1/(1.d0+nu12)
            delta = 1.d0 - nu12 - 2.d0*nu13*nu13*e1/e3
            a11 = (1.d0 - nu13*nu13*e1/e3)/delta
            a12 = c1*(a11 - 1.d0)
            a11 = c1*a11
            a13 = e1*nu13/delta
            a22 = a11
            a23 = a13
            a33 = e3*(1.d0 - nu12)/delta
            g12 = c1/2.d0
!
        endif
!
        if (lteatt('C_PLAN','OUI')) then
            a11=a11-a13*a13/a33
            a12=a12-a13*a23/a33
            a22=a22-a23*a23/a33
            a13=0.d0
            a23=0.d0
        endif
!
        if (lteatt('AXIS','OUI')) then
            poids = poids*r
            if (r .ne. 0.d0) then
                do 103 i = 1, nno
                    zr(ivectu+2*i-2) = zr(ivectu+2*i-2) + poids *( (a11*exx+a12*eyy+a13*ezz)* dfd&
                                       &x(i) + (a13*exx+a23* eyy+a33*ezz)*zr(ivf+k+i-1)/r + 2*g12&
                                       &*exy*dfdy(i))
                    zr(ivectu+2*i-1) = zr(ivectu+2*i-1) + poids *( (a12*exx+a22*eyy+a23*ezz)*dfdy&
                                       &(i) + 2*g12*exy* dfdx(i))
103              continue
            else
                do 203 i = 1, nno
                    zr(ivectu+2*i-2) = zr(ivectu+2*i-2) + poids * ( (a11*exx+a12*eyy+a13*ezz)*dfd&
                                       &x(i) + (a13*exx+a23* eyy+a33*ezz)*dfdx(i) + 2*g12*exy*dfd&
                                       &y(i))
                    zr(ivectu+2*i-1) = zr(ivectu+2*i-1) + poids * ( (a12*exx+a22*eyy+a23*ezz)*dfd&
                                       &y(i) + 2*g12*exy* dfdx(i))
203              continue
            endif
!
        else
            do 104 i = 1, nno
                zr(ivectu+2*i-2)=zr(ivectu+2*i-2) + poids * ( (a11*&
                exx+a12*eyy+a13*ezz)*dfdx(i) + 2*g12*exy*dfdy(i))
                zr(ivectu+2*i-1)=zr(ivectu+2*i-1) + poids * ( (a12*&
                exx+a22*eyy+a23*ezz)*dfdy(i) + 2*g12*exy*dfdx(i))
104          continue
        endif
101  end do
end subroutine
