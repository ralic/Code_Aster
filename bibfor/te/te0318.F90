subroutine te0318(option, nomte)
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
    include 'jeveux.h'
!
    include 'asterc/r8dgrd.h'
    include 'asterfort/dfdm2d.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/rccoma.h'
    include 'asterfort/rcvalb.h'
    include 'asterfort/u2mess.h'
    character(len=16) :: option, nomte
! ----------------------------------------------------------------------
! CALCUL DU FLUX AU CARRE AUX POINTS DE GAUSS
! ELEMENTS ISOPARAMETRIQUES 2D/2D AXI  OPTION : 'SOUR_ELGA '
!
!
! IN  OPTION : OPTION DE CALCUL
! IN  NOMTE  : NOM DU TYPE ELEMENT
!
!
!
    integer :: icodre(2), kpg, spt
    character(len=8) :: nomres(2), fami, poum
    character(len=16) :: phenom
    real(kind=8) :: dfdx(9), dfdy(9), tpg, poids, lambda, a, b
    real(kind=8) :: lambor(2), p(2, 2), point(2), orig(2)
    real(kind=8) :: fluglo(2), fluloc(2), valres(2)
    real(kind=8) :: alpha, fluxx, fluxy, xu, yu, xnorm
    integer :: ndim, nno, nnos, kp, j, k, itempe, itemp, iflux, nuno
    integer :: ipoids, ivf, idfde, igeom, imate, npg, jgano, icamas
    logical :: aniso, global
! DEB ------------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imate)
    call jevech('PTEMPSR', 'L', itemp)
    call jevech('PTEMPER', 'L', itempe)
    call jevech('PSOUR_R', 'E', iflux)
!
    call rccoma(zi(imate), 'THER', 1, phenom, icodre)
!
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    if (phenom .eq. 'THER') then
        nomres(1) = 'LAMBDA'
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', phenom, 1, 'INST', zr(itemp),&
                    1, nomres, valres, icodre, 1)
        lambda = valres(1)
        aniso = .false.
    else if (phenom .eq. 'THER_ORTH') then
        nomres(1) = 'LAMBDA_L'
        nomres(2) = 'LAMBDA_T'
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', phenom, 1, 'INST', zr(itemp),&
                    2, nomres, valres, icodre, 1)
        lambor(1) = valres(1)
        lambor(2) = valres(2)
        aniso = .true.
    else if (phenom .eq. 'THER_NL') then
        aniso = .false.
    else
        call u2mess('F', 'ELEMENTS2_63')
    endif
!
    global = .false.
    if (aniso) then
        call jevech('PCAMASS', 'L', icamas)
        if (zr(icamas) .gt. 0.d0) then
            global = .true.
            alpha = zr(icamas+1)*r8dgrd()
            p(1,1) = cos(alpha)
            p(2,1) = sin(alpha)
            p(1,2) = -sin(alpha)
            p(2,2) = cos(alpha)
        else
            orig(1) = zr(icamas+4)
            orig(2) = zr(icamas+5)
        endif
    endif
!
    a = 0.d0
    b = 0.d0
!
    do 101 kp = 1, npg
        k=(kp-1)*nno
        call dfdm2d(nno, kp, ipoids, idfde, zr(igeom),&
                    dfdx, dfdy, poids)
        tpg = 0.0d0
        fluxx = 0.0d0
        fluxy = 0.0d0
        if (.not.global .and. aniso) then
            point(1)=0.d0
            point(2)=0.d0
            do 103 nuno = 1, nno
                point(1) = point(1)+ zr(ivf+k+nuno-1)*zr(igeom+2*nuno- 2)
                point(2) = point(2)+ zr(ivf+k+nuno-1)*zr(igeom+2*nuno- 1)
103          continue
!
            xu = orig(1) - point(1)
            yu = orig(2) - point(2)
            xnorm = sqrt( xu**2 + yu**2 )
            xu = xu / xnorm
            yu = yu / xnorm
            p(1,1) = xu
            p(2,1) = yu
            p(1,2) = -yu
            p(2,2) = xu
        endif
!
        do 110 j = 1, nno
            tpg = tpg + zr(itempe+j-1)*zr(ivf+k+j-1)
            fluxx = fluxx + zr(itempe+j-1)*dfdx(j)
            fluxy = fluxy + zr(itempe+j-1)*dfdy(j)
110      continue
!
        if (phenom .eq. 'THER_NL') then
            call rcvalb(fami, kpg, spt, poum, zi(imate),&
                        ' ', phenom, 1, 'TEMP', tpg,&
                        1, 'LAMBDA', lambda, icodre, 1)
        endif
!
        if (.not.aniso) then
            fluglo(1) = lambda*fluxx
            fluglo(2) = lambda*fluxy
        else
            fluglo(1) = fluxx
            fluglo(2) = fluxy
            fluloc(1) = p(1,1)*fluxx + p(2,1)*fluxy
            fluloc(2) = p(1,2)*fluxx + p(2,2)*fluxy
            fluloc(1) = lambor(1)*fluloc(1)
            fluloc(2) = lambor(2)*fluloc(2)
            fluglo(1) = p(1,1)*fluloc(1) + p(1,2)*fluloc(2)
            fluglo(2) = p(2,1)*fluloc(1) + p(2,2)*fluloc(2)
        endif
        a = a - fluglo(1) / npg
        b = b - fluglo(2) / npg
101  end do
    do 102 kp = 1, npg
        zr(iflux+(kp-1)) = ( a**2 + b**2 ) / lambda
102  end do
end subroutine
