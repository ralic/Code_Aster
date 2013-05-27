subroutine rcangm(ndim, coor, angmas)
    implicit none
    include 'jeveux.h'
    include 'asterc/r8dgrd.h'
    include 'asterc/r8nnem.h'
    include 'asterfort/angvxy.h'
    include 'asterfort/r8inir.h'
    include 'asterfort/tecach.h'
    include 'asterfort/u2mess.h'
    include 'asterfort/utrcyl.h'
    integer :: ndim
    real(kind=8) :: angmas(7), coor(3)
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! ......................................................................
!    - ORIENTATION DU MASSIF
!
!   IN      NDIM    I      : DIMENSION DU PROBLEME
!   IN      COOR    R        COORDONNEE DU POINT
!                            (CAS CYLINDRIQUE)
!   OUT     ANGMAS  R      : ANGLE NAUTIQUE ( OU EULERIEN )
! ......................................................................
    integer :: icamas, iret, i
    real(kind=8) :: p(3, 3), xg(3), yg(3), orig(3), dire(3)
    real(kind=8) :: alpha, beta
!     ------------------------------------------------------------------
!
    call tecach('NNO', 'PCAMASS', 'L', 1, icamas,&
                iret)
    call r8inir(7, 0.d0, angmas, 1)
!
    if (iret .eq. 0) then
        call r8inir(7, 0.d0, angmas, 1)
        if (zr(icamas) .gt. 0.d0) then
            angmas(1) = zr(icamas+1)*r8dgrd()
            if (ndim .eq. 3) then
                angmas(2) = zr(icamas+2)*r8dgrd()
                angmas(3) = zr(icamas+3)*r8dgrd()
                angmas(4) = 1.d0
            endif
!           ECRITURE DES ANGLES D'EULER A LA FIN LE CAS ECHEANT
            if (abs(zr(icamas)-2.d0) .lt. 1.d-3) then
                if (ndim .eq. 3) then
                    angmas(5) = zr(icamas+4)*r8dgrd()
                    angmas(6) = zr(icamas+5)*r8dgrd()
                    angmas(7) = zr(icamas+6)*r8dgrd()
                else
                    angmas(5) = zr(icamas+1)*r8dgrd()
                endif
                angmas(4) = 2.d0
            endif
!
        else if (abs(zr(icamas)+1.d0).lt.1.d-3) then
!
! ON TRANSFORME LA DONNEE DU REPERE CYLINDRIQUE EN ANGLE NAUTIQUE
! (EN 3D, EN 2D ON MET A 0)
!
            if (ndim .eq. 3) then
                alpha=zr(icamas+1)*r8dgrd()
                beta =zr(icamas+2)*r8dgrd()
                dire(1) = cos(alpha)*cos(beta)
                dire(2) = sin(alpha)*cos(beta)
                dire(3) = -sin(beta)
                orig(1)=zr(icamas+4)
                orig(2)=zr(icamas+5)
                orig(3)=zr(icamas+6)
                call utrcyl(coor, dire, orig, p)
                do 1 i = 1, 3
                    xg(i)=p(1,i)
                    yg(i)=p(2,i)
 1              continue
                call angvxy(xg, yg, angmas)
            else
                call u2mess('F', 'ELEMENTS2_38')
                call r8inir(7, 0.d0, angmas, 1)
            endif
        endif
!
    else if (iret.eq.1) then
        call r8inir(7, r8nnem(), angmas, 1)
!
    else if (iret.eq.2) then
        call r8inir(7, 0.d0, angmas, 1)
!
    endif
!
end subroutine
