subroutine pomass(nomte, e, xnu, rho, kanl, mlv)
! aslint: disable=
    implicit none
#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/masstg.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/ptma01.h"
#include "asterfort/ptma10.h"
#include "asterfort/utmess.h"
    character(len=*) :: nomte
    real(kind=8) :: mlv(*)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2014  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     CALCULE LA MATRICE DE MASSE DES ELEMENTS DE POUTRE
!
! IN  NOMTE : NOM DU TYPE ELEMENT
!             'MECA_POU_D_E'  'MECA_POU_D_T'  'MECA_POU_C_T'
!     ------------------------------------------------------------------
!
    real(kind=8) :: g
    character(len=16) :: ch16
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: istruc, itype, kanl, lrcou, i
!
    real(kind=8) :: a, a2, alfay, alfay2, alfaz, alfaz2, ang
    real(kind=8) :: angs2, deux, e, ey, ez, rad, rho
    real(kind=8) :: un, x2iy, x2iz, xfl, xfly, xflz, xiy, xjx, xjx2
    real(kind=8) :: xiy2, xiz, xiz2, xl, xnu, zero
    real(kind=8) :: mlv2(78)
!-----------------------------------------------------------------------
    integer, parameter :: nb_cara = 17
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'A1','IY1','IZ1','AY1','AZ1','EY1','EZ1','JX1',&
                    'A2','IY2','IZ2','AY2','AZ2','EY2','EZ2','JX2','TVAR'/
!-----------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
    deux = 2.d0
    g = e/ (deux* (un+xnu))
    do i = 1, 78
        mlv2(i) = 0.0d0
    enddo
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
!
    xl = lonele()
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
!
    a      = vale_cara(1)
    xiy    = vale_cara(2)
    xiz    = vale_cara(3)
    alfay  = vale_cara(4)
    alfaz  = vale_cara(5)
    xjx    = vale_cara(8)
    a2     = vale_cara(9)
    xiy2   = vale_cara(10)
    xiz2   = vale_cara(11)
    alfay2 = vale_cara(12)
    alfaz2 = vale_cara(13)
    xjx2   = vale_cara(16)
    ey = (vale_cara(6) +vale_cara(14))/2.d0
    ez = (vale_cara(7) +vale_cara(15))/2.d0
    itype = nint(vale_cara(17))
!
    istruc = 1
    if (nomte .eq. 'MECA_POU_D_E') then
!        --- POUTRE DROITE D'EULER A 6 DDL ---
        alfay = zero
        alfaz = zero
        alfay2 = zero
        alfaz2 = zero
    else if (nomte.eq.'MECA_POU_D_T') then
!        --- POUTRE DROITE DE TIMOSKENKO A 6 DDL ---
    else if (nomte.eq.'MECA_POU_D_TG') then
!        --- POUTRE DROITE DE TIMOSKENKO A 7 DDL ---
        a2 = a
    else if (nomte.eq.'MECA_POU_C_T') then
!        --- POUTRE COURBE DE TIMOSKENKO A 6 DDL ---
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        xfl = zr(lrcou+2)
        xfly = xfl
        xflz = xfl
        if (xfl .eq. zero) then
            xfly = zr(lrcou+4)
            xflz = zr(lrcou+6)
        endif
        angs2 = asin(xl/ (deux*rad))
        ang = angs2*deux
        xl = rad*ang
        x2iy = xiy
        x2iz = xiz
        xiy = xiy/xfly
        xiz = xiz/xflz
        xiy2 = xiy2/xfly
        xiz2 = xiz2/xflz
    else
        ch16 = nomte
        call utmess('F', 'ELEMENTS2_42', sk=ch16)
    endif
!
!     --- CALCUL DE LA MATRICE DE MASSE LOCALE
!
    if (itype .lt. 10) then
!        --- POUTRE DROITE SECTION CONSTANTE OU VARIABLE (1 OU 2)
        call ptma01(kanl, itype, mlv2, istruc, rho,&
                    e, a, a2, xl, xiy,&
                    xiy2, xiz, xiz2, g, alfay,&
                    alfay2, alfaz, alfaz2, ey, ez)
        if (nomte.eq. 'MECA_POU_D_TG')then
            call masstg(mlv2, mlv)
        else
            do i = 1, 78
                mlv(i) = mlv2(i)
            enddo
        endif

!
    else if (itype.eq.10) then
!        --- POUTRE COURBE SECTION CONSTANTE ---
        call ptma10(mlv, rho, a, xl, x2iy, x2iz)
    endif
!
end subroutine
