subroutine porigi(nomte, e, xnu, xl, klv)
! aslint: disable=
    implicit none
#include "jeveux.h"
#include "asterfort/carapo.h"
#include "asterfort/jevech.h"
#include "asterfort/ptka01.h"
#include "asterfort/ptka02.h"
#include "asterfort/ptka10.h"
#include "asterfort/ptka21.h"
#include "asterfort/utmess.h"
#include "asterfort/lteatt.h"

    character(len=*) :: nomte
    real(kind=8) :: e, xnu, xl, klv(*)
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
!     CALCULE LA MATRICE DE RIGIDITE DES ELEMENTS DE POUTRE
!
! IN  NOMTE : NOM DU TYPE ELEMENT
!             'MECA_POU_D_E'  'MECA_POU_D_T'  'MECA_POU_C_T' ...
!-----------------------------------------------------------------------
    integer :: istruc, itype, lrcou, lsect, igeom
    real(kind=8) :: a, a2, alfay, alfay2, alfaz, alfaz2, ang
    real(kind=8) :: angs2, deux, ey, ez, g, rad, un
    real(kind=8) :: xfl, xfly, xflz, xiy, xiy2, xiz, xiz2
    real(kind=8) :: xjx, xjx2, xl_geom, zero, pgl_bid(3, 3), xl0
    real(kind=8) :: xig, ang_bid(3)
    logical(kind=1) :: euler
!-----------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
    deux = 2.d0
    g = e/ (deux* (un+xnu))
    euler=lteatt('EULER','OUI')
    ang_bid(1)= 0.d0
    ang_bid(2)= 0.d0
    ang_bid(3)= 0.d0
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
!
    call jevech('PCAGNPO', 'L', lsect)
    call jevech('PGEOMER', 'L', igeom)
    call carapo(zr(lsect), zr(igeom), ang_bid, xl_geom, pgl_bid,&
                itype, a, xiy, xiz, xjx,&
                alfay, alfaz, ey, ez, a2,&
                xiy2, xiz2, xjx2, alfay2, alfaz2)
    if (xl .le. zero .or.(nomte.eq.'MECA_POU_C_T')) then
        xl0 = xl_geom
    else
        xl0 = xl
    endif

!
    if (euler) then
!        --- POUTRE DROITE D'EULER A 6 DDL ---
        istruc = 1
        alfay = zero
        alfaz = zero
        alfay2 = zero
        alfaz2 = zero
    else if (nomte.eq.'MECA_POU_C_T') then
!        --- POUTRE COURBE DE TIMOSKENKO A 6 DDL ---
        istruc = 1
        call jevech('PCAARPO', 'L', lrcou)
        rad = zr(lrcou)
        xfl = zr(lrcou+2)
        xfly = xfl
        xflz = xfl
        if (xfl .eq. zero) then
            xfly = zr(lrcou+4)
            xflz = zr(lrcou+6)
        endif
        angs2 = asin(xl_geom/ (deux*rad))
        ang = angs2*deux
        xl0 = rad*ang
        xiy = xiy/xfly
        xiz = xiz/xflz
        xiy2 = xiy2/xfly
        xiz2 = xiz2/xflz
    else if (nomte.eq.'MECA_POU_D_TG')then
        itype = 30
        xig = zr(lsect-1+12)
    else
!        --- POUTRE DROITE DE TIMOSKENKO A 6 DDL ---
        istruc = 1
    endif
!
!     --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE
!
    if (itype .eq. 0) then
!        --- POUTRE DROITE A SECTION CONSTANTE ---
        call ptka01(klv, e, a, xl0, xiy,&
                    xiz, xjx, g, alfay, alfaz,&
                    ey, ez, istruc)
!
    else if (itype.eq.1 .or. itype.eq.2) then
!        --- POUTRE DROITE A SECTION VARIABLE (TYPE 1 OU 2) ---
        call ptka02(itype, klv, e, a, a2,&
                    xl0, xiy, xiy2, xiz, xiz2,&
                    xjx, xjx2, g, alfay, alfay2,&
                    alfaz, alfaz2, ey, ez, istruc)
!
    else if (itype.eq.10) then
!        --- POUTRE COURBE A SECTION CONSTANTE ---
        call ptka10(klv, e, a, xiy, xiz,&
                    xjx, g, alfay, alfaz, rad,&
                    ang, istruc)
    else if (itype.eq.30) then
        call ptka21(klv, e, a, xl0, xiy,&
                    xiz, xjx, xig, g, alfay,&
                    alfaz, ey, ez)
    endif
!
end subroutine
