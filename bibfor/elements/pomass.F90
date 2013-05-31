subroutine pomass(nomte, e, xnu, rho, kanl,&
                  mlv)
! aslint: disable=
    implicit none
    include 'jeveux.h'
    include 'asterfort/jevech.h'
    include 'asterfort/ptma01.h'
    include 'asterfort/ptma10.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: nomte
    real(kind=8) :: mlv(*)
!     ------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2012  EDF R&D                  WWW.CODE-ASTER.ORG
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
    character(len=8) :: nomail
    character(len=16) :: ch16
    integer :: iadzi, iazk24
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: istruc, itype, kanl, lrcou, lsect, lsect2, lx
!
    real(kind=8) :: a, a2, alfay, alfay2, alfaz, alfaz2, ang
    real(kind=8) :: angs2, deux, e, ey, ez, rad, rho
    real(kind=8) :: un, x2iy, x2iz, xfl, xfly, xflz, xiy
    real(kind=8) :: xiy2, xiz, xiz2, xl, xnu, zero
!-----------------------------------------------------------------------
    zero = 0.d0
    un = 1.d0
    deux = 2.d0
    g = e/ (deux* (un+xnu))
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
!
    call jevech('PCAGNPO', 'L', lsect)
    lsect = lsect - 1
    itype = nint(zr(lsect+23))
!
!     --- SECTION INITIALE ---
    a = zr(lsect+1)
    xiy = zr(lsect+2)
    xiz = zr(lsect+3)
    alfay = zr(lsect+4)
    alfaz = zr(lsect+5)
!     EY    = -ZR(LSECT+ 6)
!     EZ    = -ZR(LSECT+ 7)
!
!     --- SECTION FINALE ---
    lsect2 = lsect + 11
    a2 = zr(lsect2+1)
    xiy2 = zr(lsect2+2)
    xiz2 = zr(lsect2+3)
    alfay2 = zr(lsect2+4)
    alfaz2 = zr(lsect2+5)
    ey = - (zr(lsect+6)+zr(lsect2+6))/deux
    ez = - (zr(lsect+7)+zr(lsect2+7))/deux
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2+ (zr(lx+5)-zr(lx+2))**2+ (zr(lx+6)-zr(lx+3))**2 )
    if (xl .eq. zero) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
    endif
!
    if (nomte .eq. 'MECA_POU_D_E') then
!        --- POUTRE DROITE D'EULER A 6 DDL ---
        istruc = 1
        alfay = zero
        alfaz = zero
        alfay2 = zero
        alfaz2 = zero
    else if (nomte.eq.'MECA_POU_D_T') then
!        --- POUTRE DROITE DE TIMOSKENKO A 6 DDL ---
        istruc = 1
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
        call u2mesk('F', 'ELEMENTS2_42', 1, ch16)
    endif
!
!     --- CALCUL DE LA MATRICE DE MASSE LOCALE
!
    if (itype .lt. 10) then
!        --- POUTRE DROITE SECTION CONSTANTE OU VARIABLE (1 OU 2)
        call ptma01(kanl, itype, mlv, istruc, rho,&
                    e, a, a2, xl, xiy,&
                    xiy2, xiz, xiz2, g, alfay,&
                    alfay2, alfaz, alfaz2, ey, ez)
!
    else if (itype.eq.10) then
!        --- POUTRE COURBE SECTION CONSTANTE ---
        call ptma10(mlv, rho, a, xl, x2iy,&
                    x2iz)
    endif
!
end subroutine
