subroutine porigi(nomte, e, xnu, klv)
    implicit none
    include 'jeveux.h'
    include 'asterfort/jevech.h'
    include 'asterfort/ptka01.h'
    include 'asterfort/ptka02.h'
    include 'asterfort/ptka10.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: nomte
    real(kind=8) :: e, xnu, klv(*)
! TOLE CRP_6
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
!     CALCULE LA MATRICE DE RIGIDITE DES ELEMENTS DE POUTRE
!
! IN  NOMTE : NOM DU TYPE ELEMENT
!             'MECA_POU_D_E'  'MECA_POU_D_T'  'MECA_POU_C_T'
!     ------------------------------------------------------------------
!
    character(len=8) :: nomail
    character(len=16) :: ch16
    integer :: iadzi, iazk24
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: istruc, itype, lrcou, lsect, lsect2, lx
    real(kind=8) :: a, a2, alfay, alfay2, alfaz, alfaz2, ang
    real(kind=8) :: angs2, deux, ey, ez, g, rad, un
    real(kind=8) :: xfl, xfly, xflz, xiy, xiy2, xiz, xiz2
    real(kind=8) :: xjx, xjx2, xl, zero
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
    xjx = zr(lsect+8)
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
    xjx2 = zr(lsect2+8)
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
    if (nomte(1:12) .eq. 'MECA_POU_D_E') then
!        --- POUTRE DROITE D'EULER A 6 DDL ---
        istruc = 1
        alfay = zero
        alfaz = zero
        alfay2 = zero
        alfaz2 = zero
    else if (nomte(1:12).eq.'MECA_POU_D_T') then
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
        xiy = xiy/xfly
        xiz = xiz/xflz
        xiy2 = xiy2/xfly
        xiz2 = xiz2/xflz
    else
        ch16 = nomte
        call u2mesk('F', 'ELEMENTS2_42', 1, ch16)
    endif
!
!     --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE
!
    if (itype .eq. 0) then
!        --- POUTRE DROITE A SECTION CONSTANTE ---
        call ptka01(klv, e, a, xl, xiy,&
                    xiz, xjx, g, alfay, alfaz,&
                    ey, ez, istruc)
!
    else if (itype.eq.1 .or. itype.eq.2) then
!        --- POUTRE DROITE A SECTION VARIABLE (TYPE 1 OU 2) ---
        call ptka02(itype, klv, e, a, a2,&
                    xl, xiy, xiy2, xiz, xiz2,&
                    xjx, xjx2, g, alfay, alfay2,&
                    alfaz, alfaz2, ey, ez, istruc)
!
    else if (itype.eq.10) then
!        --- POUTRE COURBE A SECTION CONSTANTE ---
        call ptka10(klv, e, a, xiy, xiz,&
                    xjx, g, alfay, alfaz, rad,&
                    ang, istruc)
    endif
!
end subroutine
