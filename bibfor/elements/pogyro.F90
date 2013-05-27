subroutine pogyro(nomte, rho, xnu, icdmat, klv,&
                  nl)
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jevech.h'
    include 'asterfort/pmfitx.h'
    include 'asterfort/ptgy01.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    integer :: icdmat
    character(len=*) :: nomte
    real(kind=8) :: rho, xnu, klv(*)
! ------------------------------------------------------------------
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
! ======================================================================
!     CALCULE LA MATRICE GYROSCOPIQUE DES ELEMENTS DE POUTRE
!
! IN  NOMTE : NOM DU TYPE ELEMENT
!             'MECA_POU_D_E'  'MECA_POU_D_T'  'MECA_POU_D_TG'
!             'MECA_POU_D_EM' 'MECA_POU_D_TGM'
!     ------------------------------------------------------------------
    integer :: nl
!
    character(len=8) :: nomail
    character(len=16) :: ch16
    integer :: lsect, lsect2, lx, istruc, itype, iadzi, iazk24
    real(kind=8) :: zero, deux, rbid, casect(6)
    real(kind=8) :: ey, ez, xl
    real(kind=8) :: a, xiy, xiz, alfay, alfaz, alfinv
    real(kind=8) :: a2, xiy2, xiz2, alfay2, alfaz2
!     ------------------------------------------------------------------
!
    zero = 0.d0
    deux = 2.d0
!
!
!     --- RECUPERATION DES CARACTERISTIQUES GENERALES DES SECTIONS ---
!
    call jevech('PCAGNPO', 'L', lsect)
    lsect = lsect - 1
    itype = nint(zr(lsect+23))
!     --- SECTION INITIALE ---
    a = zr(lsect+1)
    xiy = zr(lsect+2)
    xiz = zr(lsect+3)
    alfay = zr(lsect+4)
    alfaz = zr(lsect+5)
!
!     --- SECTION FINALE ---
    lsect2 = lsect + 11
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
        alfinv = zero
        else if (nomte.eq.'MECA_POU_D_T'.or. nomte.eq.'MECA_POU_D_TG' )&
    then
!        --- POUTRE DROITE DE TIMOSKENKO A 6 DDL ---
        istruc = 1
        alfinv = deux/(alfay+alfaz)
        else if (nomte.eq.'MECA_POU_D_EM' .or. nomte.eq.'MECA_POU_D_TGM'&
    ) then
!        --- POUTRE DROITE MULTI-FIBRES---
        istruc = 1
        itype = 0
        call pmfitx(icdmat, 2, casect, rbid)
!   ON DIVISE PAR RHO
        a = casect(1)/rho
        xiy = casect(5)/rho
        xiz = casect(4)/rho
    else
        ch16 = nomte
        call u2mesk('F', 'ELEMENTS2_42', 1, ch16)
    endif
!
!
    if (itype .eq. 1 .or. itype .eq. 2) then
!     --- POUTRE DROITE A SECTION VARIABLE (TYPE 1 OU 2) ---
        lsect2 = lsect + 11
        a2 = zr(lsect2+1)
        xiy2 = zr(lsect2+2)
        xiz2 = zr(lsect2+3)
        alfay2 = zr(lsect2+4)
        alfaz2 = zr(lsect2+5)
!     ---- MOYENNAGE -------------------------------------
        a=(a+a2)/deux
        xiy=(xiy+xiy2)/deux
        xiz=(xiz+xiz2)/deux
        alfay=(alfay+alfay2)/deux
        alfaz=(alfaz+alfaz2)/deux
        if (nomte(1:12) .eq. 'MECA_POU_D_E') then
            alfinv = zero
        else
            alfinv = deux/(alfay+alfaz)
        endif
    else if (itype.eq.0) then
        if (nomte(1:12) .eq. 'MECA_POU_D_E') then
            alfinv = zero
        else if (nomte(1:12).eq.'MECA_POU_D_T') then
            alfinv = deux/(alfay+alfaz)
        endif
    endif
    call ptgy01(klv, nl, xnu, rho, a,&
                xl, xiy, xiz, alfinv, ey,&
                ez, istruc)
!
end subroutine
