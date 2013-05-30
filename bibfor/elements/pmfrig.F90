subroutine pmfrig(nomte, icdmat, klv)
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
    include 'asterc/r8prem.h'
    include 'asterfort/jevech.h'
    include 'asterfort/pmfitg.h'
    include 'asterfort/pmfitx.h'
    include 'asterfort/pmfk01.h'
    include 'asterfort/ptka21.h'
    include 'asterfort/tecael.h'
    include 'asterfort/u2mesk.h'
    character(len=*) :: nomte
    integer :: icdmat
    real(kind=8) :: klv(*)
!     ------------------------------------------------------------------
!     CALCULE LA MATRICE DE RIGIDITE DES ELEMENTS DE POUTRE MULTIFIBRES
!
! IN  NOMTE : NOM DU TYPE ELEMENT
!             'MECA_POU_D_EM'
!             'MECA_POU_D_TGM'
!     ------------------------------------------------------------------
!
    character(len=8) :: nomail
    character(len=16) :: ch16
    integer :: lx, lsect, iadzi, iazk24
    integer :: inbfib, nbfib, jacf
    real(kind=8) :: g, xjx, gxjx, xl, casect(6)
    real(kind=8) :: cars1(6), a, alfay, alfaz, e, xiz, xiy, ey, ez, xjg, ea
!     ------------------------------------------------------------------
!
!
!        --- POUTRE DROITE D'EULER A 6 DDL ---
    if ((nomte.ne.'MECA_POU_D_EM') .and. (nomte.ne.'MECA_POU_D_TGM')) then
        ch16 = nomte
        call u2mesk('F', 'ELEMENTS2_42', 1, ch16)
    endif
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2+ (zr(lx+5)-zr(lx+2))**2+ (zr(lx+6)-zr(lx+3))**2 )
!
    if (xl .le. r8prem()) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
    endif
!
!    --- APPEL INTEGRATION SUR SECTION ET CALCUL G TORSION
    call pmfitx(icdmat, 1, casect, g)
!
!     --- RECUPERATION DE LA CONSTANTE DE TORSION
!     --- (A PARTIR DES CARACTERISTIQUES GENERALES DES SECTIONS
!          POUR L'INSTANT)
    call jevech('PCAGNPO', 'L', lsect)
!
!
    if (nomte .eq. 'MECA_POU_D_EM') then
        xjx = zr(lsect+7)
        gxjx = g*xjx
!       --- CALCUL DE LA MATRICE DE RIGIDITE LOCALE
!        --- POUTRE DROITE A SECTION CONSTANTE ---
        call pmfk01(casect, gxjx, xl, klv)
    else if (nomte.eq.'MECA_POU_D_TGM') then
        call jevech('PNBSP_I', 'L', inbfib)
        nbfib = zi(inbfib)
        call jevech('PFIBRES', 'L', jacf)
        call pmfitg(nbfib, 3, zr(jacf), cars1)
        a = cars1(1)
        xiy = cars1(5)
        xiz = cars1(4)
!
!
!
!
        call jevech('PCAGNPO', 'L', lsect)
        lsect = lsect-1
        alfay = zr(lsect+4)
        alfaz = zr(lsect+5)
        xjx = zr(lsect+8)
        ey = -zr(lsect+6)
        ez = -zr(lsect+7)
        xjg = zr(lsect+12)
!
!
        ea = casect(1)
        e= ea/a
!
!
        call ptka21(klv, e, a, xl, xiy,&
                    xiz, xjx, xjg, g, alfay,&
                    alfaz, ey, ez)
    endif
!
!
end subroutine
