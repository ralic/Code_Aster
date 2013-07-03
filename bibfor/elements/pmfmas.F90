subroutine pmfmas(nomte, option, rhoflu, icdmat, kanl,&
                  mlv)
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
#include "asterc/r8prem.h"
#include "asterfort/jevech.h"
#include "asterfort/masstg.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfitx.h"
#include "asterfort/pmfm01.h"
#include "asterfort/ptma01.h"
#include "asterfort/tecael.h"
#include "asterfort/u2mesk.h"
    character(len=*) :: nomte, option
    real(kind=8) :: mlv(*), rhoflu
    integer :: kanl, icdmat
!
!     REMARQUE : RHOFLU EST UTILISE QUE POUR MASS_FLUI_STRU
!     ------------------------------------------------------------------
!     CALCULE LA MATRICE DE MASSE DES ELEMENTS DE POUTRE MULTIFIBRES
!
! IN  NOMTE : NOM DU TYPE ELEMENT
!             'MECA_POU_D_EM'
!             'MECA_POU_D_TGM'
!     ------------------------------------------------------------------
!
    character(len=8) :: nomail
    character(len=16) :: ch16
    integer :: lx, iadzi, iazk24, itype, i, istruc
    integer :: inbfib, nbfib, jacf, lsect
    real(kind=8) :: casrho(6), xl, rbid, cars1(6)
    real(kind=8) :: matp1(105), a, xiy, xiz, rho, casece(6), g, e
    real(kind=8) :: alfay, alfaz, ey, ez
!     ------------------------------------------------------------------
!
!
!        --- POUTRE DROITE D'EULER A 6 DDL ---
    if ((nomte .ne. 'MECA_POU_D_EM') .and. (nomte .ne. 'MECA_POU_D_TGM')) then
        ch16 = nomte
        call u2mesk('F', 'ELEMENTS2_42', 1, ch16)
    endif
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call jevech('PGEOMER', 'L', lx)
    lx = lx - 1
    xl = sqrt( (zr(lx+4)-zr(lx+1))**2+ (zr(lx+5)-zr(lx+2))**2+ (zr(lx+6)-zr(lx+3))**2 )
    if (xl .le. r8prem()) then
        call tecael(iadzi, iazk24)
        nomail = zk24(iazk24-1+3)(1:8)
        call u2mesk('F', 'ELEMENTS2_43', 1, nomail)
    endif
!
!     --- APPEL INTEGRATION SUR SECTION
    call pmfitx(icdmat, 2, casrho, rbid)
!
    if (nomte .eq. 'MECA_POU_D_EM') then
!
!       --- CALCUL DE LA MATRICE DE MASSE LOCALE
        call pmfm01(kanl, xl, casrho, mlv)
!
    else if (nomte .eq.'MECA_POU_D_TGM') then
!
        call jevech('PNBSP_I', 'L', inbfib)
        nbfib = zi(inbfib)
        call jevech('PFIBRES', 'L', jacf)
        call jevech('PCAGNPO', 'L', lsect)
        lsect = lsect - 1
        alfay = zr(lsect+4)
        alfaz = zr(lsect+5)
        ey = -zr(lsect+6)
        ez = -zr(lsect+7)
!
!
        call pmfitg(nbfib, 3, zr(jacf), cars1)
        a = cars1(1)
        xiy = cars1(5)
        xiz = cars1(4)
!
        if (option .eq. 'MASS_FLUI_STRU') then
            rho = rhoflu
        else
            rho = casrho(1)/a
        endif
!
        itype = 0
        istruc = 1
!
!    --- APPEL INTEGRATION SUR SECTION
        call pmfitx(icdmat, 1, casece, g)
!
        e = casece(1)/a
!
        do 20 i = 1, 105
            matp1(i) = 0.0d0
20      continue
        call ptma01(kanl, itype, matp1, istruc, rho,&
                    e, a, a, xl, xiy,&
                    xiy, xiz, xiz, g, alfay,&
                    alfay, alfaz, alfaz, ey, ez)
!
        call masstg(matp1, mlv)
!
    endif
!
!
!
end subroutine
