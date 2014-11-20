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
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/masstg.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfitx.h"
#include "asterfort/pmfm01.h"
#include "asterfort/pmfm21.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/utmess.h"
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
    character(len=16) :: ch16
    integer :: lx, i
    integer :: inbfib, nbfib, jacf
    real(kind=8) :: casrho(6), xl, rbid, cars1(6) ,co12, co13
    real(kind=8) :: matp1(78), a, xiy, xiz, casece(6), g
    real(kind=8) :: alfay, alfaz, ey, ez, casect(6)
!     ------------------------------------------------------------------
    integer, parameter :: nb_cara = 4
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'AY1','AZ1','EY1','EZ1'/
!     ------------------------------------------------------------------
!
!
!        --- POUTRE DROITE D'EULER A 6 DDL ---
    if ((nomte .ne. 'MECA_POU_D_EM') .and. (nomte .ne. 'MECA_POU_D_TGM')) then
        ch16 = nomte
        call utmess('F', 'ELEMENTS2_42', sk=ch16)
    endif
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call lonele(3, lx, xl)
!
!     --- APPEL INTEGRATION SUR SECTION
    call pmfitx(icdmat, 2, casrho, rbid)
!
    if (nomte .eq. 'MECA_POU_D_EM') then
!
!       --- CALCUL DE LA MATRICE DE MASSE LOCALE
        call pmfitx(icdmat, 1, casect, rbid)
        co12=casect(3)/casect(1)
        co13=-casect(2)/casect(1)
        call pmfm01(kanl, xl, co12, co13, casrho,&
                    mlv)
!
    else if (nomte .eq.'MECA_POU_D_TGM') then
!
        call jevech('PNBSP_I', 'L', inbfib)
        nbfib = zi(inbfib)
        call jevech('PFIBRES', 'L', jacf)
        call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
        alfay = vale_cara(1)
        alfaz = vale_cara(2)
        ey    = vale_cara(3)
        ez    = vale_cara(4)
!
        call pmfitg(nbfib, 3, zr(jacf), cars1)
        a = cars1(1)
        xiy = cars1(5)
        xiz = cars1(4)
!
        if (option .eq. 'MASS_FLUI_STRU') then
            casrho(1) = rhoflu * a
            casrho(4) = rhoflu * xiz
            casrho(5) = rhoflu * xiy
        endif
!
!    --- APPEL INTEGRATION SUR SECTION
        call pmfitx(icdmat, 1, casece, g)
!
        do i = 1, 78
            matp1(i) = 0.0d0
        enddo
        call pmfm21(kanl,  matp1, casrho, casece, a, &
                  xl, xiy, xiz, g, alfay,&
                  alfaz, ey, ez )
!
        call masstg(matp1, mlv)
!
    endif
!
!
!
end subroutine
