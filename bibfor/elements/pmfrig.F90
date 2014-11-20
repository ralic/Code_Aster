subroutine pmfrig(nomte, icdmat, klv)
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
    implicit none
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/jevech.h"
#include "asterfort/lonele.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfitx.h"
#include "asterfort/pmfk01.h"
#include "asterfort/pmfk21.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/utmess.h"
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
    character(len=16) :: ch16
    integer :: lx
    integer :: inbfib, nbfib, jacf
    real(kind=8) :: g, xjx, gxjx, xl, casect(6)
    real(kind=8) :: cars1(6), a, alfay, alfaz, ey, ez, xjg
!-----------------------------------------------------------------------
    integer, parameter :: nb_cara = 6
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'AY1','AZ1','EY1','EZ1','JX1','JG1'/
!     ------------------------------------------------------------------
!
!
!        --- POUTRE DROITE D'EULER A 6 DDL ---
    if ((nomte.ne.'MECA_POU_D_EM') .and. (nomte.ne.'MECA_POU_D_TGM')) then
        ch16 = nomte
        call utmess('F', 'ELEMENTS2_42', sk=ch16)
    endif
!
!     --- RECUPERATION DES COORDONNEES DES NOEUDS ---
    call lonele(3, lx, xl)
!
!    --- APPEL INTEGRATION SUR SECTION ET CALCUL G TORSION
    call pmfitx(icdmat, 1, casect, g)
!
!     --- RECUPERATION DE LA CONSTANTE DE TORSION
!     --- (A PARTIR DES CARACTERISTIQUES GENERALES DES SECTIONS
!          POUR L'INSTANT)
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
!
!
    if (nomte .eq. 'MECA_POU_D_EM') then
        xjx = vale_cara(5)
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
        alfay = vale_cara(1)
        alfaz = vale_cara(2)
        xjx   = vale_cara(5)
        ey    = vale_cara(3)
        ez    = vale_cara(4)
        xjg   = vale_cara(6)
!
        call pmfk21(klv, casect, a, xl, &
                    xjx, xjg, g, alfay,&
                    alfaz, ey, ez)
    endif
!
end subroutine
