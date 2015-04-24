subroutine pmfrig(nomte, icdmat, klv)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
! --------------------------------------------------------------------------------------------------
!
!     CALCUL DE LA MATRICE DE RIGIDITE DES ELEMENTS DE POUTRE MULTIFIBRES
!
! --------------------------------------------------------------------------------------------------
!
!   IN
!       nomte   : nom du type element 'meca_pou_d_em' 'meca_pou_d_tgm'
!       icdmat  : adresse matériau codé
!
!   OUT
!       klv     : matrice de rigidité
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
#include "jeveux.h"
#include "asterfort/lonele.h"
#include "asterfort/pmfinfo.h"
#include "asterfort/pmfitg.h"
#include "asterfort/pmfitx.h"
#include "asterfort/pmfk01.h"
#include "asterfort/pmfk21.h"
#include "asterfort/poutre_modloc.h"
#include "asterfort/utmess.h"
!
    character(len=*) :: nomte
    integer :: icdmat
    real(kind=8) :: klv(*)
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jacf
    real(kind=8) :: g, xjx, gxjx, xl, casect(6)
    real(kind=8) :: cars1(6), a, alfay, alfaz, ey, ez, xjg
    character(len=16) :: ch16
!
    integer :: nbfibr, nbgrfi, tygrfi, nbcarm, nug(10)
! --------------------------------------------------------------------------------------------------
    integer, parameter :: nb_cara = 6
    real(kind=8) :: vale_cara(nb_cara)
    character(len=8) :: noms_cara(nb_cara)
    data noms_cara /'AY1','AZ1','EY1','EZ1','JX1','JG1'/
! --------------------------------------------------------------------------------------------------
!   Poutres droites multifibres
    if ((nomte.ne.'MECA_POU_D_EM') .and. (nomte.ne.'MECA_POU_D_TGM')) then
        ch16 = nomte
        call utmess('F', 'ELEMENTS2_42', sk=ch16)
    endif
!
!   recuperation des coordonnees des noeuds
    xl = lonele()
!
!   Appel intégration sur section et calcul g torsion
    call pmfitx(icdmat, 1, casect, g)
!   Constante de torsion à partir des caractéristiques générales des sections
    call poutre_modloc('CAGNPO', noms_cara, nb_cara, lvaleur=vale_cara)
!
    if (nomte .eq. 'MECA_POU_D_EM') then
        xjx = vale_cara(5)
        gxjx = g*xjx
!       Calcul de la matrice de rigidité locale, poutre droite à section constante
        call pmfk01(casect, gxjx, xl, klv)
    else if (nomte.eq.'MECA_POU_D_TGM') then
!       Récupération des caractéristiques des fibres
        call pmfinfo(nbfibr,nbgrfi,tygrfi,nbcarm,nug,jacf=jacf)
!
        call pmfitg(tygrfi, nbfibr, nbcarm, zr(jacf), cars1)
        a     = cars1(1)
        alfay = vale_cara(1)
        alfaz = vale_cara(2)
        xjx   = vale_cara(5)
        ey    = vale_cara(3)
        ez    = vale_cara(4)
        xjg   = vale_cara(6)
!
        call pmfk21(klv, casect, a, xl, xjx, xjg, g, alfay, alfaz, ey, ez)
    endif
!
end subroutine
