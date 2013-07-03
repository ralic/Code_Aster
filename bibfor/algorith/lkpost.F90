subroutine lkpost(imate, tempd, sigf, nvi, vip)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
#include "asterfort/cos3t.h"
#include "asterfort/lcdevi.h"
#include "asterfort/lcprsc.h"
#include "asterfort/lkcrit.h"
#include "asterfort/rcvala.h"
    integer :: imate, nvi
    real(kind=8) :: tempd, sigf(6), vip(nvi)
! =================================================================
! IN  : IMATE  : ADRESSE DU MATERIAU CODE -------------------------
! --- : TEMPD  : TEMPERATURE BIDON --------------------------------
! --- : NVI    : NOMBRE DE VARIABLES INTERNES ---------------------
! OUT : VIP    : MISE A JOUR DES VARIABLES INTERNES DE POST -------
! =================================================================
    integer :: dimpar
    parameter(dimpar=12)
    integer :: cerr(dimpar)
    real(kind=8) :: mater(dimpar), i1, sii, devsig(6), lgleps, rcos3t
    real(kind=8) :: crit0, crite
    parameter(lgleps=1.0d-8)
    character(len=8) :: nomc(dimpar)
!
! =================================================================
! --- RECUPERATION DES PROPRIETES MATERIAUX -----------------------
! =================================================================
    nomc(1) = 'XI_PIC   '
    nomc(2) = 'XI_E     '
    nomc(3) = 'XI_ULT   '
    nomc(4) = 'A_0      '
    nomc(5) = 'M_0      '
    nomc(6) = 'S_0      '
    nomc(7) = 'XIV_MAX  '
    nomc(8) = 'MV_MAX   '
    nomc(9) = 'SIGMA_C  '
    nomc(10) = 'GAMMA_CJS'
    nomc(11) = 'PA       '
    nomc(12) = 'H0_EXT   '
!
    call rcvala(imate, ' ', 'LETK', 1, 'TEMP',&
                tempd, dimpar, nomc(1), mater(1), cerr(1),&
                0)
!
! =================================================================
! --- DEFINITION DU NIVEAU DE DEGRADATION DE LA ROCHE SUIVANT LE DOMAINE
! --- DOMAINE = 0 : LE COMPORTEMENT RESTE ELASTIQUE
! --- DOMAINE = 1 : LA ROCHE EST FISSUREE PRE-PIC
! --- DOMAINE = 2 : LA ROCHE EST FISSUREE POST-PIC
! --- DOMAINE = 3 : LA ROCHE EST FRACTUREE
! --- DOMAINE = 4 : LA ROCHE EST DANS SON ETAT RESIDUEL
! =================================================================
    if (vip(1) .eq. 0.0d0) then
        vip(8) = 0
    else if (vip(1).lt.mater(1)) then
        vip(8) = 1
    else if (vip(1).lt.mater(2)) then
        vip(8) = 2
    else if (vip(1).lt.mater(3)) then
        vip(8) = 3
    else
        vip(8) = 4
    endif
!
! =================================================================
! --- VARIABLE DE POST-TRAITEMENT POUR SUIVRE L'EVOLUTION DE
! --- L'ETAT DE CONTRAINTE PAR RAPPORT AUX DIFFERENTS SEUILS
! --- INDIC = 1 : LA POSITION DE L'ETAT DE CONTRAINTE EST EN-DESSOUS
! ---           : DU SEUIL D'ENDOMMAGEMENT INITIAL ET AU-DESSUS DU
! ---           : SEUIL DE VISCOSITE MAXIMAL (ATTENTION NOTION
! ---           : DIFFERENTE DE LA DEFINITION INITIALE)
! --- INDIC = 2 : LA POSITION DE L'ETAT DE CONTRAINTE EST EN-DESSOUS
! ---           : DU SEUIL D'ENDOMMAGEMENT INITIAL ET EN-DESSOUS DU
! ---           : SEUIL DE VISCOSITE MAXIMAL
! --- INDIC = 3 : LA POSITION DE L'ETAT DE CONTRAINTE EST AU-DESSUS
! ---           : DU SEUIL D'ENDOMMAGEMENT INITIAL ET EN-DESSOUS DU
! ---           : SEUIL DE VISCOSITE MAXIMAL
! --- INDIC = 4 : LA POSITION DE L'ETAT DE CONTRAINTE EST AU-DESSUS
! ---           : DU SEUIL D'ENDOMMAGEMENT INITIAL ET AU-DESSUS DU
! ---           : SEUIL DE VISCOSITE MAXIMAL
! =================================================================
    call lcdevi(sigf, devsig)
    i1 = -sigf(1)-sigf(2)-sigf(3)
    call lcprsc(devsig, devsig, sii)
    sii = sqrt(sii)
    rcos3t = -cos3t(devsig, mater(11), lgleps)
!
    crit0 = lkcrit(mater(4), mater(5), mater(6), mater(10), mater(9), mater(12), rcos3t, i1, sii)
    crite = lkcrit( 1.0d0, mater(8), mater(6), mater(10), mater(9), mater(12), rcos3t, i1, sii )
!
    if (crit0 .lt. 0.0d0 .and. crite .gt. 0.0d0) then
        vip(9) = 1
    else if (crit0.lt.0.0d0.and.crite.lt.0.0d0) then
        vip(9) = 2
    else if (crit0.gt.0.0d0.and.crite.lt.0.0d0) then
        vip(9) = 3
    else
        vip(9) = 4
    endif
end subroutine
