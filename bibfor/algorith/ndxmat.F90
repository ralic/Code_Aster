subroutine ndxmat(fonact, lischa, solveu, numedd, sddyna,&
                  numins, meelem, measse, matass)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/ascoma.h"
#include "asterfort/detrsd.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mtcmbl.h"
#include "asterfort/mtdefs.h"
#include "asterfort/ndynlo.h"
#include "asterfort/ndynre.h"
#include "asterfort/nmchex.h"
    character(len=19) :: matass
    character(len=19) :: sddyna
    integer :: fonact(*)
    integer :: numins
    character(len=19) :: meelem(*), measse(*)
    character(len=24) :: numedd
    character(len=19) :: lischa, solveu
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CALCUL)
!
! ASSEMBLAGE DE LA MATRICE GLOBALE - CAS EXPLICITE
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  NUMINS : NUMERO D'INSTANT
! IN  NUMEDD : NOM DE LA NUMEROTATION MECANIQUE
! IN  LISCHA : SD LISTE DES CHARGES
! IN  SOLVEU : NOM DU SOLVEUR DE NEWTON
! IN  MEASSE : VARIABLE CHAPEAU POUR NOM DES MATR_ASSE
! IN  MEELEM : VARIABLE CHAPEAU POUR NOM DES MATR_ELEM
! OUT MATASS : MATRICE ASSEMBLEE RESULTANTE
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: l_neum_undead, lshima, lprem
    real(kind=8) :: coemas, coeshi
    character(len=8) :: nomddl
    real(kind=8) :: coemat
    character(len=24) :: limat
    character(len=4) :: typcst
    real(kind=8) :: coemam(2)
    character(len=24) :: limam(2)
    character(len=4) :: typcsm(2)
    integer :: nbmat
    character(len=19) :: rigid, masse
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE><CALC> CALCUL MATRICE GLOBALE'
    endif
!
! --- PREMIER PAS DE TEMPS ?
!
    lprem = numins.le.1
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(measse, 'MEASSE', 'MERIGI', rigid)
    call nmchex(measse, 'MEASSE', 'MEMASS', masse)
    nomddl = ' '
!
! --- FONCTIONNALITES ACTIVEES
!
    l_neum_undead = isfonc(fonact,'NEUM_UNDEAD')
    lshima        = ndynlo(sddyna,'COEF_MASS_SHIFT')
!
! --- SUPPRESSION ANCIENNE MATRICE ASSEMBLEE
!
    call detrsd('MATR_ASSE', matass)
!
! --- COEFFICIENTS POUR MATRICES
!
    coemas = ndynre(sddyna,'COEF_MATR_MASS')
    coeshi = ndynre(sddyna,'COEF_MASS_SHIFT')
!
! --- DECALAGE DE LA MATRICE MASSE (COEF_MASS_SHIFT)
!
    if (lshima .and. lprem) then
        typcsm(1) = 'R'
        typcsm(2) = 'R'
        coemam(1) = 1.d0
        coemam(2) = coeshi
        limam(1) = masse
        limam(2) = rigid
        nbmat = 2
        call mtcmbl(nbmat, typcsm, [coemam], limam, masse,&
                    nomddl, ' ', 'ELIM=')
    endif
!
! --- MATRICES ET COEFFICIENTS
!
    typcst = 'R'
    limat = masse
    nbmat = 1
    coemat = coemas
!
! --- DEFINITION DE LA STRUCTURE DE LA MATRICE
!
    call mtdefs(matass, masse, 'V', 'R')
!
! --- ASSEMBLAGE
!
    call mtcmbl(nbmat, typcst, [coemat], limat, matass,&
                nomddl, ' ', 'ELIM=')
!
! --- PRISE EN COMPTE DE LA MATRICE TANGENTE DES FORCES SUIVEUSES
!
    if (l_neum_undead) then
        call ascoma(meelem, numedd, lischa, matass)
    endif
!
    call jedema()
end subroutine
