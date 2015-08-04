subroutine nmctcd(modele, mate  , carele, fonact, compor,&
                  carcri, sdtime, sddisc, sddyna, numins,&
                  valinc, solalg, lischa, comref, defico,&
                  resoco, resocu, numedd, veelem, veasse,&
                  measse)
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/cfdisl.h"
#include "asterfort/infdbg.h"
#include "asterfort/isfonc.h"
#include "asterfort/nmcvec.h"
#include "asterfort/nmxvec.h"
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
! aslint: disable=W1504
!
    integer :: fonact(*)
    character(len=24) :: modele
    character(len=24) :: mate, carele
    character(len=24) :: compor, carcri
    integer :: numins
    character(len=19) :: sddisc, sddyna, lischa
    character(len=24) :: defico, resoco, resocu, comref, numedd
    character(len=24) :: sdtime
    character(len=19) :: veelem(*), veasse(*), measse(*)
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CALCUL)
!
! CALCUL ET ASSEMBLAGE DES FORCES LIEES AU CONTACT
! DISCRET ET LIAISON_UNILATER
!
! ----------------------------------------------------------------------
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  RESOCU : SD RESOLUTION LIAISON_UNILATER
! IN  DEFICO : SD DEF. CONTACT
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  SDTIME : SD TIMER
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION (VOIR NMLECT)
! IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION (VOIR NMLECT)
! IN  SOLVEU : SOLVEUR
! IN  CARCRI : PARAMETRES METHODES D'INTEGRATION LOCALES (VOIR NMLECT)
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  NBVECT : NOMBRE DE VECT_ELEM DANS LA LISTE
! IN  LTYPVE : LISTE DES NOMS DES VECT_ELEM
! IN  LOPTVE : LISTE DES OPTIONS DES VECT_ELEM
! IN  LASSVE : SI VECT_ELEM A ASSEMBLER
! IN  LCALVE : SI VECT_ELEM A CALCULER
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: nbvect
    aster_logical :: lunil, lctcd, lctfd, lallv
    aster_logical :: lpenac
    character(len=6) :: ltypve(20)
    character(len=16) :: loptve(20)
    aster_logical :: lassve(20), lcalve(20)
!
! ----------------------------------------------------------------------
!
!
! --- ALL VERIF ?
!
    lallv = cfdisl(defico,'ALL_VERIF')
    if (lallv) then
        goto 99
    endif
!
! - Print
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ...... CALCUL FORCES CONTACT'
    endif
!
! --- INITIALISATIONS
!
    call nmcvec('INIT', ' ', ' ', .false._1, .false._1,&
                nbvect, ltypve, loptve, lcalve, lassve)
!
! --- FONCTIONNALITES ACTIVEES
!
    lctcd = isfonc(fonact,'CONT_DISCRET')
    lctfd = isfonc(fonact,'FROT_DISCRET')
    lpenac = cfdisl(defico,'CONT_PENA')
    lunil = isfonc(fonact,'LIAISON_UNILATER')
!
! --- FORCES DE CONTACT/FROTTEMENT DISCRETS
!
    if (lctcd) then
        call nmcvec('AJOU', 'CNCTDC', ' ', .false._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
    endif
    if ((lctfd) .or. (lpenac)) then
        call nmcvec('AJOU', 'CNCTDF', ' ', .false._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
    endif
!
! --- FORCES DE LIAISON_UNILATER (PAS DE VECT_ELEM)
!
    if (lunil) then
        call nmcvec('AJOU', 'CNUNIL', ' ', .false._1, .true._1,&
                    nbvect, ltypve, loptve, lcalve, lassve)
    endif
!
! --- CALCUL EFFECTIF
!
    call nmxvec(modele, mate  , carele, compor, carcri,&
                sdtime, sddisc, sddyna, numins, valinc,&
                solalg, lischa, comref, resoco, resocu,&
                numedd, veelem, veasse, measse, nbvect,&
                ltypve, lcalve, loptve, lassve)
!
 99 continue
!
end subroutine
