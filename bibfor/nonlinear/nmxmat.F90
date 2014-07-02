subroutine nmxmat(modelz, mate, carele, compor, carcri,&
                  sddisc, sddyna, fonact, numins, iterat,&
                  valinc, solalg, lischa, comref, defico,&
                  resoco, solveu, numedd, numfix, sdstat,&
                  sdtime, nbmatr, ltypma, loptme, loptma,&
                  lcalme, lassme, lcfint, meelem, measse,&
                  veelem, ldccvg, codere)
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
! person_in_charge: mickael.abbas at edf.fr
!
! aslint: disable=W1504
    implicit none
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/diinst.h"
#include "asterfort/nmassm.h"
#include "asterfort/nmcalm.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmrigi.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
    integer :: nbmatr
    character(len=6) :: ltypma(20)
    character(len=16) :: loptme(20), loptma(20)
    aster_logical :: lcalme(20), lassme(20)
    character(len=*) :: modelz
    character(len=*) :: mate
    character(len=24) :: sdtime, sdstat
    character(len=24) :: compor, carcri, carele
    integer :: numins, iterat, ldccvg
    character(len=19) :: sddisc, sddyna, lischa, solveu
    character(len=24) :: defico, resoco
    character(len=24) :: numedd, numfix
    character(len=24) :: comref, codere
    character(len=19) :: meelem(*), measse(*), veelem(*)
    character(len=19) :: solalg(*), valinc(*)
    integer :: fonact(*)
    aster_logical :: lcfint
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! CALCUL ET ASSEMBLAGE DES MATR_ELEM DE LA LISTE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! IN  COMPOR : COMPORTEMENT
! IN  LISCHA : LISTE DES CHARGES
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  RESOCO : SD RESOLUTION CONTACT
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  METHOD : INFORMATIONS SUR LES METHODES DE RESOLUTION (VOIR NMLECT)
! IN  PARMET : PARAMETRES DES METHODES DE RESOLUTION (VOIR NMLECT)
! IN  SOLVEU : SOLVEUR
! IN  CARCRI : PARAMETRES METHODES D'INTEGRATION LOCALES (VOIR NMLECT)
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  ITERAT : NUMERO D'ITERATION
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  NBMATR : NOMBRE DE MATR_ELEM DANS LA LISTE
! IN  LTYPMA : LISTE DES NOMS DES MATR_ELEM
! IN  LOPTME : LISTE DES OPTIONS DES MATR_ELEM
! IN  LOPTMA : LISTE DES OPTIONS DES MATR_ASSE
! IN  LASSME : SI MATR_ELEM A ASSEMBLER
! IN  LCALME : SI MATR_ELEM A CALCULER
! IN  LCFINT : .TRUE. SI VECT_ELEM DES FORCES INTERNES A CALCULER
! OUT LDCCVG : CODE RETOUR DE L'INTEGRATION DU COMPORTEMENT
!                -1 : PAS D'INTEGRATION DU COMPORTEMENT
!                 0 : CAS DU FONCTIONNEMENT NORMAL
!                 1 : ECHEC DE L'INTEGRATION DE LA LDC
!                 2 : ERREUR SUR LA NON VERIF. DE CRITERES PHYSIQUES
!                 3 : SIZZ PAS NUL POUR C_PLAN DEBORST
! OUT CODERE : CHAM_ELEM CODE RETOUR ERREUR INTEGRATION LDC
!
! ----------------------------------------------------------------------
!
    character(len=6) :: typmat
    integer :: imatr
    character(len=16) :: optcal, optass
    character(len=19) :: matele, matass
    character(len=1) :: base
    real(kind=8) :: instam, instap
    aster_logical :: lcalc, lasse
!
! ----------------------------------------------------------------------
!
!
!
! --- INITIALISATIONS
!
    base = 'V'
    instam = diinst(sddisc,numins-1)
    instap = diinst(sddisc,numins)
    ldccvg = -1
!
! --- SI CALCUL DES FORCES INTERNES
!
    if (lcfint) then
        ASSERT(.false.)
    endif
!
! --- CALCUL ET ASSEMBLAGE DES MATR_ELEM
!
    do 10 imatr = 1, nbmatr
!
! --- MATR_ELEM COURANTE
!
        typmat = ltypma(imatr)
        optcal = loptme(imatr)
        optass = loptma(imatr)
        lcalc = lcalme(imatr)
        lasse = lassme(imatr)
!
! --- CALCULER MATR_ELEM
!
        if (lcalc) then
            call nmchex(meelem, 'MEELEM', typmat, matele)
            if (typmat .eq. 'MERIGI') then
                call nmrigi(modelz, mate, carele, compor, carcri,&
                            sddyna, sdstat, sdtime, fonact, iterat,&
                            valinc, solalg, comref, meelem, veelem,&
                            optcal, ldccvg, codere)
            else
                if ((typmat.eq.'MEELTC') .or. (typmat.eq.'MEELTF')) then
                    call nmtime(sdtime, 'INI', 'CTCC_MATR')
                    call nmtime(sdtime, 'RUN', 'CTCC_MATR')
                endif
                call nmcalm(typmat, modelz, lischa, mate, carele,&
                            compor, instam, instap, carcri, valinc,&
                            solalg, optcal, base, meelem, defico,&
                            resoco, matele)
                if ((typmat.eq.'MEELTC') .or. (typmat.eq.'MEELTF')) then
                    call nmtime(sdtime, 'END', 'CTCC_MATR')
                    call nmrinc(sdstat, 'CTCC_MATR')
                endif
            endif
        endif
!
! --- ASSEMBLER MATR_ELEM
!
        if (lasse) then
            call nmtime(sdtime, 'INI', 'ASSE_MATR')
            call nmtime(sdtime, 'RUN', 'ASSE_MATR')
            call nmchex(measse, 'MEASSE', typmat, matass)
            call nmassm(fonact, lischa, solveu, numedd, numfix,&
                        typmat, optass, meelem, matass)
            call nmtime(sdtime, 'END', 'ASSE_MATR')
        endif
 10 end do
!
end subroutine
