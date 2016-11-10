subroutine nmxmat(modelz    , mate       , carele, ds_constitutive, sddisc,&
                  sddyna    , fonact     , numins, iterat         , valinc,&
                  solalg    , lischa     , comref, numedd         , numfix,&
                  ds_measure, ds_algopara, nbmatr, ltypma         , loptme,&
                  loptma    , lcalme     , lassme, lcfint         , meelem,&
                  measse    , veelem     , ldccvg, ds_contact_)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "asterfort/assert.h"
#include "asterfort/diinst.h"
#include "asterfort/nmassm.h"
#include "asterfort/nmcalm.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmrigi.h"
#include "asterfort/nmrinc.h"
#include "asterfort/nmtime.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
    integer :: nbmatr
    character(len=6) :: ltypma(20)
    character(len=16) :: loptme(20), loptma(20)
    aster_logical :: lcalme(20), lassme(20)
    character(len=*) :: modelz
    character(len=*) :: mate
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=24) :: carele
    integer :: numins, iterat, ldccvg
    character(len=19) :: sddisc, sddyna, lischa
    character(len=24) :: numedd, numfix
    character(len=24) :: comref
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
    character(len=19) :: meelem(*), measse(*), veelem(*)
    character(len=19) :: solalg(*), valinc(*)
    integer :: fonact(*)
    aster_logical :: lcfint
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
    type(NL_DS_Contact), optional, intent(in) :: ds_contact_
!
! --------------------------------------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL - UTILITAIRE)
!
! CALCUL ET ASSEMBLAGE DES MATR_ELEM DE LA LISTE
!
! --------------------------------------------------------------------------------------------------
!
! IN  MODELE : MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  MATE   : CHAMP MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  COMREF : VARI_COM DE REFERENCE
! In  ds_constitutive  : datastructure for constitutive laws management
! IN  LISCHA : LISTE DES CHARGES
! IO  ds_measure       : datastructure for measure and statistics management
! In  ds_contact       : datastructure for contact management
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  NUMINS : NUMERO D'INSTANT
! IN  ITERAT : NUMERO D'ITERATION
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! In  ds_algopara      : datastructure for algorithm parameters
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
!
! --------------------------------------------------------------------------------------------------
!
    character(len=6) :: typmat
    integer :: imatr
    character(len=16) :: optcal, optass
    character(len=19) :: matele, matass
    character(len=1) :: base
    real(kind=8) :: instam, instap
    aster_logical :: lcalc, lasse
!
! --------------------------------------------------------------------------------------------------
!
    base   = 'V'
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
    do imatr = 1, nbmatr
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
                call nmrigi(modelz, mate, carele, ds_constitutive,&
                            sddyna, ds_measure, fonact, iterat,&
                            valinc, solalg, comref, meelem, veelem,&
                            optcal, ldccvg)
            else
                if ((typmat.eq.'MEELTC') .or. (typmat.eq.'MEELTF')) then
                    call nmtime(ds_measure, 'Init', 'Cont_Elem')
                    call nmtime(ds_measure, 'Launch', 'Cont_Elem')
                endif
                call nmcalm(typmat, modelz, lischa, mate       , carele,&
                            ds_constitutive, instam, instap, valinc     , solalg,&
                            optcal, base  , meelem, ds_contact_, matele,&
                            fonact)
                if ((typmat.eq.'MEELTC') .or. (typmat.eq.'MEELTF')) then
                    call nmtime(ds_measure, 'Stop', 'Cont_Elem')
                    call nmrinc(ds_measure, 'Cont_Elem')
                endif
            endif
        endif
!
! --- ASSEMBLER MATR_ELEM
!
        if (lasse) then
            call nmtime(ds_measure, 'Init', 'Matr_Asse')
            call nmtime(ds_measure, 'Launch', 'Matr_Asse')
            call nmchex(measse, 'MEASSE', typmat, matass)
            call nmassm(fonact, lischa, numedd, numfix, ds_algopara,&
                        typmat, optass, meelem, matass)
            call nmtime(ds_measure, 'Stop', 'Matr_Asse')
        endif
    end do
!
end subroutine
