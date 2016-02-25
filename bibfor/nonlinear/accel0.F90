subroutine accel0(modele    , numedd, numfix     , fonact, lischa,&
                  ds_contact, maprec, solveu     , valinc, sddyna,&
                  ds_measure, ds_algopara, meelem, measse,&
                  veelem    , veasse, solalg)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterfort/copisd.h"
#include "asterfort/detlsp.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/lspini.h"
#include "asterfort/nmassi.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmdebg.h"
#include "asterfort/nmprac.h"
#include "asterfort/nmreso.h"
#include "asterfort/utmess.h"
#include "asterfort/vtzero.h"
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=19) :: solveu, maprec, lischa
    character(len=19) :: sddyna
    type(NL_DS_Measure), intent(inout) :: ds_measure
    character(len=24) :: numedd, numfix, modele
    type(NL_DS_Contact), intent(in) :: ds_contact
    character(len=19) :: meelem(*), measse(*), veasse(*), veelem(*)
    character(len=19) :: solalg(*), valinc(*)
    integer :: fonact(*)
    type(NL_DS_AlgoPara), intent(in) :: ds_algopara
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (DYNAMIQUE)
!
! CALCUL DE L'ACCELERATION INITIALE
!
! ----------------------------------------------------------------------
!
!
!     ==> ON SUPPOSE QUE LA VITESSE INITIALE EST NULLE
!                    QUE LES DEPLACEMENTS IMPOSES SONT NULS
!     ==> ON NE PREND EN COMPTE QUE LES CHARGES DYNAMIQUES, CAR LES
!         CHARGES STATIQUES SONT EQUILIBREES PAR LES FORCES INTERNES
!
!
! IN  MODELE : NOM DU MODELE
! IN  NUMEDD : NUME_DDL (VARIABLE AU COURS DU CALCUL)
! IN  NUMFIX : NUME_DDL (FIXE AU COURS DU CALCUL)
! IN  LISCHA : LISTE DES CHARGES
! In  ds_contact       : datastructure for contact management
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IO  ds_measure       : datastructure for measure and statistics management
! IN  SDDYNA : SD POUR LA DYNAMIQUE
! IN  VEELEM : VARIABLE CHAPEAU POUR NOM DES VECT_ELEM
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! In  ds_algopara      : datastructure for algorithm parameters
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    integer :: neq
    integer :: faccvg, rescvg
    character(len=19) :: matass, depso1, depso2
    character(len=19) :: cncine, cncinx, cndonn, k19bla
    character(len=19) :: accmoi
!
! ----------------------------------------------------------------------
!
    call infdbg('MECA_NON_LINE', ifm, niv)
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CALCUL DE L''ACCELERATION INITIALE'
    endif
    call utmess('I', 'MECANONLINE_24')
!
! --- INITIALISATIONS
!
    k19bla = ' '
    cndonn = '&&CNCHAR.DONN'
    cncinx = '&&CNCHAR.CINE'
    matass = '&&ACCEL0.MATASS'
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
!
! --- DECOMPACTION VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'ACCMOI', accmoi)
    call nmchex(veasse, 'VEASSE', 'CNCINE', cncine)
    call nmchex(solalg, 'SOLALG', 'DEPSO1', depso1)
    call nmchex(solalg, 'SOLALG', 'DEPSO2', depso2)
!
! --- ASSEMBLAGE ET FACTORISATION DE LA MATRICE
!
    call nmprac(fonact, lischa, numedd, numfix    , solveu     ,&
                sddyna, ds_measure, ds_contact, ds_algopara,&
                meelem, measse, maprec, matass    , faccvg)
    if (faccvg .eq. 2) then
        call vtzero(accmoi)
        call utmess('A', 'MECANONLINE_69')
        goto 999
    endif
!
! --- CALCUL DU SECOND MEMBRE
!
    call nmassi(modele, numedd, lischa, fonact, sddyna,&
                valinc, veelem, veasse, cndonn, matass)
!
! --- POUR LE CALCUL DE DDEPLA, IL FAUT METTRE CNCINE A ZERO
!
    call copisd('CHAMP_GD', 'V', cncine, cncinx)
    call vtzero(cncinx)
!
! --- RESOLUTION DIRECTE
!
    call nmreso(fonact, cndonn, k19bla, cncinx, solveu,&
                maprec, matass, depso1, depso2, rescvg)
    if (rescvg .eq. 1) then
        call vtzero(accmoi)
        call utmess('A', 'MECANONLINE_70')
        goto 999
    endif
!
! --- DEPENDAMMENT DU SOLVEUR, TRAITEMENT PARTICULIER
!
    call lspini(solveu)
!
! --- RECOPIE SOLUTION
!
    call copisd('CHAMP_GD', 'V', depso1, accmoi)
!
999 continue
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ...... ACCMOI : '
        call nmdebg('VECT', accmoi, ifm)
    endif
!
! --- MENAGE
!
! --- EN PREMIER DE L'EVENTUELLE INSTANCE MUMPS (SI PRE_COND 'LDLT_SP')
    call detlsp(matass, solveu)
! --- EN SECOND DE LA MATRICE ASSEMBLEE
    call detrsd('MATR_ASSE', matass)
!
end subroutine
