subroutine nmfpas(fonact, sddyna, sdpilo, sddisc, nbiter,&
                  numins, eta, valinc, solalg, veasse)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/copisd.h"
#include "asterfort/diinst.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmadat.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmchsv.h"
#include "asterfort/nmpiac.h"
    character(len=19) :: solalg(*), valinc(*), veasse(*)
    character(len=19) :: sddyna, sdpilo, sddisc
    real(kind=8) :: eta
    integer :: nbiter, numins
    integer :: fonact(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! MISE A JOUR DES INFORMATIONS POUR UN NOUVEAU PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
!
! IN  SDDYNA : SD DYNAMIQUE
! IN  SDPILO : SD PILOTAGE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  ETA    : PARAMETRE DE PILOTAGE
! IN  NUMINS : NUMERO D'INSTANT
! IN  NBITER : NOMBRE D'ITERATIONS DE NEWTON
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
!
!
!
!
    logical :: ldyna, lmpas
    logical :: lpilo
    integer :: jcfsc
    character(len=19) :: depmoi, varmoi, sigmoi, commoi, vitmoi, accmoi
    character(len=19) :: depplu, varplu, sigplu, complu, vitplu, accplu
    character(len=19) :: depdel, depold, strmoi, strplu
    character(len=24) :: cfsc
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lpilo = isfonc(fonact,'PILOTAGE')
    lmpas = ndynlo(sddyna,'MULTI_PAS')
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'VITMOI', vitmoi)
    call nmchex(valinc, 'VALINC', 'ACCMOI', accmoi)
    call nmchex(valinc, 'VALINC', 'SIGMOI', sigmoi)
    call nmchex(valinc, 'VALINC', 'VARMOI', varmoi)
    call nmchex(valinc, 'VALINC', 'COMMOI', commoi)
    call nmchex(valinc, 'VALINC', 'STRMOI', strmoi)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
    call nmchex(valinc, 'VALINC', 'SIGPLU', sigplu)
    call nmchex(valinc, 'VALINC', 'VARPLU', varplu)
    call nmchex(valinc, 'VALINC', 'COMPLU', complu)
    call nmchex(valinc, 'VALINC', 'STRPLU', strplu)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
    call nmchex(solalg, 'SOLALG', 'DEPOLD', depold)
!
! --- ADAPTATION DU NOUVEAU PAS DE TEMPS
!
    call nmadat(sddisc, numins, nbiter, valinc)
!
! --- POUR UNE PREDICTION PAR EXTRAP. DES INCREMENTS DU PAS D'AVANT
!
    call copisd('CHAMP_GD', 'V', depdel, depold)
!
! --- ETAT AU DEBUT DU NOUVEAU PAS DE TEMPS
!
    call copisd('CHAMP_GD', 'V', depplu, depmoi)
    call copisd('CHAMP_GD', 'V', sigplu, sigmoi)
    call copisd('CHAMP_GD', 'V', varplu, varmoi)
    call copisd('VARI_COM', 'V', complu, commoi)
    call copisd('CHAMP_GD', 'V', strplu, strmoi)
!
! --- ETAT AU DEBUT DU NOUVEAU PAS DE TEMPS EN DYNAMIQUE
!
    if (ldyna) then
        call copisd('CHAMP_GD', 'V', vitplu, vitmoi)
        call copisd('CHAMP_GD', 'V', accplu, accmoi)
    endif
!
! --- REACTUALISATION DES BORNES DE PILOTAGE SI DEMANDE
!
    if (lpilo) then
        call nmpiac(sdpilo, eta)
    endif
!
! --- SAUVEGARDE DU SECOND MEMBRE SI MULTI_PAS EN DYNAMIQUE
!
    if (lmpas) then
        call nmchsv(fonact, veasse, sddyna)
!
!   Sauvegarde de INSTAM pour poursuite
!
        cfsc = sddyna(1:15)//'.COEF_SCH'
        call jeveuo(cfsc, 'E', jcfsc)
        zr(jcfsc-1+24) = diinst(sddisc,numins-1)
    endif
!
    call jedema()
!
end subroutine
