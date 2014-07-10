subroutine ndxnpa(modele, mate, carele, fonact, sdimpr,&
                  sddisc, sddyna, sdnume, numedd, numins,&
                  valinc, solalg)
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
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/isnnem.h"
#include "asterfort/copisd.h"
#include "asterfort/diinst.h"
#include "asterfort/dismoi.h"
#include "asterfort/initia.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/ndnpas.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmimpa.h"
#include "asterfort/nmvcle.h"
    integer :: fonact(*)
    character(len=19) :: sddyna, sdnume, sddisc
    character(len=24) :: modele, mate, carele
    character(len=24) :: sdimpr
    integer :: numins
    character(len=24) :: numedd
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! INITIALISATION DES CHAMPS D'INCONNUES POUR UN NOUVEAU PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  MATE   : CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  NUMEDD : NUME_DDL
! IN  NUMINS : NUMERO INSTANT COURANT
! IN  SDIMPR : SD AFFICHAGE
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDNUME : NOM DE LA SD NUMEROTATION
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
! ----------------------------------------------------------------------
!
    aster_logical :: lgrot
    aster_logical :: scotch
    integer :: neq
    character(len=19) :: depmoi, varmoi
    character(len=19) :: depplu, varplu, vitplu, accplu
    character(len=19) :: complu, depdel
    real(kind=8) :: instap
    integer :: jdepde
    integer :: indro
    real(kind=8), pointer :: depp(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    instap = diinst(sddisc,numins)
    scotch = .false.
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
!
! --- FONCTIONNALITES ACTIVEES
!
    lgrot = isfonc(fonact,'GD_ROTA')
!
! --- ELEMENTS DE STRUCTURES EN GRANDES ROTATIONS
!
    if (lgrot) then
        call jeveuo(sdnume(1:19)//'.NDRO', 'L', indro)
    else
        indro = isnnem()
    endif
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'VARMOI', varmoi)
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
    call nmchex(valinc, 'VALINC', 'VARPLU', varplu)
    call nmchex(valinc, 'VALINC', 'COMPLU', complu)
    call nmchex(solalg, 'SOLALG', 'DEPDEL', depdel)
!
! --- TRAITEMENT DES VARIABLES DE COMMANDE
!
    call nmvcle(modele, mate, carele, instap, complu)
!
! --- ESTIMATIONS INITIALES DES VARIABLES INTERNES
!
    call copisd('CHAMP_GD', 'V', varmoi, varplu)
!
! --- INITIALISATION DES DEPLACEMENTS
!
    call copisd('CHAMP_GD', 'V', depmoi, depplu)
!
! --- INITIALISATION DE L'INCREMENT DE DEPLACEMENT DEPDEL
!
    call jeveuo(depdel//'.VALE', 'E', jdepde)
    call jeveuo(depplu//'.VALE', 'L', vr=depp)
    call initia(neq, lgrot, zi(indro), depp, zr(jdepde))
!
! --- INITIALISATIONS EN DYNAMIQUE
!
    call ndnpas(fonact, numedd, numins, sddisc, sddyna,&
                scotch, valinc, solalg)
!
! --- DOIT-ON ACTIVER L'AFFICHAGE POUR CE PAS DE TEMPS ?
!
    call nmimpa(numins, sdimpr)
!
    call jedema()
!
end subroutine
