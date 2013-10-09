subroutine nmnpas(modele, noma, mate, carele, lischa,&
                  fonact, sdimpr, sddisc, sdsuiv, sddyna,&
                  sdnume, sdstat, sdtime, numedd, numins,&
                  conv, defico, resoco, valinc, solalg,&
                  solveu)
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
! aslint: disable=W1504
    implicit none
#include "jeveux.h"
#include "asterc/isnnem.h"
#include "asterc/r8vide.h"
#include "asterfort/cfinit.h"
#include "asterfort/copisd.h"
#include "asterfort/diinst.h"
#include "asterfort/dismoi.h"
#include "asterfort/initia.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mmapin.h"
#include "asterfort/ndnpas.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmimin.h"
#include "asterfort/nmnkft.h"
#include "asterfort/nmvcle.h"
    integer :: fonact(*)
    character(len=8) :: noma
    character(len=19) :: sddyna, sdnume, sddisc, lischa, solveu
    character(len=24) :: modele, mate, carele
    integer :: numins
    real(kind=8) :: conv(*)
    character(len=24) :: sdimpr, sdstat, sdtime, sdsuiv
    character(len=24) :: defico, resoco, numedd
    character(len=19) :: solalg(*), valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! INITIALISATIONS POUR LE NOUVEAU PAS DE TEMPS
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  NOMA   : NOM DU MAILLAGE
! IN  MATE   : CHAMP DE MATERIAU
! IN  CARELE : CARACTERISTIQUES DES ELEMENTS DE STRUCTURE
! IN  LISCHA : LISTE DES CHARGEMENTS
! IN  FONACT : FONCTIONNALITES ACTIVEES
! IN  NUMEDD : NUME_DDL
! IN  NUMINS : NUMERO INSTANT COURANT
! IN  SDIMPR : SD AFFICHAGE
! IN  SDTIME : SD TIMER
! IN  SDSTAT : SD STATISTIQUES
! IN  SDDISC : SD DISCRETISATION TEMPORELLE
! IN  SDSUIV : SD SUIVI_DDL
! IN  SDNUME : NOM DE LA SD NUMEROTATION
! IN  DEFICO : SD DEFINITION DU CONTACT
! IN  RESOCO : SD RESOLUTION DU CONTACT
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! IN  SOLALG : VARIABLE CHAPEAU POUR INCREMENTS SOLUTIONS
!
! ----------------------------------------------------------------------
!
    logical :: lgrot, ldyna, lnkry
    logical :: lcont, leltc, lctcc
    integer :: neq
    character(len=19) :: depmoi, varmoi
    character(len=19) :: depplu, varplu, vitplu, accplu
    character(len=19) :: complu, depdel
    real(kind=8) :: instan
    integer :: jdepp, jdepde
    integer :: indro
    character(len=2) :: codret
    logical :: scotch
    character(len=24) :: mdecol
    integer :: jmdeco
    integer :: iterat
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    call dismoi('NB_EQUA', numedd, 'NUME_DDL', repi=neq)
    scotch = .false.
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    lcont = isfonc(fonact,'CONTACT')
    lgrot = isfonc(fonact,'GD_ROTA')
    lnkry = isfonc(fonact,'NEWTON_KRYLOV')
    leltc = isfonc(fonact,'ELT_CONTACT')
    lctcc = isfonc(fonact,'CONT_CONTINU')
!
! --- REINITIALISATION DU TABLEAU DE CONVERGENCE
!
    conv(3) = r8vide()
    conv(4) = r8vide()
!
! --- INSTANT COURANT
!
    instan = diinst(sddisc,numins)
!
! --- INITIALISATION DES IMPRESSIONS
!
    call nmimin(sdimpr, fonact, sddisc, sdsuiv, numins)
!
! --- POUTRES EN GRANDES ROTATIONS
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
    call nmvcle(modele, mate, carele, lischa, instan,&
                complu, codret)
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
    call jeveuo(depplu//'.VALE', 'L', jdepp)
    call initia(neq, lgrot, zi(indro), zr(jdepp), zr(jdepde))
!
! --- INITIALISATIONS EN DYNAMIQUE
!
    if (ldyna) then
        if (lctcc) then
            mdecol = resoco(1:14)//'.MDECOL'
            call jeveuo(mdecol, 'L', jmdeco)
            scotch = zl(jmdeco+1-1)
        else
            scotch = .false.
        endif
        call ndnpas(fonact, numedd, numins, sddisc, sddyna,&
                    scotch, valinc, solalg)
    endif
!
! --- NEWTON-KRYLOV : COPIE DANS LA SD SOLVEUR DE LA PRECISION DE LA
!                     RESOLUTION POUR LA PREDICTION (FORCING-TERM)
    if (lnkry) then
        iterat=-1
        call nmnkft(solveu, sddisc, iterat)
    endif
!
! --- INITIALISATIONS POUR LE CONTACT
!
    if (lcont) then
        call cfinit(noma, fonact, defico, resoco, numins,&
                    sddyna, valinc)
    endif
!
! --- APPARIEMENT INITIAL POUR CONTACT CONTINU
!
    if (leltc) then
        call mmapin(modele, noma, defico, resoco, numedd,&
                    numins, sdstat, sdtime)
    endif
!
    call jedema()
!
end subroutine
