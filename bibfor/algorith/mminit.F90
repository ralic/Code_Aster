subroutine mminit(noma, defico, resoco, sddyna, valinc,&
                  sdnume)
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfdisl.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/copisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/misazl.h"
#include "asterfort/mm_cycl_init.h"
#include "asterfort/ndynlo.h"
#include "asterfort/nmchex.h"
#include "asterfort/xmiszl.h"
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
    character(len=8), intent(in) :: noma
    character(len=24), intent(in) :: defico, resoco
    character(len=19), intent(in) :: valinc(*)
    character(len=19), intent(in) :: sddyna
    character(len=19), intent(in) :: sdnume
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - ALGORITHME)
!
! INITIALISATIONS
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD DEFINITION DU CONTACT
! IN  RESOCO : SD RESOLUTION DU CONTACT
! IN  SDDYNA : SD DYNAMIQUE
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
!
! ----------------------------------------------------------------------
!
    integer :: ifm, niv
    aster_logical :: ldyna
    aster_logical :: lctcc, ltfcm, lxfcm
    character(len=19) :: depgeo, deplam,depinit
    character(len=19) :: vitini, accini
    character(len=19) :: depmoi, accplu, vitplu
    character(len=19) :: xseuco, xseucp
    character(len=19) :: xindco, xmemco, xindcp, xmemcp, xcohes, xcohep
    character(len=24) :: tabfin, etatct
    integer :: jtabf, jetat
    integer :: ztabf, zetat
    integer :: ipc, ntpc
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('CONTACT', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<CONTACT> INITIALISATIONS'
    endif
!
! --- FONCTIONNALITES ACTIVEES
!
    ldyna = ndynlo(sddyna,'DYNAMIQUE')
    ltfcm = cfdisl(defico,'CONT_XFEM_GG')
    lctcc = cfdisl(defico,'FORMUL_CONTINUE')
    lxfcm = cfdisl(defico,'FORMUL_XFEM')
!
! --- PARAMETRES
!
    ztabf = cfmmvd('ZTABF')
    zetat = cfmmvd('ZETAT')
    ntpc = cfdisi(defico,'NTPC' )
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(valinc, 'VALINC', 'VITPLU', vitplu)
    call nmchex(valinc, 'VALINC', 'ACCPLU', accplu)
!
! --- MISE A ZERO LAGRANGIENS (LAMBDA TOTAUX)
!
!
! --- SAUVEGARDE DES DEPLACEMENTS/LAGS_C INST MOINS 
!     POUR EVENTUEL TRAITEMENT DU SEUIL_AUTO
!

    if (ltfcm) then
        call xmiszl(depmoi, defico, noma)
    else if (lctcc) then
        depinit = resoco(1:14)//'.INIT'
        call copisd('CHAMP_GD', 'V', depmoi, depinit)
        call misazl(sdnume, depmoi)
        if (ldyna) then
            call misazl(sdnume, accplu)
            call misazl(sdnume, vitplu)
        endif
    endif
!
! --- RETABLISSEMENT DE L ETAT DE CONTACT DU DERNIER PAS CONVERGE
! --- POUR PERMETTRE LE REDECOUPAGE (CF. MMMRES)
!
    if (.not.lxfcm) then
        tabfin = resoco(1:14)//'.TABFIN'
        etatct = resoco(1:14)//'.ETATCT'
        call jeveuo(tabfin, 'E', jtabf)
        call jeveuo(etatct, 'L', jetat)
        do ipc = 1, ntpc
            zr(jtabf+ztabf*(ipc-1)+22) = zr(jetat-1+zetat*(ipc-1)+1)
            zr(jtabf+ztabf*(ipc-1)+16) = zr(jetat-1+zetat*(ipc-1)+2)
            zr(jtabf+ztabf*(ipc-1)+17) = zr(jetat-1+zetat*(ipc-1)+3)
        end do
    else
        xindco = resoco(1:14)//'.XFIN'
        xmemco = resoco(1:14)//'.XMEM'
        xindcp = resoco(1:14)//'.XFIP'
        xmemcp = resoco(1:14)//'.XMEP'
        xseuco = resoco(1:14)//'.XFSE'
        xseucp = resoco(1:14)//'.XFSP'
        xcohes = resoco(1:14)//'.XCOH'
        xcohep = resoco(1:14)//'.XCOP'
        call copisd('CHAMP_GD', 'V', xindcp, xindco)
        call copisd('CHAMP_GD', 'V', xmemcp, xmemco)
        call copisd('CHAMP_GD', 'V', xseucp, xseuco)
        call copisd('CHAMP_GD', 'V', xcohep, xcohes)
    endif
!
! --- AFIN QUE LE VECTEUR DES FORCES D'INERTIE NE SOIT PAS MODIFIE AU
! --- COURS DE LA BOUCLE DES CONTRAINTES ACTIVES PAR L'APPEL A OP0070
! --- ON LE DUPLIQUE ET ON UTILISE CETTE COPIE FIXE  (VITINI,ACCINI)
!
    vitini = resoco(1:14)//'.VITI'
    accini = resoco(1:14)//'.ACCI'
    if (ldyna) then
        call copisd('CHAMP_GD', 'V', vitplu, vitini)
        call copisd('CHAMP_GD', 'V', accplu, accini)
    endif
!
! --- SAUVEGARDE DEPLACEMENTS A L'INSTANT MOINS PR BCL GEOMETRIE
!
    depgeo = resoco(1:14)//'.DEPG'
    call copisd('CHAMP_GD', 'V', depmoi, depgeo)
!
! --- SAUVEGARDE DEPLACEMENTS A L'INSTANT MOINS PR BCL FROTTEMENT
!
    deplam = resoco(1:14)//'.DEPF'
    call copisd('CHAMP_GD', 'V', depmoi, deplam)
!
    call jedema()
end subroutine
