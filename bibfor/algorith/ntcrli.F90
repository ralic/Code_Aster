subroutine ntcrli(instin, lisins, sddisc)
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
#include "asterc/gettco.h"
#include "asterc/r8vide.h"
#include "asterfort/diinst.h"
#include "asterfort/getvr8.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcrlm.h"
#include "asterfort/nmcrls.h"
#include "asterfort/nmdifi.h"
#include "asterfort/nmdini.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=19) :: sddisc, lisins
    real(kind=8) :: instin
!
! ----------------------------------------------------------------------
!
! ROUTINE THER_* (STRUCTURES DE DONNES)
!
! CREATION SD DISCRETISATION
!
! ----------------------------------------------------------------------
!
!
! IN  INSTIN : INSTANT INITIAL QUAND ETAT_INIT
!                R8VIDE SI NON DEFINI
! IN  LISINS : LISTE D'INSTANTS (SD_LISTR8 OU SD_LIST_INST)
! OUT SDDISC : SD DISCRETISATION
!
! ----------------------------------------------------------------------
!
    integer :: jinst
    integer :: numini, numfin, nume_inst
    integer :: iocc
    integer :: n1
    integer :: nbtemp, nbinst
    real(kind=8) :: tole
    real(kind=8) :: dtmin, dt0
    aster_logical :: linsti, linsei
    character(len=24) :: lisifr, lisdit
    character(len=24) :: tpsipo, tpsinf
    character(len=24) :: tpsdin, tpsbcl
    integer :: jnivtp, jbcle
    integer :: ifm, niv
    character(len=16) :: typeco, motfac
    character(len=19) :: provli
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infniv(ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<THERNONLINE> ... CREATION SD DISCRETISATION'
    endif
!
! --- INITIALISATIONS
!
    linsti = .false.
    iocc = 1
    motfac = 'INCREMENT'
    provli = '&&NMCRLI.PROVLI'
!
! --- NOM SDS DE LA SDLIST
!
    lisifr = lisins(1:8)//'.LIST.INFOR'
    lisdit = lisins(1:8)//'.LIST.DITR'
!
! --- NOM SD_DISC
!
    tpsdin = sddisc(1:19)//'.DINI'
    tpsbcl = sddisc(1:19)//'.BCLE'
    tpsipo = sddisc(1:19)//'.LIPO'
    tpsinf = sddisc(1:19)//'.LINF'
!
! --- OBJETS NIVEAUX DE BOUCLE
! --- 1 - ITERAT (NEWTON)
! --- 2 - NUMINS (PAS DE TEMPS)
! --- 3 - NIVEAU (BOUCLE CONTACT/XFEM)
! --- 4 - PREMIE (0 SI TOUT PREMIER, 1 SINON)
!
    call wkvect(tpsbcl, 'V V I', 4, jbcle)
!
! --- LECTURE DE LA LISTE D'INSTANTS
!
    call gettco(lisins, typeco)
!
! --- CREATION LISTE D'INSTANT PROV. ET TPSINF
!
    if (typeco .eq. 'LISTR8_SDASTER') then
        call nmcrlm(lisins, sddisc, provli, tpsinf)
    else if (typeco.eq.'LIST_INST') then
        call jedup1(lisdit, 'V', provli)
        call jedup1(lisifr, 'V', tpsinf)
    endif
!
! --- INFOS LISTE D'INSTANTS
!
    call utdidt('L', sddisc, 'LIST', 'DTMIN',&
                valr_ = dtmin)
    call utdidt('L', sddisc, 'LIST', 'NBINST',&
                vali_ = nbinst)
!
! --- ACCES LISTE D'INSTANTS PROVISOIRE
!
    call jeveuo(provli, 'L', jinst)
!
! --- TOLERANCE POUR RECHERCHE DANS LISTE D'INSTANTS
!
    call getvr8(motfac, 'PRECISION', iocc=1, scal=tole, nbret=n1)
    tole = abs(dtmin) * tole
!
! --- L'INSTANT DE L'ETAT INITIAL EXISTE-T-IL ?
!
    if (instin .eq. r8vide()) then
        linsei = .false.
    else
        linsei = .true.
    endif
!
! --- DETERMINATION DU NUMERO D'ORDRE INITIAL
!
    call nmdini(motfac, iocc, provli, instin, linsei,&
                tole, nbinst, linsti, numini)
!
! --- DETERMINATION DU NUMERO D'ORDRE FINAL
!
    call nmdifi(motfac, iocc, provli, tole, nbinst,&
                numfin)
!
! --- VERIFICATION SENS DE LA LISTE
!
    if (numini .ge. numfin) then
        call utmess('F', 'DISCRETISATION_92')
    endif
!
! --- RETAILLAGE DE LA LISTE D'INSTANT PROVISOIRE
!
    call nmcrls(sddisc, provli, numini, numfin, linsti,&
                instin, nbtemp, dtmin)
!
! --- INDICATEUR DU NIVEAU DE SUBDIVISION DES PAS DE TEMPS
!
    call wkvect(tpsdin, 'V V I', nbtemp, jnivtp)
    do nume_inst = 1, nbtemp
        zi(jnivtp-1+nume_inst) = 1
    end do
!
! --- ENREGISTREMENT DES INFORMATIONS
!
    dt0 = diinst(sddisc,1) - diinst(sddisc,0)
!
    call utdidt('E', sddisc, 'LIST', 'DT-',&
                valr_ = dt0)
    call utdidt('E', sddisc, 'LIST', 'NBINST',&
                vali_ = nbtemp)
    call utdidt('E', sddisc, 'LIST', 'DTMIN',&
                valr_ = dtmin)
!
! --- STOCKAGE DE LA LISTE DES INSTANTS DE PASSAGE OBLIGATOIRES
!
    call jedup1(provli, 'V', tpsipo)
!
    call jedetr(provli)
    call jedema()
!
end subroutine
