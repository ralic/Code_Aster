subroutine nmcrli(instin, lisins, sddisc)
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
    implicit     none
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/getvr8.h"
#include "asterc/r8vide.h"
#include "asterfort/diinst.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jedupo.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmcrlm.h"
#include "asterfort/nmcrls.h"
#include "asterfort/nmcrpc.h"
#include "asterfort/nmdifi.h"
#include "asterfort/nmdini.h"
#include "asterfort/u2mess.h"
#include "asterfort/utdidt.h"
#include "asterfort/wkvect.h"
    character(len=19) :: sddisc, lisins
    real(kind=8) :: instin
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES)
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
    integer :: numini, numfin, numins
    integer :: ibid, iocc
    integer :: n1
    integer :: nbtemp, nbinst
    real(kind=8) :: tole
    real(kind=8) :: r8bid
    real(kind=8) :: dtmin, dt0
    logical :: linsti, linsei
    character(len=8) :: k8bid, result
    character(len=24) :: tpsipo
    character(len=24) :: tpsrpc, tpspil, tpsdin, tpsite, tpsbcl
    integer :: jreapc, jpil, jnivtp, jiter, jbcle
    character(len=24) :: lisifr, lisdit
    character(len=24) :: tpsinf
    integer :: ifm, niv
    character(len=16) :: typeco, motfac, typres, nomcmd
    character(len=19) :: provli
    integer :: iarg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION SD DISCRETISATION'
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
! --- NOM SDS DE LA SDDISC
!
    tpsdin = sddisc(1:19)//'.DINI'
    tpsbcl = sddisc(1:19)//'.BCLE'
    tpsite = sddisc(1:19)//'.ITER'
    tpsipo = sddisc(1:19)//'.LIPO'
!
    tpsrpc = sddisc(1:19)//'.REPC'
    tpspil = sddisc(1:19)//'.EPIL'
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
! --- OBJET REACTUALISATION DU PRECONDITIONNEUR
!
    call wkvect(tpsrpc, 'V V I', 1, jreapc)
    zi(jreapc) = 0
!
! --- OBJET CHOIX DE LA SOLUTION EQUATION DE PILOTAGE
!
    call wkvect(tpspil, 'V V I', 2, jpil)
    zi(jpil) = 1
    zi(jpil+1) = 1
!
! --- LECTURE DE LA LISTE D'INSTANTS
!
    call gettco(lisins, typeco)
!
! --- CREATION LISTE D'INSTANT PROVISOIRE ET TPSINF
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
    call utdidt('L', sddisc, 'LIST', ibid, 'DTMIN',&
                dtmin, ibid, k8bid)
    call utdidt('L', sddisc, 'LIST', ibid, 'NBINST',&
                r8bid, nbinst, k8bid)
!
! --- ACCES LISTE D'INSTANTS PROVISOIRE
!
    call jeveuo(provli, 'L', jinst)
!
! --- TOLERANCE POUR RECHERCHE DANS LISTE D'INSTANTS
!
    call getvr8(motfac, 'PRECISION', 1, iarg, 1,&
                tole, n1)
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
        call u2mess('F', 'DISCRETISATION_92')
    endif
!
! --- RETAILLAGE DE LA LISTE D'INSTANT PROVISOIRE -> SDDISC.DITR
!
    call nmcrls(sddisc, provli, numini, numfin, linsti,&
                instin, nbtemp, dtmin)
!
! --- INDICATEUR DU NIVEAU DE SUBDIVISION DES PAS DE TEMPS
!
    call wkvect(tpsdin, 'V V I', nbtemp, jnivtp)
    do 30 numins = 1, nbtemp
        zi(jnivtp-1+numins) = 1
30  end do
!
! --- VECTEUR POUR STOCKER ITERAT NEWTON
!
    call wkvect(tpsite, 'V V I', nbtemp, jiter)
!
! --- ENREGISTREMENT DES INFORMATIONS
!
    dt0 = diinst(sddisc,1) - diinst(sddisc,0)
!
    call utdidt('E', sddisc, 'LIST', ibid, 'DT-',&
                dt0, ibid, k8bid)
    call utdidt('E', sddisc, 'LIST', ibid, 'NBINST',&
                r8bid, nbtemp, k8bid)
    call utdidt('E', sddisc, 'LIST', ibid, 'DTMIN',&
                dtmin, ibid, k8bid)
!
! --- STOCKAGE DE LA LISTE DES INSTANTS DE PASSAGE OBLIGATOIRES (JALONS)
!
    call jedupo(sddisc(1:19)//'.DITR', 'V', tpsipo, .false.)
!
! --- CREATION DE LA TABLE DES PARAMETRES CALCULES
!
    call getres(result, typres, nomcmd)
    if (nomcmd .ne. 'CALC_POINT_MAT') then
        call nmcrpc(result)
    endif
!
    call jedetr(provli)
    call jedema()
!
end subroutine
