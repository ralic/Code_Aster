subroutine nmcrsu(sddisc, lisins, parcri, limpex, lctcd,&
                  solveu, defico)
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
    implicit none
#include "jeveux.h"
#include "asterc/gettco.h"
#include "asterc/r8vide.h"
#include "asterfort/crsvit.h"
#include "asterfort/crsvsi.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedup1.h"
#include "asterfort/jemarq.h"
#include "asterfort/nmcerr.h"
#include "asterfort/nmcrld.h"
#include "asterfort/u2mess.h"
#include "asterfort/utdidt.h"
#include "asterfort/wkvect.h"
    character(len=19) :: sddisc, lisins, solveu
    character(len=24) :: defico
    real(kind=8) :: parcri(*)
    logical :: limpex, lctcd
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (STRUCTURES DE DONNES)
!
! CREATION SD DISCRETISATION - SUBDIVISION AUTO
!
! ----------------------------------------------------------------------
!
!
! IN  PARCRI : PARAMETRES DES CRITERES DE CONVERGENCE (CF NMDOCN)
! IN  LISINS : SD_LIST_INST OU SD_LISTR8
! IN  SDDISC : SD DISCRETISATION
! IN  LCTCD  : .TRUE. SI CONTACT DISCRET
! IN  LIMPEX : .TRUE. SI IMPLEX
! IN  SOLVEU : SD SOLVEUR
! IN  DEFICO : SD DE DEFINITION DU CONTACT
!
! ----------------------------------------------------------------------
!
    character(len=16) :: pred
    character(len=16) :: metlis, modetp
    integer :: iret
    real(kind=8) :: r8bid, elasdt, valr
    integer :: nadapt, iadapt
    integer :: iter1, iter2, ibid
    integer :: ifm, niv, itmx, vali
    logical :: ldeco, lreapc
    real(kind=8) :: rgmaxi, rgrela, inikry
    character(len=8) :: k8bid
    character(len=19) :: even
    character(len=16) :: typeco, nopara, decoup, reacpc
    character(len=24) :: lisevr, lisevk, lisesu
    character(len=24) :: lisavr, lisavk, listpr, listpk
    character(len=24) :: tpsevr, tpsevk, tpsesu
    character(len=24) :: tpsavr, tpsavk, tpstpr, tpstpk
    character(len=24) :: tpsext
    integer :: jtpsex
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<MECANONLINE> ... CREATION SD SUBDIVISION'
    endif
!
! --- INITIALISATIONS
!
    rgmaxi = parcri(3)
    rgrela = parcri(2)
    inikry = 0.9d0
    call utdidt('L', sddisc, 'LIST', ibid, 'NADAPT',&
                r8bid, nadapt, k8bid)
    call utdidt('L', sddisc, 'LIST', ibid, 'METHODE',&
                r8bid, ibid, metlis)
!
! --- NOM SDS DE LA LISINS
!
    lisevr = lisins(1:8)//'.ECHE.EVENR'
    lisevk = lisins(1:8)//'.ECHE.EVENK'
    lisesu = lisins(1:8)//'.ECHE.SUBDR'
    lisavr = lisins(1:8)//'.ADAP.EVENR'
    lisavk = lisins(1:8)//'.ADAP.EVENK'
    listpr = lisins(1:8)//'.ADAP.TPLUR'
    listpk = lisins(1:8)//'.ADAP.TPLUK'
!
! --- NOM SDS DE LA SDDISC
!
    tpsevr = sddisc(1:19)//'.EEVR'
    tpsevk = sddisc(1:19)//'.EEVK'
    tpsesu = sddisc(1:19)//'.ESUR'
    tpsavr = sddisc(1:19)//'.AEVR'
    tpsavk = sddisc(1:19)//'.AEVK'
    tpstpr = sddisc(1:19)//'.ATPR'
    tpstpk = sddisc(1:19)//'.ATPK'
!
! --- LECTURE DE LA LISTE D'INSTANTS
!
    call gettco(lisins, typeco)
!
    if (typeco .eq. 'LISTR8_SDASTER') then
!
! ----- CREATION EVENEMENTS ERREURS: ARRET
!
        call nmcrld(sddisc)
    else if (typeco.eq.'LIST_INST') then
!
! ----- COPIE LOCALE DES OBJETS DE LA LISINS
!
        call jedup1(lisevr, 'V', tpsevr)
        call jedup1(lisevk, 'V', tpsevk)
        call jedup1(lisesu, 'V', tpsesu)
        if (nadapt .ne. 0) then
            call jedup1(lisavr, 'V', tpsavr)
            call jedup1(lisavk, 'V', tpsavk)
            call jedup1(listpr, 'V', tpstpr)
            call jedup1(listpk, 'V', tpstpk)
        endif
    endif
!
! --- RECUPERATION DES CRITERES DE CONVERGENCE GLOBAUX
!
    iter1 = nint(parcri(1))
    itmx = iter1
    elasdt = 0.d0
!
! --- SI ON NE DONNE PAS NEWTON/PAS_MINI_ELAS ALORS ON NE DOIT PAS
! --- TENIR COMPTE DE ITER_GLOB_ELAS
!
    call getvr8('NEWTON', 'PAS_MINI_ELAS', iocc=1, scal=valr, nbret=iret)
    if (iret .le. 0) then
        iter2 = iter1
    else
        elasdt = valr
        iter2 = nint(parcri(5))
    endif
!
! --- DECOUPAGE ACTIVE
!
    call utdidt('L', sddisc, 'LIST', ibid, 'EXIS_DECOUPE',&
                r8bid, ibid, decoup)
    ldeco = decoup.eq.'OUI'
!
! --- SI NEWTON/PREDICTION ='DEPL_CALCULE', ALORS ON INTERDIT
! --- LA SUBDIVISION
!
    call getvtx('NEWTON', 'PREDICTION', iocc=1, scal=pred, nbret=iret)
    if (iret .ne. 0) then
        if (pred .eq. 'DEPL_CALCULE') then
            if (ldeco) then
                call u2mess('F', 'SUBDIVISE_99')
            endif
        endif
    endif
!
! --- SI ON DOIT DECOUPER - CAPTURE MATRICE SINGULIERE DANS SOLVEUR
!
    if (ldeco) then
        if (solveu(1:8) .ne. '&&OP0033') then
            call crsvsi(solveu)
        endif
    endif
!
! --- REACTUALISATION AUTOMATIQUE DU PRECONDITIONNEUR LDLT_SP ACTIVEE
!
    call utdidt('L', sddisc, 'LIST', ibid, 'EXIS_REAC_PRECOND',&
                r8bid, ibid, reacpc)
    lreapc = reacpc.eq.'OUI'
!
! --- SI ON DOIT REACTUALISER LDLT_SP - CAPTURE ECHEC SOLVEUR ITERATIF
!
    if (lreapc) then
        if (solveu(1:8) .ne. '&&OP0033') then
            call crsvit(solveu)
        endif
    endif
!
! --- EN GESTION AUTO, AVEC UN CRITERE D'ADAPTATION EN SEUIL SUR
!     NB_ITER_NEWT, ON MET VALE = ITER_GLOB_MAXI/2 SI VALE N'A PAS
!     ETE RENSIGNE DANS DEFI_LIST_INST
!     ON NE CONSIDERE PAS LE CAS DE DE ITER_GLOB_ELAS CAR C'EST ACTIVE
!     (MATRICE SECANTE) QU'EN CAS DE DIFFICULTE
!
    if (metlis .eq. 'AUTO') then
        call getvis('CONVERGENCE', 'ITER_GLOB_MAXI', iocc=1, scal=itmx, nbret=iret)
        do 25 iadapt = 1, nadapt
            call utdidt('L', sddisc, 'ADAP', iadapt, 'NOM_EVEN',&
                        r8bid, ibid, even)
            if (even .eq. 'SEUIL_SANS_FORMULE') then
                call utdidt('L', sddisc, 'ADAP', iadapt, 'NOM_PARA',&
                            r8bid, ibid, nopara)
                if (nopara .eq. 'NB_ITER_NEWT') then
                    call utdidt('L', sddisc, 'ADAP', iadapt, 'VALE',&
                                r8bid, vali, k8bid)
                    if (vali .eq. 0) then
                        vali = itmx / 2
                        valr = vali
                        call utdidt('E', sddisc, 'ADAP', iadapt, 'VALE',&
                                    valr, ibid, k8bid)
                    endif
                endif
            endif
25      continue
    endif
!
! --- VERIF COHERENCE AVEC IMPLEX
!
    if (metlis .eq. 'AUTO') then
        do 27 iadapt = 1, nadapt
            call utdidt('L', sddisc, 'ADAP', iadapt, 'METHODE',&
                        r8bid, ibid, modetp)
            if (modetp .eq. 'IMPLEX') then
                if (.not.limpex) call u2mess('F', 'MECANONLINE6_4')
            endif
27      continue
    endif
!
! --- CREATION SD STOCKAGE DES INFOS EN COURS DE CALCUL
!
    call nmcerr(sddisc, iter1, iter2, elasdt, rgmaxi,&
                rgrela, inikry, lctcd, defico)
!
! --- OBJET POUR PROLONGEMENT DECOUPE
!
    tpsext = sddisc(1:19)//'.AEXT'
    call wkvect(tpsext, 'V V R', 3, jtpsex)
    zr(jtpsex-1+1) = r8vide()
    zr(jtpsex-1+2) = r8vide()
    zr(jtpsex-1+3) = r8vide()
!
    call jedema()
!
end subroutine
