subroutine nmadat(sddisc, numins, nbiter, valinc)
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
! person_in_charge: samuel.geniaut at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8maem.h"
#include "asterc/r8vide.h"
#include "asterfort/compr8.h"
#include "asterfort/diadap.h"
#include "asterfort/diinst.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/juveca.h"
#include "asterfort/nmcadt.h"
#include "asterfort/nmdcei.h"
#include "asterfort/nmjalo.h"
#include "asterfort/utdidt.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=19) :: valinc(*)
    character(len=19) :: sddisc
    integer :: numins, nbiter
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME)
!
! GESTION DE L'ADAPTATION DE PAS DE TEMPS
!   CALCUL DE DTPLUS
!
! ----------------------------------------------------------------------
!
!
! In  sddisc           : datastructure for time discretization
! IN  NUMINS : NUMERO D'INSTANT
! IN  NBITER : NOMBRE D'ITERATIONS DE NEWTON
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
!
!
!
!
    integer :: ifm, niv
    integer :: nb_adapt, i_adapt, jdt
    character(len=19) :: metlis, modetp, dtplus
    real(kind=8) :: r8bid, dt, min, pasmin, pasmax, dtm, jalon
    real(kind=8) :: newins, newdt, deltac
    real(kind=8) :: inst, prec, valr(2)
    real(kind=8) :: insfin, insref
    aster_logical :: ladap, uncrok
    character(len=24) :: tpsite
    integer :: jiter
    integer :: nb_inst, nmax, inspas
    character(len=24) :: tpsext
    integer :: jtpsex
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('MECA_NON_LINE', ifm, niv)
!
! --- INITIALISATIONS
!
    dtplus = '&&NMADAP.DTPLUS'
!
! - Parameters
!
    call utdidt('L', sddisc, 'LIST', 'PAS_MINI',&
                valr_ = pasmin)
    call utdidt('L', sddisc, 'LIST', 'PAS_MAXI',&
                valr_ = pasmax)
    call utdidt('L', sddisc, 'LIST', 'METHODE',&
                valk_ = metlis)
    call utdidt('L', sddisc, 'LIST', 'NBINST',&
                vali_ = nb_inst)
    call utdidt('L', sddisc, 'LIST', 'NB_PAS_MAXI',&
                vali_ = nmax)
!
! --- NOM SDS DE LA SDDISC
!
    tpsite = sddisc(1:19)//'.ITER'
!
! --- PRECISION SUR LES INSTANTS
! --- (LIEE A CELLE DE VAL_MIN DE PAS_MINI DANS DEFI_LIST_INST.CAPY)
!
    prec = 1.d-12
!
! --- ON NE FAIT DE L'ADAPTATION DE PAS DE TEMPS QU'EN GESTION AUTO
!
    if (metlis .eq. 'MANUEL') then
        goto 999
    endif
!
! --- INSTANT COURANT
!
    inst = diinst(sddisc,numins)
!
! --- PROCHAIN INSTANT DE PASSAGE OBLIGATOIRE (JALON) ?
!
    call nmjalo(sddisc, inst, prec, jalon)
    if (jalon .eq. r8vide()) goto 999
!
! --- NOMBRE DE SCHEMAS D'ADAPTATION : NADAPT
!
    call utdidt('L', sddisc, 'LIST', 'NADAPT',&
                vali_ = nb_adapt)
!
! --- LISTE DES NADAPT PAS DE TEMPS POSSIBLES
!
    call wkvect(dtplus, 'V V R', nb_adapt, jdt)
!
! --- PAS DE TEMPS PAR DEFAUT (LE DERNIER, SAUF SI JALON) : DTM
!
    call utdidt('L', sddisc, 'LIST', 'DT-',&
                valr_ = dtm)
!
! --- STOCKAGE DU NOMBRE D'ITERATIONS DE NEWTON ET EXTENSION
!
    call jeveuo(tpsite, 'L', jiter)
    zi(jiter-1+numins) = nbiter
    call juveca(tpsite, nb_inst+1)
!
! ----------------------------------------------------------------------
!    CALCUL DU PAS DE TEMPS
! ----------------------------------------------------------------------
!
    call utmess('I', 'ADAPTATION_1')
!
    do i_adapt = 1, nb_adapt
!
        call utdidt('L', sddisc, 'ADAP', 'METHODE', index_ = i_adapt,&
                    valk_ = modetp)
!
        zr(jdt-1+i_adapt) = r8vide()
!
! ----- DOIT-ON ADAPTER ?
!
        ladap = diadap(sddisc,i_adapt)
        if (ladap) then
            call nmcadt(sddisc, i_adapt, numins, valinc, zr(jdt-1+i_adapt))
        endif
        newdt = zr(jdt-1+i_adapt)
!
! ----- AFFICHAGE
!
        if (newdt .ne. r8vide()) then
            call utmess('I', 'ADAPTATION_2', sk=modetp, sr=newdt)
        else
            call utmess('I', 'ADAPTATION_3', sk=modetp)
        endif
    end do
!
! --- ON CHOISIT LE PLUS PETIT DT PARMI LES NADAPT PAS DE TEMPS
! --- POSSIBLES
! --- SI AUCUN CRITERE N'EST VERIFIE, ON PREND LE PAS DE TEMPS "MOINS"
!
    dt = r8maem()
    uncrok = .false.
    do i_adapt = 1, nb_adapt
        newdt = zr(jdt-1+i_adapt)
        if (newdt .ne. r8vide()) then
            dt = min(dt,newdt)
            uncrok = .true.
        endif
    end do
!
    if (uncrok) then
        call utmess('I', 'ADAPTATION_5', sr=dt)
    else
        dt = dtm
        call utmess('I', 'ADAPTATION_4', sr=dt)
    endif
!
! --- PROJECTION SUR LA BORNE SUP (POUR TOUTES LES METHODES)
!
    if (dt .gt. pasmax) then
!       EMISSION DU MESSAGE D'INFO (SAUF POUR IMPLEX)
        if (modetp(1:6) .ne. 'IMPLEX') then
            valr(1) = dt
            valr(2) = pasmax
            call utmess('I', 'ADAPTATION_12', nr=2, valr=valr)
        endif
        dt = pasmax
    endif
!
! --- PROJECTION SUR LA BORNE INF POUR IMPLEX
! --- (ATTENTION : A FAIRE AVANT L'AJUSTEMENT / JALON)
!
    if ((modetp.eq.'IMPLEX') .or. (modetp.eq.'IMPLEX2')) then
        if (dt .lt. pasmin) dt = pasmin
    endif
!
! --- LA DECOUPE DU PAS DE TEMPS PEUT DONNER UN DELTAT MAXI A RESPECTER
!
    tpsext = sddisc(1:19)//'.AEXT'
    call jeveuo(tpsext, 'E', jtpsex)
    insref = zr(jtpsex-1+1)
    deltac = zr(jtpsex-1+2)
    insfin = zr(jtpsex-1+3)
    if (insref .ne. r8vide()) then
        if (inst .le. insfin) then
            dt = deltac
            call utmess('I', 'ADAPTATION_10', sr=dt)
        else
            zr(jtpsex-1+1) = r8vide()
        endif
    endif
!
! --- AJUSTEMENT DE DT EN FONCTION DU PROCHAIN JALON
!
    if (compr8(inst+dt ,'GT',jalon,prec,1)) then
!       LE NOUVEAU PAS DEPASSE LE PROCHAIN IPO :
!       ON FORCE A Y PASSER ET ON N'ENREGISTRE PAS DT
        dt = jalon-inst
    else if (compr8(inst+dt ,'GT',jalon-pasmin,prec,1)) then
!       NOUVEAU DE PAS INFERIEUR A JALON, MAIS TROP PROCHE DE JALON :
!       ON FORCE A Y PASSER ET ON ENREGISTRE DT
        dt = jalon-inst
        call utdidt('E', sddisc, 'LIST', 'DT-',&
                    valr_ = dt)
    else
!       NOUVEAU PAS DE TEMPS OK
!       ON ENREGISTRE DT
        call utdidt('E', sddisc, 'LIST', 'DT-',&
                    valr_ = dt)
    endif
!
    call utmess('I', 'ADAPTATION_6', sr=dt)
!
! --- ON VERIFIE LES GARDE FOUS
!
    if (modetp(1:6) .ne. 'IMPLEX') then
        if (dt .lt. pasmin) then
            call utmess('F', 'ADAPTATION_11', sr=dt)
        endif
    endif
    if (numins .gt. nmax) then
        call utmess('F', 'ADAPTATION_13')
    endif
!
! --- INSERTION DU NOUVEL INSTANT
!
    inspas = 1
    newins = inst+dt
    call nmdcei(sddisc, numins, [newins], nb_inst, inspas,&
                'ADAP', r8bid)
!
999 continue
!
    call jedetr(dtplus)
    call jedema()
end subroutine
