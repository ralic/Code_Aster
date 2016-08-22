subroutine mdinit(basemo, nbmode, nbnoli, depgen, vitgen,&
                  vint, ier, tinit, reprise, accgen,&
                  index)
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
!
   implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/extrac.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nlget.h"
#include "asterfort/nlsav.h"
#include "asterfort/nltype.h"
#include "asterfort/utmess.h"
    character(len=8) :: basemo
    integer :: nbmode, nbnoli
    real(kind=8) :: depgen(*), vitgen(*)
    real(kind=8), pointer :: vint(:)
    integer :: ier
    real(kind=8) :: tinit
    aster_logical, optional, intent(out) :: reprise
    real(kind=8), optional, intent(out) :: accgen(*)
    integer, optional, intent(out) :: index
!
!
! DONNEES INITIALES
!
! IN  : BASEMO : NOM DU CONCEPT BASE MODALE
! IN  : NBMODE : NOMBRE DE MODES
! IN  : NBNOLI : NOMBRE DE NON LINEARITES
! OUT : DEPGEN : DEPLACEMENTS GENERALISES
! OUT : VITGEN : VITESSES GENERALISEES
! OUT : VINT   : VARIABLES INTERNES 
!                (ON RETOURNE UNE VALEUR UNIQUEMENT SI nbnoli>0 ET QU'ON
!                 EST DANS UN CAS DE REPRISE)
! OUT : IER    : CODE RETOUR
! --------------------------------------------------------------------------------------------------
    integer :: im, ic
    character(len=19) :: nomdep, nomvit
    character(len=8) :: tran, crit, inter
! --------------------------------------------------------------------------------------------------
    integer               :: jdesc, jrefe, jvint, n1, jvind, vmessi(2)
    integer               :: nbinst, nc, ni, np, nt, nbvint, nbnoli0, nl_type
    real(kind=8)          :: prec
    character(len=8)      :: sd_nl
    character(len=24)     :: no1_name, no2_name, vmessk(6), nltype_k0, nltype_k1
    integer     , pointer :: types(:) => null()
    real(kind=8), pointer :: acce(:) => null()
    real(kind=8), pointer :: disc(:) => null()
    real(kind=8), pointer :: depl(:) => null()
    real(kind=8), pointer :: vite(:) => null()
    real(kind=8), pointer :: depi(:) => null()
    real(kind=8), pointer :: viti(:) => null()
    character(len=24), pointer :: inti(:) => null()
! --------------------------------------------------------------------------------------------------
    call jemarq()
    sd_nl = '&&OP29NL'
    ier = 0
    if (present(reprise)) reprise = .false.
!
    nbvint = 0
    if (nbnoli.gt.0) nbvint = size(vint)
!
!     --- DEPLACEMENT ---
    call getvid('ETAT_INIT', 'DEPL', iocc=1, scal=nomdep, nbret=n1)
    if (n1 .ne. 0) then
        call jeveuo(nomdep//'.VALE', 'L', vr=depi)
!
!        --- VERIF COMPATIBILITE DES BASES DE PROJECTION
        call jeveuo(nomdep//'.REFE', 'L', jrefe)
        if (zk24(jrefe)(1:8) .ne. basemo) then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_42')
        endif
        call jeveuo(nomdep//'.DESC', 'L', jdesc)
        if (zi(jdesc+1) .ne. nbmode) then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_43')
        endif
        do im = 1, nbmode
            depgen(im) = depi(im)
        enddo
    endif
!
!     --- VITESSE ---
    call getvid('ETAT_INIT', 'VITE', iocc=1, scal=nomvit, nbret=n1)
    if (n1 .ne. 0) then
        call jeveuo(nomvit//'.VALE', 'L', vr=viti)
!
!        --- VERIF COMPATIBILITE DES BASES DE PROJECTION
        call jeveuo(nomvit//'.REFE', 'L', jrefe)
        if (zk24(jrefe)(1:8) .ne. basemo) then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_42')
        endif
        call jeveuo(nomvit//'.DESC', 'L', jdesc)
        if (zi(jdesc+1) .ne. nbmode) then
            ier = ier + 1
            call utmess('E', 'ALGORITH5_43')
        endif
        do im = 1, nbmode
            vitgen(im) = viti(im)
        enddo
    endif
!
!     --- CAS D UNE REPRISE ---
    call getvid('ETAT_INIT', 'RESULTAT', iocc=1, scal=tran, nbret=nt)
    if (nt .ne. 0) then
!       recuperation du descripteur
        call jeveuo(tran//'           .DESC', 'L', jdesc)
        nbnoli0 = zi(jdesc+2)
        if (nbnoli0 .ne. nbnoli) then
            vmessi(1) = nbnoli0
            vmessi(2) = nbnoli
            call utmess('F', 'ALGORITH5_82', ni=2, vali=vmessi)
        endif
        if (nbnoli .ne. 0) then
!           recuperation des donnees sur les non linearites
            call jeveuo(tran//'        .NL.INTI', 'L', vk24=inti)
            call jeveuo(tran//'        .NL.TYPE', 'L', vi=types)
            do ic = 1, nbnoli
                nltype_k0 = nltype(types(ic))
                call nlget(sd_nl, _NL_TYPE, iocc=ic, iscal=nl_type)
                nltype_k1 = nltype(nl_type)
                call nlget(sd_nl, _NO1_NAME, iocc=ic, kscal=no1_name)
                call nlget(sd_nl, _NO2_NAME, iocc=ic, kscal=no2_name)
                if (    (nltype_k0.ne.nltype_k1)&
                    .or.(inti((ic-1)*5+2).ne.no1_name)&
                    .or.(inti((ic-1)*5+3).ne.no2_name)) then
                    vmessk(1)=nltype_k0
                    vmessk(2)=nltype_k1
                    vmessk(3)=inti((ic-1)*5+2)
                    vmessk(4)=no1_name
                    vmessk(5)=inti((ic-1)*5+3)
                    vmessk(6)=no2_name
                    call utmess('F', 'ALGORITH5_83', nk=6, valk=vmessk)
                endif
            enddo
        endif
!       recuperation des champs depl vite vint
!           les calculs sont donc deja fait au pas de recuperation
        call getvtx('ETAT_INIT', 'CRITERE', iocc=1, scal=crit, nbret=nc)
        call getvr8('ETAT_INIT', 'PRECISION', iocc=1, scal=prec, nbret=np)
        if (nc.eq.0) crit = 'RELATIF'
        if (np.eq.0) prec = 1.d-6
        
        call getvr8('ETAT_INIT', 'INST_INIT', iocc=1, scal=tinit, nbret=ni)
        call jeveuo(tran//'           .DISC', 'E', vr=disc)
        call jelira(tran//'           .DISC', 'LONUTI', nbinst)
        if (ni .eq. 0) tinit = disc(nbinst)
!       Deplacement
        inter = 'NON'
        call jeveuo(tran//'           .DEPL', 'E', vr=depl)
        call extrac(inter, prec, crit, nbinst, disc,&
                    tinit, depl, nbmode, depgen, ier, index)
        if (ier .ne. 0) then
            call utmess('F', 'ALGORITH5_46')
        endif
!       Vitesse
        call jeveuo(tran//'           .VITE', 'E', vr=vite)
        inter = 'NON'
        call extrac(inter, prec, crit, nbinst, disc,&
                    tinit, vite, nbmode, vitgen, ier)
        if (ier .ne. 0) then
            call utmess('F', 'ALGORITH5_47')
        endif
!       Acceleration
        if (present(accgen)) then
            call jeveuo(tran//'           .ACCE', 'E', vr=acce)
            inter = 'NON'
            call extrac(inter, prec, crit, nbinst, disc,&
                        tinit, acce, nbmode, accgen, ier)
            if (ier .ne. 0) then
                call utmess('F', 'ALGORITH5_47')
            endif
        endif
!       Variables internes
        if (nbnoli .gt. 0) then
            call jeveuo(tran//'        .NL.VIND', 'L', jvind)
            nbvint = zi(jvind+nbnoli)-1
            call jeveuo(tran//'        .NL.VINT', 'E', jvint)
            inter = 'NON'
            call extrac(inter, prec, crit, nbinst, disc,&
                        tinit, zr(jvint), nbvint, vint, ier)
            if (ier .ne. 0) then
                call utmess('F', 'ALGORITH5_48')
            endif
        endif
        if (present(reprise)) reprise = .true.
    end if
!
    call jedema()
end subroutine
