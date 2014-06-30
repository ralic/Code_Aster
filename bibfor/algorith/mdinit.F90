subroutine mdinit(basemo, nbmode, nbchoc, depgen, vitgen,&
                  vint, ier, tinit, intitu, noecho, &
                  reprise, accgen)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    implicit none
    character(len=8) :: basemo
    integer :: nbmode, nbchoc
    real(kind=8) :: depgen(*), vitgen(*), vint(*)
    integer :: ier
    real(kind=8) :: tinit
    character(len=8), optional, intent(in) :: intitu(*), noecho(nbchoc,*)
    logical(kind=1), optional, intent(out) :: reprise
    real(kind=8), optional, intent(out) :: accgen(*)
!
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
#include "asterfort/mdtr74grd.h"
#include "asterfort/utmess.h"
!
! DONNEES INITIALES
!
! IN  : BASEMO : NOM DU CONCEPT BASE MODALE
! IN  : NBMODE : NOMBRE DE MODES
! IN  : NBCHOC : NOMBRE DE CHOCS
! OUT : DEPGEN : DEPLACEMENTS GENERALISES
! OUT : VITGEN : VITESSES GENERALISEES
! OUT : VINT   : VARIABLES INTERNES (POUR LE FLAMBAGE DE CHOC)
!                (ON RETOURNE UNE VALEUR UNIQUEMENT SI NBCHOC>0 ET QU'ON
!                 EST DANS UN CAS  DE REPRISE)
! OUT : IER    : CODE RETOUR
! --------------------------------------------------------------------------------------------------
    integer :: im,   ic
    character(len=19) :: nomdep, nomvit
    character(len=8) :: tran, crit, inter
! --------------------------------------------------------------------------------------------------
    integer ::  jdesc,  jrefe, jvint,   n1
    integer :: nbinst, nc, ni, np, nt, nbvint, nbchoc0
    real(kind=8) :: prec
    integer :: vmessi(2)
    character(len=8) :: vmessk(6)
    real(kind=8), pointer :: acce(:) => null()
    real(kind=8), pointer :: disc(:) => null()
    real(kind=8), pointer :: depl(:) => null()
    real(kind=8), pointer :: vite(:) => null()
    real(kind=8), pointer :: depi(:) => null()
    real(kind=8), pointer :: viti(:) => null()
    character(len=8), pointer :: ncho(:) => null()
    character(len=8), pointer :: inti(:) => null()
! --------------------------------------------------------------------------------------------------
    call jemarq()
    ier = 0
    if ( present(reprise) ) reprise = .false.
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
!       récupération du descripteur
        call jeveuo(tran//'           .DESC', 'L', jdesc)
        nbchoc0 = zi(jdesc+2)
        if ( nbchoc0 .ne. nbchoc ) then
            vmessi(1) = nbchoc0
            vmessi(2) = nbchoc
            call utmess('F', 'ALGORITH5_82',ni=2,vali=vmessi)
        endif
        if ( nbchoc .ne. 0 ) then
            ASSERT( present(intitu) .and. present(noecho) )
!           récupération des données sur les dipositifs de choc (cf mdallo)
            call jeveuo(tran//'           .INTI', 'L', vk8=inti)
            call jeveuo(tran//'           .NCHO', 'L', vk8=ncho)
            do ic = 1 , nbchoc
                if ( (inti(ic).ne.intitu(ic)).or. &
                     (ncho(ic).ne.noecho(ic,1)).or. &
                     (ncho(1+nbchoc+ic-1).ne.noecho(ic,5)) ) then
                    vmessk(1)=inti(ic)
                    vmessk(2)=intitu(ic)
                    vmessk(3)=ncho(ic)
                    vmessk(4)=noecho(ic,1)
                    vmessk(5)=ncho(1+nbchoc+ic-1)
                    vmessk(6)=noecho(ic,5)
                    call utmess('F', 'ALGORITH5_83',nk=6,valk=vmessk)
                endif
            enddo
        endif
!       récupération des champs depl vite vint
!           les calculs sont donc déjà fait au pas de récupération
        call getvtx('ETAT_INIT', 'CRITERE', iocc=1, scal=crit, nbret=nc)
        call getvr8('ETAT_INIT', 'PRECISION', iocc=1, scal=prec, nbret=np)
        call getvr8('ETAT_INIT', 'INST_INIT', iocc=1, scal=tinit, nbret=ni)
        call jeveuo(tran//'           .DISC', 'E', vr=disc)
        call jelira(tran//'           .DISC', 'LONUTI', nbinst)
        if (ni .eq. 0) tinit = disc(nbinst)
!       Déplacement
        inter = 'NON'
        call jeveuo(tran//'           .DEPL', 'E', vr=depl)
        call extrac(inter, prec, crit, nbinst, disc,&
                    tinit, depl, nbmode, depgen, ier)
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
!       Accelération
        if ( present(accgen) ) then
            call jeveuo(tran//'           .ACCE', 'E', vr=acce)
            inter = 'NON'
            call extrac(inter, prec, crit, nbinst, disc,&
                        tinit, acce, nbmode, accgen, ier)
            if (ier .ne. 0) then
                call utmess('F', 'ALGORITH5_47')
            endif
        endif
!       Variables internes
        if (nbchoc .gt. 0) then
            call jeveuo(tran//'           .VINT', 'E', jvint)
            inter = 'NON'
            nbvint = nbchoc*mdtr74grd('MAXVINT')
            call extrac(inter, prec, crit, nbinst, disc,&
                        tinit, zr(jvint), nbvint, vint, ier)
            if (ier .ne. 0) then
                call utmess('F', 'ALGORITH5_48')
            endif
        endif
        if ( present(reprise) ) reprise = .true.
    endif
!
    call jedema()
end subroutine
