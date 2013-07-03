subroutine mdinit(basemo, nbmode, nbchoc, depgen, vitgen,&
                  vint, ier, tinit)
    implicit none
#include "jeveux.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/extrac.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mess.h"
    integer :: nbmode, ier
    real(kind=8) :: depgen(*), vitgen(*), vint(*)
    character(len=8) :: basemo
! ----------------------------------------------------------------------
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
!     DONNEES INITIALES
!     ------------------------------------------------------------------
! IN  : BASEMO : NOM DU CONCEPT BASE MODALE
! IN  : NBMODE : NOMBRE DE MODES
! IN  : NBCHOC : NOMBRE DE CHOCS
! OUT : DEPGEN : DEPLACEMENTS GENERALISES
! OUT : VITGEN : VITESSES GENERALISEES
! OUT : VINT   : VARIABLES INTERNES (POUR LE FLAMBAGE DE CHOC)
!                (ON RETOURNE UNE VALEUR UNIQUEMENT SI NBCHOC>0 ET QU'ON
!                 EST DANS UN CAS  DE REPRISE)
! OUT : IER    : CODE RETOUR
! ----------------------------------------------------------------------
    integer :: im, jdepi, jviti
    character(len=19) :: nomdep, nomvit
    character(len=8) :: tran, crit, inter
    character(len=1) :: k1bid
    integer :: iarg
!     ------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    integer :: jdeplt, jdesc, jinst, jrefe, jvint, jvitet, n1
    integer :: nbchoc, nbinst, nc, ni, np, nt
    real(kind=8) :: prec, tinit
!-----------------------------------------------------------------------
    call jemarq()
    ier = 0
!
!     --- DEPLACEMENT ---
    call getvid('ETAT_INIT', 'DEPL', 1, iarg, 1,&
                nomdep, n1)
    if (n1 .ne. 0) then
        call jeveuo(nomdep//'.VALE', 'L', jdepi)
!
!        --- VERIF COMPATIBILITE DES BASES DE PROJECTION
        call jeveuo(nomdep//'.REFE', 'L', jrefe)
        if (zk24(jrefe)(1:8) .ne. basemo) then
            ier = ier + 1
            call u2mess('E', 'ALGORITH5_42')
        endif
        call jeveuo(nomdep//'.DESC', 'L', jdesc)
        if (zi(jdesc+1) .ne. nbmode) then
            ier = ier + 1
            call u2mess('E', 'ALGORITH5_43')
        endif
        do 10 im = 1, nbmode
            depgen(im) = zr(jdepi+im-1)
10      continue
    endif
!
!     --- VITESSE ---
    call getvid('ETAT_INIT', 'VITE', 1, iarg, 1,&
                nomvit, n1)
    if (n1 .ne. 0) then
        call jeveuo(nomvit//'.VALE', 'L', jviti)
!
!        --- VERIF COMPATIBILITE DES BASES DE PROJECTION
        call jeveuo(nomvit//'.REFE', 'L', jrefe)
        if (zk24(jrefe)(1:8) .ne. basemo) then
            ier = ier + 1
            call u2mess('E', 'ALGORITH5_42')
        endif
        call jeveuo(nomvit//'.DESC', 'L', jdesc)
        if (zi(jdesc+1) .ne. nbmode) then
            ier = ier + 1
            call u2mess('E', 'ALGORITH5_43')
        endif
        do 20 im = 1, nbmode
            vitgen(im) = zr(jviti+im-1)
20      continue
    endif
!
!     --- CAS D UNE REPRISE ---
    call getvid('ETAT_INIT', 'RESULTAT', 1, iarg, 1,&
                tran, nt)
    if (nt .ne. 0) then
!     --- RECUPERATION DES CHAMPS DEPL VITE ET ACCE ---
        call getvtx('ETAT_INIT', 'CRITERE', 1, iarg, 1,&
                    crit, nc)
        call getvr8('ETAT_INIT', 'PRECISION', 1, iarg, 1,&
                    prec, np)
        call getvr8('ETAT_INIT', 'INST_INIT', 1, iarg, 1,&
                    tinit, ni)
        call jeveuo(tran//'           .DEPL', 'E', jdeplt)
        call jeveuo(tran//'           .DISC', 'E', jinst)
        call jelira(tran//'           .DISC', 'LONUTI', nbinst, k1bid)
        if (ni .eq. 0) tinit = zr(jinst+nbinst-1)
        inter = 'NON'
        call extrac(inter, prec, crit, nbinst, zr(jinst),&
                    tinit, zr(jdeplt), nbmode, depgen, ier)
        if (ier .ne. 0) then
            call u2mess('F', 'ALGORITH5_46')
        endif
        call jeveuo(tran//'           .VITE', 'E', jvitet)
        inter = 'NON'
        call extrac(inter, prec, crit, nbinst, zr(jinst),&
                    tinit, zr(jvitet), nbmode, vitgen, ier)
        if (ier .ne. 0) then
            call u2mess('F', 'ALGORITH5_47')
        endif
        if (nbchoc .gt. 0) then
            call jeveuo(tran//'           .VINT', 'E', jvint)
            inter = 'NON'
            call extrac(inter, prec, crit, nbinst, zr(jinst),&
                        tinit, zr(jvint), nbchoc, vint, ier)
            if (ier .ne. 0) then
                call u2mess('F', 'ALGORITH5_48')
            endif
        endif
    endif
!
    call jedema()
end subroutine
