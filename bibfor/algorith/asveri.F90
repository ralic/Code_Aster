subroutine asveri(knomsy, nbopt, meca, psmo, stat,&
                  tronc, monoap, nbsup, nsupp, nomsup,&
                  ndir, nordr, nbmode)
    implicit none
#include "asterc/getres.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsorac.h"
#include "asterfort/rsutnc.h"
#include "asterfort/rsvpar.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/vrdesc.h"
#include "asterfort/vrnoli.h"
#include "asterfort/vrrefe.h"
    integer :: ndir(*), nordr(*), nsupp(*)
    integer :: vali, nbsup
    character(len=*) :: knomsy(*), meca, psmo, stat, nomsup(nbsup, *)
    logical :: tronc, monoap
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     COMMANDE : COMB_SISM_MODAL
!        VERIFICATION DES OPTIONS DE CALCUL ET DES MODES
!     ------------------------------------------------------------------
! IN  : KNOMSY : VECTEUR DES OPTIONS DE CALCUL
! IN  : NBOPT  : NOMBRE D'OPTIONS DE CALCUL
! IN  : MECA   : MODES MECANIQUES
! IN  : STAT   : MODES STATIQUES
! IN  : TRONC  : PRISE EN COMPTE DE LA TRONCATURE
! IN  : MONOAP : = .TRUE.  , STRUCTURE MONO-SUPPORT
!                = .FALSE. , STRUCTURE MULTI-SUPPORT
! IN  : NBSUP  : NOMBRE DE SUPPORTS
! IN  : NOMSUP : VECTEUR DES NOMS DES SUPPORTS
! IN  : NDIR   : VECTEUR DES DIRECTIONS DE CALCUL
! IN  : NORDR  : NUMERO D'ORDRE DES MODES MECANIQUES
! IN  : NBMODE : NOMBRE DE MODES MECANIQUES
!     ------------------------------------------------------------------
    character(len=4) :: ctyp
    character(len=8) :: k8b, resu, noeu, cmp, nomcmp(3)
    character(len=16) :: nomsy, concep, nomcmd, acces(3), monacc, monpar
    character(len=19) :: chextr, chext2
    character(len=24) :: valk(3)
    complex(kind=8) :: cbid
    integer :: iarg
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: ib, ibid, id, ier, im, in, inum
    integer :: iordr, iret, irt, irt1, irt2, is, nbmode
    integer :: nbopt, nbtrou, ns
    real(kind=8) :: r8b, rb
!-----------------------------------------------------------------------
    data  nomcmp / 'DX' , 'DY' , 'DZ' /
    data  acces  / 'ACCE    X       ' , 'ACCE    Y       ',&
     &               'ACCE    Z       ' /
!     ------------------------------------------------------------------
!
    call getres(resu, concep, nomcmd)
    ier = 0
    call getvtx('DEPL_MULT_APPUI', 'NOM_CAS', iocc=1, nbval=0, nbret=ns)
!
!     --- VERIFICATION DES CHAMPS DONNES ---
    if (monoap) then
        if (tronc) then
            do 10 id = 1, 3
                if (ndir(id) .eq. 1) then
                    call rsorac(psmo, 'NOEUD_CMP', ibid, r8b, acces(id),&
                                cbid, r8b, k8b, iordr, 1,&
                                nbtrou)
                    if (nbtrou .ne. 1) then
                        ier = ier + 1
                        valk (1) = psmo
                        valk (2) = acces(id)
                        call u2mesg('E', 'ALGORITH12_12', 2, valk, 0,&
                                    0, 0, 0.d0)
                        goto 10
                    endif
                    monpar = 'ACCE_IMPO'
                    call rsvpar(psmo, iordr, 'TYPE_DEFO', ib, rb,&
                                monpar, iret)
                    if (iret .ne. 100) then
                        ier = ier + 1
                        valk (1) = psmo
                        valk (2) = acces(id)
                        valk (3) = monpar
                        call u2mesg('E', 'ALGORITH12_13', 3, valk, 0,&
                                    0, 0, 0.d0)
                    endif
                endif
10          continue
        endif
    else
        do 12 id = 1, 3
            if (ndir(id) .eq. 1) then
                do 14 is = 1, nsupp(id)
                    noeu = nomsup(is,id)
                    cmp = nomcmp(id)
                    monacc = noeu//cmp
                    if (ns .ne. 0) then
                        call rsorac(stat, 'NOEUD_CMP', ibid, r8b, monacc,&
                                    cbid, r8b, k8b, iordr, 1,&
                                    nbtrou)
                        if (nbtrou .ne. 1) then
                            ier = ier + 1
                            valk (1) = stat
                            valk (2) = monacc
                            call u2mesg('E', 'ALGORITH12_14', 2, valk, 0,&
                                        0, 0, 0.d0)
                            goto 16
                        endif
                        monpar = 'DEPL_IMPO'
                        call rsvpar(stat, iordr, 'TYPE_DEFO', ib, rb,&
                                    monpar, iret)
                        if (iret .ne. 100) then
                            ier = ier + 1
                            valk (1) = stat
                            valk (2) = monacc
                            valk (3) = monpar
                            call u2mesg('E', 'ALGORITH12_15', 3, valk, 0,&
                                        0, 0, 0.d0)
                        endif
16                      continue
                    endif
                    if (tronc) then
                        call rsorac(psmo, 'NOEUD_CMP', ibid, r8b, monacc,&
                                    cbid, r8b, k8b, iordr, 1,&
                                    nbtrou)
                        if (nbtrou .ne. 1) then
                            ier = ier + 1
                            valk (1) = psmo
                            valk (2) = monacc
                            call u2mesg('E', 'ALGORITH12_12', 2, valk, 0,&
                                        0, 0, 0.d0)
                            goto 14
                        endif
                        monpar = 'ACCE_DDL_IMPO'
                        call rsvpar(psmo, iordr, 'TYPE_DEFO', ib, rb,&
                                    monpar, iret)
                        if (iret .ne. 100) then
                            ier = ier + 1
                            valk (1) = psmo
                            valk (2) = monacc
                            valk (3) = monpar
                            call u2mesg('E', 'ALGORITH12_13', 3, valk, 0,&
                                        0, 0, 0.d0)
                        endif
                    endif
14              continue
            endif
12      continue
    endif
!
!     --- VERIFICATION DES OPTIONS DE CALCUL ---
    do 20 in = 1, nbopt
        nomsy = knomsy(in)
        if (nomsy(1:4) .eq. 'VITE' .and. .not.monoap) then
            valk (1) = nomsy
            call u2mesg('E', 'ALGORITH12_18', 1, valk, 0,&
                        0, 0, 0.d0)
            ier = ier + 1
        endif
        if (nomsy(1:4) .eq. 'VITE') goto 20
        if (nomsy(1:4) .eq. 'ACCE') goto 20
        call rsutnc(meca, nomsy, 0, k8b, ibid,&
                    nbtrou)
        if (nbtrou .eq. 0) then
            ier = ier + 1
            valk (1) = meca
            valk (2) = nomsy
            call u2mesg('E', 'ALGORITH12_7', 2, valk, 0,&
                        0, 0, 0.d0)
            goto 20
        endif
        do 22 im = 1, nbmode
            call rsexch(' ', meca, nomsy, nordr(im), chext2,&
                        iret)
            if (iret .ne. 0) then
                inum = nordr(im)
                ier = ier + 1
                valk (1) = meca
                valk (2) = nomsy
                vali = inum
                call u2mesg('E', 'ALGORITH12_20', 2, valk, 1,&
                            vali, 0, 0.d0)
            endif
22      continue
        if (tronc) then
            call rsutnc(psmo, nomsy, 0, k8b, ibid,&
                        nbtrou)
            if (nbtrou .eq. 0) then
                ier = ier + 1
                valk (1) = psmo
                valk (2) = nomsy
                call u2mesg('E', 'ALGORITH12_21', 2, valk, 0,&
                            0, 0, 0.d0)
            endif
        endif
        if (( .not.monoap ) .and. (ns.ne.0)) then
            call rsutnc(stat, nomsy, 0, k8b, ibid,&
                        nbtrou)
            if (nbtrou .eq. 0) then
                ier = ier + 1
                valk (1) = stat
                valk (2) = nomsy
                call u2mesg('E', 'ALGORITH12_22', 2, valk, 0,&
                            0, 0, 0.d0)
            endif
        endif
20  end do
!
!     --- ON VERIFIE QUE LES CHAM_NOS ET CHAM_ELEMS SONT IDENTIQUES ---
    do 30 in = 1, nbopt
        nomsy = knomsy(in)
        if (nomsy(1:4) .eq. 'VITE') goto 30
        if (nomsy(1:4) .eq. 'ACCE') goto 30
!
!        --- ON RECUPERE LE PREMIER CHAMP ---
        call rsexch('F', meca, nomsy, nordr(1), chextr,&
                    iret)
        call dismoi('F', 'TYPE_CHAMP', chextr, 'CHAMP', ibid,&
                    ctyp, iret)
!
!        --- ON VERIFIE QUE LES SUIVANTS SONT IDENTIQUES ---
        do 32 im = 2, nbmode
            call rsexch('F', meca, nomsy, nordr(im), chext2,&
                        iret)
            if (ctyp(1:2) .eq. 'NO') then
                call vrrefe(chextr, chext2, irt)
            else if (ctyp(1:2).eq.'EL') then
                call vrdesc(chextr, chext2, irt1)
                call vrnoli(chextr, chext2, irt2)
                irt = irt1 + irt2
            endif
            if (irt .ne. 0) then
                ier = ier + 1
                valk(1) = chextr
                valk(2) = chext2
                call u2mesk('E', 'ALGORITH_35', 2, valk)
            endif
32      continue
        if (monoap) then
            if (tronc) then
                do 34 id = 1, 3
                    if (ndir(id) .eq. 1) then
                        call rsorac(psmo, 'NOEUD_CMP', ibid, r8b, acces( id),&
                                    cbid, r8b, k8b, iordr, 1,&
                                    nbtrou)
                        if (nbtrou .eq. 1) then
                            call rsexch('F', psmo, nomsy, iordr, chext2,&
                                        iret)
                            if (ctyp(1:2) .eq. 'NO') then
                                call vrrefe(chextr, chext2, irt)
                            else if (ctyp(1:2).eq.'EL') then
                                call vrdesc(chextr, chext2, irt1)
                                call vrnoli(chextr, chext2, irt2)
                                irt = irt1 + irt2
                            endif
                            if (irt .ne. 0) then
                                ier = ier + 1
                                valk(1) = chextr
                                valk(2) = chext2
                                call u2mesk('E', 'ALGORITH_35', 2, valk)
                            endif
                        endif
                    endif
34              continue
            endif
        else
!
            do 36 id = 1, 3
                if (ndir(id) .eq. 1) then
                    do 38 is = 1, nsupp(id)
                        noeu = nomsup(is,id)
                        cmp = nomcmp(id)
                        monacc = noeu//cmp
                        if (ns .ne. 0) then
                            call rsorac(stat, 'NOEUD_CMP', ibid, r8b, monacc,&
                                        cbid, r8b, k8b, iordr, 1,&
                                        nbtrou)
                            if (nbtrou .eq. 1) then
                                call rsexch('F', stat, nomsy, iordr, chext2,&
                                            iret)
                                if (ctyp(1:2) .eq. 'NO') then
                                    call vrrefe(chextr, chext2, irt)
                                else if (ctyp(1:2).eq.'EL') then
                                    call vrdesc(chextr, chext2, irt1)
                                    call vrnoli(chextr, chext2, irt2)
                                    irt = irt1 + irt2
                                endif
                                if (irt .ne. 0) then
                                    ier = ier + 1
                                    valk(1) = chextr
                                    valk(2) = chext2
                                    call u2mesk('E', 'ALGORITH_35', 2, valk)
                                endif
                            endif
                        endif
                        if (tronc) then
                            call rsorac(psmo, 'NOEUD_CMP', ibid, r8b, monacc,&
                                        cbid, r8b, k8b, iordr, 1,&
                                        nbtrou)
                            if (nbtrou .eq. 1) then
                                call rsexch('F', psmo, nomsy, iordr, chext2,&
                                            iret)
                                if (ctyp(1:2) .eq. 'NO') then
                                    call vrrefe(chextr, chext2, irt)
                                else if (ctyp(1:2).eq.'EL') then
                                    call vrdesc(chextr, chext2, irt1)
                                    call vrnoli(chextr, chext2, irt2)
                                    irt = irt1 + irt2
                                endif
                                if (irt .ne. 0) then
                                    ier = ier + 1
                                    valk(1) = chextr
                                    valk(2) = chext2
                                    call u2mesk('E', 'ALGORITH_35', 2, valk)
                                endif
                            endif
                        endif
38                  continue
                endif
36          continue
        endif
30  end do
!
    if (ier .ne. 0) call u2mess('F', 'ALGORITH_25')
!
end subroutine
