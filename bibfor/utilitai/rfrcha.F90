subroutine rfrcha()
    implicit none
!     ------------------------------------------------------------------
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
!     OPERATEUR "RECU_FONCTION"
!     ------------------------------------------------------------------
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/dismoi.h"
#include "asterfort/foattr.h"
#include "asterfort/focste.h"
#include "asterfort/foimpr.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lxlgut.h"
#include "asterfort/ordonn.h"
#include "asterfort/posddl.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesg.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utch19.h"
#include "asterfort/utcmp1.h"
#include "asterfort/utnono.h"
    integer :: lvale, lg1, lg2, iddl, inoeud, nch
    integer :: n1, iret, ivari
    integer :: nm, ngm, npoint, np, nn
    integer :: ngn, ibid, ie, nc, ifm, niv, nusp
    real(kind=8) :: epsi, valr
    complex(kind=8) :: valc
    character(len=1) :: type
    character(len=24) :: valk(2)
    character(len=4) :: typch2
    character(len=8) :: k8b, crit, maille, noma, intres
    character(len=8) :: noeud, cmp, nomgd
    character(len=16) :: nomcmd, typcon, typcha
    character(len=19) :: nomfon, cham19
    character(len=24) :: nogno, nogma
    integer :: iarg, vali
!     ------------------------------------------------------------------
    call jemarq()
! --- RECUPERATION DU NIVEAU D'IMPRESSION
    call infmaj()
    call infniv(ifm, niv)
!
    call getres(nomfon, typcon, nomcmd)
!
    call getvtx(' ', 'CRITERE', 0, iarg, 1,&
                crit, n1)
    call getvr8(' ', 'PRECISION', 0, iarg, 1,&
                epsi, n1)
    intres = 'NON     '
    call getvtx(' ', 'INTERP_NUME', 0, iarg, 1,&
                intres, n1)
!
    npoint = 0
    cmp = ' '
    noeud = ' '
    maille = ' '
    nogma = ' '
    nogno = ' '
    call getvtx(' ', 'MAILLE', 0, iarg, 1,&
                maille, nm)
    call getvtx(' ', 'GROUP_MA', 0, iarg, 1,&
                nogma, ngm)
    call getvis(' ', 'SOUS_POINT', 0, iarg, 1,&
                nusp, np)
    if (np .eq. 0) nusp = 0
    call getvis(' ', 'POINT', 0, iarg, 1,&
                npoint, np)
    call getvtx(' ', 'NOEUD', 0, iarg, 1,&
                noeud, nn)
    call getvtx(' ', 'GROUP_NO', 0, iarg, 1,&
                nogno, ngn)
!
!     -----------------------------------------------------------------
!                      --- CAS D'UN CHAM_GD ---
!     -----------------------------------------------------------------
    call getvid(' ', 'CHAM_GD', 0, iarg, 1,&
                cham19, nch)
    if (nch .ne. 0) then
        call dismoi('F', 'TYPE_SUPERVIS', cham19, 'CHAMP', ibid,&
                    typcha, ie)
        call dismoi('F', 'NOM_MAILLA', cham19, 'CHAMP', ibid,&
                    noma, ie)
        if (typcha(1:7) .eq. 'CHAM_NO') then
!       ----------------------------------
            if (ngn .ne. 0) then
                call utnono(' ', noma, 'NOEUD', nogno, noeud,&
                            iret)
                if (iret .eq. 10) then
                    call u2mesk('F', 'ELEMENTS_67', 1, nogno)
                else if (iret.eq.1) then
                    valk (1) = nogno
                    valk (2) = noeud
                    call u2mesk('A', 'SOUSTRUC_87', 2, valk)
                endif
            endif
            call getvtx(' ', 'NOM_CMP', 0, iarg, 1,&
                        cmp, nc)
            call posddl('CHAM_NO', cham19, noeud, cmp, inoeud,&
                        iddl)
            if (inoeud .eq. 0) then
                lg1 = lxlgut(noeud)
                call u2mesk('F', 'UTILITAI_92', 1, noeud(1:lg1))
            else if (iddl.eq.0) then
                lg1 = lxlgut(noeud)
                lg2 = lxlgut(cmp)
                valk(1) = cmp(1:lg2)
                valk(2) = noeud(1:lg1)
                call u2mesk('F', 'UTILITAI_93', 2, valk)
            endif
            call jeveuo(cham19//'.VALE', 'L', lvale)
            call focste(nomfon, cmp, zr(lvale+iddl-1), 'G')
            goto 10
        else if (typcha(1:9).eq.'CHAM_ELEM') then
!     -----------------------------------
! ---  VERIFICATION DE LA PRESENCE DES MOTS CLE GROUP_MA (OU MAILLE)
! ---  ET GROUP_NO (OU NOEUD OU POINT) DANS LE CAS D'UN CHAM_ELEM
            if (ngm .ne. 0) then
                call utnono(' ', noma, 'MAILLE', nogma, maille,&
                            iret)
                if (iret .eq. 10) then
                    call u2mesk('F', 'ELEMENTS_73', 1, nogma)
                else if (iret.eq.1) then
                    valk (1) = maille
                    call u2mesg('A', 'UTILITAI6_72', 1, valk, 0,&
                                0, 0, 0.d0)
                endif
            endif
            if (ngn .ne. 0) then
                call utnono(' ', noma, 'NOEUD', nogno, noeud,&
                            iret)
                if (iret .eq. 10) then
                    call u2mesk('F', 'ELEMENTS_67', 1, nogno)
                else if (iret.eq.1) then
                    valk (1) = nogno
                    valk (2) = noeud
                    call u2mesk('A', 'SOUSTRUC_87', 2, valk)
                endif
            endif
            call dismoi('F', 'TYPE_CHAMP', cham19, 'CHAMP', ibid,&
                        typch2, ie)
            if (typch2 .eq. 'ELEM') then
                npoint = 1
                nusp = 1
                noeud = ' '
                if (maille .eq. ' ') call u2mess('F', 'CHAMPS_11')
            else if (typch2.eq.'ELNO') then
                nusp = 1
                if (maille .eq. ' ' .or. (noeud.eq.' ' .and. npoint.eq.0)) call u2mess(&
                                                                           'F', 'CHAMPS_12')
            else
                if (maille .eq. ' ' .or. npoint .eq. 0) call u2mess('F', 'CHAMPS_13')
            endif
            call dismoi('F', 'NOM_GD', cham19, 'CHAM_ELEM', ibid,&
                        nomgd, ie)
            call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                        type, ie)
            if (type .ne. 'R') then
                call u2mess('F', 'UTILITAI4_19')
            endif
            call utcmp1(nomgd, ' ', 1, cmp, ivari)
            call utch19(cham19, noma, maille, noeud, npoint,&
                        nusp, ivari, cmp, type, valr,&
                        valc, vali, iret)
            if (iret .eq. 0) then
                call focste(nomfon, cmp, valr, 'G')
            endif
            goto 10
        else
            call u2mesk('F', 'UTILITAI4_20', 1, typcha)
        endif
    endif
!
!     -----------------------------------------------------------------
10  continue
    call foattr(' ', 1, nomfon)
!
!     --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
!         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
    call ordonn(nomfon, 0)
!
    call titre()
    if (niv .gt. 1) call foimpr(nomfon, niv, ifm, 0, k8b)
!
    call jedema()
end subroutine
