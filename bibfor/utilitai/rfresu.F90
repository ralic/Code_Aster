subroutine rfresu()
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
#include "asterfort/focrr2.h"
#include "asterfort/focrr3.h"
#include "asterfort/focrrs.h"
#include "asterfort/foimpr.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ordonn.h"
#include "asterfort/rsutnc.h"
#include "asterfort/titre.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
#include "asterfort/utcmp1.h"
#include "asterfort/utnono.h"
    integer :: nbtrou, numer1, l, n1, iret, ivari
    integer :: nm, ngm, npoint, np, nn, npr, ngn, ibid, ie
    integer :: nres, ifm, niv, nusp, iarg
    real(kind=8) :: epsi
    character(len=8) :: k8b, crit, maille, noma, intres
    character(len=8) :: noeud, cmp, nomgd
    character(len=16) :: nomcmd, typcon, nomcha, npresu
    character(len=19) :: nomfon, cham19, resu
    character(len=24) :: valk(3), nogma, nogno
!
!     ------------------------------------------------------------------
    call jemarq()
!
    call getres(nomfon, typcon, nomcmd)
!
! --- RECUPERATION DU NIVEAU D'IMPRESSION
    call infmaj()
    call infniv(ifm, niv)
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
!                       --- CAS D'UN RESULTAT ---
!     -----------------------------------------------------------------
!
!
!
    call getvid(' ', 'RESULTAT ', 0, iarg, 1,&
                resu, nres)
!
    if (nres .ne. 0) then
        call getvtx(' ', 'NOM_PARA_RESU', 0, iarg, 1,&
                    npresu, npr)
        if (npr .ne. 0) then
            if (intres(1:3) .ne. 'NON') call u2mess('F', 'UTILITAI4_21')
            call focrr3(nomfon, resu, npresu, 'G', iret)
            goto 10
        endif
!
        call getvtx(' ', 'NOM_CHAM', 0, iarg, 1,&
                    nomcha, l)
        call rsutnc(resu, nomcha, 1, cham19, numer1,&
                    nbtrou)
        if (nbtrou .eq. 0) call u2mesk('F', 'UTILITAI4_22', 1, nomcha)
        call dismoi('F', 'NOM_MAILLA', cham19, 'CHAMP', ibid,&
                    noma, ie)
        call dismoi('F', 'NOM_GD', cham19, 'CHAMP', ibid,&
                    nomgd, ie)
        if (ngn .ne. 0) then
            call utnono(' ', noma, 'NOEUD', nogno, noeud,&
                        iret)
            if (iret .eq. 10) then
                call u2mesk('F', 'ELEMENTS_67', 1, nogno)
            else if (iret.eq.1) then
                valk(1) = nogno
                valk(2) = noeud
                call u2mesk('A', 'SOUSTRUC_87', 2, valk)
            endif
        endif
        if (ngm .ne. 0) then
            call utnono(' ', noma, 'MAILLE', nogma, maille,&
                        iret)
            if (iret .eq. 10) then
                call u2mesk('F', 'ELEMENTS_73', 1, nogma)
            else if (iret.eq.1) then
                valk(1) = maille
                call u2mesk('A', 'UTILITAI6_72', 1, valk)
            endif
        endif
        call utcmp1(nomgd, ' ', 1, cmp, ivari)
        if (intres(1:3) .eq. 'NON') then
            call focrrs(nomfon, resu, 'G', nomcha, maille,&
                        noeud, cmp, npoint, nusp, ivari,&
                        iret)
        else
            call focrr2(nomfon, resu, 'G', nomcha, maille,&
                        noeud, cmp, npoint, nusp, ivari,&
                        iret)
        endif
        goto 10
    endif
10  continue
    call foattr(' ', 1, nomfon)
!     --- VERIFICATION QU'ON A BIEN CREER UNE FONCTION ---
!         ET REMISE DES ABSCISSES EN ORDRE CROISSANT
    call ordonn(nomfon, 0)
!
    call titre()
    if (niv .gt. 1) call foimpr(nomfon, niv, ifm, 0, k8b)
!
    call jedema()
end subroutine
