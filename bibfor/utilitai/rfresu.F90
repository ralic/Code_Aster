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
#include "asterfort/dismoi.h"
#include "asterfort/foattr.h"
#include "asterfort/focrr2.h"
#include "asterfort/focrr3.h"
#include "asterfort/focrrs.h"
#include "asterfort/foimpr.h"
#include "asterfort/getvid.h"
#include "asterfort/getvis.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/ordonn.h"
#include "asterfort/rsutnc.h"
#include "asterfort/titre.h"
#include "asterfort/utcmp1.h"
#include "asterfort/utmess.h"
#include "asterfort/utnono.h"
    integer :: nbtrou, numer1(1), l, n1, iret, ivari
    integer :: nm, ngm, npoint, np, nn, npr, ngn
    integer :: nres, ifm, niv, nusp
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
    call getvtx(' ', 'CRITERE', scal=crit, nbret=n1)
    call getvr8(' ', 'PRECISION', scal=epsi, nbret=n1)
    intres = 'NON     '
    call getvtx(' ', 'INTERP_NUME', scal=intres, nbret=n1)
!
    npoint = 0
    cmp = ' '
    noeud = ' '
    maille = ' '
    nogma = ' '
    nogno = ' '
    call getvtx(' ', 'MAILLE', scal=maille, nbret=nm)
    call getvtx(' ', 'GROUP_MA', scal=nogma, nbret=ngm)
    call getvis(' ', 'SOUS_POINT', scal=nusp, nbret=np)
    if (np .eq. 0) nusp = 0
    call getvis(' ', 'POINT', scal=npoint, nbret=np)
    call getvtx(' ', 'NOEUD', scal=noeud, nbret=nn)
    call getvtx(' ', 'GROUP_NO', scal=nogno, nbret=ngn)
!
!     -----------------------------------------------------------------
!                       --- CAS D'UN RESULTAT ---
!     -----------------------------------------------------------------
!
!
!
    call getvid(' ', 'RESULTAT ', scal=resu, nbret=nres)
!
    if (nres .ne. 0) then
        call getvtx(' ', 'NOM_PARA_RESU', scal=npresu, nbret=npr)
        if (npr .ne. 0) then
            if (intres(1:3) .ne. 'NON') then
                call utmess('F', 'UTILITAI4_21')
            endif
            call focrr3(nomfon, resu, npresu, 'G', iret)
            goto 10
        endif
!
        call getvtx(' ', 'NOM_CHAM', scal=nomcha, nbret=l)
        call rsutnc(resu, nomcha, 1, cham19, numer1,&
                    nbtrou)
        if (nbtrou .eq. 0) then
            call utmess('F', 'UTILITAI4_22', sk=nomcha)
        endif
        call dismoi('NOM_MAILLA', cham19, 'CHAMP', repk=noma)
        call dismoi('NOM_GD', cham19, 'CHAMP', repk=nomgd)
        if (ngn .ne. 0) then
            call utnono(' ', noma, 'NOEUD', nogno, noeud,&
                        iret)
            if (iret .eq. 10) then
                call utmess('F', 'ELEMENTS_67', sk=nogno)
            else if (iret.eq.1) then
                valk(1) = nogno
                valk(2) = noeud
                call utmess('A', 'SOUSTRUC_87', nk=2, valk=valk)
            endif
        endif
        if (ngm .ne. 0) then
            call utnono(' ', noma, 'MAILLE', nogma, maille,&
                        iret)
            if (iret .eq. 10) then
                call utmess('F', 'ELEMENTS_73', sk=nogma)
            else if (iret.eq.1) then
                valk(1) = maille
                call utmess('A', 'UTILITAI6_72', sk=valk(1))
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
 10 continue
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
