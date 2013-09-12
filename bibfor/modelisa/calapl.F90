subroutine calapl(char, ligrmo, noma)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/alcart.h"
#include "asterfort/codent.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: char, noma
    character(len=*) :: ligrmo
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
!
! BUT : STOCKAGE DES CHARGES REPARTIES DANS UNE CARTE ALLOUEE SUR LE
!       LIGREL DU MODELE ( FORCES DE LAPLACE )
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRMO : NOM DU LIGREL DE MODELE
!      NOMA   : NOM DU MAILLAGE
! ----------------------------------------------------------------------
    integer :: ima, nbflp, jvalv, jncmp, iocc, into, jtran, jno, nbtou, nbma
    integer :: jma, nbma2, jma2, ntra, nsym, jnuma
    real(kind=8) :: rbid
    character(len=8) :: k8b, typmcl(2), typmc2(2)
    character(len=16) :: motclf, motcle(2), motcl2(2), listma, ltrans
    character(len=19) :: carte
    character(len=24) :: mesmai, mesma2, connex
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    motclf = 'INTE_ELEC'
    call getfac(motclf, nbflp)
!
    mesmai = '&&CALAPL.MES_MAILLES'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
    mesma2 = '&&CALAPL.MES_MAILLES_2'
    motcl2(1) = 'GROUP_MA_2'
    motcl2(2) = 'MAILLE_2'
    typmc2(1) = 'GROUP_MA'
    typmc2(2) = 'MAILLE'
!
    connex = noma//'.CONNEX'
    call jelira(connex, 'NMAXOC', into)
!
    do 10 iocc = 1, nbflp
!
        carte(1:17) = char//'.CHME.'//'FL1'
        listma(1:14) = char//'.LISMA'
        ltrans(1:14) = char//'.TRANS'
        call codent(iocc, 'D0', carte(18:19))
        call codent(iocc, 'D0', listma(15:16))
        call codent(iocc, 'D0', ltrans(15:16))
!
        call alcart('G', carte, noma, 'LISTMA')
!
        call jeveuo(carte//'.NCMP', 'E', jncmp)
        call jeveuo(carte//'.VALV', 'E', jvalv)
!
!        STOCKAGE DE VALEURS NULLES SUR TOUT LE MAILLAGE
!
        zk8(jncmp) = 'LISTMA'
        zk8(jncmp+1) = 'TRANS'
        zk16(jvalv) = ' '
        zk16(jvalv+1) = ' '
        call nocart(carte, 1, ' ', 'NOM', 0,&
                    ' ', 0, ligrmo, 2)
!
        call wkvect(ltrans, 'G V R', 6, jtran)
        zr(jtran) = 0.d0
        zr(jtran+1) = 0.d0
        zr(jtran+2) = 0.d0
        zr(jtran+3) = 0.d0
        zr(jtran+4) = 0.d0
        zr(jtran+5) = 0.d0
!
        zk8(jncmp) = 'LISTMA'
        zk8(jncmp+1) = 'TRANS'
        zk16(jvalv) = listma
        zk16(jvalv+1) = ltrans
!
        call getvr8(motclf, 'TRANS', iocc=iocc, nbval=0, nbret=ntra)
        call getvr8(motclf, 'SYME', iocc=iocc, nbval=0, nbret=nsym)
        ntra = -ntra
        nsym = -nsym
        if (ntra .ne. 0) then
            call getvr8(motclf, 'TRANS', iocc=iocc, nbval=ntra, vect=zr(jtran),&
                        nbret=ntra)
        endif
        if (nsym .ne. 0) then
            call getvr8(motclf, 'SYME', iocc=iocc, nbval=nsym, vect=zr(jtran),&
                        nbret=nsym)
        endif
!
! ------ GEOMETRIE DU CONDUCTEUR SECONDAIRE
!
        call reliem(ligrmo, noma, 'NU_MAILLE', motclf, iocc,&
                    2, motcl2, typmc2, mesma2, nbma2)
        if (nbma2 .eq. 0) goto 10
!
! ------ GEOMETRIE DU CONDUCTEUR PRINCIPAL
!
        call getvtx(motclf, 'TOUT', iocc=iocc, scal=k8b, nbret=nbtou)
        if (nbtou .ne. 0) then
            if (nbma2 .eq. 0) then
                call wkvect(listma, 'G V I', 2*into, jnuma)
                do 12 ima = 1, into
                    call jeveuo(jexnum(connex, ima), 'L', jno)
                    zi(jnuma+2*ima-2) = zi(jno)
                    zi(jnuma+2*ima-1) = zi(jno+1)
12              continue
            else
                call jeveuo(mesma2, 'L', jma2)
                call wkvect(listma, 'G V I', 2*nbma2, jnuma)
                do 14 ima = 1, nbma2
                    call jeveuo(jexnum(connex, zi(jma2+ima-1)), 'L', jno)
                    zi(jnuma+2*ima-2) = zi(jno)
                    zi(jnuma+2*ima-1) = zi(jno+1)
14              continue
                call jedetr(mesma2)
            endif
            call nocart(carte, 1, ' ', 'NOM', 0,&
                        ' ', 0, ligrmo, 2)
!
        else
            call reliem(ligrmo, noma, 'NU_MAILLE', motclf, iocc,&
                        2, motcle, typmcl, mesmai, nbma)
            if (nbma .eq. 0) goto 10
            call jeveuo(mesmai, 'L', jma)
            if (nbma2 .eq. 0) then
                call wkvect(listma, 'G V I', 2*nbma, jnuma)
                do 16 ima = 1, nbma
                    call jeveuo(jexnum(connex, zi(jma+ima-1)), 'L', jno)
                    zi(jnuma+2*ima-2) = zi(jno)
                    zi(jnuma+2*ima-1) = zi(jno+1)
16              continue
            else
                call jeveuo(mesma2, 'L', jma2)
                call wkvect(listma, 'G V I', 2*nbma2, jnuma)
                do 18 ima = 1, nbma2
                    call jeveuo(jexnum(connex, zi(jma2+ima-1)), 'L', jno)
                    zi(jnuma+2*ima-2) = zi(jno)
                    zi(jnuma+2*ima-1) = zi(jno+1)
18              continue
                call jedetr(mesma2)
            endif
            call nocart(carte, 3, k8b, 'NUM', nbma,&
                        k8b, zi(jma), ' ', 2)
            call jedetr(mesmai)
        endif
!
10  end do
!
    call jedema()
end subroutine
