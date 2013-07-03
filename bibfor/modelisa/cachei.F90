subroutine cachei(char, ligrmo, noma, fonree, param,&
                  motcl)
    implicit   none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getvid.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/alcart.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesk.h"
#include "asterfort/u2mess.h"
    character(len=4) :: fonree
    character(len=5) :: param
    character(len=8) :: char, noma
    character(len=*) :: ligrmo, motcl
!-----------------------------------------------------------------------
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
! BUT : STOCKAGE DES CHARGES DE DEFORMATIONS INITIALES REPARTIES
!       DANS UNE CARTE ALLOUEE SUR LE LIGREL DU MODELE
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRMO : NOM DU LIGREL DE MODELE
!      NOMA   : NOM DU MAILLAGE
!      FONREE : FONC OU REEL
!      PARAM  : NOM DU TROISIEME CHAMP DE LA CARTE (EPSIN)
!      MOTCL  : MOT-CLE FACTEUR
!
!-----------------------------------------------------------------------
!
    integer :: ibid, i, nchei, ncmp, jvale, jvalv, jncmp, iocc, nxx, nyy, nzz
    integer :: nxy, nxz, nyz, nex, nky, nkz, nexx, neyy, nexy, nkxx, nkyy, nkxy
    integer :: nbtou, ier, nbma, jma
    real(kind=8) :: epxx, epyy, epzz, epxy, epxz, epyz, epx, xky, xkz, xexx
    real(kind=8) :: xeyy, xexy, xkxx, xkyy, xkxy
    character(len=8) :: k8b, kepxx, kepyy, kepzz, kepxy, kepxz, kepyz, mod
    character(len=8) :: modeli, typmcl(2)
    character(len=16) :: motclf, motcle(2)
    character(len=19) :: carte
    character(len=24) :: mesmai
    integer :: iarg
!     ------------------------------------------------------------------
!
    call jemarq()
!
    motclf = motcl
    call getfac(motclf, nchei)
!
    carte = char//'.CHME.'//param
!
! --- MODELE ASSOCIE AU LIGREL DE CHARGE
!
    call dismoi('F', 'NOM_MODELE', char(1:8), 'CHARGE', ibid,&
                mod, ier)
!
! --- MODELISATION DU MODELE
!
    call dismoi('F', 'MODELISATION', mod, 'MODELE', ibid,&
                modeli, ier)
!
    if (fonree .eq. 'REEL') then
        call alcart('G', carte, noma, 'EPSI_R')
    else if (fonree.eq.'FONC') then
        call alcart('G', carte, noma, 'EPSI_F')
    else
        call u2mesk('F', 'MODELISA2_37', 1, fonree)
    endif
!
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
    call jeveuo(carte//'.VALE', 'E', jvale)
!
    ncmp = 6
    if (fonree .eq. 'REEL') ncmp = 15
!
    zk8(jncmp-1+1) = 'EPXX'
    zk8(jncmp-1+2) = 'EPYY'
    zk8(jncmp-1+3) = 'EPZZ'
    zk8(jncmp-1+4) = 'EPXY'
    zk8(jncmp-1+5) = 'EPXZ'
    zk8(jncmp-1+6) = 'EPYZ'
    if (fonree .eq. 'REEL') then
        zk8(jncmp-1+7) = 'EPX'
        zk8(jncmp-1+8) = 'KY'
        zk8(jncmp-1+9) = 'KZ'
        zk8(jncmp-1+10) = 'EXX'
        zk8(jncmp-1+11) = 'EYY'
        zk8(jncmp-1+12) = 'EXY'
        zk8(jncmp-1+13) = 'KXX'
        zk8(jncmp-1+14) = 'KYY'
        zk8(jncmp-1+15) = 'KXY'
    endif
    if (fonree .eq. 'REEL') then
        do 10 i = 1, ncmp
            zr(jvalv-1+i) = 0.d0
10      continue
    else
        do 12 i = 1, ncmp
            zk8(jvalv-1+i) = '&FOZERO'
12      continue
    endif
    call nocart(carte, 1, ' ', 'NOM', 0,&
                ' ', 0, ligrmo, ncmp)
!
    mesmai = '&&CACHEI.MES_MAILLES'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
    do 20 iocc = 1, nchei
!
        if (fonree .eq. 'REEL') then
            call getvr8(motclf, 'EPXX', iocc, iarg, 1,&
                        epxx, nxx)
            call getvr8(motclf, 'EPYY', iocc, iarg, 1,&
                        epyy, nyy)
            call getvr8(motclf, 'EPZZ', iocc, iarg, 1,&
                        epzz, nzz)
            call getvr8(motclf, 'EPXY', iocc, iarg, 1,&
                        epxy, nxy)
            call getvr8(motclf, 'EPXZ', iocc, iarg, 1,&
                        epxz, nxz)
            call getvr8(motclf, 'EPYZ', iocc, iarg, 1,&
                        epyz, nyz)
            call getvr8(motclf, 'EPX', iocc, iarg, 1,&
                        epx, nex)
            call getvr8(motclf, 'KY', iocc, iarg, 1,&
                        xky, nky)
            call getvr8(motclf, 'KZ', iocc, iarg, 1,&
                        xkz, nkz)
            call getvr8(motclf, 'EXX', iocc, iarg, 1,&
                        xexx, nexx)
            call getvr8(motclf, 'EYY', iocc, iarg, 1,&
                        xeyy, neyy)
            call getvr8(motclf, 'EXY', iocc, iarg, 1,&
                        xexy, nexy)
            call getvr8(motclf, 'KXX', iocc, iarg, 1,&
                        xkxx, nkxx)
            call getvr8(motclf, 'KYY', iocc, iarg, 1,&
                        xkyy, nkyy)
            call getvr8(motclf, 'KXY', iocc, iarg, 1,&
                        xkxy, nkxy)
!
            do 22 i = 1, ncmp
                zr(jvalv-1+i) = 0.d0
22          continue
!
            if (nxx .ne. 0) zr(jvalv-1+1) = epxx
            if (nyy .ne. 0) zr(jvalv-1+2) = epyy
            if (nzz .ne. 0) zr(jvalv-1+3) = epzz
            if (nxy .ne. 0) zr(jvalv-1+4) = epxy
            if (nxz .ne. 0) zr(jvalv-1+5) = epxz
            if (nyz .ne. 0) zr(jvalv-1+6) = epyz
!
            if (nex .ne. 0) zr(jvalv-1+7) = epx
            if (nky .ne. 0) zr(jvalv-1+8) = xky
            if (nkz .ne. 0) zr(jvalv-1+9) = xkz
            if (nexx .ne. 0) zr(jvalv-1+10) = xexx
            if (neyy .ne. 0) zr(jvalv-1+11) = xeyy
            if (nexy .ne. 0) zr(jvalv-1+12) = xexy
            if (nkxx .ne. 0) zr(jvalv-1+13) = xkxx
            if (nkyy .ne. 0) zr(jvalv-1+14) = xkyy
            if (nkxy .ne. 0) zr(jvalv-1+15) = xkxy
            if ((nky.ne.0.or.nkz.ne.0) .and. (modeli.eq.'POU_C_T')) then
                call u2mess('F', 'MODELISA2_38')
            endif
        else
            call getvid(motclf, 'EPXX', iocc, iarg, 1,&
                        kepxx, nxx)
            call getvid(motclf, 'EPYY', iocc, iarg, 1,&
                        kepyy, nyy)
            call getvid(motclf, 'EPZZ', iocc, iarg, 1,&
                        kepzz, nzz)
            call getvid(motclf, 'EPXY', iocc, iarg, 1,&
                        kepxy, nxy)
            call getvid(motclf, 'EPXZ', iocc, iarg, 1,&
                        kepxz, nxz)
            call getvid(motclf, 'EPYZ', iocc, iarg, 1,&
                        kepyz, nyz)
            do 111 i = 1, ncmp
                zk8(jvalv-1+i) = '&FOZERO'
111          continue
            if (nxx .ne. 0) zk8(jvalv-1+1) = kepxx
            if (nyy .ne. 0) zk8(jvalv-1+2) = kepyy
            if (nzz .ne. 0) zk8(jvalv-1+3) = kepzz
            if (nxy .ne. 0) zk8(jvalv-1+4) = kepxy
            if (nxz .ne. 0) zk8(jvalv-1+5) = kepxz
            if (nyz .ne. 0) zk8(jvalv-1+6) = kepyz
        endif
!
        call getvtx(motclf, 'TOUT', iocc, iarg, 1,&
                    k8b, nbtou)
!
        if (nbtou .ne. 0) then
!
            call nocart(carte, 1, ' ', 'NOM', 0,&
                        ' ', 0, ligrmo, ncmp)
        else
            call reliem(ligrmo, noma, 'NU_MAILLE', motclf, iocc,&
                        2, motcle, typmcl, mesmai, nbma)
            if (nbma .eq. 0) goto 20
            call jeveuo(mesmai, 'L', jma)
            call nocart(carte, 3, k8b, 'NUM', nbma,&
                        k8b, zi(jma), ' ', ncmp)
            call jedetr(mesmai)
        endif
20  end do
!
    call jedema()
end subroutine
