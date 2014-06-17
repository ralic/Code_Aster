subroutine cachei(char, ligrmo, noma, fonree, param,&
                  motcl)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/alcart.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
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
    integer :: i, nchei, ncmp, jvale, jvalv,  iocc, nxx, nyy, nzz
    integer :: nxy, nxz, nyz, nex, nky, nkz, nexx, neyy, nexy, nkxx, nkyy, nkxy
    integer :: nbtou, nbma, jma
    real(kind=8) :: epxx, epyy, epzz, epxy, epxz, epyz, epx, xky, xkz, xexx
    real(kind=8) :: xeyy, xexy, xkxx, xkyy, xkxy
    character(len=8) :: k8b, kepxx, kepyy, kepzz, kepxy, kepxz, kepyz, mod
    character(len=8) :: modeli, typmcl(2)
    character(len=16) :: motclf, motcle(2)
    character(len=19) :: carte
    character(len=24) :: mesmai
    character(len=8), pointer :: vncmp(:) => null()
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
    call dismoi('NOM_MODELE', char(1:8), 'CHARGE', repk=mod)
!
! --- MODELISATION DU MODELE
!
    call dismoi('MODELISATION', mod, 'MODELE', repk=modeli)
!
    if (fonree .eq. 'REEL') then
        call alcart('G', carte, noma, 'EPSI_R')
    else if (fonree.eq.'FONC') then
        call alcart('G', carte, noma, 'EPSI_F')
    else
        call utmess('F', 'MODELISA2_37', sk=fonree)
    endif
!
    call jeveuo(carte//'.NCMP', 'E', vk8=vncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
    call jeveuo(carte//'.VALE', 'E', jvale)
!
    ncmp = 6
    if (fonree .eq. 'REEL') ncmp = 15
!
    vncmp(1) = 'EPXX'
    vncmp(2) = 'EPYY'
    vncmp(3) = 'EPZZ'
    vncmp(4) = 'EPXY'
    vncmp(5) = 'EPXZ'
    vncmp(6) = 'EPYZ'
    if (fonree .eq. 'REEL') then
        vncmp(7) = 'EPX'
        vncmp(8) = 'KY'
        vncmp(9) = 'KZ'
        vncmp(10) = 'EXX'
        vncmp(11) = 'EYY'
        vncmp(12) = 'EXY'
        vncmp(13) = 'KXX'
        vncmp(14) = 'KYY'
        vncmp(15) = 'KXY'
    endif
    if (fonree .eq. 'REEL') then
        do i = 1, ncmp
            zr(jvalv-1+i) = 0.d0
        end do
    else
        do i = 1, ncmp
            zk8(jvalv-1+i) = '&FOZERO'
        end do
    endif
    call nocart(carte, 1, ncmp)
!
    mesmai = '&&CACHEI.MES_MAILLES'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
    do iocc = 1, nchei
!
        if (fonree .eq. 'REEL') then
            call getvr8(motclf, 'EPXX', iocc=iocc, scal=epxx, nbret=nxx)
            call getvr8(motclf, 'EPYY', iocc=iocc, scal=epyy, nbret=nyy)
            call getvr8(motclf, 'EPZZ', iocc=iocc, scal=epzz, nbret=nzz)
            call getvr8(motclf, 'EPXY', iocc=iocc, scal=epxy, nbret=nxy)
            call getvr8(motclf, 'EPXZ', iocc=iocc, scal=epxz, nbret=nxz)
            call getvr8(motclf, 'EPYZ', iocc=iocc, scal=epyz, nbret=nyz)
            call getvr8(motclf, 'EPX', iocc=iocc, scal=epx, nbret=nex)
            call getvr8(motclf, 'KY', iocc=iocc, scal=xky, nbret=nky)
            call getvr8(motclf, 'KZ', iocc=iocc, scal=xkz, nbret=nkz)
            call getvr8(motclf, 'EXX', iocc=iocc, scal=xexx, nbret=nexx)
            call getvr8(motclf, 'EYY', iocc=iocc, scal=xeyy, nbret=neyy)
            call getvr8(motclf, 'EXY', iocc=iocc, scal=xexy, nbret=nexy)
            call getvr8(motclf, 'KXX', iocc=iocc, scal=xkxx, nbret=nkxx)
            call getvr8(motclf, 'KYY', iocc=iocc, scal=xkyy, nbret=nkyy)
            call getvr8(motclf, 'KXY', iocc=iocc, scal=xkxy, nbret=nkxy)
!
            do i = 1, ncmp
                zr(jvalv-1+i) = 0.d0
            end do
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
                call utmess('F', 'MODELISA2_38')
            endif
        else
            call getvid(motclf, 'EPXX', iocc=iocc, scal=kepxx, nbret=nxx)
            call getvid(motclf, 'EPYY', iocc=iocc, scal=kepyy, nbret=nyy)
            call getvid(motclf, 'EPZZ', iocc=iocc, scal=kepzz, nbret=nzz)
            call getvid(motclf, 'EPXY', iocc=iocc, scal=kepxy, nbret=nxy)
            call getvid(motclf, 'EPXZ', iocc=iocc, scal=kepxz, nbret=nxz)
            call getvid(motclf, 'EPYZ', iocc=iocc, scal=kepyz, nbret=nyz)
            do i = 1, ncmp
                zk8(jvalv-1+i) = '&FOZERO'
            end do
            if (nxx .ne. 0) zk8(jvalv-1+1) = kepxx
            if (nyy .ne. 0) zk8(jvalv-1+2) = kepyy
            if (nzz .ne. 0) zk8(jvalv-1+3) = kepzz
            if (nxy .ne. 0) zk8(jvalv-1+4) = kepxy
            if (nxz .ne. 0) zk8(jvalv-1+5) = kepxz
            if (nyz .ne. 0) zk8(jvalv-1+6) = kepyz
        endif
!
        call getvtx(motclf, 'TOUT', iocc=iocc, scal=k8b, nbret=nbtou)
!
        if (nbtou .ne. 0) then
!
            call nocart(carte, 1, ncmp)
        else
            call reliem(ligrmo, noma, 'NU_MAILLE', motclf, iocc,&
                        2, motcle, typmcl, mesmai, nbma)
            if (nbma .eq. 0) goto 20
            call jeveuo(mesmai, 'L', jma)
            call nocart(carte, 3, ncmp, mode='NUM', nma=nbma,&
                        limanu=zi(jma))
            call jedetr(mesmai)
        endif
 20     continue
    end do
!
    call jedema()
end subroutine
