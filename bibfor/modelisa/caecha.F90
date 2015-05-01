subroutine caecha(char, ligrmo, noma, ndim, fonree)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/char_affe_neum.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/tecart.h"
#include "asterfort/vetyma.h"
    integer :: ndim
    character(len=4) :: fonree
    character(len=8) :: char, noma
    character(len=*) :: ligrmo
!-----------------------------------------------------------------------
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! BUT : STOCKAGE DE COEF_H ET TEMP_EXT DANS UNE CARTE ALLOUEE SUR LE
!       LIGREL DU MODELE
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRMO : NOM DU LIGREL DE MODELE
!      NOMA   : NOM DU MAILLAGE
!      NDIM   : DIMENSION DU PROBLEME (2D OU 3D)
!      FONREE : FONC OU REEL
!
!-----------------------------------------------------------------------
    integer :: necha, ncmp, jvalv1, jvalv2,   n, ncmp1, ncmp2, ncmps(2)
    integer :: iocc
    real(kind=8) :: r8b
    character(len=8) :: k8b
    character(len=16) :: motclf
    character(len=19) :: carte1, carte2, cartes(2)
    character(len=8), pointer :: vncmp1(:) => null()
    character(len=8), pointer :: vncmp2(:) => null()
!     ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'ECHANGE'
    call getfac(motclf, necha)
!
    carte1 = char//'.CHTH.COEFH'
    carte2 = char//'.CHTH.T_EXT'
!
    if (fonree .eq. 'REEL') then
        call alcart('G', carte1, noma, 'COEH_R')
        call alcart('G', carte2, noma, 'TEMP_R')
    else if (fonree.eq.'FONC') then
        call alcart('G', carte1, noma, 'COEH_F')
        call alcart('G', carte2, noma, 'TEMP_F')
    else
        ASSERT(.false.)
    endif
!
    call jeveuo(carte1//'.NCMP', 'E', vk8=vncmp1)
    call jeveuo(carte1//'.VALV', 'E', jvalv1)
    call jeveuo(carte2//'.NCMP', 'E', vk8=vncmp2)
    call jeveuo(carte2//'.VALV', 'E', jvalv2)
!
! --- STOCKAGE DE FLUX NULS SUR TOUT LE MAILLAGE
!
    ncmp = 3
    vncmp1(1) = 'H'
    vncmp1(2) = 'H_INF'
    vncmp1(3) = 'H_SUP'
    vncmp2(1) = 'TEMP'
    vncmp2(2) = 'TEMP_INF'
    vncmp2(3) = 'TEMP_SUP'
    if (fonree .eq. 'REEL') then
        zr(jvalv1-1+1) = 0.d0
        zr(jvalv1-1+2) = 0.d0
        zr(jvalv1-1+3) = 0.d0
        zr(jvalv2-1+1) = 0.d0
        zr(jvalv2-1+2) = 0.d0
        zr(jvalv2-1+3) = 0.d0
    else
        zk8(jvalv1-1+1) = '&FOZERO'
        zk8(jvalv1-1+2) = '&FOZERO'
        zk8(jvalv1-1+3) = '&FOZERO'
        zk8(jvalv2-1+1) = '&FOZERO'
        zk8(jvalv2-1+2) = '&FOZERO'
        zk8(jvalv2-1+3) = '&FOZERO'
    endif
    call nocart(carte1, 1, ncmp)
    call nocart(carte2, 1, ncmp)
!
! --- STOCKAGE DANS LA CARTE
!
    do iocc = 1, necha
        ncmp1 = 0
        ncmp2 = 0
        if (fonree .eq. 'REEL') then
            call getvr8(motclf, 'COEF_H', iocc=iocc, scal=r8b, nbret=n)
            if (n .eq. 1) then
                ncmp1= ncmp1 + 1
                vncmp1(ncmp1) = 'H'
                zr(jvalv1-1+ncmp1) = r8b
            endif
            call getvr8(motclf, 'COEF_H_INF', iocc=iocc, scal=r8b, nbret=n)
            if (n .eq. 1) then
                ncmp1 = ncmp1 + 1
                vncmp1(ncmp1) = 'H_INF'
                zr(jvalv1-1+ncmp1) = r8b
            endif
            call getvr8(motclf, 'COEF_H_SUP', iocc=iocc, scal=r8b, nbret=n)
            if (n .eq. 1) then
                ncmp1 = ncmp1 + 1
                vncmp1(ncmp1) = 'H_SUP'
                zr(jvalv1-1+ncmp1) = r8b
            endif
            call getvr8(motclf, 'TEMP_EXT', iocc=iocc, scal=r8b, nbret=n)
            if (n .eq. 1) then
                ncmp2 = ncmp2 + 1
                vncmp2(ncmp2) = 'TEMP'
                zr(jvalv2-1+ncmp2) = r8b
            endif
            call getvr8(motclf, 'TEMP_EXT_INF', iocc=iocc, scal=r8b, nbret=n)
            if (n .eq. 1) then
                ncmp2 = ncmp2 + 1
                vncmp2(ncmp2) = 'TEMP_INF'
                zr(jvalv2-1+ncmp2) = r8b
            endif
            call getvr8(motclf, 'TEMP_EXT_SUP', iocc=iocc, scal=r8b, nbret=n)
            if (n .eq. 1) then
                ncmp2 = ncmp2 + 1
                vncmp2(ncmp2) = 'TEMP_SUP'
                zr(jvalv2-1+ncmp2) = r8b
            endif
        else
            call getvid(motclf, 'COEF_H', iocc=iocc, scal=k8b, nbret=n)
            if (n .eq. 1) then
                ncmp1 = ncmp1 + 1
                vncmp1(ncmp1) = 'H'
                zk8(jvalv1-1+ncmp1) = k8b
            endif
            call getvid(motclf, 'COEF_H_INF', iocc=iocc, scal=k8b, nbret=n)
            if (n .eq. 1) then
                ncmp1 = ncmp1 + 1
                vncmp1(ncmp1) = 'H_INF'
                zk8(jvalv1-1+ncmp1) = k8b
            endif
            call getvid(motclf, 'COEF_H_SUP', iocc=iocc, scal=k8b, nbret=n)
            if (n .eq. 1) then
                ncmp1 = ncmp1 + 1
                vncmp1(ncmp1) = 'H_SUP'
                zk8(jvalv1-1+ncmp1) = k8b
            endif
            call getvid(motclf, 'TEMP_EXT', iocc=iocc, scal=k8b, nbret=n)
            if (n .eq. 1) then
                ncmp2 = ncmp2 + 1
                vncmp2(ncmp2) = 'TEMP'
                zk8(jvalv2-1+ncmp2) = k8b
            endif
            call getvid(motclf, 'TEMP_EXT_INF', iocc=iocc, scal=k8b, nbret=n)
            if (n .eq. 1) then
                ncmp2 = ncmp2 + 1
                vncmp2(ncmp2) = 'TEMP_INF'
                zk8(jvalv2-1+ncmp2) = k8b
            endif
            call getvid(motclf, 'TEMP_EXT_SUP', iocc=iocc, scal=k8b, nbret=n)
            if (n .eq. 1) then
                ncmp2 = ncmp2 + 1
                vncmp2(ncmp2) = 'TEMP_SUP'
                zk8(jvalv2-1+ncmp2) = k8b
            endif
        endif
!
        cartes(1) = carte1
        cartes(2) = carte2
        ncmps(1) = ncmp1
        ncmps(2) = ncmp2
        call char_affe_neum(noma, ndim, motclf, iocc, 2,&
                            cartes, ncmps)
!
    end do
    call tecart(carte1)
    call tecart(carte2)
!
    call jedema()
end subroutine
