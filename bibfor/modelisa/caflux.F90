subroutine caflux(char, ligrmo, noma, ndim, fonree)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/alcart.h"
#include "asterfort/assert.h"
#include "asterfort/char_affe_neum.h"
#include "asterfort/getvem.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/tbexp2.h"
#include "asterfort/tbliva.h"
#include "asterfort/tecart.h"
#include "asterfort/utmess.h"
#include "asterfort/vetyma.h"
    integer :: ndim
    character(len=4) :: fonree
    character(len=8) :: char, noma
    character(len=*) :: ligrmo
! ----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! BUT : STOCKAGE DES FLUX DANS UNE (OU 2) CARTE ALLOUEE SUR LE
!       LIGREL DU MODELE
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRMO : NOM DU LIGREL DE MODELE
!      NBET   : NOMBRE TOTAL DE MAILLES
!      NDIM   : DIMENSION DU PROBLEME (2D OU 3D)
!      FONREE : FONC OU REEL
!
!-----------------------------------------------------------------------
    integer :: ibid, nflux, jvalv1, jvalv2, jncmp1, jncmp2, iocc, n, n1, n2, n3
    integer :: n4, n5, n6, n7, n8, n11, n12, ngr, ncmp, ncmp1, ncmps(2)
    integer :: ncmp2, iret
    real(kind=8) :: r8b, aire, xlong
    complex(kind=8) :: c16b
    logical :: icre1, icre2
    character(len=8) :: k8b, nomtab
    character(len=16) :: motclf
    character(len=19) :: cart1, cart2, cartes(2)
    character(len=24) :: para, mongrm
    character(len=24) :: valk(2)
    integer :: iarg
! ----------------------------------------------------------------------
!
!     VERIFICATION DE L'EXCLUSION :   / FLUN FLUN_INF FLUN_SUP
!                                     / FLUX_X FLUX_Y FLUX_Z
!
!     AU PASSAGE, ON NOTE S'IL FAUT CREER 1 OU 2 CARTES :
!       CART1 : CARTE(FLUN)   (ICRE1 = .TRUE.)
!       CART2 : CARTE(FLUX)   (ICRE2 = .TRUE.)
!      LES 2  :               (ICRE1 = ICRE2 = .TRUE. )
!
    call jemarq()
    icre1 = .false.
    icre2 = .false.
    motclf = 'FLUX_REP'
    call getfac(motclf, nflux)
!
    do iocc = 1, nflux
        n5 = 0
        if (fonree .eq. 'REEL') then
            call getvr8(motclf, 'FLUN', iocc=iocc, nbval=0, nbret=n11)
            call getvr8(motclf, 'FLUN_INF', iocc=iocc, nbval=0, nbret=n2)
            call getvr8(motclf, 'FLUN_SUP', iocc=iocc, nbval=0, nbret=n3)
            call getvid(motclf, 'CARA_TORSION', iocc=iocc, nbval=0, nbret=n12)
            n1 = n11 + n12
        else if (fonree.eq.'FONC') then
            call getvid(motclf, 'FLUN', iocc=iocc, nbval=0, nbret=n1)
            call getvid(motclf, 'FLUN_INF', iocc=iocc, nbval=0, nbret=n2)
            call getvid(motclf, 'FLUN_SUP', iocc=iocc, nbval=0, nbret=n3)
            call getvid(motclf, 'FLUX_X', iocc=iocc, nbval=0, nbret=n6)
            call getvid(motclf, 'FLUX_Y', iocc=iocc, nbval=0, nbret=n7)
            call getvid(motclf, 'FLUX_Z', iocc=iocc, nbval=0, nbret=n8)
            n5 = n6+n7+n8
        else
            ASSERT(.false.)
        endif
        n4 = n1+n2+n3
        if ((n5.ne.0) .and. (n4.ne.0)) then
            if (fonree .eq. 'FONC') then
                call utmess('F', 'MODELISA2_64')
            endif
        endif
        if (n4 .ne. 0) icre1 = .true.
        if (n5 .ne. 0) icre2 = .true.
    end do
!
!     ALLOCATION EVENTUELLE DES CARTES CART1 ET CART2 :
!
    cart1= char//'.CHTH.FLURE'
    cart2= char//'.CHTH.FLUR2'
    if (fonree .eq. 'REEL') then
        if (icre1) call alcart('G', cart1, noma, 'FLUN_R')
        if (icre2) call alcart('G', cart2, noma, 'FLUX_R')
    else if (fonree.eq.'FONC') then
        if (icre1) call alcart('G', cart1, noma, 'FLUN_F')
        if (icre2) call alcart('G', cart2, noma, 'FLUX_F')
    else
        ASSERT(.false.)
    endif
!
    if (icre1) then
        call jeveuo(cart1//'.NCMP', 'E', jncmp1)
        call jeveuo(cart1//'.VALV', 'E', jvalv1)
    endif
    if (icre2) then
        call jeveuo(cart2//'.NCMP', 'E', jncmp2)
        call jeveuo(cart2//'.VALV', 'E', jvalv2)
    endif
!
!      STOCKAGE DE FLUX NULS SUR TOUT LE MAILLAGE
!
    if (icre1) then
        ncmp=3
        zk8(jncmp1-1+1) = 'FLUN'
        zk8(jncmp1-1+2) = 'FLUN_INF'
        zk8(jncmp1-1+3) = 'FLUN_SUP'
        if (fonree .eq. 'REEL') then
            zr(jvalv1-1+1) = 0.d0
            zr(jvalv1-1+2) = 0.d0
            zr(jvalv1-1+3) = 0.d0
        else
            zk8(jvalv1-1+1) = '&FOZERO'
            zk8(jvalv1-1+2) = '&FOZERO'
            zk8(jvalv1-1+3) = '&FOZERO'
        endif
        call nocart(cart1, 1, ncmp)
    endif
!
    if (icre2) then
        ncmp=3
        zk8(jncmp2-1+1) = 'FLUX'
        zk8(jncmp2-1+2) = 'FLUY'
        zk8(jncmp2-1+3) = 'FLUZ'
        if (fonree .eq. 'REEL') then
            zr(jvalv2-1+1) = 0.d0
            zr(jvalv2-1+2) = 0.d0
            zr(jvalv2-1+3) = 0.d0
        else
            zk8(jvalv2-1+1) = '&FOZERO'
            zk8(jvalv2-1+2) = '&FOZERO'
            zk8(jvalv2-1+3) = '&FOZERO'
        endif
        call nocart(cart2, 1, ncmp)
    endif
!
!     STOCKAGE DANS LES CARTES
!
    do iocc = 1, nflux
        ncmp1 = 0
        ncmp2 = 0
!
        if (fonree .eq. 'REEL') then
!
            call getvid(motclf, 'CARA_TORSION', iocc=iocc, scal=nomtab, nbret=n)
            if (n .eq. 1) then
!              VERIFICATION DES PARAMETRES DE LA TABLE 'NOMTAB'
                call tbexp2(nomtab, 'AIRE')
                call tbexp2(nomtab, 'LONGUEUR')
                call tbexp2(nomtab, 'GROUP_MA')
!
                call getvem(noma, 'GROUP_MA', motclf, 'GROUP_MA', iocc,&
                            iarg, 1, mongrm, ngr)
                para = 'AIRE'
                call tbliva(nomtab, 1, 'GROUP_MA', ibid, r8b,&
                            c16b, mongrm, k8b, r8b, para,&
                            k8b, ibid, aire, c16b, k8b,&
                            iret)
                if (iret .eq. 1) then
                    valk (1) = para
                    valk (2) = nomtab
                    call utmess('F', 'MODELISA8_34', nk=2, valk=valk)
                else if (iret .eq. 2) then
                    valk (1) = para
                    call utmess('F', 'MODELISA8_35', sk=valk(1))
                else if (iret .eq. 3) then
                    valk (1) = mongrm
                    call utmess('F', 'MODELISA8_36', sk=valk(1))
                endif
                para = 'LONGUEUR'
                call tbliva(nomtab, 1, 'GROUP_MA', ibid, r8b,&
                            c16b, mongrm, k8b, r8b, para,&
                            k8b, ibid, xlong, c16b, k8b,&
                            iret)
                if (iret .eq. 1) then
                    valk (1) = para
                    valk (2) = nomtab
                    call utmess('F', 'MODELISA8_34', nk=2, valk=valk)
                else if (iret .eq. 2) then
                    valk (1) = para
                    call utmess('F', 'MODELISA8_35', sk=valk(1))
                else if (iret .eq. 3) then
                    valk (1) = mongrm
                    call utmess('F', 'MODELISA8_36', sk=valk(1))
                endif
                ncmp1 = ncmp1 + 1
                zk8(jncmp1-1 + ncmp1) = 'FLUN'
                zr(jvalv1-1 + ncmp1) = 2.0d0 * aire / xlong
            endif
            call getvr8(motclf, 'FLUN', iocc=iocc, scal=r8b, nbret=n)
            if (n .eq. 1) then
                ncmp1 = ncmp1 + 1
                zk8(jncmp1-1 + ncmp1) = 'FLUN'
                zr(jvalv1-1 + ncmp1) = r8b
            endif
            call getvr8(motclf, 'FLUN_INF', iocc=iocc, scal=r8b, nbret=n)
            if (n .eq. 1) then
                ncmp1 = ncmp1 + 1
                zk8(jncmp1-1 + ncmp1) = 'FLUN_INF'
                zr(jvalv1-1 + ncmp1) = r8b
            endif
            call getvr8(motclf, 'FLUN_SUP', iocc=iocc, scal=r8b, nbret=n)
            if (n .eq. 1) then
                ncmp1 = ncmp1 + 1
                zk8(jncmp1-1 + ncmp1) = 'FLUN_SUP'
                zr(jvalv1-1 + ncmp1) = r8b
            endif
!
        else
            call getvid(motclf, 'FLUN', iocc=iocc, scal=k8b, nbret=n)
            if (n .eq. 1) then
                ncmp1 = ncmp1 + 1
                zk8(jncmp1-1 + ncmp1) = 'FLUN'
                zk8(jvalv1-1 + ncmp1) = k8b
            endif
            call getvid(motclf, 'FLUN_INF', iocc=iocc, scal=k8b, nbret=n)
            if (n .eq. 1) then
                ncmp1 = ncmp1 + 1
                zk8(jncmp1-1 + ncmp1) = 'FLUN_INF'
                zk8(jvalv1-1 + ncmp1) = k8b
            endif
            call getvid(motclf, 'FLUN_SUP', iocc=iocc, scal=k8b, nbret=n)
            if (n .eq. 1) then
                ncmp1 = ncmp1 + 1
                zk8(jncmp1-1 + ncmp1) = 'FLUN_SUP'
                zk8(jvalv1-1 + ncmp1) = k8b
            endif
!
            call getvid(motclf, 'FLUX_X', iocc=iocc, scal=k8b, nbret=n)
            if (n .eq. 1) then
                ncmp2 = ncmp2 + 1
                zk8(jncmp2-1 + ncmp2) = 'FLUX'
                zk8(jvalv2-1 + ncmp2) = k8b
            endif
            call getvid(motclf, 'FLUX_Y', iocc=iocc, scal=k8b, nbret=n)
            if (n .eq. 1) then
                ncmp2 = ncmp2 + 1
                zk8(jncmp2-1 + ncmp2) = 'FLUY'
                zk8(jvalv2-1 + ncmp2) = k8b
            endif
            call getvid(motclf, 'FLUX_Z', iocc=iocc, scal=k8b, nbret=n)
            if (n .eq. 1) then
                ncmp2 = ncmp2 + 1
                zk8(jncmp2-1 + ncmp2) = 'FLUZ'
                zk8(jvalv2-1 + ncmp2) = k8b
            endif
        endif
!
!
        cartes(1) = cart1
        cartes(2) = cart2
        ncmps(1) = ncmp1
        ncmps(2) = ncmp2
        call char_affe_neum(noma, ndim, motclf, iocc, 2,&
                            cartes, ncmps)
!
    end do
!
    if (icre1) call tecart(cart1)
    if (icre2) call tecart(cart2)
!
    call jedema()
end subroutine
