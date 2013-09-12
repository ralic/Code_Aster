subroutine carayo(char, ligrmo, noma, fonree)
    implicit none
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterfort/alcart.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/getvtx.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nocart.h"
#include "asterfort/reliem.h"
#include "asterfort/u2mesk.h"
    character(len=4) :: fonree
    character(len=8) :: char, noma
    character(len=*) :: ligrmo
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
! BUT : STOCKAGE DE SIGMA (CONSTANTE DE BOLZMAN), EPSILON (EMISSIVITE)
!       ET TMP_INF DANS UNE CARTE ALLOUEE SUR LE LIGREL DU MODELE
!
! ARGUMENTS D'ENTREE:
!      CHAR   : NOM UTILISATEUR DU RESULTAT DE CHARGE
!      LIGRMO : NOM DU LIGREL DE MODELE
!      NOMA   : NOM DU MAILLAGE
!      FONREE : FONC OU REEL
!-----------------------------------------------------------------------
    integer :: nrayo, jvalv, jncmp, ncmp, n, iocc, nbtou, nbma, jma
    character(len=8) :: k8b, typmcl(2)
    character(len=16) :: motclf, motcle(2)
    character(len=19) :: carte
    character(len=24) :: mesmai
    integer :: iarg
!     ------------------------------------------------------------------
    call jemarq()
!
    motclf = 'RAYONNEMENT'
    call getfac(motclf, nrayo)
!
    carte = char//'.CHTH.RAYO'
!
    if (fonree .eq. 'REEL') then
        call alcart('G', carte, noma, 'RAYO_R')
    else if (fonree.eq.'FONC') then
        call alcart('G', carte, noma, 'RAYO_F')
    else
        call u2mesk('F', 'MODELISA2_37', 1, fonree)
    endif
!
    call jeveuo(carte//'.NCMP', 'E', jncmp)
    call jeveuo(carte//'.VALV', 'E', jvalv)
!
! --- STOCKAGE DE FLUX NULS SUR TOUT LE MAILLAGE
!
    ncmp = 3
    zk8(jncmp-1+1) = 'SIGMA'
    zk8(jncmp-1+2) = 'EPSIL'
    zk8(jncmp-1+3) = 'TPINF'
    if (fonree .eq. 'REEL') then
        zr(jvalv-1+1) = 0.d0
        zr(jvalv-1+2) = 0.d0
        zr(jvalv-1+3) = 0.d0
    else
        zk8(jvalv-1+1) = '&FOZERO'
        zk8(jvalv-1+2) = '&FOZERO'
        zk8(jvalv-1+3) = '&FOZERO'
    endif
    call nocart(carte, 1, ' ', 'NOM', 0,&
                ' ', 0, ligrmo, ncmp)
!
    mesmai = '&&CARAYO.MES_MAILLES'
    motcle(1) = 'GROUP_MA'
    motcle(2) = 'MAILLE'
    typmcl(1) = 'GROUP_MA'
    typmcl(2) = 'MAILLE'
!
! --- STOCKAGE DANS LA CARTE
!
    do 10 iocc = 1, nrayo
        if (fonree .eq. 'REEL') then
            call getvr8(motclf, 'SIGMA', iocc=iocc, scal=zr(jvalv), nbret=n)
            call getvr8(motclf, 'EPSILON', iocc=iocc, scal=zr(jvalv+1), nbret=n)
            call getvr8(motclf, 'TEMP_EXT', iocc=iocc, scal=zr(jvalv+2), nbret=n)
        else
            call getvid(motclf, 'SIGMA', iocc=iocc, scal=zk8(jvalv), nbret=n)
            call getvid(motclf, 'EPSILON', iocc=iocc, scal=zk8(jvalv+1), nbret=n)
            call getvid(motclf, 'TEMP_EXT', iocc=iocc, scal=zk8(jvalv+2), nbret=n)
        endif
!
        call getvtx(motclf, 'TOUT', iocc=iocc, scal=k8b, nbret=nbtou)
        if (nbtou .ne. 0) then
            call nocart(carte, 1, ' ', 'NOM', 0,&
                        ' ', 0, ligrmo, ncmp)
!
        else
            call reliem(ligrmo, noma, 'NU_MAILLE', motclf, iocc,&
                        2, motcle, typmcl, mesmai, nbma)
            if (nbma .eq. 0) goto 10
            call jeveuo(mesmai, 'L', jma)
            call nocart(carte, 3, k8b, 'NUM', nbma,&
                        k8b, zi(jma), ' ', ncmp)
            call jedetr(mesmai)
        endif
!
10  end do
!
    call jedema()
!
end subroutine
