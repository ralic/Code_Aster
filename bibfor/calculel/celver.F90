subroutine celver(celz, typver, arret, iret)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/iisnan.h"
#include "asterc/isnnem.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
    character(len=*) :: celz, typver, arret
    integer :: iret
! ------------------------------------------------------------------
! BUT : VERIFIER QUE LE CHAM_ELEM  CEL  A UNE CERTAINE PROPRIETE
!       SINON ERREUR '<F>' (OU BIEN CODE_RETOUR:1)
! ------------------------------------------------------------------
!
! CELZ    IN/JXIN  K19 : SD CHAM_ELEM A VERIFIER
!
! TYPVER  IN       K*  : TYPE DE VERIFICATION A EFFECTUER
!     /'NBSPT_1'    : LES ELEMENTS DU CHAM_ELEM N'ONT QU'1 SOUS-POINT
!     /'NBVARI_CST' : POUR UN CHAM_ELEM(VARI_R), ON VERIFIE QUE
!                     TOUS LES ELEMENTS ONT LE MEME NOMBRE DE CMPS
!     /'PAS_NAN'    : IL N'Y A PAS DE VALEURS "NAN" DANS LE CHAMP
!                     (VOIR CESCEL.F POUR LA DEFINITION DE "NAN")
!
! ARRET   IN   K* :  /'STOP' : ON ARRET LE CODE EN ERREUR FATALE
!                    /'COOL' : ON LAISSE PASSER MAIS ON REND IRET=1
!
! IRET   OUT   I :  /  0 : LA CONDITION EST VERIFIEE
!                   /  1 : LA CONDITION N'EST PAS VERIFIEE
!-----------------------------------------------------------------------
!
!     ------------------------------------------------------------------
    character(len=8) :: tsca, nomgd
    character(len=3) :: knan
    character(len=19) :: cel
    integer :: kk, mxspt, igr, ngrel, nel, iel, iprem, ncdyn, ncdyn1
    integer :: imolo, inan, nb1, k, jcelv
    aster_logical :: lnan
    integer, pointer :: celd(:) => null()
!
!     ------------------------------------------------------------------
    call jemarq()
    cel = celz
    iret = 0
!
    call jeexin(cel//'.CELD', kk)
    if (kk .eq. 0) then
        call utmess('F', 'CALCULEL_47', sk=cel)
    endif
!
    call jeveuo(cel//'.CELD', 'L', vi=celd)
!
!
    if (typver .eq. 'NBVARI_CST') then
!     --------------------------------
        ngrel = celd(2)
        iprem = 0
        do igr = 1, ngrel
            imolo = celd(celd(4+igr)+2)
            if (imolo .eq. 0) goto 20
            nel = celd(celd(4+igr)+1)
            do iel = 1, nel
                ncdyn = celd(celd(4+igr)+4+4* (iel-1)+2)
                iprem = iprem + 1
                if (iprem .eq. 1) then
                    ncdyn1 = ncdyn
                else
                    if (ncdyn .ne. ncdyn1) then
                        if (arret .ne. 'COOL') then
                            call utmess('F', 'CALCULEL_48', sk=cel)
                        else
                            iret = 1
                        endif
                    endif
                endif
            end do
 20         continue
        end do
!
!
    else if (typver.eq.'NBSPT_1') then
!     --------------------------------
        mxspt = celd(3)
        if (mxspt .gt. 1) then
            if (arret .ne. 'COOL') then
                call utmess('F', 'CALCULEL_49', sk=cel)
            else
                iret = 1
            endif
        endif
!
!
    else if (typver.eq.'PAS_NAN') then
!     --------------------------------
        call dismoi('NOM_GD', cel, 'CHAMP', repk=nomgd)
        call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk=tsca)
        call jeveuo(cel//'.CELV', 'L', jcelv)
        call jelira(cel//'.CELV', 'LONMAX', nb1)
        lnan=.false.
        inan = isnnem()
        knan = '???'
!
        if (tsca .eq. 'R') then
            do k = 1, nb1
                if (iisnan(zr(jcelv-1+k)) .eq. 1) lnan=.true.
            end do
        else if (tsca.eq.'C') then
            do k = 1, nb1
                if (iisnan(dble(zc(jcelv-1+k))) .eq. 1) lnan=.true.
            end do
        else if (tsca.eq.'I') then
            do k = 1, nb1
                if (zi(jcelv-1+k) .eq. inan) lnan=.true.
            end do
        else if (tsca.eq.'K8') then
            do k = 1, nb1
                if (zk8(jcelv-1+k) .eq. knan) lnan=.true.
            end do
        else if (tsca.eq.'K24') then
            do k = 1, nb1
                if (zk24(jcelv-1+k) .eq. knan) lnan=.true.
            end do
        else
            ASSERT(.false.)
        endif
!
        if (lnan) then
            if (arret .ne. 'COOL') then
                call utmess('F', 'CALCULEL4_1', sk=cel)
            else
                iret = 1
            endif
        endif
!
!
    else
        ASSERT(.false.)
    endif
!
    call jedema()
end subroutine
