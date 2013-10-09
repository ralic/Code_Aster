subroutine etenc2(cartz, iret)
    implicit none
!
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
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=19) :: cart
    character(len=*) :: cartz
    integer :: iret
! ----------------------------------------------------------------------
!     ENTREES:
!     CARTZ  : NOM DE LA CARTE A ETENDRE
!     SORTIES:
!     ON CREE L'OBJET  CART.PTMA
!
!     IRET : CODE RETOUR :
!              0 --> OK
!              1 --> LA CARTE CONCERNE AUSSI DES MAILLES TARDIVES
! ----------------------------------------------------------------------
    character(len=24) :: valk
    integer :: nbma, nbedit, igd, code, ient, i, ii, nb
    integer :: desc, grpma, lima
    integer :: jptma, iret0, ibid
    integer :: noli
    integer :: vali(3)
    character(len=8) :: ma
!
    call jemarq()
    cart = cartz
!
    iret = 0
    call jeexin(cart//'.DESC', iret0)
    if (iret0 .le. 0) then
        call utmess('F', 'CALCULEL2_49')
    endif
!
    call jeveuo(cart//'.NOLI', 'L', noli)
    call jeveuo(cart//'.DESC', 'L', desc)
!
!
!     1- ALLOCATION DE .PTMA :
!     ----------------------------------------
    call dismoi('NOM_MAILLA', cart, 'CARTE', repk=ma)
    call dismoi('NB_MA_MAILLA', ma, 'MAILLAGE', repi=nbma)
!
    if (nbma .gt. 0) then
        call jeexin(cart//'.PTMA', ibid)
!       -- LA CARTE EST DEJA ETENDUE:
        if (ibid .gt. 0) goto 50
        call wkvect(cart//'.PTMA', 'V V I', nbma, jptma)
    endif
!
!
!     2- REMPLISSAGE DE .PTMA :
!     ----------------------------------------
    nbedit = zi(desc-1+3)
    do 40 igd = 1, nbedit
        code = zi(desc-1+3+2*igd-1)
        ient = zi(desc-1+3+2*igd)
!
!        ------ GROUPE PREDEFINI "TOUT":
        if (code .eq. 1) then
            do 10 i = 1, nbma
                zi(jptma-1+i) = igd
 10         continue
            goto 40
        endif
        if ((code.eq.-1)) iret = 1
!
!        ------- GROUPE DE MAILLES DU MAILLAGE:
        if (code .eq. 2) then
            call jelira(jexnum(ma//'.GROUPEMA', ient), 'LONUTI', nb)
            call jeveuo(jexnum(ma//'.GROUPEMA', ient), 'L', grpma)
            do 20 i = 1, nb
                ii = zi(grpma-1+i)
                zi(jptma-1+ii) = igd
 20         continue
            goto 40
        endif
!
!        ------- LISTE TARDIVE DE MAILLES ASSOCIEE A LA CARTE:
        if (abs(code) .eq. 3) then
            call jelira(jexnum(cart//'.LIMA', ient), 'LONMAX', nb)
            call jeveuo(jexnum(cart//'.LIMA', ient), 'L', lima)
            if (code .gt. 0) then
                do 30 i = 1, nb
                    ii = zi(lima-1+i)
                    if (ii .le. 0) then
                        valk = cart
                        vali (1) = ient
                        vali (2) = i
                        vali (3) = ii
                        call utmess('F', 'CALCULEL5_85', sk=valk, ni=3, vali=vali)
                    endif
                    zi(jptma-1+ii) = igd
 30             continue
            else
                iret = 1
            endif
            goto 40
        endif
 40 end do
 50 continue
!
!
    call jedema()
end subroutine
