subroutine carcomp(carte_1, carte_2, iret)
!
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
!
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
    character(len=*), intent(in) :: carte_1
    character(len=*), intent(in) :: carte_2
    integer, intent(out) :: iret
!
! --------------------------------------------------------------------------------------------------
!
! Utility - Fields
!
! Comparison of two CARTE
!
! --------------------------------------------------------------------------------------------------
!
! In  carte_1   : first CARTE
! In  carte_2   : second CARTE
! Out iret      : 0 if same CARTE
!                 1 if not
!              
! --------------------------------------------------------------------------------------------------
!
    integer :: nb_elem, ielem, icmp, iret1, iret2
    character(len=8) :: mesh_1, mesh_2, mesh
    character(len=19) :: carte_1s, carte_2s
    integer :: jcesd1, jcesv1, jcesl1
    integer :: jcesd2, jcesv2, jcesl2
    integer :: iad1, iad2
    character(len=8) :: nomgd1, nomgd2, nomgd, type_test, elem_name
    character(len=3) :: tsca
    real(kind=8) :: zero, valr1, valr2, valr_error
    integer :: vali1, vali2
    integer :: ncmp1, ncmp2, nb_cmp
    character(len=80) :: valk1, valk2
    logical(kind=1) :: vall1, vall2, lok
    real(kind=8) :: epsi
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
!
! - Initializations
!
    iret     = 0
    zero     = 0.d0
    carte_1s = '&&CARCOMP.C1S'
    carte_2s = '&&CARCOMP.C2S'
    epsi     = 1.d-15
!
! - <GRANDEUR>
!
    call dismoi('NOM_GD', carte_1, 'CARTE', repk = nomgd1)
    call dismoi('NOM_GD', carte_2, 'CARTE', repk = nomgd2)
    if (nomgd1 .eq. nomgd2) then
        nomgd = nomgd1
    else
        call utmess('I', 'UTILITAI8_67')
        iret = 1
        goto 99
    endif
    call dismoi('TYPE_SCA', nomgd, 'GRANDEUR', repk = tsca)
!
! - Mesh
!
    call dismoi('NOM_MAILLA', carte_1, 'CHAMP', repk = mesh_1)
    call dismoi('NOM_MAILLA', carte_2, 'CHAMP', repk = mesh_2)
    if (mesh_1 .eq. mesh_2) then
        mesh = mesh_1
    else
        call utmess('I', 'UTILITAI8_68')
        iret = 1
        goto 99
    endif
    call dismoi('NB_MA_MAILLA', mesh, 'MAILLAGE', repi = nb_elem)
!
! - Changing in simpler datastructure
!
    call carces(carte_1, 'ELEM', ' ', 'V', carte_1s,&
                'A', iret1)
    call carces(carte_2, 'ELEM', ' ', 'V', carte_2s,&
                'A', iret2)
    call jeveuo(carte_1s//'.CESD', 'L', jcesd1)
    call jeveuo(carte_1s//'.CESV', 'L', jcesv1)
    call jeveuo(carte_1s//'.CESL', 'L', jcesl1)
    call jeveuo(carte_2s//'.CESD', 'L', jcesd2)
    call jeveuo(carte_2s//'.CESV', 'L', jcesv2)
    call jeveuo(carte_2s//'.CESL', 'L', jcesl2)
!
! - Comparing
!
    do ielem = 1, nb_elem
        ncmp1 = zi(jcesd1-1+5+4*(ielem-1)+3)
        ncmp2 = zi(jcesd2-1+5+4*(ielem-1)+3)
        if (ncmp1 .ne. ncmp2) then
            nb_cmp = ncmp1
        else
            call jenuno(jexnum(mesh//'.NOMMAI', ielem), elem_name)
            call utmess('I', 'UTILITAI8_69', sk = elem_name)
            iret = 1
            goto 99
        endif
        do icmp = 1, nb_cmp
            call cesexi('C', jcesd1, jcesl1, ielem, 1,&
                        1, icmp, iad1)
            call cesexi('C', jcesd2, jcesl2, ielem, 1,&
                        1, icmp, iad2)
            if (iad1 .eq. 0) then
                if (iad2 .ne. 0) then
                    call jenuno(jexnum(mesh//'.NOMMAI', ielem), elem_name)
                    call utmess('I', 'UTILITAI8_70', sk = elem_name)
                    iret = 1
                    goto 99
                endif
            elseif (iad2 .eq. 0) then
                if (iad1 .ne. 0) then
                    call jenuno(jexnum(mesh//'.NOMMAI', ielem), elem_name)
                    call utmess('I', 'UTILITAI8_70', sk = elem_name)
                    iret = 1
                    goto 99
                endif
            else
                if (tsca .eq. 'R') then
                    valr1 = zr(jcesv1-1-iad1)
                    valr2 = zr(jcesv2-1-iad2)
                    if (valr2 .ne. zero) then
                        valr_error = (valr1-valr2)/valr2
                        type_test  = 'RELATIF'
                    else
                        valr_error = valr1-valr2
                        type_test  = 'ABSOLU'
                    endif
                    if (type_test .eq. 'RELATIF') then
                        lok = ( abs( valr_error ) .le. epsi*abs(valr2))
                    else
                        lok = ( abs( valr_error ) .le. epsi )
                    endif
                else if (tsca.eq.'I') then
                    vali1 = zi(jcesv1-1-iad1)
                    vali2 = zi(jcesv2-1-iad2)
                    lok = vali1 .eq. vali2
                else if (tsca.eq.'L') then
                    vall1 = zl(jcesv1-1-iad1)
                    vall2 = zl(jcesv2-1-iad2)
                    lok = vall1 .and. vall2
                else if (tsca.eq.'K8') then
                    valk1 = zk8(jcesv1-1-iad1)
                    valk2 = zk8(jcesv2-1-iad2)
                    lok = valk1 .eq. valk2
                else if (tsca.eq.'K16') then
                    valk1 = zk16(jcesv1-1-iad1)
                    valk2 = zk16(jcesv2-1-iad2)
                    lok = valk1 .eq. valk2
                else if (tsca.eq.'K24') then
                    valk1 = zk24(jcesv1-1-iad1)
                    valk2 = zk24(jcesv2-1-iad2)
                    lok = valk1 .eq. valk2
                else if (tsca.eq.'K32') then
                    valk1 = zk32(jcesv1-1-iad1)
                    valk2 = zk32(jcesv2-1-iad2)
                    lok = valk1 .eq. valk2
                else if (tsca.eq.'K80') then
                    valk1 = zk80(jcesv1-1-iad1)
                    valk2 = zk80(jcesv2-1-iad2)
                    lok = valk1 .eq. valk2
                else
                    ASSERT(.false.)
                endif
            endif
            if (.not.lok) then
                call jenuno(jexnum(mesh//'.NOMMAI', ielem), elem_name)
                call utmess('I', 'UTILITAI8_71', sk = elem_name)
                iret = 1
                goto 99
            endif
        end do
    end do
!
 99 continue
!
    call detrsd('CHAM_ELEM_S', carte_1s)
    call detrsd('CHAM_ELEM_S', carte_2s)
!
    call jedema()
!
end subroutine
