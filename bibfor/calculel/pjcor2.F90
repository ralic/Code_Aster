subroutine pjcor2(noca, cns1z, ces2z, ligrel, corres,&
                  nompaz, option, iret)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
! --------------------------------------------------------------------------------------------------
!
!                   Commande PROJ_CHAMP
!
!   Recopie les valeurs projetées aux NOEUDS du maillage "SOUS_POINT"
!   sur les sous-points du maillage 2
!   Utilisation de la sd corres (tableau auxiliaire .PJEF_SP)
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    character(len=*) :: cns1z, ces2z
    character(len=8) :: noca, nompaz
    character(len=16) :: option
    character(len=16) :: corres
    character(len=19) :: ligrel
    integer :: iret
!
#include "jeveux.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesvar.h"
#include "asterfort/detrsd.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
!
! --------------------------------------------------------------------------------------------------
!
    character(len= 8) :: nomgd, nompar
    character(len=19) :: cns1, ces2, cel2, dcel
    character(len=24) :: valk(5)
    integer :: ipo
    integer :: jce2c, jce2l, jce2d, jce2k
    integer :: jcns1c, jcns1l
    integer :: nbno1, ncmp1
    integer :: iad2, icmp, ima, ipt, isp, jlgrf
!    integer :: ima, ipt, isp, jlgrf
!
    real(kind=8), pointer :: cnsv(:) => null()
    real(kind=8), pointer :: cesv(:) => null()
    integer, pointer :: cnsd(:) => null()
    character(len=8), pointer :: cnsk(:) => null()
    integer, pointer :: pjef_sp(:) => null()
! --------------------------------------------------------------------------------------------------
    call jemarq()
    iret = 0
    cns1 = cns1z
    ces2 = ces2z
!
    cel2 = '&&PJCOR2.CEL2'
!
! --------------------------------------------------------------------------------------------------
!   Récupération d'informations dans cns1
    call jeveuo(cns1//'.CNSK', 'L', vk8=cnsk)
    call jeveuo(cns1//'.CNSD', 'L', vi=cnsd)
    call jeveuo(cns1//'.CNSC', 'L', jcns1c)
    call jeveuo(cns1//'.CNSV', 'L', vr=cnsv)
    call jeveuo(cns1//'.CNSL', 'L', jcns1l)
    call jelira(cns1//'.CNSC', 'LONMAX', ncmp1)
!
    nomgd = cnsk(2)
    nbno1 = cnsd(1)
!
    if (nomgd .eq. 'TEMP_R') then
        option = 'INI_SP_MATER'
        nompar = 'PTEMMAT'
    else if (nomgd.eq.'HYDR_R') then
        option = 'INI_SP_MATER'
        nompar = 'PHYDMAT'
    else if (nomgd.eq.'NEUT_R') then
        option = 'INI_SP_MATER'
        nompar = 'PNEUMAT'
    else if (nomgd.eq.'SIEF_R') then
        option = 'INI_SP_RIGI'
        nompar = 'PCONTPR'
    else
        ASSERT(.false.)
    endif
!
! --------------------------------------------------------------------------------------------------
!   Allocation de ces2 (ELGA):
    call detrsd('CHAM_ELEM_S', ces2)
!
    dcel='&&PJCOR2'
!
    call jeveuo(ligrel//'.LGRF', 'L', jlgrf)
    call cesvar(noca, ' ', ligrel, dcel)
    call alchml(ligrel, option, nompar, 'V', cel2,&
                iret, dcel)
!
    nompaz=nompar
    if (iret .eq. 1) then
        valk(1) = nompar
        valk(2) = option
        valk(3) = ligrel
        valk(4) = cel2
        call utmess('F', 'CALCULEL_50', nk=4, valk=valk)
    endif
!
    call celces(cel2, 'V', ces2)
    call detrsd('CHAM_ELEM', cel2)
!
    call jeveuo(ces2//'.CESD', 'L', jce2d)
    call jeveuo(ces2//'.CESC', 'L', jce2c)
    call jeveuo(ces2//'.CESV', 'E', vr=cesv)
    call jeveuo(ces2//'.CESL', 'E', jce2l)
    call jeveuo(ces2//'.CESK', 'L', jce2k)
!
! --------------------------------------------------------------------------------------------------
!   Remplissage des valeurs de ces2
    call jeveuo(corres//'.PJEF_SP', 'L', vi=pjef_sp)
!   nbno1 est le nombre de pseudo-noeuds du maillage 2
    do ipo = 1, nbno1
            ima= pjef_sp(3*(ipo-1)+1)
            ipt= pjef_sp(3*(ipo-1)+2)
            isp= pjef_sp(3*(ipo-1)+3)
        do icmp = 1, ncmp1
            call cesexi('C', jce2d, jce2l, ima, ipt,&
                        isp, icmp, iad2)
            if (iad2 .gt. 0) then
                cesv(iad2)=cnsv((ipo-1)*ncmp1+icmp)
            endif
        enddo
    enddo
!
    call jedema()
end subroutine
