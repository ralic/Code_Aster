subroutine op0048()
    implicit none
#include "asterf_types.h"
#include "asterc/getres.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/cesexi.h"
#include "asterfort/chpchd.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnscre.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisd.h"
#include "asterfort/getvid.h"
#include "asterfort/getvr8.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jelira.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/vrcins.h"
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: sam.cuvilliez at edf.fr
!-----------------------------------------------------------------------
!
!     Operateur cache POST_K_VARC, appele depuis la macro POST_K1_K2_K3
!
!     Produit le cham_no de variables de commande pour une sd_resultat 
!     donnee a l'instant considere. Le champ est type de la maniere
!     suivante, en fonction du nom de la variable de commande :
!         TEMP  -> TEMP_R
!         NEUT1 -> NEUT_R
!
!     Les seules varc autorisees sont : TEMP, NEUT1
!
!-----------------------------------------------------------------------
!
    integer :: nvrcmx
    parameter ( nvrcmx = 2 )
    character(len=8) :: nomvrc(nvrcmx), nomgrd(nvrcmx), nomcmp(nvrcmx)
    data nomvrc/ 'TEMP'  , 'NEUT1'  /
    data nomgrd/ 'TEMP_R', 'NEUT_R' /
    data nomcmp/ 'TEMP'  , 'X1'     /
!
    character(len=6) :: nompro
    parameter (nompro='OP0048')
!
    integer :: ier, iret, ibid, nbno, ino, k, nbcvrc, jcesd1, jcesl1
    integer :: nbma, ima, iad, itrou
    real(kind=8) :: inst
    character(len=2) :: codret
    character(len=8) :: chnout, resu, model, chmat, carael, tych, mesh
    character(len=8) :: nomvarc, varc
    character(len=16) :: nomsym, k16bi1, k16bi2
    character(len=19) :: celvrc, cnovrc, cnsvrc, cnsvr2, ces1
    character(len=19) :: cart2
    integer, pointer :: cnsvrc_d(:) => null()
    real(kind=8), pointer :: cnsvrc_v(:) => null()
    real(kind=8), pointer :: cnsvr2_v(:) => null()
    character(len=8), pointer :: cvrcvarc(:) => null()
    character(len=16), pointer :: cesv(:) => null()
    aster_logical, pointer :: cnsvrc_l(:) => null()
    aster_logical, pointer :: cnsvr2_l(:) => null()
 
!-----------------------------------------------------------------------
!
    call jemarq()
!
!-----------------------------------------------------------------------
!    prealables
!-----------------------------------------------------------------------
!
!   recup des entrees / sortie de l'operateur
    call getvid(' ', 'RESULTAT', scal=resu, nbret=ier)
    call getvr8(' ', 'INST', scal=inst, nbret=ier)
    call getres(chnout, k16bi1, k16bi2)
!
!   recup du modele
    call dismoi('NOM_MODELE', resu, 'RESULTAT', repk=model)
    ASSERT( (model .ne. '#AUCUN') .or. (model .ne. '#PLUSIEURS') )
!
!   recup du maillage
    call dismoi('NOM_MAILLA', model, 'MODELE', repk=mesh)
    call dismoi('NB_NO_MAILLA', mesh, 'MAILLAGE', repi=nbno)
    call dismoi('NB_MA_MAILLA', mesh, 'MAILLAGE', repi=nbma)
!
!   recup du cham_mater
    call dismoi('CHAM_MATER', resu, 'RESULTAT', repk=chmat)
    ASSERT( (chmat .ne. '#AUCUN') .or. (chmat .ne. '#PLUSIEURS') )
!
!   presence de variables de commande obligatoire
    call jeexin(chmat//'.CVRCVARC', iret)
    ASSERT( iret .gt. 0 )
!
!   pas de CARA_ELEM
    call dismoi('CARA_ELEM', resu, 'RESULTAT', repk=carael)
    ASSERT( carael .eq. '#AUCUN' )
!
!   verif que une et une seule varc est presente parmi nomvrc()
    call jelira(chmat//'.CVRCVARC', 'LONMAX', nbcvrc)
    call jeveuo(chmat//'.CVRCVARC', 'L', vk8=cvrcvarc)
    nomvarc=cvrcvarc(1)
    itrou = indik8(nomvrc, nomvarc, 1, nvrcmx)
    ASSERT( itrou .gt. 0 )
    do k = 1, nbcvrc
        ASSERT( nomvarc .eq. cvrcvarc(k) )
    enddo
!
!   verif pour interdire le cas particulier de la VARC TEMP aux points
!   de Gauss xfem (cas du chainage thermo mecanique complet avec xfem)
    do k = 1, nbcvrc
        varc=cvrcvarc(k)
        cart2 = chmat//'.'//varc//'.2'
        ces1='&&'//nompro//'.CES1'
        call carces(cart2, 'ELEM', ' ', 'V', ces1, 'A', iret)
        ASSERT(iret.eq.0)
        call jeveuo(ces1//'.CESD', 'L', jcesd1)
        call jeveuo(ces1//'.CESL', 'L', jcesl1)
        call jeveuo(ces1//'.CESV', 'L', vk16=cesv)
        do ima = 1, nbma
            call cesexi('C', jcesd1, jcesl1, ima, 1, 1, 1, iad)
            if ( iad .le. 0 ) then
                cycle
            endif
            iad=iad-1
            nomsym=cesv(iad+4)
            if (nomsym .eq. 'TEMP_ELGA') then
                call utmess('F', 'XFEM_1', sk=chmat)
            endif
        enddo
        call detrsd('CHAM_ELEM_S', ces1)
    enddo
!
!-----------------------------------------------------------------------
!   recup du cham_elem (ELNO) de varc a l'instant inst
!-----------------------------------------------------------------------
!
    celvrc = '&&'//nompro//'.CELVRC'
    call vrcins(model, chmat, ' ', inst, celvrc, codret, nompaz='PVARCNO')
    ASSERT( codret .eq. 'OK' )
    call exisd('CHAM_ELEM', celvrc, iret)
    ASSERT( iret .eq. 1 )
    call dismoi('TYPE_CHAMP', celvrc, 'CHAMP', repk=tych)
    ASSERT( tych .eq. 'ELNO' )
!
!-----------------------------------------------------------------------
!   transformation en un cham_no de varc a l'instant inst, dont
!   le type de grandeur depend de la variable de commande :
!   TEMP  -> TEMP_R
!   NEUT1 -> NEUT_R
!-----------------------------------------------------------------------
!
!   transformation cham_elem (ELNO) -> cham_no : celvrc -> cnovrc
    cnovrc = '&&'//nompro//'.CNOVRC'
    call chpchd(celvrc, 'NOEU', ' ', 'NON', 'V', cnovrc)
    call detrsd('CHAM_ELEM', celvrc)

!   transformation cham_no -> cham_no_s : cnovrc -> cnsvrc
    cnsvrc = '&&'//nompro//'.CNSVRC'
    call cnocns(cnovrc, 'V', cnsvrc)
    call jeveuo(cnsvrc//'.CNSD', 'L', vi=cnsvrc_d)
!       on s'assure qu'il n'y a qu'une seule cmp
    ASSERT( cnsvrc_d(2) .eq. 1 )
    call detrsd('CHAM_NO', cnovrc)
!
!   creation d'un cham_no_s cnsvr2 dont le type de la grandeur depend
!   de nomvarc
    cnsvr2 = '&&'//nompro//'.CNSVR2'
    call cnscre(mesh, nomgrd(itrou), 1, [nomcmp(itrou)], 'V', cnsvr2)
!
!   copie de cnsvrc dans cnsvr2
    call jeveuo(cnsvrc//'.CNSL', 'L', vl=cnsvrc_l)
    call jeveuo(cnsvrc//'.CNSV', 'L', vr=cnsvrc_v)
    call jeveuo(cnsvr2//'.CNSL', 'E', vl=cnsvr2_l)
    call jeveuo(cnsvr2//'.CNSV', 'E', vr=cnsvr2_v)
    do ino = 1, nbno
        if ( cnsvrc_l(ino) ) then
            cnsvr2_l(ino) = .true.
            cnsvr2_v(ino) = cnsvrc_v(ino)
        endif
    enddo
    call detrsd('CHAM_NO_S', cnsvrc)
!
!   transformation cham_no_s ->  cham_no : cnsvr2 -> chnout
    call cnscno(cnsvr2, ' ', 'NON', 'G', chnout, 'F', ibid)
    call detrsd('CHAM_NO_S', cnsvr2)
!
!-----------------------------------------------------------------------
!   fin
!-----------------------------------------------------------------------
!
    call jedema()
!
end subroutine
