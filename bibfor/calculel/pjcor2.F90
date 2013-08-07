subroutine pjcor2(noca, cns1z, ces2z, ligrel, corres,&
                  nompaz, iret)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!
    implicit none
#include "jeveux.h"
!
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
#include "asterfort/u2mesk.h"
    character(len=*) :: cns1z, ces2z
    character(len=8) :: noca, nompaz
    character(len=16) :: corres, option
    character(len=19) :: ligrel
    integer :: iret
!
! COMMANDE PROJ_CHAMP
!   RECOPIE DES VALEURS PROJETEES AUX NOEUDS DU MAILLAGE "SOUS-POINT"
!     SUR LES SOUS-POINTS DU MAILLAGE 2
!   UTILISATION DE LA SD CORRES (TABLEAU AUXILIAIRE .PJEF_SP)
!     ------------------------------------------------------------------
!     VARIABLES LOCALES:
!     ------------------
    character(len=8) :: nomgd, nompar
    character(len=19) :: cns1, ces2, cel2
    character(len=19) :: dcel
    character(len=24) :: valk(5)
    integer :: jpo, ipo
    integer :: jce2c, jce2l, jce2v, jce2d, jce2k
!
!
    integer :: jcns1c, jcns1l, jcns1v, jcns1k, jcns1d
    integer :: nbno1, ncmp1
    integer :: iad2
    integer :: icmp, icmp1
!
    integer :: ima, ipt, isp, jlgrf
!
!     ------------------------------------------------------------------
!
    call jemarq()
    iret = 0
    cns1 = cns1z
    ces2 = ces2z
!
!
    cel2 = '&&PJCOR2.CEL2'
!
!     1- RECUPERATION D'INFORMATIONS DANS CNS1 :
!     ------------------------------------------
    call jeveuo(cns1//'.CNSK', 'L', jcns1k)
    call jeveuo(cns1//'.CNSD', 'L', jcns1d)
    call jeveuo(cns1//'.CNSC', 'L', jcns1c)
    call jeveuo(cns1//'.CNSV', 'L', jcns1v)
    call jeveuo(cns1//'.CNSL', 'L', jcns1l)
    call jelira(cns1//'.CNSC', 'LONMAX', ncmp1)
!
    nomgd = zk8(jcns1k-1+2)
    nbno1 = zi(jcns1d-1+1)
!
!
!
    option = 'INI_SP_MATER'
!
    if (nomgd .eq. 'TEMP_R') then
        nompar = 'PTEMMAT'
    else if (nomgd.eq.'HYDR_R') then
        nompar = 'PHYDMAT'
    else if (nomgd.eq.'NEUT_R') then
        nompar = 'PNEUMAT'
    else
        ASSERT(.false.)
    endif
!
!------------------------------------------------------------------
!     2- ALLOCATION DE CES2 (ELGA):
!
!
!     -----------------------------
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
!
    if (iret .eq. 1) then
        valk(1) = nompar
        valk(2) = option
        valk(3) = ligrel
        valk(4) = cel2
        call u2mesk('F', 'CALCULEL_50', 4, valk)
    endif
!
    call celces(cel2, 'V', ces2)
    call detrsd('CHAM_ELEM', cel2)
!
    call jeveuo(ces2//'.CESD', 'L', jce2d)
    call jeveuo(ces2//'.CESC', 'L', jce2c)
    call jeveuo(ces2//'.CESV', 'E', jce2v)
    call jeveuo(ces2//'.CESL', 'E', jce2l)
    call jeveuo(ces2//'.CESK', 'L', jce2k)
!
!
!
!
!------------------------------------------------------------------
!     3- REMPLISSAGE DES VALEURS DE CES2 :
!     -------------------------------
!
!
    call jeveuo(corres//'.PJEF_SP', 'L', jpo)
!
! NBNO1 EST LE NOMBRE DE PSEUDO-NOEUDS DU MAILLAGE 2
!
    do 92 icmp1 = 1, ncmp1
        icmp=icmp1
!
        do 98 ipo = 1, nbno1
            ima= zi(jpo+3*(ipo-1))
            ipt= zi(jpo+3*(ipo-1)+1)
            isp= zi(jpo+3*(ipo-1)+2)
!
            call cesexi('C', jce2d, jce2l, ima, ipt,&
                        isp, icmp, iad2)
            if (iad2 .le. 0) goto 98
            zr(jce2v-1+iad2)=zr(jcns1v+(ipo-1)*ncmp1+icmp-1)
!
98      continue
92  end do
!
!
    call jedema()
end subroutine
