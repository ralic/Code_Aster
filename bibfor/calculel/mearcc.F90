subroutine mearcc(option, mo, chin, chout)
    implicit none
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterfort/assert.h"
#include "asterfort/celces.h"
#include "asterfort/cescel.h"
#include "asterfort/cescre.h"
#include "asterfort/cesexi.h"
#include "asterfort/cesred.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/srlima.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=8) :: mo
    character(len=16) :: option
    character(len=24) :: chin, chout
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!     BUT: REDUIRE LE CHAMP DE CONTRAINTES (ELNO) DES ELEMENTS 3D
!          A LEURS FACES POUR L'OPTION SIRO_ELEM
!
!     IN  MO     : NOM DU MODELE
!     IN  OPTION : NOM DE L'OPTION
!     IN  CHIN   : CHAMP DE CONTRAINTE ELNO DES ELEMENTS 3D
!     OUT CHOUT  : CHAMP DE CONTRAINTES ELNO REDUIT AUX MAILLES DE PEAU
!
!
    integer :: nbcmp, nbnomx
    parameter   (nbcmp=6,nbnomx=9)
!
    integer :: nbma, ibid, iret, jma2d, jma3d, ndim
    integer :: jcesv3, jcesd3, jcesk3, jcesl3, jcesc3, jcesv2, jcesd2, ima
    integer :: jcesk2, jcesl2, jcesc2, jlcnx, jcnx, ipt, icp, ino2, ino3
    integer :: jco3, jco2, npt3, npt2, ipt2, ipt3, jpt3d, k, npt
    integer :: iad3, iad2, nucmp, numasu, numavo
!
    character(len=8) :: ma, comp(nbcmp), k8b, nomasu, nomavo, valk(2)
    character(len=19) :: chous, chins
    character(len=24) :: mail2d, mail3d, mailto, ligrmo
!
    data comp/'SIXX','SIYY','SIZZ','SIXY','SIXZ','SIYZ'/
!
    mail2d='&&MEARCC.MAILLE_FACE'
    mail3d='&&MEARCC.MAILLE_3D_SUPP'
    mailto='&&MEARCC.MAILLE_2D_3D'
!
    call jemarq()
!
    call dismoi('F', 'NOM_MAILLA', mo, 'MODELE', ibid,&
                ma, iret)
    call dismoi('F', 'DIM_GEOM', ma, 'MAILLAGE', ndim,&
                k8b, iret)
    ASSERT(ndim.eq.3)
!
!     RECUPERATION DES MAILLES DE FACES ET DES MAILLES 3D SUPPORT
    call srlima(mo, mail2d, mail3d, mailto, nbma)
    call jeveuo(mail2d, 'L', jma2d)
    call jeveuo(mail3d, 'L', jma3d)
!
!     =============================================================
! --- POUR CHAQUE NOEUD DE LA MAILLE 3D SUPPORT, ON RECOPIE LES
!     CONTRAINTES AUX NOEUDS DE LA MAILLE DE PEAU
!     =============================================================
!
!     TRANSFORMATION DU CHAMP 3D (IN) EN CHAMP SIMPLE
    chins='&&MEARCC.CHIN_S'
    call celces(chin, 'V', chins)
    call jeveuo(chins//'.CESV', 'L', jcesv3)
    call jeveuo(chins//'.CESD', 'L', jcesd3)
    call jeveuo(chins//'.CESK', 'L', jcesk3)
    call jeveuo(chins//'.CESL', 'L', jcesl3)
    call jeveuo(chins//'.CESC', 'L', jcesc3)
!
!     CREATION DU CHAMP 2D (OUT) SIMPLE
    chous='&&MEARCC.CHOUT_S'
    call cescre('V', chous, 'ELNO', ma, 'SIEF_R',&
                nbcmp, comp, -1, -1, -nbcmp)
!
    call jeveuo(chous//'.CESV', 'E', jcesv2)
    call jeveuo(chous//'.CESD', 'E', jcesd2)
    call jeveuo(chous//'.CESK', 'E', jcesk2)
    call jeveuo(chous//'.CESL', 'E', jcesl2)
    call jeveuo(chous//'.CESC', 'E', jcesc2)
!
!     CORRESPONDANCE PT_MAILLE 2D / PT_MAILLE 3D: ZI(JPT3D)
!     POUR CHAQUE POINT DE LA MAILLE 2D, ON CHERCHE LE
!     POINT DE LA MAILLE 3D CORRESPONDANT
    call wkvect('&&MEARCC.PT3D', 'V V I', nbma*nbnomx, jpt3d)
    call jeveuo(ma//'.CONNEX', 'L', jcnx)
    call jeveuo(jexatr(ma//'.CONNEX', 'LONCUM'), 'L', jlcnx)
    do 100 ima = 1, nbma
        jco3=jcnx+zi(jlcnx-1+zi(jma3d+ima-1))-1
        jco2=jcnx+zi(jlcnx-1+zi(jma2d+ima-1))-1
        npt3=zi(jcesd3-1+5+4*(zi(jma3d+ima-1)-1)+1)
        npt2=zi(jcesd2-1+5+4*(zi(jma2d+ima-1)-1)+1)
        k=0
        do 110 ipt2 = 1, npt2
            ino2=zi(jco2+ipt2-1)
            do 111 ipt3 = 1, npt3
                ino3=zi(jco3+ipt3-1)
                if (ino3 .eq. ino2) then
                    k=k+1
                    zi(jpt3d+nbnomx*(ima-1)+k-1)=ipt3
                    goto 110
                endif
111          continue
110      continue
100  continue
!
!
!     REMPLISSAGE DU CHAMP SIMPLE 3D
    do 200 ima = 1, nbma
        npt=zi(jcesd2-1+5+4*(zi(jma2d+ima-1)-1)+1)
        call jenuno(jexnum(ma//'.NOMMAI', zi(jma2d+ima-1)), k8b)
        call jenuno(jexnum(ma//'.NOMMAI', zi(jma3d+ima-1)), k8b)
        do 210 ipt = 1, npt
            do 220 icp = 1, nbcmp
                nucmp=indik8( zk8(jcesc3), comp(icp), 1, zi(jcesd3+1)&
                )
!
                call cesexi('C', jcesd3, jcesl3, zi(jma3d+ima-1), zi(jpt3d+nbnomx*(ima-1)+ipt-1),&
                            1, nucmp, iad3)
                if (iad3 .eq. 0) then
                    numasu=zi(jma2d+ima-1)
                    numavo=zi(jma3d+ima-1)
                    call jenuno(jexnum(ma//'.NOMMAI', numasu), nomasu)
                    call jenuno(jexnum(ma//'.NOMMAI', numavo), nomavo)
                    valk(1)=nomavo
                    valk(2)=nomasu
                    call utmess('F', 'CALCULEL5_52', nk=2, valk=valk)
                endif
                call cesexi('S', jcesd2, jcesl2, zi(jma2d+ima-1), ipt,&
                            1, nucmp, iad2)
                zr(jcesv2-iad2-1)=zr(jcesv3+iad3-1)
                zl(jcesl2-iad2-1)=.true.
220          continue
210      continue
200  end do
!
    call cesred(chous, nbma, zi(jma2d), 0, k8b,&
                'V', chous)
!
    call dismoi('F', 'NOM_LIGREL', mo, 'MODELE', ibid,&
                ligrmo, iret)
!
    call cescel(chous, ligrmo, option, 'PSIG3D', 'OUI',&
                ibid, 'V', chout, 'F', ibid)
!
    call jedetr('&&MEARCC.PT3D')
    call jedetr(mail2d)
    call jedetr(mail3d)
    call jedetr(mailto)
    call detrsd('CHAM_ELEM_S', chous)
    call detrsd('CHAM_ELEM_S', chins)
!
    call jedema()
!
end subroutine
