subroutine op0008()
    implicit none
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
! ......................................................................
!     COMMANDE:  CALC_VECT_ELEM
!
! ......................................................................
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterc/gettco.h"
#include "asterc/getvid.h"
#include "asterc/getvis.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/chpver.h"
#include "asterfort/copisd.h"
#include "asterfort/dismoi.h"
#include "asterfort/infmaj.h"
#include "asterfort/jecreo.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/me2mac.h"
#include "asterfort/me2mme.h"
#include "asterfort/me2mth.h"
#include "asterfort/mecact.h"
#include "asterfort/memare.h"
#include "asterfort/rcmfmc.h"
#include "asterfort/sdmpic.h"
#include "asterfort/ss2mme.h"
#include "asterfort/u2mess.h"
#include "asterfort/vefnme.h"
#include "asterfort/wkvect.h"
    logical :: fnoevo
    integer :: nbchme, ibid, ich, icha, ied, ierd, jlmat, jlvf, ncha, nh
    integer :: nbss, n1, n3, n4, n5, n6, n7, n9, iad, iresu, jrelr, iexi, nbresu
    real(kind=8) :: time, tps(6), partps(3), vcmpth(4)
    complex(kind=8) :: cbid
    logical :: exitim
    character(len=8) :: matez, modele, cha, cara, kbid, k8bid, kmpic
    character(len=8) :: nomcmp(6), mo1, tych, materi, ncmpth(4), k8b
    character(len=16) :: type, oper, suropt, typco
    character(len=19) :: vresul, matel, resuel
    character(len=24) :: time2, cham, vfono, vafono, vmatel, ch24, mate
    integer :: iarg
    data nomcmp/'INST    ','DELTAT  ','THETA   ','KHI     ',&
     &     'R       ','RHO     '/
    data ncmpth/'TEMP','TEMP_MIL','TEMP_INF','TEMP_SUP'/
    data vcmpth/4*0.d0/
    data tps/0,2*1.0d0,3*0/
!
    call jemarq()
    call infmaj()
    vfono = ' '
    vafono = ' '
!
    call getres(matez, type, oper)
    matel=matez
!
    call getvtx(' ', 'OPTION', 0, iarg, 1,&
                suropt, n3)
!
!     - ON VERIFIE LE NOM DU MODELE:
!     -------------------------------
    modele = ' '
    call getvid(' ', 'MODELE', 0, iarg, 1,&
                modele, n1)
    call getvid(' ', 'CHARGE', 0, iarg, 0,&
                cha, ncha)
!
!
    if (ncha .lt. 0) then
        ncha = -ncha
        call jecreo(matel(1:8)//'.CHARGES', 'V V K8')
        n3=max(1,ncha)
        call jeecra(matel(1:8)//'.CHARGES', 'LONMAX', n3, ' ')
        call jeveuo(matel(1:8)//'.CHARGES', 'E', icha)
        call getvid(' ', 'CHARGE', 0, iarg, ncha,&
                    zk8(icha), ibid)
!
        call dismoi('F', 'NOM_MODELE', zk8(icha), 'CHARGE', ibid,&
                    mo1, ied)
        if ((n1.eq.1) .and. (modele.ne.mo1)) call u2mess('F', 'CALCULEL3_88')
!
        modele = mo1
        do 10,ich = 1,ncha
        call dismoi('F', 'NOM_MODELE', zk8(icha-1+ich), 'CHARGE', ibid,&
                    k8bid, ied)
        if (k8bid .ne. modele) then
            call u2mess('F', 'CALCULEL3_89')
        endif
10      continue
    endif
!
    if (suropt .eq. 'FORC_NODA') then
        call getvid(' ', 'SIEF_ELGA', 0, iarg, 1,&
                    cham, n6)
        if (n6 .ne. 0) then
            call chpver('F', cham(1:19), 'ELGA', 'SIEF_R', ierd)
        endif
    endif
!
    if (suropt .ne. 'FORC_NODA') then
        call dismoi('F', 'NB_SS_ACTI', modele, 'MODELE', nbss,&
                    kbid, ied)
    else
        nbss = 0
    endif
!
!
    cara = ' '
    materi = ' '
    call getvid(' ', 'CARA_ELEM', 0, iarg, 1,&
                cara, n5)
    call getvid(' ', 'CHAM_MATER', 0, iarg, 1,&
                materi, n4)
    if (n4 .ne. 0) then
        call rcmfmc(materi, mate)
    else
        mate = ' '
    endif
!
    call getvr8(' ', 'INST', 0, iarg, 1,&
                time, n7)
    exitim = .false.
    if (n7 .eq. 1) exitim = .true.
    call getvis(' ', 'MODE_FOURIER', 0, iarg, 1,&
                nh, n9)
    if (n9 .eq. 0) nh = 0
!
!     -- VERIFICATION DES CHARGES:
    if ((suropt.eq.'CHAR_MECA') .or. (suropt.eq.'CHAR_MECA_LAGR')) then
        do 20,ich = 1,ncha
        call dismoi('F', 'TYPE_CHARGE', zk8(icha-1+ich), 'CHARGE', ibid,&
                    k8bid, ied)
        if (k8bid(1:5) .ne. 'MECA_') then
            call u2mess('F', 'CALCULEL3_91')
        endif
20      continue
    endif
!
    if ((suropt.eq.'CHAR_THER')) then
        do 30,ich = 1,ncha
        call dismoi('F', 'TYPE_CHARGE', zk8(icha-1+ich), 'CHARGE', ibid,&
                    k8bid, ied)
        if (k8bid(1:5) .ne. 'THER_') call u2mess('F', 'CALCULEL3_92')
30      continue
    endif
!
    if ((suropt.eq.'CHAR_ACOU')) then
        do 40,ich = 1,ncha
        call dismoi('F', 'TYPE_CHARGE', zk8(icha-1+ich), 'CHARGE', ibid,&
                    k8bid, ied)
        if (k8bid(1:5) .ne. 'ACOU_') call u2mess('F', 'CALCULEL3_93')
40      continue
    endif
!
    if ((suropt.eq.'FORC_NODA')) then
        call dismoi('F', 'TYPE_CHAMP', cham, 'CHAMP', ibid,&
                    tych, ierd)
        if (tych(1:4) .ne. 'ELGA') call u2mess('F', 'CALCULEL3_94')
    endif
!
!
!
    if (suropt .eq. 'CHAR_MECA') then
!     ----------------------------------
!        -- TRAITEMENT DES ELEMENTS FINIS CLASSIQUES (.RELR)
!           (ET CREATION DE L'OBJET .RERR).
        call me2mme(modele, ncha, zk8(icha), mate, cara,&
                    exitim, time, matel, nh, 'G')
!
!        -- TRAITEMENT DES SOUS-STRUCTURES EVENTUELLES. (.RELC):
        call ss2mme(modele, 'SOUS_STRUC', matel, 'G')
!
!
    else if (suropt.eq.'CHAR_THER') then
!     ----------------------------------
        tps(1) = time
        time2 = '&TIME'
        call mecact('V', time2, 'MODELE', modele//'.MODELE', 'INST_R  ',&
                    6, nomcmp, ibid, tps, cbid,&
                    kbid)
        call mecact('V', '&&OP0008.PTEMPER', 'MODELE', modele//'.MODELE', 'TEMP_R',&
                    4, ncmpth, ibid, vcmpth, cbid,&
                    kbid)
        call me2mth(modele, ncha, zk8(icha), mate, cara,&
                    time2, '&&OP0008.PTEMPER', matel)
    else if (suropt.eq.'CHAR_ACOU') then
        call me2mac(modele, ncha, zk8(icha), mate, matel)
!
!
    else if (suropt.eq.'FORC_NODA') then
!     ----------------------------------
!       - ON CHERCHE LE NOM DU MODELE A ATTACHER AU VECT_ELEM :
        call jeveuo(cham(1:19)//'.CELK', 'L', iad)
        k8b = zk24(iad)(1:8)
        call gettco(k8b, typco)
        if (typco(1:14) .eq. 'MODELE_SDASTER') then
            modele = k8b
        else
            call getvid(' ', 'MODELE', 0, iarg, 1,&
                        modele, n1)
            if (n1 .eq. 0) call u2mess('F', 'CALCULEL3_95')
        endif
!
        partps(1) = 0.d0
        partps(2) = 0.d0
        partps(3) = 0.d0
        ch24 = ' '
        fnoevo=.false.
        call vefnme(modele, cham, cara, ' ', ' ',&
                    vfono, mate, ' ', nh, fnoevo,&
                    partps, ' ', ch24, ' ', suropt,&
                    ' ', 'V')
        call jeveuo(vfono, 'L', jlvf)
        vafono = zk24(jlvf)
        call jelira(vfono, 'LONUTI', nbchme, k8bid)
        vmatel = matel//'.RELR'
        call memare('G', matel, modele, mate, cara,&
                    'CHAR_MECA')
        call wkvect(vmatel, 'G V K24', nbchme, jlmat)
        vresul = matel(1:8)//'.VE001     '
        zk24(jlmat) = vresul
        call copisd('CHAMP', 'G', vafono(1:19), vresul)
    endif
!
!
!
!     -- SI MATEL N'EST PAS MPI_COMPLET, ON LE COMPLETE :
!     ----------------------------------------------------
    call jelira(matel//'.RELR', 'LONMAX ', nbresu, kbid)
    call jeveuo(matel//'.RELR', 'L', jrelr)
    do iresu = 1, nbresu
        resuel=zk24(jrelr+iresu-1)(1:19)
        call jeexin(resuel//'.RESL', iexi)
        if (iexi .eq. 0) goto 101
        call dismoi('F', 'MPI_COMPLET', resuel, 'RESUELEM', ibid,&
                    kmpic, ibid)
        ASSERT((kmpic.eq.'OUI').or.(kmpic.eq.'NON'))
        if (kmpic .eq. 'NON') call sdmpic('RESUELEM', resuel)
101     continue
    end do
!
!
!
!
    call jedema()
end subroutine
