subroutine rdtres(resu1, resu2, noma1, noma2, corrn,&
                  corrm, iocc)
    implicit none
#include "jeveux.h"
!
#include "asterc/gettco.h"
#include "asterc/getvr8.h"
#include "asterc/getvtx.h"
#include "asterfort/assert.h"
#include "asterfort/cormgi.h"
#include "asterfort/dismoi.h"
#include "asterfort/exlima.h"
#include "asterfort/initel.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/rdtchp.h"
#include "asterfort/rsadpa.h"
#include "asterfort/rscrsd.h"
#include "asterfort/rsexch.h"
#include "asterfort/rsnoch.h"
#include "asterfort/rsnopa.h"
#include "asterfort/rsutc4.h"
#include "asterfort/rsutnu.h"
#include "asterfort/u2mesk.h"
#include "asterfort/wkvect.h"
    character(len=8) :: noma1, noma2, resu1, resu2
    character(len=24) :: corrn, corrm
    integer :: iocc
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!-------------------------------------------------------------------
! BUT: REDUIRE UNE SD_RESULTAT SUR UN MAILLAGE REDUIT
!
!  RESU1  : IN  : LA SD_RESULTAT A REDUIRE
!  RESU2  : OUT : LA SD_RESULTAT REDUITE
!  NOMA1  : IN  : MAILLAGE AVANT REDUCTION
!  NOMA2  : IN  : MAILLAGE REDUIT
!  IOCC   : IN  : NUMERO DE L'OCURENCE DU MOT-CLE FACTEUR RESU
!  CORRN  : IN  : NOM DE L'OBJET CONTENANT LA CORRESPONDANCE
!                 INO_RE -> INO
!  CORRM  : IN  : NOM DE L'OBJET CONTENANT LA CORRESPONDANCE
!                 IMA_RE -> IMA
!-------------------------------------------------------------------
!
    integer :: i, nbordr, ibid, nbpara, jpa, nbac, nbpa, iad1, iad2, j
    integer :: iret, nbsym, isym, iordr, jordr, n1, j1, ima2, ima1
    integer :: nbgrel, igr, jcorrm, iel, nbma1, nbma2, jcoinv, cret
    integer :: ite, jmail2
    real(kind=8) :: prec
    character(len=1) :: kbid
    character(len=16) :: typres, nomsym(200), nopara
    character(len=8) :: model1, crit, type, model2, kchml
    character(len=19) :: chp, chpre, ligrel
    logical :: acceno, redpos
    integer :: iarg
!     -----------------------------------------------------------------
!
    call jemarq()
!
    ASSERT(noma1.ne.noma2)
    ASSERT(resu1.ne.resu2)
    ASSERT(corrn.ne.' ')
    ASSERT(corrm.ne.' ')
!
!
!
!     1- FABRICATION DU LIGREL REDUIT (LIGREL) SI NECESSAIRE :
!     --------------------------------------------------------
    call dismoi('F', 'EXI_CHAM_ELEM', resu1, 'RESULTAT', ibid,&
                kchml, iret)
    if (kchml .eq. 'OUI') then
        call dismoi('F', 'MODELE_1', resu1, 'RESULTAT', ibid,&
                    model1, iret)
        ASSERT(model1(1:1).ne.'#')
        ASSERT(model1.ne.' ')
    else
        model1=' '
    endif
    if (model1 .ne. ' ') then
!       1.1- ON RESTREINT LE LIGREL DU MODELE SUR LES MAILLES RETENUES :
        call exlima('RESTREINT', 1, 'V', model1, ligrel)
!
!       1.2- IL FAUT ENSUITE TRANSFERER LE LIGREL DE NOMA1 SUR NOMA2 :
!       1.2.1  OBJET .LGRF A MODIFIER :
        call jeveuo(ligrel//'.LGRF', 'E', j1)
        ASSERT(zk8(j1-1+1).eq.noma1)
        zk8(j1-1+1)=noma2
!       1.2.2  OBJET .NBNO A MODIFIER :
        call jeveuo(ligrel//'.NBNO', 'E', j1)
        zi(j1-1+1)=0
!
!       1.2.3 OBJET .LIEL A MODIFIER :
!       -- ON A BESOIN DE LA CORRESPONDANCE INVERSE :
        call jeveuo(corrm, 'L', jcorrm)
        call dismoi('F', 'NB_MA_MAILLA', noma1, 'MAILLAGE', nbma1,&
                    kbid, iret)
        call dismoi('F', 'NB_MA_MAILLA', noma2, 'MAILLAGE', nbma2,&
                    kbid, iret)
        call wkvect('&&RDTRES.CORRM_INV', 'V V I', nbma1, jcoinv)
        do 10,ima2=1,nbma2
        ima1=zi(jcorrm-1+ima2)
        ASSERT(ima1.gt.0)
        zi(jcoinv-1+ima1)=ima2
10      continue
!
        call jelira(ligrel//'.LIEL', 'NUTIOC', nbgrel)
        do 30,igr=1,nbgrel
        call jelira(jexnum(ligrel//'.LIEL', igr), 'LONMAX', n1)
        call jeveuo(jexnum(ligrel//'.LIEL', igr), 'E', j1)
        do 20,iel=1,n1-1
        ima1=zi(j1-1+iel)
        ima2=zi(jcoinv-1+ima1)
        zi(j1-1+iel)=ima2
20      continue
30      continue
!
!       1.2.4  OBJETS A DETRUIRE :
        call jedetr(ligrel//'.NEMA')
        call jedetr(ligrel//'.SSSA')
        call jedetr(ligrel//'.PRNS')
        call jedetr(ligrel//'.LGNS')
!       1.2.5  OBJETS A RECONSTRUIRE :
        call jedetr(ligrel//'.PRNM')
        call initel(ligrel)
        call jedetr(ligrel//'.REPE')
        call cormgi('V', ligrel)
!
!       1.2.5  CONSTRUCTION D'UN "FAUX" MODELE NECESSAIRE
!              A IRCHME / ELGA :
        model2=resu2
        call wkvect(model2//'.MAILLE', 'V V I', nbma2, jmail2)
        do 31,igr=1,nbgrel
        call jelira(jexnum(ligrel//'.LIEL', igr), 'LONMAX', n1)
        call jeveuo(jexnum(ligrel//'.LIEL', igr), 'E', j1)
        ite=zi(j1-1+n1)
        do 21,iel=1,n1-1
        ima2=zi(j1-1+iel)
        ASSERT(ima2.gt.0)
        ASSERT(ima2.le.nbma2)
        zi(jmail2-1+ima2)=ite
21      continue
31      continue
!
!
    else
        ligrel=' '
        model2=' '
    endif
!
!
!
!     2- ALLOCATION DE RESU2 :
!     ------------------------------------
    call getvr8('RESU', 'PRECISION', 1, iarg, 1,&
                prec, n1)
    call getvtx('RESU', 'CRITERE', 1, iarg, 1,&
                crit, n1)
    call rsutnu(resu1, 'RESU', iocc, '&&RDTRES.NUME_ORDRE', nbordr,&
                prec, crit, iret)
    if (iret .ne. 0) then
        call u2mesk('F', 'CALCULEL4_61', 1, resu1)
    endif
    if (nbordr .eq. 0) then
        call u2mesk('F', 'CALCULEL4_62', 1, resu1)
    endif
    call jeveuo('&&RDTRES.NUME_ORDRE', 'L', jordr)
    call gettco(resu1, typres)
    call rscrsd('V', resu2, typres, nbordr)
!
!
!
!     3- ON CALCULE LES CHAMPS DE RESU2 :
!     ------------------------------------
    call rsutc4(resu1, 'RESU', iocc, 200, nomsym,&
                nbsym, acceno)
    ASSERT(nbsym.gt.0)
    cret=0
    do 50,isym=1,nbsym
    redpos=.true.
    do 40,i=1,nbordr
    iordr=zi(jordr+i-1)
    call rsexch(' ', resu1, nomsym(isym), iordr, chp,&
                iret)
    if (iret .gt. 0) goto 40
    call rsexch(' ', resu2, nomsym(isym), iordr, chpre,&
                iret)
!
!         -- REDUCTION DU CHAMP :
    call rdtchp(corrn, corrm, chp, chpre, 'V',&
                noma1, noma2, ligrel, cret)
    if (cret .eq. 0) then
        call rsnoch(resu2, nomsym(isym), iordr)
    else
        redpos=.false.
    endif
40  continue
    if (.not.redpos) call u2mesk('A', 'CALCULEL4_7', 1, nomsym( isym))
    50 end do
!
!
!     4- ON RECOPIE LES VARIABLES D'ACCES :
!     ------------------------------------
    call rsnopa(resu1, 2, '&&RDTRES.NOMS_PARA', nbac, nbpa)
    nbpara=nbac+nbpa
    call jeveuo('&&RDTRES.NOMS_PARA', 'L', jpa)
    do 70,i=1,nbordr
    iordr=zi(jordr+i-1)
    do 60 j = 1, nbpara
        nopara=zk16(jpa-1+j)
!
!         -- CERTAINS PARAMETRES NE DOIVENT PAS ETRE RECOPIES:
        if (nopara .eq. 'CARAELEM') goto 60
        if (nopara .eq. 'CHAMPMAT') goto 60
        if (nopara .eq. 'EXCIT') goto 60
!
        call rsadpa(resu1, 'L', 1, nopara, iordr,&
                    1, iad1, type)
        call rsadpa(resu2, 'E', 1, nopara, iordr,&
                    1, iad2, type)
        if (nopara .eq. 'MODELE') ASSERT(type.eq.'K8')
        if (type .eq. 'I') then
            zi(iad2)=zi(iad1)
        else if (type.eq.'R') then
            zr(iad2)=zr(iad1)
        else if (type.eq.'C') then
            zc(iad2)=zc(iad1)
        else if (type.eq.'K80') then
            zk80(iad2)=zk80(iad1)
        else if (type.eq.'K32') then
            zk32(iad2)=zk32(iad1)
        else if (type.eq.'K24') then
            zk24(iad2)=zk24(iad1)
        else if (type.eq.'K16') then
            zk16(iad2)=zk16(iad1)
        else if (type.eq.'K8') then
            if (nopara .ne. 'MODELE') then
                zk8(iad2)=zk8(iad1)
            else
                zk8(iad2)=model2
            endif
        else
            ASSERT(.false.)
        endif
60  continue
    70 end do
    call jedetr('&&RDTRES.NOMS_PARA')
    call jedetr('&&RDTRES.NUME_ORDRE')
    call jedetr('&&RDTRES.CORRM_INV')
!
!
    call jedema()
end subroutine
