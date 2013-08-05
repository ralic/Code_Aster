subroutine cmpcha(nomcha, nomcmp, corr1, corr2, ncmp,&
                  ncmpmx)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/exisdg.h"
#include "asterfort/infniv.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
    character(len=*) :: nomcha, nomcmp, corr1, corr2
    integer :: ncmp, ncmpmx
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
! ----------------------------------------------------------------------
! BUT : FOURNIR LE NOMBRE ET LE NOM DES COMPOSANTES DU CHAMP NOMCHA
!       AINSI QUE LA CORRESPONDANCE "COMPOSANTE CHAMP <=> COMPOSANTE
!       GRANDEUR ASSOCIEE"
!       FONCTIONNE AVEC LES CARTES, LES CHAM_NO ET LES CHAM_EL*
! ----------------------------------------------------------------------
! ARGUMENTS :
! ===========
! NOMCHA  IN  K19 : NOM DU CHAMP
! NCMPMX  OUT I   : NOMBRE DE COMPOSANTES DE LA GRANDEUR ASSOCIEE
! NCMP    OUT I   : NOMBRE DE COMPOSANTES EFFECTIVEMENT PRESENTES DANS
!                   LE CHAMP
! NOMCMP  IN/JXOUT K*  :  (LONG=NCMP)
!                   NOM DU VECTEUR JEVEUX QUI CONTIENDRA EN SORTIE LE
!                   NOM DES COMPOSANTES PRESENTES DANS LE CHAMP
! CORR1  IN/JXOUT K*  :   (LONG=NCMPMX)
!              NOM DU VECTEUR JEVEUX QUI CONTIENDRA EN SORTIE LA
!              CORRESPONDANCE ENTRE LE NUMERO D'1 CMP DE LA GRANDEUR
!              ASSOCIEE ET LE NUMERO DE LA MEME CMP DANS NOMCMP
! CORR2  IN/JXOUT K*  :   (LONG=NCMP)
!              NOM DU VECTEUR JEVEUX QUI CONTIENDRA EN SORTIE LA
!              CORRESPONDANCE INVERSE DE CORR1
! ----------------------------------------------------------------------
!  EXEMPLE :
!  SI NOMCHA EST UN CHAMP DE DEPL_R NE CONTENANT QUE 'DY' ET 'DRZ' :
!    NCMP=2
!    NOMCP=('DY','DRZ')
!    CORR1=(0,1,0,0,2,0,...)
!    CORR2=(2,5)
! ----------------------------------------------------------------------
! person_in_charge: nicolas.sellenet at edf.fr
    integer :: ifm, niv, ibid, iret, nbgr, jceld, nec, jcmpgd, jnocmp
    integer :: jcorr1, igr, imolo, jmolo, gd, nbpt, ipt, k, iadg, icmp
    integer :: jdesc, long, jprno, jnueq, nbno, ino, ncmpp, jcorr2
    integer :: ngrmx, nbedit, igd, ient, debgd, dg(50), ior, kpt, kcmp
    logical :: diff
    character(len=1) :: kbid
    character(len=8) :: nomgd, ma
    character(len=16) :: typsd
    character(len=19) :: ch19, profcn
!
    ch19=nomcha
    call infniv(ifm, niv)
    call jemarq()
!
    call dismoi('F', 'TYPE_CHAMP', nomcha, 'CHAMP', ibid,&
                typsd, iret)
!
    if (typsd .eq. 'NOEU') then
        call dismoi('F', 'NOM_GD', ch19, 'CHAM_NO', ibid,&
                    nomgd, ibid)
    else if (typsd(1:2).eq.'EL') then
        call dismoi('F', 'NOM_GD', ch19, 'CHAM_ELEM', ibid,&
                    nomgd, ibid)
    else if (typsd.eq.'CART') then
        call dismoi('F', 'NOM_GD', ch19, 'CARTE', ibid,&
                    nomgd, ibid)
    else
        ASSERT(.false.)
    endif
!
    call dismoi('F', 'NB_EC', nomgd, 'GRANDEUR', nec,&
                kbid, ibid)
    call dismoi('F', 'NB_CMP_MAX', nomgd, 'GRANDEUR', ncmpmx,&
                kbid, ibid)
    call dismoi('F', 'NUM_GD', nomgd, 'GRANDEUR', gd,&
                kbid, ibid)
    call jeveuo(jexnum('&CATA.GD.NOMCMP', gd), 'L', jcmpgd)
!
!
!
!     -- 1. POUR ECONOMISER LES APPELS A EXISDG, ON VA CALCULER
!           UN DESCRIPTEUR_GRANDEUR (DG) "ENVELOPPE" DE TOUS LES
!           POINTS DU CHAMP.
!     ----------------------------------------------------------------
    ASSERT(nec.le.50)
    do 10,k=1,50
    dg(k)=0
    10 end do
!
!
!     -- 1.1 CAS DES CHAM_NO
!     ----------------------------------------------------------------
    if (typsd .eq. 'NOEU') then
        call jeveuo(ch19//'.DESC', 'L', jdesc)
        call dismoi('F', 'NOM_MAILLA', ch19, 'CHAM_NO', ibid,&
                    ma, ibid)
        call dismoi('F', 'NB_NO_MAILLA', ma, 'MAILLAGE', nbno,&
                    kbid, ibid)
!
!        -- 1.1.1 CAS DES CHAM_NO A REPRESENTATION CONSTANTE :
        if (zi(jdesc-1+2) .lt. 0) then
            profcn=' '
            call jelira(ch19//'.DESC', 'LONMAX', long, kbid)
            ASSERT(long.eq.(2+nec))
            iadg=jdesc-1+3
            do 20,k=1,nec
            dg(k)=zi(iadg-1+k)
20          continue
!
!        -- 1.1.2 CAS DES CHAM_NO A PROF_CHNO:
        else
            call dismoi('F', 'PROF_CHNO', ch19, 'CHAM_NO', ibid,&
                        profcn, ibid)
            call jeveuo(jexnum(profcn//'.PRNO', 1), 'L', jprno)
            call jeveuo(profcn//'.NUEQ', 'L', jnueq)
            do 40,ino=1,nbno
            ncmpp=zi(jprno-1+(ino-1)*(nec+2)+2)
            if (ncmpp .ne. 0) then
                iadg=jprno-1+(ino-1)*(nec+2)+3
                do 30,k=1,nec
                dg(k)=ior(dg(k),zi(iadg-1+k))
30              continue
            endif
40          continue
        endif
!
!
!     -- 1.2 CAS DES CHAM_ELEM
!     ----------------------------------------------------------------
    else if (typsd(1:2).eq.'EL') then
        call jeveuo(ch19//'.CELD', 'L', jceld)
        nbgr=zi(jceld-1+2)
!
        do 70 igr = 1, nbgr
            imolo=zi(jceld-1+zi(jceld-1+4+igr)+2)
            if (imolo .eq. 0) goto 70
            call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
            ASSERT(zi(jmolo-1+1).le.3)
            ASSERT(zi(jmolo-1+2).eq.gd)
            diff=(zi(jmolo-1+4).gt.10000)
            nbpt=mod(zi(jmolo-1+4),10000)
!
            do 60,ipt=1,nbpt
            kpt=1
            if (diff) kpt=ipt
            iadg=jmolo-1+4+(kpt-1)*nec+1
            do 50,k=1,nec
            dg(k)=ior(dg(k),zi(iadg-1+k))
50          continue
60          continue
70      continue
!
!
!     -- 1.3 CAS DES CARTES
!     ----------------------------------------------------------------
    else if (typsd.eq.'CART') then
        call jeveuo(ch19//'.DESC', 'L', jdesc)
        ngrmx=zi(jdesc-1+2)
        nbedit=zi(jdesc-1+3)
        do 90 igd = 1, nbedit
            ient=zi(jdesc-1+3+2*igd)
            if (ient .ne. 0) then
                debgd=3+2*ngrmx+(igd-1)*nec+1
                do 80 k = 1, nec
                    dg(k)=ior(dg(k),zi(jdesc-1+debgd-1+k))
80              continue
            endif
90      continue
!
    else
        ASSERT(.false.)
    endif
!
!
!
!     -- CALCUL DE COOR1 :
!     --------------------
    call wkvect(corr1, 'V V I', ncmpmx, jcorr1)
    ncmp=0
    do 100 icmp = 1, ncmpmx
        if (exisdg(dg(1),icmp)) then
            ncmp=ncmp+1
            zi(jcorr1-1+icmp)=ncmp
        endif
100  end do
!
!
!
!     -- CALCUL DE NOMCMP ET CORR2 :
!     ------------------------------------
    call wkvect(nomcmp, 'V V K8', ncmp, jnocmp)
    call wkvect(corr2, 'V V I', ncmp, jcorr2)
    kcmp=0
    do 110,icmp=1,ncmpmx
    if (zi(jcorr1-1+icmp) .gt. 0) then
        kcmp=kcmp+1
        zi(jcorr2-1+kcmp)=icmp
        zk8(jnocmp-1+kcmp)=zk8(jcmpgd-1+icmp)
    endif
    110 end do
    ASSERT(kcmp.eq.ncmp)
!
!
    call jedema()
end subroutine
