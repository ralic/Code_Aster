subroutine eclpgc(ch1, ch2, ligrel, ma2, prchno,&
                  nomfpg)
    implicit none
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
!---------------------------------------------------------------------
! BUT : "ECLATER" LE CHAM_ELEM_ELGA CH1 POUR CREER LE CHAM_NO CH2 SUR
!        SUR LE MAILLAGE MA2.
!
! ARGUMENTS :
!  IN/JXN   CH1 : CHAM_ELEM_ELGA A ECLATER
!  IN/JXOUT CH2 : NOM DU CHAM_NO A CREER
!  IN/JXIN  LIGREL : NOM DU LIGREL CORRESPONDANT AUX MAILLES QUI
!           INTERESSENT L'UTILISATEUR.
!           LIGREL EST EVENTUELLEMMENT UN "SOUS" LIGREL DU LIGREL
!           ASSOCIE A CH1.
!  IN/JXIN  MA2 : NOM DU MAILLAGE QUI "PORTERA" LE CHAM_NO CH2
!  IN/JXIN  PRCHNO : NOM DE LA SD_PROF_CHNO QUI SERA ASSOCIEE A CH2
!  IN/JXIN  NOMFPG : NOM D'UN OBJET JEVEUX CONTENANT LE NOM DE LA
!           FAMILLE DE PG A UTILISER POUR CHAQUE MAILLE DE LIGREL.
!
! REMARQUES :
!  * MA2 IL EST TRES IMPORTANT QUE LE MAILLAGE MA2 FOURNI SOIT CELUI
!      QUI A ETE OBTENU PAR LA ROUTINE ECLPGM (AVEC LE MEME
!      LIGREL EN ENTREE).
!      LA "JUSTESSE" DU CHAMP CREE (CH2) PROVIENT DU FAIT QUE DANS
!      LES 2 ROUTINES ECLPGM ET ECLPGC, ON PARCOURT LES MEMES MAILLES
!      DANS LE MEME ORDRE.
!  * NOMFPG PEUT ETRE OBTENU PAR LA ROUTINE CELFPG
!  * LIGREL PEUT ETRE OBTENU PAR LA ROUTINE EXLIMA OU BIEN ON
!           L'EXTRAIT DE CH1 (DISMOI).
!  * PRCHNO PEUT ETRE ' '. DANS CE CAS, ON EN CREERA UN DIFFERENT
!           A CHAQUE APPEL A ECLPGC.
!           CHOISIR PRCHNO /= ' ' PERMET D'ECONOMISER CETTE SD SI
!           PLUSIEURS CHAMPS PEUVENT LA PARTAGER.
!           C'EST LE CAS EN GENERAL POUR LA BOUCLE SUR LES NUME_ORDRE
!
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/celcel.h"
#include "asterfort/celver.h"
#include "asterfort/chligr.h"
#include "asterfort/cmpcha.h"
#include "asterfort/cnscno.h"
#include "asterfort/cnscre.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/eclaty.h"
#include "asterfort/exisdg.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelibe.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/typele.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
!
! ---------------------------------------------------------------------
!     VARIABLES NECESSAIRES A L'APPEL DE ECLATY :
!     ON COMPREND LE SENS DE CES VARIABLES EN REGARDANT ECLATY
    integer :: mxnbn2, mxnbpi, mxnbte, mxnbse
!     MXNBN2 : MAX DU NOMBRE DE NOEUDS D'UN SOUS-ELEMENT (HEXA8)
    parameter(mxnbn2=8)
!     MXNBPI : MAX DU NOMBRE DE POINT_I (HEXA A 27 POINTS DE GAUSS)
!     MXNBPI = 4X4X4
    parameter(mxnbpi=64)
!     MXNBTE : MAX DU NOMBRE DE TERMES DE LA C.L. DEFINISSANT 1 POINT_I
!              AU PLUS LES 8 SOMMETS D'UN HEXA8
    parameter(mxnbte=8)
!     MXNBSE : MAX DU NOMBRE DE SOUS-ELEMENTS
    parameter (mxnbse=27)
!
    integer :: corsel(mxnbse), nse1
    integer :: connx(mxnbn2, mxnbse), nsomm1(mxnbpi, mxnbte)
    integer :: nterm1(mxnbpi), nbno2(mxnbse), tyma(mxnbse)
    real(kind=8) :: csomm1(mxnbpi, mxnbte)
! ---------------------------------------------------------------------
    logical(kind=1) :: lvari
    integer :: numa, jnofpg, kk
    integer :: k, te, npg1, npoini, ideca2
    integer :: igr,  jcmaco,  jcliel, jcnsl2
    integer :: ibid, nbpg, ino, nbgr, inogl, kse
    integer :: iamol1,  jcnsv2, mxcmp
    integer :: ima, nbelgr, jval2, nbno, nddl, iddl, adiel
    integer :: iipg, jceld1,  moloc1, ncmpmx
    parameter(mxcmp=100)
    integer :: nuddl(mxcmp), mxvari, iel, ncmp, jnocmp, jcorr1
    character(len=8) :: ma2, nomg1, nomg2, elrefa, fapg
    character(len=16) :: nomte
    character(len=16) :: optio, param
    character(len=19) :: ligrel, ch1, ch2s, ch2, prchno, ch1b
    character(len=24) :: valk(2), nomfpg
    character(len=24), pointer :: celk(:) => null()
    real(kind=8), pointer :: celv(:) => null()
    integer, pointer :: liel(:) => null()
    integer, pointer :: connex(:) => null()
!     FONCTIONS FORMULES :
!     NBNOMA(IMA)=NOMBRE DE NOEUDS DE LA MAILLE IMA
#define nbnoma(ima) zi(jcmaco-1+ima+1)-zi(jcmaco-1+ima)
!     NUMGLM(IMA,INO)=NUMERO GLOBAL DU NOEUD INO DE LA MAILLE IMA
!                     IMA ETANT UNE MAILLE DU MAILLAGE.
#define numglm(ima,ino) connex(zi(jcmaco+ima-1)+ino-1)
#define numail(igr,iel) liel(zi(jcliel+igr-1)+iel-1)
! DEB -----------------------------------------------------------------
    call jemarq()
!
!
    ch1b=ch1
    call dismoi('NOM_GD', ch1b, 'CHAMP', repk=nomg1)
    if (nomg1(5:6) .ne. '_R') then
        call utmess('F', 'CALCULEL2_39')
    endif
    nomg2=nomg1
    lvari=(nomg1.eq.'VARI_R')
    if (lvari) nomg2='VAR2_R'
!
!
!
!     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
    call celver(ch1b, 'NBSPT_1', 'COOL', kk)
    if (kk .eq. 1) then
        call utmess('I', 'PREPOST_36', sk=nomg1)
        call celcel('PAS_DE_SP', ch1b, 'V', '&&ECLPGC.CH1B1')
        ch1b='&&ECLPGC.CH1B1'
    endif
!
!     -- PROJECTION SUR LE LIGREL REDUIT SI NECESSAIRE :
    call jeveuo(ch1b//'.CELK', 'L', vk24=celk)
    if (celk(3)(1:4) .ne. 'ELGA') then
        call utmess('F', 'CALCULEL2_41')
    endif
    if (celk(1)(1:19) .ne. ligrel) then
        optio=celk(2)
        param=celk(6)
        call chligr(ch1b, ligrel, optio, param, 'V',&
                    '&&ECLPGC.CH1B2')
        ch1b='&&ECLPGC.CH1B2'
    endif
!
    call jeexin(ch1b//'.CELD', ibid)
    if (ibid .eq. 0) goto 90
!
    call jeveuo(ch1b//'.CELV', 'L', vr=celv)
    call jeveuo(ch1b//'.CELD', 'L', jceld1)
    call jeveuo(ch1b//'.CELD', 'L', jceld1)
!     -- MXVARI : NOMBRE MAXI DE VXX SI VARI_R
    mxvari=max(1,zi(jceld1-1+4))
!
!
!     -- ON CHERCHE LES CMPS PRESENTES DANS LE CHAM_ELEM CH1B :
!         NCMP : NOMBRE DE CMPS PRESENTES
!         '&&ECLPGC.CORR1': CONTIENT LA CORRESPONDANCE ENTRE LE
!                           NUMERO D'1 CMP DU CHAM_ELEM ET LE
!                           NUMERO D'1 CMP DU CHAM_ELEM_S
!         '&&ECLPGC.NOM_CMP': CONTIENT LES NOMS DES CMPS DU CHAM_ELEM_S
!     -----------------------------------------------------------------
    if (.not.lvari) then
!
!
        call cmpcha(ch1b, '&&ECLPGC.NOM_CMP', '&&ECLPGC.CORR1', '&&ECLPGC.CORR2', ncmp,&
                    ncmpmx)
        call jeveuo('&&ECLPGC.NOM_CMP', 'L', jnocmp)
        call jeveuo('&&ECLPGC.CORR1', 'L', jcorr1)
    else
!       -- POUR VARI_R :
        ASSERT(nomg2.eq.'VAR2_R')
        call dismoi('NB_CMP_MAX', nomg2, 'GRANDEUR', repi=ncmpmx)
        ASSERT(mxvari.le.ncmpmx)
        ncmp=mxvari
        call jeveuo(jexnom('&CATA.GD.NOMCMP', nomg2), 'L', jnocmp)
        call wkvect('&&ECLPGC.CORR1', 'V V I', ncmp, jcorr1)
        do k = 1, ncmp
            zi(jcorr1-1+k)=k
        end do
    endif
    ASSERT(ncmp.le.mxcmp)
!
!
!
!
!       -- CREATION D'UN CHAM_NO_S : CH2S
!       -----------------------------------------------------
    ch2s='&&ECLPGC.CH2S'
    call cnscre(ma2, nomg2, ncmp, zk8(jnocmp), 'V',&
                ch2s)
    call jeveuo(ch2s//'.CNSV', 'E', jcnsv2)
    call jeveuo(ch2s//'.CNSL', 'E', jcnsl2)
!
!
!
!     -- REMPLISSAGE DU CHAM_NO :
!     ---------------------------
    call jeveuo(nomfpg, 'L', jnofpg)
    call jeveuo(ligrel//'.LIEL', 'L', vi=liel)
    call jeveuo(jexatr(ligrel//'.LIEL', 'LONCUM'), 'L', jcliel)
    call jeveuo(ma2//'.CONNEX', 'L', vi=connex)
    call jeveuo(jexatr(ma2//'.CONNEX', 'LONCUM'), 'L', jcmaco)
    ima=0
    nbgr=nbgrel(ligrel)
    do igr = 1, nbgr
        moloc1=zi(jceld1-1+zi(jceld1-1+4+igr)+2)
        if (moloc1 .eq. 0) goto 80
!
        if (.not.lvari) ASSERT(mxvari.eq.1)
!
        call jeveuo(jexnum('&CATA.TE.MODELOC', moloc1), 'L', iamol1)
        ASSERT(zi(iamol1-1+1).eq.3)
        nbpg=zi(iamol1-1+4)
!
        numa=numail(igr,1)
        elrefa=zk16(jnofpg-1+numa)(1:8)
        fapg=zk16(jnofpg-1+numa)(9:16)
        if (fapg .eq. ' ') goto 80
!
!           -- ON VERIFIE QUE C'EST UN CHAMP "ELGA/IDEN" :
!           ----------------------------------------------
        ASSERT(.not.((nbpg.lt.0).or.(nbpg.gt.10000)))
!
!           -- ON ECLATE LE TYPE_ELEM :
!           ---------------------------
        te=typele(ligrel,igr)
        call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
        call eclaty(nomte, elrefa, fapg, npg1, npoini,&
                    nterm1, nsomm1, csomm1, tyma, nbno2,&
                    connx, mxnbn2, mxnbpi, mxnbte, mxnbse,&
                    nse1, corsel)
        if (npg1 .ne. 0) then
            if (nbpg .ne. npg1) then
                valk(1)=nomte
                call utmess('F', 'CALCULEL2_42', sk=valk(1))
            endif
        else
!            -- ON IGNORE LES AUTRES ELEMENTS :
            ASSERT(nse1.eq.0)
            goto 80
!
        endif
        nbelgr=nbelem(ligrel,igr)
!
!            -- QUELLES SONT LES CMPS PORTEES PAR LES POINTS DE GAUSS ?
!            ----------------------------------------------------------
        if (lvari) then
            nddl=mxvari
            do k = 1, nddl
                nuddl(k)=k
            end do
        else
            nddl=0
            do k = 1, ncmpmx
                if (exisdg(zi(iamol1-1+4+1),k)) then
                    nddl=nddl+1
                    nuddl(nddl)=zi(jcorr1-1+k)
                endif
            end do
        endif
!
!
!          -- BOUCLE SUR TOUS LES POINTS DE GAUSS DU GREL :
!          ------------------------------------------------
        do iel = 1, nbelgr
            if (lvari) nddl=zi(jceld1-1+zi(jceld1-1+4+igr)+4+ 4*(iel- 1)+2)
!
            do kse = 1, nse1
!            -- AU POINT DE GAUSS IIPG CORRESPOND LA MAILLE NUMERO IMA
!               DANS MA2.
                iipg=corsel(kse)
                ima=ima+1
!
                nbno=nbnoma(ima)
                if (nbno .gt. 27) then
                    call utmess('F', 'CALCULEL2_43')
                endif
                do ino = 1, nbno
                    inogl=numglm(ima,ino)
                    do iddl = 1, nddl
                        ideca2=ncmp*(inogl-1)+nuddl(iddl)
                        jval2=jcnsv2-1+ideca2
                        zl(jcnsl2-1+ideca2)=.true.
                        adiel=zi(jceld1-1+zi(jceld1-1+4+igr)+4+4*(iel-&
                        1)+4)
                        zr(jval2)=celv(adiel-1+nddl*(iipg-1)+&
                        iddl)
                    end do
                end do
            end do
        end do
 80     continue
    end do
!
!     -- ON ESSAYE DE FAIRE UN PEU DE PLACE EN MEMOIRE :
    call detrsd('CHAMP_GD', '&&ECLPGC.CH1B1')
    call detrsd('CHAMP_GD', '&&ECLPGC.CH1B2')
    call jelibe(ch1//'.CELD')
    call jelibe(ch1//'.CELV')
    call jelibe(ma2//'.CONNEX')
!
!
!         -- ON TRANSFORME CH2S EN VRAI CHAM_NO :
!         ----------------------------------------
!     -- 2 JELIBE POUR ECONOMISER LA MEMOIRE EN DESSOUS (MARQUE):
    call jelibe(ch2s//'.CNSL')
    call jelibe(ch2s//'.CNSV')
    call cnscno(ch2s, prchno, 'NON', 'G', ch2,&
                'F', ibid)
    call detrsd('CHAM_NO_S', ch2s)
!
    call jedetr('&&ECLPGC.NOM_CMP')
    call jedetr('&&ECLPGC.CORR1')
    call jedetr('&&ECLPGC.CORR2')
!
!
 90 continue
    call detrsd('CHAMP_GD', '&&ECLPGC.CH1B1')
    call detrsd('CHAMP_GD', '&&ECLPGC.CH1B2')
    call jedema()
end subroutine
