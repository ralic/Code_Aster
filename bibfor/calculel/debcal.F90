subroutine debcal(nomop, ligrel, nin, lchin, lpain,&
                  nout, lchout)
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
!
! person_in_charge: jacques.pellet at edf.fr
    implicit none
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
#include "asterc/indik8.h"
#include "asterc/isnnem.h"
#include "asterfort/assert.h"
#include "asterfort/chlici.h"
#include "asterfort/chligr.h"
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/etenca.h"
#include "asterfort/grdeur.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/mecoel.h"
#include "asterfort/nbec.h"
#include "asterfort/scalai.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    character(len=16) :: nomop
    character(len=19) :: ligrel
    integer :: nin, nout
    character(len=19) :: lchin(nin), lchout(nout)
    character(len=8) :: lpain(nin)
! ----------------------------------------------------------------------
!
!     BUT :
!      1. VERIFIER LES "ENTREES" DE CALCUL.
!      2. INITIALISER CERTAINS OBJETS POUR LE CALCUL AINSI QUE LES
!       COMMUNS CONTENANT LES ADRESSES DES OBJETS DES CATALOGUES.
!
!     ENTREES:
!        NOMOP  :  NOM D'1 OPTION
!        LIGREL :  NOM DU LIGREL SUR LEQUEL ON DOIT FAIRE LE DEBCAL
!        NIN    :  NOMBRE DE CHAMPS PARAMETRES "IN"
!        NOUT   :  NOMBRE DE CHAMPS PARAMETRES "OUT"
!        LCHIN  :  LISTE DES NOMS DES CHAMPS "IN"
!        LCHOUT :  LISTE DES NOMS DES CHAMPS "OUT"
!        LPAIN  :  LISTE DES NOMS DES PARAMETRES "IN"
!
!     SORTIES:
!       ALLOCATION D'OBJETS DE TRAVAIL ET MISE A JOUR DE COMMONS
!
! ----------------------------------------------------------------------
    integer :: desc, iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno
    integer :: i, iaopds, iaoppa, npario, iamloc, ilmloc, nparin
    integer :: iadsgd
    integer :: iachii, iachik, iachix
    integer :: ianoop, ianote, nbobtr, iaobtr, nbobmx
    integer :: ibid, nbpara, iret, j
    integer ::  jpar, igd, nec, ncmpmx, iii, num
    integer :: iarefe, ianbno, jproli, ianueq, iret1
    integer :: iret2
    character(len=8) :: k8bi, typsca
    character(len=4) :: knum, tych
    character(len=8) :: nompar, ma, ma2, k8bi1, k8bi2
    character(len=19) :: chin, chou, ligre2
    character(len=24) :: noprno, objdes, valk(5)
!---------------- COMMUNS POUR CALCUL ----------------------------------
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
    common /caii04/iachii,iachik,iachix
    common /caii05/ianoop,ianote,nbobtr,iaobtr,nbobmx
!
!
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
!-----------------------------------------------------------------------
    call dismoi('NOM_MAILLA', ligrel, 'LIGREL', repk=ma)
!
!
!     -- VERIFICATION QUE LES CHAMPS "IN" ONT DES NOMS LICITES:
!     ---------------------------------------------------------
    do 10 i = 1, nin
        nompar=lpain(i)
        call chlici(nompar, 8)
        if (nompar .ne. ' ') then
            call chlici(lchin(i), 19)
        endif
 10 end do
!
!
!     -- VERIFICATION DE L'EXISTENCE DES CHAMPS "IN"
!     ---------------------------------------------------
    call wkvect('&&CALCUL.LCHIN_EXI', 'V V L', max(1, nin), iachix)
    nbobtr=nbobtr+1
    zk24(iaobtr-1+nbobtr)='&&CALCUL.LCHIN_EXI'
    do 20 i = 1, nin
        chin=lchin(i)
        zl(iachix-1+i)=.true.
        if (lpain(i)(1:1) .eq. ' ') then
            zl(iachix-1+i)=.false.
        else if (chin(1:1).eq.' ') then
            zl(iachix-1+i)=.false.
        else
            call jeexin(chin//'.DESC', iret1)
            call jeexin(chin//'.CELD', iret2)
            if ((iret1+iret2) .eq. 0) zl(iachix-1+i)=.false.
        endif
 20 end do
!
!
!     -- ON VERIFIE QUE LES CHAMPS "IN" ONT UN MAILLAGE SOUS-JACENT
!        IDENTIQUE AU MAILLAGE ASSOCIE AU LIGREL :
!     -------------------------------------------------------------
    do 30 i = 1, nin
        chin=lchin(i)
        if (.not.(zl(iachix-1+i))) goto 30
        call dismoi('NOM_MAILLA', chin, 'CHAMP', repk=ma2)
        if (ma2 .ne. ma) then
            valk(1)=chin
            valk(2)=ligrel
            valk(3)=ma2
            valk(4)=ma
            call utmess('F', 'CALCULEL2_27', nk=4, valk=valk)
        endif
 30 end do
!
!
!     -- VERIFICATION QUE LES CHAMPS "OUT" SONT DIFFERENTS
!        DES CHAMPS "IN"
!     ---------------------------------------------------
    do 50 i = 1, nout
        chou=lchout(i)
        do 40 j = 1, nin
            chin=lchin(j)
            if (.not.zl(iachix-1+j)) goto 40
            if (chin .eq. chou) then
                call utmess('F', 'CALCULEL2_28', sk=chou)
            endif
 40     continue
 50 end do
!
!
!     INITIALISATION DU COMMON CAII04 :
!     ---------------------------------
    call wkvect('&&CALCUL.LCHIN_I', 'V V I', max(1, 11*nin), iachii)
    nbobtr=nbobtr+1
    zk24(iaobtr-1+nbobtr)='&&CALCUL.LCHIN_I'
    call wkvect('&&CALCUL.LCHIN_K8', 'V V K8', max(1, 2*nin), iachik)
    nbobtr=nbobtr+1
    zk24(iaobtr-1+nbobtr)='&&CALCUL.LCHIN_K8'
    nbpara = zi(iaopds-1+2) + zi(iaopds-1+3)
    do 60 i = 1, nin
        chin=lchin(i)
!
!        -- SI LE CHAMP EST BLANC OU S'IL N'EXISTE PAS
!           , ON NE FAIT RIEN :
        if (chin(1:1) .eq. ' ') goto 60
        call jeexin(chin//'.DESC', iret1)
        if (iret1 .gt. 0) objdes=chin//'.DESC'
        call jeexin(chin//'.CELD', iret2)
        if (iret2 .gt. 0) objdes=chin//'.CELD'
        if ((iret1+iret2) .eq. 0) goto 60
        nompar=lpain(i)
!
!        -- SI LE PARAMETRE EST INCONNU POUR L'OPTION CALCULEE, ON NE
!        -- FAIT RIEN:
        jpar=indik8(zk8(iaoppa),nompar,1,nbpara)
        if (jpar .eq. 0) goto 60
!
        call dismoi('TYPE_CHAMP', chin, 'CHAMP', repk=tych)
!
!        -- SI LE CHAMP EST UN RESUELEM DE TYPE "VOISIN_VF"
!           ON NE SAIT PAS ENCORE FAIRE ...
        ASSERT(.not.(evfini.eq.1.and.tych.eq.'RESL'))
!
!        -- SI LE CHAMP EST UN CHAM_ELEM( OU UN RESUELEM)
!           ET QU'IL N'A PAS ETE CALCULE AVEC LE LIGREL DE CALCUL,
!           ON LE TRANSPORTE SUR CE LIGREL
!           (ET ON MODIFIE SON NOM DANS LCHIN)
        if ((tych(1:2).eq.'EL') .or. (tych.eq.'RESL')) then
            call dismoi('NOM_LIGREL', chin, 'CHAMP', repk=ligre2)
            if (ligre2 .ne. ligrel) then
                call codent(i, 'G', knum)
                lchin(i)='&&CALCUL.CHML.'//knum
!           -- ATTENTION, POUR CHLIGR, IL FAUT IACTIF=0
                call mecoel(0)
                call chligr(chin, ligrel, nomop, nompar, 'V',&
                            lchin(i))
                call mecoel(1)
!
                call jeexin(lchin(i)(1:19)//'.CELD', ibid)
                chin=lchin(i)
                objdes(1:19)=chin
                nbobtr=nbobtr+1
                zk24(iaobtr-1+nbobtr)=lchin(i)//'.CELD'
                nbobtr=nbobtr+1
                if (tych(1:2) .eq. 'EL') then
                    zk24(iaobtr-1+nbobtr)=lchin(i)//'.CELK'
                else
                    zk24(iaobtr-1+nbobtr)=lchin(i)//'.NOLI'
                endif
                nbobtr=nbobtr+1
                zk24(iaobtr-1+nbobtr)=lchin(i)//'.CELV'
            endif
        endif
!
!
        igd=grdeur(nompar)
        zi(iachii-1+11*(i-1)+1)=igd
!
        nec=nbec(igd)
        zi(iachii-1+11*(i-1)+2)=nec
!
        typsca=scalai(igd)
        zk8(iachik-1+2*(i-1)+2)=typsca
!
        call jelira(jexnum('&CATA.GD.NOMCMP', igd), 'LONMAX', ncmpmx)
        zi(iachii-1+11*(i-1)+3)=ncmpmx
!
        call jelira(objdes, 'DOCU', cval=k8bi)
        zk8(iachik-1+2*(i-1)+1)=k8bi
!
        call jeveuo(objdes, 'L', desc)
        zi(iachii-1+11*(i-1)+4)=desc
!
!         -- SI LA GRANDEUR ASSOCIEE AU CHAMP N'EST PAS CELLE ASSOCIEE
!            AU PARAMETRE, ON ARRETE TOUT :
        if (igd .ne. zi(desc)) then
            call jenuno(jexnum('&CATA.GD.NOMGD', igd), k8bi1)
            call jenuno(jexnum('&CATA.GD.NOMGD', zi(desc)), k8bi2)
            valk(1)=chin
            valk(2)=k8bi2
            valk(3)=nompar
            valk(4)=k8bi1
            valk(5)=nomop
            call utmess('F', 'CALCULEL2_29', nk=5, valk=valk)
        endif
!
        call jeexin(chin//'.VALE', iret)
        if (iret .gt. 0) then
            call jeveuo(chin//'.VALE', 'L', iii)
            zi(iachii-1+11*(i-1)+5)=iii
        endif
!
        call jeexin(chin//'.CELV', iret)
        if (iret .gt. 0) then
            call jeveuo(chin//'.CELV', 'L', iii)
            zi(iachii-1+11*(i-1)+5)=iii
        endif
!
!        -- POUR LES CARTES :
        if (zk8(iachik-1+2*(i-1)+1)(1:4) .eq. 'CART') then
!
!           -- SI LA CARTE N'EST PAS CONSTANTE, ON L'ETEND:
            if (.not.(zi(desc-1+2).eq.1.and.zi(desc-1+4).eq.1)) then
                call etenca(chin, ligrel, iret)
                if (iret .gt. 0) goto 70
                call jeexin(chin//'.PTMA', iret)
                if (iret .gt. 0) then
                    call jeveuo(chin//'.PTMA', 'L', iii)
                    zi(iachii-1+11*(i-1)+6)=iii
                    nbobtr=nbobtr+1
                    zk24(iaobtr-1+nbobtr)=chin//'.PTMA'
                endif
                call jeexin(chin//'.PTMS', iret)
                if (iret .gt. 0) then
                    call jeveuo(chin//'.PTMS', 'L', iii)
                    zi(iachii-1+11*(i-1)+7)=iii
                    nbobtr=nbobtr+1
                    zk24(iaobtr-1+nbobtr)=chin//'.PTMS'
                endif
            endif
        endif
!
!        -- POUR LES CHAM_NO A PROFIL_NOEUD:
        if (zk8(iachik-1+2*(i-1)+1)(1:4) .eq. 'CHNO') then
            num=zi(desc-1+2)
            if (num .gt. 0) then
                call jeveuo(chin//'.REFE', 'L', iarefe)
                noprno=zk24(iarefe-1+2)(1:19)//'.PRNO'
                call jeveuo(jexnum(noprno, 1), 'L', iii)
                zi(iachii-1+11*(i-1)+8)=iii
                call jeveuo(ligrel//'.NBNO', 'L', ianbno)
                if (zi(ianbno-1+1) .gt. 0) then
                    call jenonu(jexnom(noprno(1:19)//'.LILI', ligrel//'      '), jproli)
                    if (jproli .eq. 0) then
                        zi(iachii-1+11*(i-1)+9)=isnnem()
                    else
                        call jeveuo(jexnum(noprno, jproli), 'L', iii)
                        zi(iachii-1+11*(i-1)+9)=iii
                    endif
                endif
                call jeveuo(noprno(1:19)//'.NUEQ', 'L', ianueq)
                zi(iachii-1+11*(i-1)+10)=ianueq
                zi(iachii-1+11*(i-1)+11)=1
            endif
        endif
 60 end do
!
    goto 80
!
!     -- SORTIE ERREUR:
 70 continue
    chin=lchin(i)
    call utmess('F', 'CALCULEL2_30', sk=chin)
!
!     -- SORTIE NORMALE:
 80 continue
!
end subroutine
