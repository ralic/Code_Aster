subroutine xpodim(malini, mailc, modvis, licham, nsetot,&
                  nnntot, ncotot, listno, cns1, cns2,&
                  ces1, ces2, cel2, cesvi1, cesvi2,&
                  ior, resuco, nbnoc, nbmac, logrma,&
                  dirgrm, maxfem, ngfon, comps1, comps2)
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: samuel.geniaut at edf.fr
!
! aslint: disable=W1504
    implicit none
!
#include "jeveux.h"
!
#include "asterc/getres.h"
#include "asterfort/alchml.h"
#include "asterfort/assert.h"
#include "asterfort/carces.h"
#include "asterfort/celces.h"
#include "asterfort/cescre.h"
#include "asterfort/cnocns.h"
#include "asterfort/cnscre.h"
#include "asterfort/dismoi.h"
#include "asterfort/gmgnre.h"
#include "asterfort/infniv.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedup1.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/rsexch.h"
#include "asterfort/wkvect.h"
#include "asterfort/xismec.h"
    integer :: nsetot, nnntot, ncotot, nbnoc, ior, ngfon
    character(len=8) :: maxfem, malini, resuco, modvis
    character(len=19) :: cns1, cns2, ces1, ces2, cel2, cesvi1, cesvi2
    character(len=19) :: comps1, comps2
    character(len=24) :: mailc, listno, logrma, dirgrm, licham
!
!
!   DIMENTIONNEMENT DES OBJETS DU NOUVEAU MAILLAGE MAXFEM
!                ET DES NOUVEAUX CHAMPS RESULTAT
!
!   IN
!       MALINI : MAILLAGE SAIN
!       MAILC  : LISTE DES NUMEROS DES MAILLES NON SOUS-DECOUPEES
!       MODVIS : MODELE DE VISU (X-FEM)
!       LICHAM : LISTE DES CHAMPS A POST-TRAITER
!       NSETOT : NOMBRE TOTAL DE SOUS-ELEMENT
!       NNNTOT : NOMBRE TOTAL DE NOUVEAUX NOEUDS
!       NCOTOT : LONGUEUR DE LA CONNECTIVITE DES NOUVEAUX NOEUDS
!       CNS1   : CHAMP_NO_S DU DEPLACEMENT EN ENTREE
!       CES1   : CHAMP_ELEM_S DE CONTRAINTES EN ENTREE
!       MAXFEM : MAILLAGE FISSURE (SI POST_CHAMP_XFEM)
!       IOR    : POSITION DU NUMERO D'ORDRE (SI POST_CHAMP_XFEM)
!       RESUCO : NOM DU CONCEPT RESULTAT DONT ON EXTRAIT LES CHAMPS
!       LOGRMA : LONGUEUR DES NOUVEAUX GROUP_MA
!       DIRGRM : VECTEUR D'INDIRECTION ENTRE LES GROUP_MA
!       NGFON  : NOMBRE TOTAL DE FOND DE FISSURES
!
!   OUT
!       MAXFEM : MAILLAGE FISSURE (SI POST_MAIL_XFEM)
!       CNS2   : CHAMP_NO_S DU DEPLACEMENT EN SORTIE
!       CES2   : CHAMP_ELEM_S DE CONTRAINTES EN SORTIE
!       CEL2   : CHAMP_ELEM DE CONTRAINTES EN SORTIE
!       NBNOC  : NOMBRE DE NOEUDS CLASSIQUES DU MAILLAGE FISSURE
!       NBMAC  : NOMBRE DE MAILLES CLASSIQUES DU MAILLAGE FISSURE
!       LISTNO : LISTE DES NUMEROS DES NOEUDS CLASSIQUES
!
!
!
    integer :: ier, nbmac, nbma2, nbno, nbno2, iret, jdime, igeomr, nbid
    integer :: iadesc, ibid, iarefe, iacoo2, jtypm2, jtrav, jno, jmac
    integer :: ndim, jord, iord, i, ifm, niv, nmaxsp, nmaxcm, nbcham
    integer :: jcnsk1, jcvid1, jnbpt, jcesd2
    integer :: jcnsk2, jcnsd2, jcnsc2, jdirgr
    integer :: igma1, nbgma, n, jlogma, nbgma1, nbgma2, cptgr2, jlicha
    integer :: jresd1, jresc1, nbcmp
    character(len=3) :: tsca
    character(len=8) :: k8b, ldep3(6), ldep2(4), ltemp(1)
    character(len=16) :: k16b, nomcmd
    character(len=19) :: coord2, ligrel, chn1, chsig1
    character(len=19) :: nomgd
    character(len=24) :: ordr, gpptnm, nogma
    character(len=24) :: comp1
    data          ldep3/ 'DX','DY','DZ','LAGS_C','LAGS_F1','LAGS_F2'/
    data          ldep2/ 'DX','DY',     'LAGS_C','LAGS_F1'          /
    data          ltemp/ 'TEMP'                                     /
!
    call jemarq()
    call infniv(ifm, niv)
!
!     NOM DE LA COMMANDE (POST_MAIL_XFEM OU POST_CHAM_XFEM)
    call getres(k8b, k16b, nomcmd)
!
!     NOMBRE DE MAILLES CLASSIQUES
    call jeexin(mailc, ier)
    if (ier .eq. 0) then
        nbmac = 0
        nbnoc = 0
    else
        call jeveuo(mailc, 'L', jmac)
        call jelira(mailc, 'LONMAX', nbmac)
!       RECHERCHE DE LA LISTE DE NOEUDS SOUS-JACENTE
        call dismoi('F', 'NB_NO_MAILLA', malini, 'MAILLAGE', nbno,&
                    k8b, iret)
        call wkvect('&&XPODIM.LITRAV', 'V V I', nbno, jtrav)
        call wkvect(listno, 'V V I', nbno, jno)
        call gmgnre(malini, nbno, zi(jtrav), zi(jmac), nbmac,&
                    zi(jno), nbnoc, 'TOUS')
        call jedetr('&&XPODIM.LITRAV')
    endif
!
!     NOMBRE DE MAILLES DU NOUVEAU MAILLAGE : NBMA2
    nbma2 = nbmac + nsetot
!
!     NOMBRE DE NOEUDS DU NOUVEAU MAILLAGE : NBNO2
    nbno2 = nbnoc + nnntot
    if (nomcmd .eq. 'POST_MAIL_XFEM') then
!
!       ---------------------------------------------------------------
!                   TRAITEMENT POUR POST_MAIL_XFEM
!       ---------------------------------------------------------------
!
!       CHANGEMENT DU .DIME
!       .DIME(1) : NOMBRE DE NOEUDS PHYSIQUES DU MAILLAGE
!       .DIME(3) : NOMBRE DE MAILLES DU MAILLAGE
        call jedup1(malini//'.DIME', 'G', maxfem//'.DIME')
        call jeveuo(maxfem//'.DIME', 'E', jdime)
        zi(jdime-1+1) = nbno2
        zi(jdime-1+3) = nbma2
        ASSERT(zi(jdime-1+2).eq.0)
        ASSERT(zi(jdime-1+4).eq.0)
        ASSERT(zi(jdime-1+5).eq.0)
!
!       CREATION DES .NOMMAI, .NOMNOE, .TYPMAIL, .COORDO  .CONNEX
!       DU MAILLAGE 2
        call jecreo(maxfem//'.NOMMAI', 'G N K8')
        call jeecra(maxfem//'.NOMMAI', 'NOMMAX', nbma2)
!
        call jecreo(maxfem//'.NOMNOE', 'G N K8')
        call jeecra(maxfem//'.NOMNOE', 'NOMMAX', nbno2)
!
        coord2= maxfem//'.COORDO'
        call jenonu(jexnom('&CATA.GD.NOMGD', 'GEOM_R'), igeomr)
        call wkvect(coord2//'.DESC', 'G V I', 3, iadesc)
        call jeecra(coord2//'.DESC', 'DOCU', cval='CHNO')
        zi (iadesc-1+1)= igeomr
!       -- TOUJOURS 3 COMPOSANTES X, Y ET Z
        zi (iadesc-1+2)= -3
!       -- 14 = 2**1 + 2**2 + 2**3
        zi (iadesc-1+3)= 14
        call wkvect(coord2//'.REFE', 'G V K24', 4, iarefe)
        zk24(iarefe-1+1)= maxfem
        call wkvect(coord2//'.VALE', 'G V R', 3*nbno2, iacoo2)
!
        call wkvect(maxfem//'.TYPMAIL', 'G V I', nbma2, jtypm2)
!
        call jecrec(maxfem//'.CONNEX', 'G V I', 'NU', 'CONTIG', 'VARIABLE',&
                    nbma2)
        call jeecra(maxfem//'.CONNEX', 'LONT', ncotot)
!
!       .GROUPEMA
!       NOMBRE DE GROUP_MA A CREER DANS MAXFEM : NBGMA2
!       (PREND EN COMPTE LES FONDS DE FISSURE ET LES GROUPES DE MAILLES
!       DU MAILLAGE INITIAL)
        nbgma2 = ngfon
        call jeexin(malini//'.GROUPEMA', igma1)
        if (igma1 .ne. 0) then
            call jeveuo(logrma, 'L', jlogma)
            call jelira(logrma, 'LONMAX', nbgma)
            call jelira(malini//'.GROUPEMA', 'NUTIOC', nbgma1)
            ASSERT(nbgma.eq.nbgma1)
            do 30 i = 1, nbgma
                if (zi(jlogma-1+i) .ne. 0) nbgma2 = nbgma2 + 1
30          continue
        endif
        if (nbgma2 .ne. 0) then
            gpptnm = maxfem//'.PTRNOMMAI'
            call jecreo(gpptnm, 'G N K24')
            call jeecra(gpptnm, 'NOMMAX', nbgma2)
            call jecrec(maxfem//'.GROUPEMA', 'G V I', 'NO '//gpptnm, 'DISPERSE', 'VARIABLE',&
                        nbgma2)
!         CREATION DU VECTEUR D'INDIRECTION DES GROUP_MA
            if (igma1 .ne. 0) then
                call wkvect(dirgrm, 'V V I', nbgma, jdirgr)
                cptgr2 = 0
                do 31,i=1,nbgma
                if (zi(jlogma-1+i) .ne. 0) then
                    call jenuno(jexnum(malini//'.GROUPEMA', i), nogma)
                    call jecroc(jexnom(maxfem//'.GROUPEMA', nogma))
                    n = zi(jlogma-1+i)
                    cptgr2 = cptgr2 + 1
                    zi(jdirgr-1+i)=cptgr2
                    call jeecra(jexnum(maxfem//'.GROUPEMA', cptgr2), 'LONMAX', n)
                    call jeecra(jexnum(maxfem//'.GROUPEMA', cptgr2), 'LONUTI', n)
                    write(ifm,808) nogma,n
                endif
31              continue
            endif
        endif
        write(ifm,*)' '
!
!
        if (niv .gt. 1) then
            write(ifm,*)'CREATION .NOMAI DE LONGUEUR ',nbma2
            write(ifm,*)'CREATION .TYPMAIL DE LONGUEUR ',nbma2
            write(ifm,*)'CREATION .NOMNOE DE LONGUEUR ',nbno2
            write(ifm,*)'CREATION .COORDO.VALE DE LONGUEUR ',3*nbno2
            write(ifm,*)'CREATION .CONNEX DE LONGUEUR ',ncotot
            write(ifm,*)'CREATION .GROUPEMA ',nbgma2
        endif
!
    else if (nomcmd.eq.'POST_CHAM_XFEM') then
!
!       ---------------------------------------------------------------
!                   TRAITEMENT POUR POST_CHAM_XFEM
!       ---------------------------------------------------------------
!
!       CHAMP A POST-TRAITER
        call jelira(licham, 'LONMAX', nbcham)
        call jeveuo(licham, 'L', jlicha)
!
        call dismoi('F', 'DIM_GEOM', malini, 'MAILLAGE', ndim,&
                    k8b, ier)
        call dismoi('F', 'NB_NO_MAILLA', maxfem, 'MAILLAGE', nbid,&
                    k8b, ier)
        ASSERT(nbno2.eq.nbid)
!
        ASSERT(nbcham.gt.0)
!
        chn1 ='&&XPODIM.CHN1'
!
!       EXTRACTION DES DEPLACEMENTS : CNS1
        ordr=resuco//'           .ORDR'
        call jeveuo(ordr, 'L', jord)
        iord=zi(jord-1+ior)
        call rsexch('F', resuco, zk16(jlicha-1+1), iord, chn1,&
                    ier)
        call cnocns(chn1, 'V', cns1)
        call jeveuo(cns1//'.CNSK', 'L', jcnsk1)
!
!       CREATION D'UN CHAMP SIMPLE : CNS2
        if (xismec()) then
!         CAS DE LA MECANIQUE
            if (ndim .eq. 2) then
                call cnscre(maxfem, 'DEPL_R', 2*ndim, ldep2, 'V',&
                            cns2)
            else
                ASSERT(ndim.eq.3)
                call cnscre(maxfem, 'DEPL_R', 2*ndim, ldep3, 'V',&
                            cns2)
            endif
        else
!         CAS DE LA THERMIQUE
            call cnscre(maxfem, 'TEMP_R', 1, ltemp, 'V',&
                        cns2)
        endif
!
        call jeveuo(cns2//'.CNSK', 'E', jcnsk2)
        call jeveuo(cns2//'.CNSD', 'E', jcnsd2)
        call jeveuo(cns2//'.CNSC', 'E', jcnsc2)
!
!       REMPLISSAGE DES DIFFERENTS OBJETS DU CNS2
        zk8(jcnsk2-1+1)=maxfem
        zk8(jcnsk2-1+2)=zk8(jcnsk1-1+2)
!
        zi(jcnsd2-1+1)=nbno2
        if (xismec()) then
!         CAS DE LA MECANIQUE
            zi(jcnsd2-1+2)=2*ndim
            do 20 i = 1, 2*ndim
                if (ndim .eq. 3) zk8(jcnsc2-1+i)=ldep3(i)
                if (ndim .eq. 2) zk8(jcnsc2-1+i)=ldep2(i)
20          continue
        else
!         CAS DE LA THERMIQUE
            zi(jcnsd2-1+2)=1
            zk8(jcnsc2-1+1)=ltemp(1)
        endif
!
!       CONTRAINTES
        if (nbcham .gt. 1) then
            chsig1 = '&&XPODIM.CHE1'
            call rsexch('F', resuco, zk16(jlicha-1+2), iord, chsig1,&
                        ier)
            call celces(chsig1, 'V', ces1)
!
!         CREATION D'UN CHAMP SIMPLE
            ligrel = modvis//'.MODELE'
            call alchml(ligrel, 'FULL_MECA', 'PCONTMR', 'V', cel2,&
                        iret, ' ')
            call celces(cel2, 'V', ces2)
!
            if (nbcham .gt. 2) then
!           VARIABLES INTERNES
                chsig1 = '&&XPODIM.CHE1'
                call rsexch(' ', resuco, zk16(jlicha-1+3), iord, chsig1,&
                            ier)
            else
                ier = 1
            endif
!
!         SI LE CHAMP EXISTE
            if (ier .eq. 0) then
                call celces(chsig1, 'V', cesvi1)
!
!           TROUVER LES VALEURS MAJORANTES DU NOMBRE DE POINTS DE GAUSS,
!           NOMBRE DES SOUS-POINTS ET NOMBRE DE COMPOSANTES
!
                call jeveuo(cesvi1//'.CESD', 'L', jcvid1)
                call jeveuo(ces2//'.CESD', 'L', jcesd2)
!
                nmaxsp = zi(jcvid1-1 +4)
                nmaxcm = zi(jcvid1-1 +5)
                call wkvect('&&XPODIM.NBPT', 'V V I', nbma2, jnbpt)
!
                do 10,i = 1,nbma2
                zi(jnbpt-1+i) = zi(jcesd2-1+5 + 4*(i-1) + 1)
!
10              continue
!
                call cescre('V', cesvi2, 'ELGA', maxfem, 'VARI_R',&
                            -nmaxcm, ' ', zi(jnbpt), -nmaxsp, -nmaxcm)
            endif
!
        endif
!
!       COMPORTEMENT
!       RECUPERATION DE LA CARTE DE COMPORTEMENT UTILISEE DANS LE CALCUL
        call rsexch(' ', resuco, 'COMPORTEMENT', iord, comp1,&
                    iret)
!
!       SI LA CARTE DE COMPORTEMENT EXISTE
        if (iret .eq. 0) then
!
!         NOM ET TYPE DE LA GRANDEUR (NORMALEMENT TYPE='K16')
            call dismoi('F', 'NOM_GD', comp1, 'CARTE', ibid,&
                        nomgd, iret)
            call dismoi('F', 'TYPE_SCA', nomgd, 'GRANDEUR', ibid,&
                        tsca, ibid)
            ASSERT(tsca.eq.'K16')
!
!         CREATION CHAM_ELEM_S 1 A PARTIR DE LA CARTE 1
            call carces(comp1, 'ELEM', ' ', 'V', comps1,&
                        'A', iret)
            call jeveuo(comps1//'.CESD', 'L', jresd1)
            call jeveuo(comps1//'.CESC', 'L', jresc1)
!
!         NB CMP
            nbcmp = zi(jresd1-1+2)
!
!         CREATION CHAM_ELEM_S 2 : COMPS2
            call cescre('V', comps2, 'ELEM', maxfem, nomgd,&
                        nbcmp, zk8( jresc1), ibid, -1, -nbcmp)
!
        endif
!
    endif
!
    call jedetr('&&XPODIM.NBPT')
    call jedema()
!
    808 format (30x,a8,2x,i12)
!
!
end subroutine
