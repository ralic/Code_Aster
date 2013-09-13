subroutine nulili(lligr, lili, base, molocz, nomgds,&
                  igds, mailla, nec, ncmp, nlili)
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
! person_in_charge: jacques.pellet at edf.fr
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/dismoi.h"
#include "asterfort/infniv.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jeveut.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbcmp.h"
#include "asterfort/nbec.h"
#include "asterfort/nbgrel.h"
#include "asterfort/typele.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
!
    integer :: igds, nec, nlili, nbelm, iadnem, iadlie, jligr2, k
    character(len=*) :: lligr
    character(len=24) :: lili
    character(len=*) :: molocz
    character(len=8) :: mailla, moloc
    character(len=1) :: base
!----------------------------------------------------------------------
! ---- OBJET : CREATION DU CHAMP .LILI D'UNE S.D. NUME_DDL
!              ET DES OJBETS TEMPORAIRES : .ADNE ET .ADLI
!----------------------------------------------------------------------
! IN  K24 LLIGR  : LISTE DES NOMS DES LIGRELS
!                  SUPPORTANT LA NUMEROTATION (VECTEUR DE K24)
! IN/JXOUT  K24 LILI   : NOM DE L OBJET LILI QUI SERA CREE
! IN  K1   BASE   : ' G ' POUR CREER LILI SUR BASE GLOBALE
!                   ' V ' POUR CREER LILI SUR BASE VOLATILE
! IN  K19 PREF   : PREFIXE DES OBJETS TEMPORAIRES CREES
! IN  K8  MOLOCZ : NOM DU MODE_LOCAL PRECISANT LES DDLS A NUMEROTER
!                   SI ' ' MOLOCZ EST DETERMINE GRACE AU PHENOMENE
! OUT K8  NOMGDS    : NOM DE LA GRANDEUR (SIMPLE) A NUMEROTER
! OUT I    IGDS     : NUMERO DE LA GRANDEUR  NOMGDS
! OUT K8   MAILLA : NOM DU MAILLAGE
! OUT I    NEC    : NBRE D ENTIERS CODES POUR IGDS
! OUT I    NCMP   : NBRE DE CMP POUR IGDS
! OUT I    NLILI  : DIMENSION DE L OBJET CREE LILI
!----------------------------------------------------------------------
! ATTENTION : NE PAS FAIRE JEMARQ/JEDEMA CAR CETTE ROUTINE
!             RECOPIE DES ADRESSES JEVEUX DANS .ADNE ET .ADLI
!----------------------------------------------------------------------
!
!
!    --- DESCRIPTION DES OBJETS ADNE ET ADLI ---
!     ADNE (1          ) = NBRE DE MAILLES DU MAILLAGE
!     ADNE (2          ) = 0
!     ADNE (3          ) = 0
!     ADLI (1          ) = 0
!     ADLI (2          ) = 0
!     ADLI (3          ) = 0
!     POUR 2<=ILI<=NLILI
!     ADNE (3*(ILI-1)+1) = NBRE MAX D'OBJETS DE LA COLLECTION
!                            LILI(ILI).NEMA
!     ADNE (3*(ILI-1)+2) = ADRESSE DE L'OBJET LILI(ILI).NEMA
!     ADNE (3*(ILI-1)+3) = ADRESSE DU VECTEUR DES LONG. CUMULEES DE
!                            LILI(ILI).NEMA
!     ADLI (3*(ILI-1)+1) = NBRE MAX D'OBJETS DE LA COLLECTION
!                            LILI(ILI).LIEL
!     ADLI (3*(ILI-1)+2) = ADRESSE DE L'OBJET LILI(ILI).LIEL
!     ADLI (3*(ILI-1)+3) = ADRESSE DU VECTEUR DES LONG. CUMULEES DE
!                            LILI(ILI).LIEL
!-----------------------------------------------------------------------
!
!-----------------------------------------------------------------------
    character(len=8) :: nomgds, kbid
!
!     VARIABLES LOCALES
!-----------------------
    character(len=8) :: k8, exiele
    character(len=16) :: pheno, phe, nomte
    character(len=19) :: prefix, ligrel, nomlig, k19, valk(6)
    character(len=24) :: nomli, lligr2
    integer :: iad, ibid, ier, ierc, ierd, ifm, igr
    integer :: illigr, iligr, iret, kkk
    integer :: nbgr, nbsup, ncmp, niv, nligr, jmoloc, imode, ite
!
!----------------------------------------------------------------------
    call infniv(ifm, niv)
    moloc=molocz
!
!     IFM = IUNIFI('MESSAGE')
    prefix = lili(1:14)
!
!---- NBRE DE LIGRELS REFERENCES = DIM(LLIGR)
!
    call jeveuo(lligr, 'L', illigr)
    call jelira(lligr, 'LONUTI', nligr)
!
!
    lligr2='&&NULILI.LLIGR2'
    call wkvect(lligr2, 'V V K24', nligr, jligr2)
    do 662,k=1,nligr
    zk24(jligr2-1+k) =zk24(illigr-1+k)
    662 end do
    call jeecra(lligr2, 'LONUTI', nligr)
    nlili = nligr+1
    if (nlili .eq. 1) then
        call utmess('F', 'ASSEMBLA_29')
    endif
!
!---- CREATION DU REPERTOIRE .LILI DE TOUS LES NOMS DE LIGRELS /=
!
    call jecreo(lili, base//' N  K24 ')
    call jeecra(lili, 'NOMMAX', nlili)
!---- LILI(1)= '&MAILLA'
    call jecroc(jexnom(lili, '&MAILLA'))
!
!
!---- CREATION DES OBJETS .ADNE ET .ADLI SUR 'V'
    call jecreo(prefix//'.ADNE', ' V V I')
    call jeecra(prefix//'.ADNE', 'LONMAX', 3*nlili)
    call jeveuo(prefix//'.ADNE', 'E', iadnem)
    call jecreo(prefix//'.ADLI', ' V V I')
    call jeecra(prefix//'.ADLI', 'LONMAX', 3*nlili)
    call jeveuo(prefix//'.ADLI', 'E', iadlie)
!
!---- CHARGEMENT DE LILI, ADNE, ADLI
!
    do 10 iligr = 1, nligr
        nomli = zk24(jligr2+iligr-1)
!
!---- VERIFICATION DE L'UNICITE DU PHENOMENE
        call dismoi('F', 'PHENOMENE', nomli, 'LIGREL', kkk,&
                    phe, ierc)
        if (iligr .eq. 1) then
            pheno = phe
        else if (pheno.ne.phe) then
            call utmess('F', 'ASSEMBLA_30')
        endif
        call jecroc(jexnom(lili, nomli))
!
!---- RECUPERATION DU NOM DU MAILLAGE ET VERIFICATION DE SON UNICITE
        call jeveut(nomli(1:19)//'.LGRF', 'L', iad)
        k8 = zk8(iad)
        k19 = nomli
        if (iligr .eq. 1) then
            mailla(1:8) = k8
            nomlig = k19
        else if (mailla(1:8).ne.k8) then
            valk(1)=mailla
            valk(2)=k8
            valk(3)=nomlig
            valk(4)=k19
            valk(5)=nomlig(1:8)
            valk(6)=k19(1:8)
            call utmess('F', 'ASSEMBLA_66', nk=6, valk=valk)
        endif
!
!
!        -- SI LE LIGREL NE CONTIENT PAS D'ELEMENTS ON VA A FIN BCLE:
        call dismoi('F', 'EXI_ELEM', nomli, 'LIGREL', ibid,&
                    exiele, ierd)
        if (exiele(1:3) .eq. 'NON') goto 10
!
        call jeexin(nomli(1:19)//'.NEMA', iret)
        if (iret .ne. 0) then
!
!---- ADNE(3*(ILIGR)+1)=NBRE DE MAILLES SUP DU LIGREL NOMLI
!
            call jelira(nomli(1:19)//'.NEMA', 'NUTIOC', nbsup)
            zi(iadnem+3* (iligr)) = nbsup
            call jeveut(nomli(1:19)//'.NEMA', 'L', iad)
            zi(iadnem+3* (iligr)+1) = iad
            call jeveut(jexatr(nomli(1:19)//'.NEMA', 'LONCUM'), 'L', iad)
            zi(iadnem+3* (iligr)+2) = iad
        else
            zi(iadnem+3* (iligr)) = 0
            zi(iadnem+3* (iligr)+1) = 2**30
            zi(iadnem+3* (iligr)+2) = 2**30
        endif
!
!---- ADLI(3*(ILIGR)+1)=NBRE DE MAILLES DU LIGREL NOMLI
!
        call jelira(nomli(1:19)//'.LIEL', 'NUTIOC', nbgr)
        zi(iadlie+3* (iligr)) = nbgr
        call jeveut(nomli(1:19)//'.LIEL', 'L', iad)
        zi(iadlie+3* (iligr)+1) = iad
        call jeveut(jexatr(nomli(1:19)//'.LIEL', 'LONCUM'), 'L', iad)
        zi(iadlie+3* (iligr)+2) = iad
10  end do
!
!
    call dismoi('F', 'NB_MA_MAILLA', mailla(1:8), 'MAILLAGE', nbelm,&
                kbid, ierc)
    zi(iadnem) = nbelm
!
!
!---- CALCUL DE : NOMGDS, IGDS, NEC , NCMP
!------------------------------------------
    if (moloc .eq. ' ') then
        call dismoi('F', 'NOM_GD', pheno, 'PHENOMENE', ibid,&
                    nomgds, ier)
    else
        ligrel = zk24(jligr2-1+1)
        do 20 igr = 1, nbgrel(ligrel)
            ite = typele(ligrel,igr)
            call jenuno(jexnum('&CATA.TE.NOMTE', ite), nomte)
            call jenonu(jexnom('&CATA.TE.NOMMOLOC', nomte//moloc), imode)
            if (imode .gt. 0) then
                call jeveuo(jexnum('&CATA.TE.MODELOC', imode), 'L', jmoloc)
                call jenuno(jexnum('&CATA.GD.NOMGD', zi(jmoloc-1+2)), nomgds)
                goto 30
            endif
20      continue
        ASSERT(.false.)
30      continue
    endif
    call jenonu(jexnom('&CATA.GD.NOMGD', nomgds), igds)
    ASSERT(igds.ne.0)
!
    nec = nbec(igds)
    ncmp = nbcmp(igds)
!
!
!
    call jedetr(lligr2)
!
end subroutine
