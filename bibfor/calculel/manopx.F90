subroutine manopx(ligrel, option, param, chsgeo, exixfm,&
                  kecono)
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
! person_in_charge: samuel.geniaut at edf.fr
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/celces.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/indk32.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenonu.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/modat2.h"
#include "asterfort/nucalc.h"
#include "asterfort/typele.h"
#include "asterfort/wkvect.h"
!
    character(len=19) :: ligrel, chsgeo
    character(len=16) :: option
    character(len=8) :: param
    character(len=3) :: exixfm
    character(len=24) :: kecono
! ------------------------------------------------------------------
! BUT: REGARDER DANS LE LIGREL S'IL Y A DES ELEMENTS XFEM
!      ET SI LE CHAMP ELGA (OPTION/PARAM) UTILISE UNE FAMILLE XFEM..
!
!      * CALCULER UN OBJET (KECONO) DISANT SI CHAQUE GREL
!          PEUT ETRE STOCKE "ECONOMIQUE"
!      SI FAMILLE XFEM :
!        * CALCULER LE CHAMP SIMPLE CHSGEO CONTENANT LES COORDONNNEES
!          DES POINTS DE GAUSS DES FAMILLES XFEM...
! ------------------------------------------------------------------
!     ARGUMENTS:
!     ----------
! LIGREL  IN/JXIN  K19 : LIGREL
! OPTION,PARAM  IN  K* : OPTION ET PARAMETRE PERMETTANT DE DETERMINER
!                        LA FAMILLE DE PG UTILISEE.
! EXIXFM  OUT K3 : 'OUI' : IL EXISTE DES GRELS AVEC FAMILLE XFEM...
!                  'NON' SINON
! KECONO  IN/JXOUT K24 : VECTEUR D'ENTIERS LONG=NBGREL(LIGREL)
!      V(IGR) = 1 : LE GREL IGR UTILISE UNE FAMILLE XFEM...
!             = 0 SINON
! CHSGEO  IN/JXOUT K19 : CHAM_ELEM_S (GEOM_R) DE TYPE 'ELGA'
!
! REMARQUE :
!   L'OBJET CHSGEO N'EST CREE QUE SI EXIXFM='OUI'
! ------------------------------------------------------------------
!
!     ------------------------------------------------------------------
!
    integer :: nbout, nbin
    parameter   (nbout=1, nbin=6)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer ::  iopt, iopt1, nute, numc, igr, nbgrel
    integer :: jecono, imolo, jmolo, nec, kfpg
    integer :: igd, jpnlfp, nblfpg, jnolfp, nbfam, jfpgl
    integer :: k, nuflpg, nufgpg
    character(len=8) :: nomgd, elrefe, ma, mo
    character(len=16) :: nofpg, nomte
    character(len=24) :: chgeom
    character(len=32) :: noflpg
!     ------------------------------------------------------------------
    call jemarq()
!
    call dismoi('NOM_MAILLA', ligrel, 'LIGREL', repk=ma)
    call dismoi('NOM_MODELE', ligrel, 'LIGREL', repk=mo)
    call jelira(ligrel//'.LIEL', 'NMAXOC', nbgrel)
!
    call jeveuo('&CATA.TE.PNLOCFPG', 'L', jpnlfp)
    call jelira('&CATA.TE.NOLOCFPG', 'LONMAX', nblfpg)
    call jeveuo('&CATA.TE.NOLOCFPG', 'L', jnolfp)
!
!
!     1. CALCUL DE KECONO ET EXIXFM :
!     ------------------------------------------------------------------
    exixfm='NON'
    call wkvect(kecono, 'V V I', nbgrel, jecono)
    do 1 igr = 1, nbgrel
        zi(jecono-1+igr)=1
  1 end do
    call jenonu(jexnom('&CATA.OP.NOMOPT', 'XFEM_XPG'), iopt1)
    call jenonu(jexnom('&CATA.OP.NOMOPT', option), iopt)
    do 2 igr = 1, nbgrel
        nute = typele(ligrel,igr)
        call jenuno(jexnum('&CATA.TE.NOMTE', nute), nomte)
!
!       L'ELEMENT SAIT-IL CALCULER XFEM_XPG ?
        numc = nucalc(iopt1,nute,1)
        if (numc .lt. 0) goto 2
!
        imolo = modat2(iopt,nute,param)
        if (imolo .eq. 0) goto 2
!
        call jeveuo(jexnum('&CATA.TE.MODELOC', imolo), 'L', jmolo)
        igd = zi(jmolo-1+2)
        call jenuno(jexnum('&CATA.GD.NOMGD', igd), nomgd)
        call dismoi('NB_EC', nomgd, 'GRANDEUR', repi=nec)
        kfpg = zi(jmolo-1+4+nec+1)
!
!       -- FAMILLE "LISTE"
        if (kfpg .lt. 0) then
!          FAMILLE "LISTE" :
            call jelira(jexnum('&CATA.TE.FPG_LISTE', -kfpg), 'LONMAX', nbfam)
            nbfam=nbfam-1
            call jeveuo(jexnum('&CATA.TE.FPG_LISTE', -kfpg), 'L', jfpgl)
            elrefe=zk8(jfpgl-1+nbfam+1)
            do 3 k = 1, nbfam
                noflpg = nomte//elrefe//zk8(jfpgl-1+k)
                nuflpg = indk32(zk32(jpnlfp),noflpg,1,nblfpg)
                nufgpg = zi(jnolfp-1+nuflpg)
                call jenuno(jexnum('&CATA.TM.NOFPG', nufgpg), nofpg)
                if (nofpg(9:12) .eq. 'XFEM') then
                    exixfm='OUI'
                    zi(jecono-1+igr)=0
                endif
  3         continue
!
!       -- FAMILLE "ORDINAIRE"
        else
            call jenuno(jexnum('&CATA.TM.NOFPG', kfpg), nofpg)
            if (nofpg(9:12) .eq. 'XFEM') then
                exixfm='OUI'
                zi(jecono-1+igr)=0
            endif
        endif
!
  2 end do
    if (exixfm .eq. 'NON') goto 9999
!
!
!     2. CALCUL DE CHSGEO :
!     ------------------------------------------------------------------
    chgeom='&&MANOPX.CHGEOM'
    lpain(1) = 'PGEOMER'
    lchin(1) = ma//'.COORDO'
    lpain(2) = 'PPINTTO'
    lchin(2) = mo//'.TOPOSE.PIN'
    lpain(3) = 'PCNSETO'
    lchin(3) = mo//'.TOPOSE.CNS'
    lpain(4) = 'PHEAVTO'
    lchin(4) = mo//'.TOPOSE.HEA'
    lpain(5) = 'PLONCHA'
    lchin(5) = mo//'.TOPOSE.LON'
    lpain(6) = 'PPMILTO'
    lchin(6) = mo//'.TOPOSE.PMI'
    lpaout(1) = 'PXFGEOM'
    lchout(1) = chgeom
!
    call calcul('S', 'XFEM_XPG', ligrel, nbin, lchin,&
                lpain, nbout, lchout, lpaout, 'V',&
                'OUI')
    call celces(chgeom, 'V', chsgeo)
    call detrsd('CHAMP', chgeom)
!
9999 continue
    call jedema()
end subroutine
