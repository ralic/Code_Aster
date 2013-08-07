subroutine alresl(opt, ligrel, nochou, nompar, base)
    implicit none
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
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/digde2.h"
#include "asterfort/digde3.h"
#include "asterfort/digde4.h"
#include "asterfort/dismoi.h"
#include "asterfort/grdeur.h"
#include "asterfort/inpara.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/modatt.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/scalai.h"
#include "asterfort/teattr.h"
#include "asterfort/typele.h"
#include "asterfort/u2mess.h"
#include "asterfort/wkvect.h"
    integer :: opt
    character(len=19) :: ligrel, nochou
    character(len=8) :: nompar
    character(len=*) :: base
! ----------------------------------------------------------------------
!     ENTREES:
!      OPT   : OPTION
!     LIGREL : NOM DE LIGREL
!     NOCHOU : NOM DU RESUELEM A ALLOUER
!      NOMPAR: NOM DU PARAMETRE
!      BASE  : 'G', 'V' OU 'L'
!
!     SORTIES:
!     + CREATION DU RESUELEM DE NOM NOCHOU
!       (LE RESUELEM PEUT ETRE "VIDE")
!
! ----------------------------------------------------------------------
!
!     FONCTIONS EXTERNES:
!     -------------------
    character(len=1) :: bas2
    character(len=16) :: nomopt, codvoi, nomte
    integer :: iamaco, ilmaco, iamsco, ilmsco, ialiel, illiel
    common /caii03/iamaco,ilmaco,iamsco,ilmsco,ialiel,illiel
    integer :: evfini, calvoi, jrepe, jptvoi, jelvoi
    common /caii19/evfini,calvoi,jrepe,jptvoi,jelvoi
!
!     VARIABLES LOCALES:
    integer :: ngrel, igr, te, nel, mode, ncmpel, ipar
    integer :: desc, gd, jnoli, iel, idesc
    integer :: lon1, lontot, iparmx, ibid
    integer :: jrsvi, dim1
    character(len=8) :: scal, nomgd, tymat
    logical :: lmatvf
!
!
    call jemarq()
    bas2 = base
!
    call jenuno(jexnum('&CATA.OP.NOMOPT', opt), nomopt)
    ngrel = nbgrel(ligrel)
    gd = grdeur(nompar)
    scal = scalai(gd)
!
!
!     -- LE RESUELEM DOIT-IL ETRE CREE ?
!     ----------------------------------
    iparmx = 0
    do 10 igr = 1, ngrel
        te = typele(ligrel,igr)
        ipar = inpara(opt,te,'OUT',nompar)
        iparmx = max(iparmx,ipar)
10  end do
    if (iparmx .eq. 0) goto 30
!
!
!     -- LE RESUELEM EST-IL "VOISIN_VF" ET DE TYPE "MATRICE" ?
!        (I.E. LMATVF=.TRUE.)
    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
    call dismoi('F', 'TYPE_MATRICE', nomgd, 'GRANDEUR', ibid,&
                tymat, ibid)
    lmatvf=.false.
    if (evfini .eq. 1) then
        if (tymat .eq. ' ') then
!         -- C'EST UN RESUELEM DE TYPE "VECTEUR"
            lmatvf=.false.
        else
            lmatvf=.true.
            if (tymat .ne. 'NON_SYM') call u2mess('F', 'CALCULEL4_12')
        endif
    endif
!
!
!     ----CREATION DE L'OBJET NOLI :
    call wkvect(nochou//'.NOLI', bas2//' V K24', 4, jnoli)
    zk24(jnoli-1+1) = ligrel
    zk24(jnoli-1+2) = nomopt
    zk24(jnoli-1+3) = 'MPI_COMPLET'
    if (lmatvf) zk24(jnoli-1+4) = 'VOISIN_VF'
!
!     ----CREATION DE L'OBJET DESC :
    call wkvect(nochou//'.DESC', bas2//' V I', 2+ngrel, idesc)
    call jeecra(nochou//'.DESC', 'DOCU', cval='RESL')
!
!     ---CREATION DE LA COLLECTION DIPERSEE RESL  :
    call jecrec(nochou//'.RESL', bas2//' V '//scal(1:4), 'NU', 'DISPERSE', 'VARIABLE',&
                ngrel)
!
    if (lmatvf) then
        call jecrec(nochou//'.RSVI', bas2//' V I', 'NU', 'CONTIG', 'VARIABLE',&
                    ngrel)
        lontot=0
        do 22 igr = 1, ngrel
            nel = nbelem(ligrel,igr)
            lontot=lontot+nel+1
22      continue
        call jeecra(nochou//'.RSVI', 'LONT', lontot, ' ')
    endif
!
!
!     -- REMPLISSAGE DE DESC ET ALLOCATION DE .RESL:
!     ----------------------------------------------
    call jeveuo(nochou//'.DESC', 'E', desc)
    zi(desc-1+1) = gd
    zi(desc-1+2) = ngrel
    do 20 igr = 1, ngrel
        nel = nbelem(ligrel,igr)
        te = typele(ligrel,igr)
        ipar = inpara(opt,te,'OUT',nompar)
!
!        -- SI LE TYPE_ELEMENT NE CONNAIT PAS LE PARAMETRE:
        if (ipar .eq. 0) then
            zi(desc-1+2+igr) = 0
!          -- LES COLLECTIONS NUMEROTEES CONTIG DOIVENT ETRE COMPLETES:
            if (lmatvf) then
                call jeecra(jexnum(nochou//'.RSVI', igr), 'LONMAX', 1)
                call jeveuo(jexnum(nochou//'.RSVI', igr), 'E', jrsvi)
            endif
        else
            mode = modatt(opt,te,'OUT',ipar)
            ASSERT(mode.gt.0)
            zi(desc-1+2+igr) = mode
            call jecroc(jexnum(nochou//'.RESL', igr))
            if (lmatvf) call jecroc(jexnum(nochou//'.RSVI', igr))
!
            if (.not.lmatvf) then
                ncmpel = digde2(mode)
                call jeecra(jexnum(nochou//'.RESL', igr), 'LONMAX', ncmpel*nel)
            else
                call jeecra(jexnum(nochou//'.RSVI', igr), 'LONMAX', nel+ 1)
                call jeveuo(jexnum(nochou//'.RSVI', igr), 'E', jrsvi)
                call jenuno(jexnum('&CATA.TE.NOMTE', te), nomte)
                call teattr(nomte, 'S', 'TYPE_VOISIN', codvoi, ibid)
!
!           -- CALCUL DE LA LONGUEUR DU GREL : LONTOT
                lontot=0
                zi(jrsvi-1+1)=lontot+1
                dim1=digde3(mode,'L')
                do 21, iel=1,nel
                lon1=digde4(igr,iel,opt,ipar,dim1,codvoi)
                lontot=lontot+lon1
                zi(jrsvi-1+iel+1)=lontot+1
21              continue
                call jeecra(jexnum(nochou//'.RESL', igr), 'LONMAX', lontot)
            endif
        endif
20  end do
30  continue
    call jedema()
end subroutine
