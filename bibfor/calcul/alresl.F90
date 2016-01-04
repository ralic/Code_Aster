subroutine alresl(opt, ligrel, nochou, nompar, base)
implicit none

! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
! THIS PROGRAM IS FREE SOFTWARE; YOU CAN REDISTRIBUTE IT AND/OR MODIFY
! IT UNDER THE TERMS OF THE GNU GENERAL PUBLIC LICENSE AS PUBLISHED BY
! THE FREE SOFTWARE FOUNDATION; EITHER VERSION 2 OF THE LICENSE, OR
! (AT YOUR OPTION) ANY LATER VERSION.

! THIS PROGRAM IS DISTRIBUTED IN THE HOPE THAT IT WILL BE USEFUL, BUT
! WITHOUT ANY WARRANTY; WITHOUT EVEN THE IMPLIED WARRANTY OF
! MERCHANTABILITY OR FITNESS FOR A PARTICULAR PURPOSE. SEE THE GNU
! GENERAL PUBLIC LICENSE FOR MORE DETAILS.

! YOU SHOULD HAVE RECEIVED A COPY OF THE GNU GENERAL PUBLIC LICENSE
! ALONG WITH THIS PROGRAM; IF NOT, WRITE TO EDF R&D CODE_ASTER,
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     arguments:
!     ----------
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/digde2.h"
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
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"

    integer :: opt
    character(len=19) :: ligrel, nochou
    character(len=8) :: nompar
    character(len=*) :: base
! ----------------------------------------------------------------------
!     entrees:
!      opt   : option
!     ligrel : nom de ligrel
!     nochou : nom du resuelem a allouer
!      nompar: nom du parametre
!      base  : 'g', 'v'

!     sorties:
!     + creation du resuelem de nom nochou
!       (le resuelem peut etre "vide")
! ----------------------------------------------------------------------
    character(len=1) :: bas1
    character(len=16) :: nomopt

    integer :: ngrel, igr, te, nel, mode, ncmpel, ipar
    integer :: desc, gd, jnoli, idesc
    integer ::  iparmx
    character(len=8) :: scal, nomgd, tymat
! ----------------------------------------------------------------------

    call jemarq()
    bas1 = base

    call jenuno(jexnum('&CATA.OP.NOMOPT', opt), nomopt)
    ngrel = nbgrel(ligrel)
    gd = grdeur(nompar)
    scal = scalai(gd)


!   -- le resuelem doit-il etre cree ?
!   ----------------------------------
    iparmx = 0
    do igr = 1, ngrel
        te = typele(ligrel,igr,1)
        ipar = inpara(opt,te,'OUT',nompar)
        iparmx = max(iparmx,ipar)
    end do
    if (iparmx .eq. 0) goto 30


    call jenuno(jexnum('&CATA.GD.NOMGD', gd), nomgd)
    call dismoi('TYPE_MATRICE', nomgd, 'GRANDEUR', repk=tymat)


!   -- creation de l'objet .NOLI :
    call wkvect(nochou//'.NOLI', bas1//' V K24', 4, jnoli)
    zk24(jnoli-1+1) = ligrel
    zk24(jnoli-1+2) = nomopt
    zk24(jnoli-1+3) = 'MPI_COMPLET'

!   -- creation de l'objet .DESC :
    call wkvect(nochou//'.DESC', bas1//' V I', 2+ngrel, idesc)
    call jeecra(nochou//'.DESC', 'DOCU', cval='RESL')

!   -- creation de la collection dipersee .RESL  :
    call jecrec(nochou//'.RESL', bas1//' V '//scal(1:4), 'NU', 'DISPERSE', 'VARIABLE',&
                ngrel)


!   -- remplissage de desc et allocation de .RESL:
!   ----------------------------------------------
    call jeveuo(nochou//'.DESC', 'E', desc)
    zi(desc-1+1) = gd
    zi(desc-1+2) = ngrel
    do igr = 1, ngrel
        nel = nbelem(ligrel,igr,1)
        te = typele(ligrel,igr,1)
        ipar = inpara(opt,te,'OUT',nompar)

!        -- si le type_element ne connait pas le parametre:
        if (ipar .eq. 0) then
            zi(desc-1+2+igr) = 0
        else
            mode = modatt(opt,te,'OUT',ipar)
            ASSERT(mode.gt.0)
            zi(desc-1+2+igr) = mode
            call jecroc(jexnum(nochou//'.RESL', igr))

            ncmpel = digde2(mode)
            call jeecra(jexnum(nochou//'.RESL', igr), 'LONMAX', ncmpel*nel)
        endif
    end do
 30 continue
    call jedema()
end subroutine
