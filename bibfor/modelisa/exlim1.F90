subroutine exlim1(lismai, nbmail, modelz, basez, ligrez)
    implicit none
#include "jeveux.h"
#include "asterfort/adalig.h"
#include "asterfort/cormgi.h"
#include "asterfort/dismoi.h"
#include "asterfort/initel.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jeecra.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexatr.h"
#include "asterfort/jexnum.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_allocate.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/assert.h"
#include "asterc/cheksd.h"
!
    integer :: lismai(*), nbmail
    character(len=*) :: modelz, basez, ligrez

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
! in  : lismai : liste des numeros de mailles constituant le
!                ligrel a creer.
!                Remarque : les mailles de numero 0 sont ignorees.
! in  : nbmail : longueur de la liste des mailles
! in  : modelz : nom du modele referencant les mailles de lismai
!                des grels
! in  : basez  : base sur-laquelle on cree le ligrel
! out : ligrez : ligrel a creer
!----------------------------------------------------------------------
!
!
    character(len=1) :: base
    character(len=8) :: modele, noma, nomail
    character(len=19) :: ligrel, ligrmo
    character(len=24) :: cptlie
    integer :: i, j, lont, numvec, numail, igrel, nbmam, k
    integer :: lcliel,   jdnb, iadm, jdli
    integer :: jtyp, jnel, typele, typel1, nbtyel, itype, nmel,nbmail_nz
    integer, pointer :: lismai_nz(:) => null()
    integer, pointer :: liel(:) => null()
    integer, pointer :: repe(:) => null()
!     ------------------------------------------------------------------
!
    call jemarq()
!
    base = basez
    modele = modelz
    ligrel = ligrez
!
! --- MAILLAGE ASSOCIE AU MODELE
!     --------------------------
    call dismoi('NOM_MAILLA', modele, 'MODELE', repk=noma)
!
! --- LIGREL DU MODELE
!     ----------------
    call dismoi('NOM_LIGREL', modele, 'MODELE', repk=ligrmo)
!
    call jeveuo(ligrmo//'.REPE', 'L', vi=repe)
    call jeveuo(jexatr(ligrmo//'.LIEL', 'LONCUM'), 'L', lcliel)
    call jeveuo(ligrmo//'.LIEL', 'L', vi=liel)
!
! --- OBJET NBNO
!     ----------
    call wkvect(ligrel//'.NBNO', base//' V I', 1, jdnb)
    zi(jdnb) = 0
!
! --- OBJET .LGRF
!     ------------
    call jedupo(ligrmo//'.LGRF', base, ligrel//'.LGRF', .false._1)

!   -- On retire les mailles 0 de lismai :
!   --------------------------------------
    AS_ALLOCATE(vi=lismai_nz, size=nbmail)
    nbmail_nz=0
    do k=1,nbmail
        if (lismai(k).gt.0) then
            nbmail_nz=nbmail_nz+1
            lismai_nz(nbmail_nz)=lismai(k)
        endif
    enddo



! --- TYPE D'ELEMENT ET NOMBRE DE MAILLES PAR TYPE
!     --------------------------------------------
    call wkvect('&&EXLIM1.TYPE_NOMBRE', 'V V I', 2*nbmail_nz, jtyp)
    jnel = jtyp + nbmail_nz
!
    typel1 = 0
    nbtyel = 0
    itype = 0
    do i = 1, nbmail_nz
        numail = lismai_nz(i)
        igrel = repe(1+2*(numail-1))
        if (igrel .eq. 0) then
            call jenuno(jexnum(noma//'.NOMMAI', numail), nomail)
            call utmess('F', 'MODELISA4_50', sk=nomail)
        endif
        iadm = zi(lcliel+igrel)
        typele = liel(iadm-1)
        if (typele .eq. typel1) then
            zi(jnel-1+itype) = zi(jnel-1+itype) + 1
        else
            nbtyel = nbtyel + 1
            itype = nbtyel
            typel1 = typele
            zi(jnel-1+itype) = 1
            zi(jtyp-1+nbtyel) = typele
        endif
    end do
!
    nbmam = 0
    do i = 1, nbtyel
        nbmam = max ( nbmam, zi(jnel-1+i) )
    end do


!   -- objet liel
!   =============
    cptlie = ligrel//'.LIEL'
    lont = nbtyel * (nbmam+1)
    call jecrec(cptlie, base//' V I', 'NU', 'CONTIG', 'VARIABLE',&
                nbtyel)
    call jeecra(cptlie, 'LONT', lont)
    call jeveuo(cptlie, 'E', jdli)

!   -- stockage des groupes elements dans liel
!   ------------------------------------------
    numvec = 0
    numail = 0
    do i = 1, nbtyel
        nmel = zi(jnel-1+i)
!
        call jecroc(jexnum(cptlie, i))
        call jeecra(jexnum(cptlie, i), 'LONMAX', nmel+1)
!
        do j = 1, nmel
            numvec = numvec + 1
            numail = numail + 1
            zi(jdli+numvec-1) = lismai_nz(numail)
        end do
!
        numvec = numvec + 1
        zi(jdli+numvec-1) = zi(jtyp-1+i)
!
    end do
!
    call jedetr('&&EXLIM1.TYPE_NOMBRE')


!   --  adaptation de la taille des grels et initialisation des elements
!   ---------------------------------------------------------------------
    call adalig(ligrel)
    call cormgi(base, ligrel)
    call initel(ligrel)
!
    AS_DEALLOCATE(vi=lismai_nz)
    call jedema()
end subroutine
