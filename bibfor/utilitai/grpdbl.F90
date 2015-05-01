subroutine grpdbl(maz, typgz)
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cpclma.h"
#include "asterfort/jecrec.h"
#include "asterfort/jecreo.h"
#include "asterfort/jecroc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeecra.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=*) :: maz, typgz
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ----------------------------------------------------------------------
!  BUT : SUPPRIMER LES DOUBLONS DANS MA.GROUPENO ET MA.GROUPEMA
! ----------------------------------------------------------------------
!       MAZ   : NOM DE LA STRUCTURE DE DONNEES A TESTER
!       TYPGZ : /GROUPEMA / GROUPENO
! ----------------------------------------------------------------------
!
    integer :: igr, nbgr, ie, nbe, jgroup, nue, nbno, nbma
    integer ::  k,  nbent, jgrou2
    integer :: imax, nelim, lnew,  je, kk
    character(len=4) ::  clas
    character(len=8) :: ma, typgr
    character(len=9) :: ptrn
    character(len=24) :: nomgr, nvnogr, nvptno
    integer, pointer :: dime(:) => null()
    integer, pointer :: lent(:) => null()
    integer, pointer :: vlnew(:) => null()
! -DEB------------------------------------------------------------------
!
    call jemarq()
!
!
    ma = maz
    typgr=typgz
    ASSERT(typgr.eq.'GROUPENO'.or.typgr.eq.'GROUPEMA')
    if (typgr .eq. 'GROUPENO') then
        ptrn='PTRNOMNOE'
        nvnogr='&&GRPDBL.GROUPENO'
        nvptno='&&GRPDBL.PTRNOMNOE'
    else
        ptrn='PTRNOMMAI'
        nvnogr='&&GRPDBL.GROUPEMA'
        nvptno='&&GRPDBL.PTRNOMMAI'
    endif
!
    call jeveuo(ma//'.DIME', 'L', vi=dime)
    nbno=dime(1)
    nbma=dime(3)
    if (typgr .eq. 'GROUPENO') then
        nbent=nbno
    else
        nbent=nbma
    endif
!
!
    AS_ALLOCATE(vi=lent, size=nbent)
    call jelira(ma//'.'//typgr, 'NOMUTI', nbgr)
!
    call jecreo(nvptno, 'V N K24')
    call jeecra(nvptno, 'NOMMAX', nbgr)
    call jecrec(nvnogr, 'V V I', 'NO '//nvptno, 'DISPERSE', 'VARIABLE',&
                nbgr)
!
    do 21,igr = 1,nbgr
    call jelira(jexnum(ma//'.'//typgr, igr), 'LONMAX', nbe)
    call jeveuo(jexnum(ma//'.'//typgr, igr), 'L', jgroup)
    call jenuno(jexnum(ma//'.'//typgr, igr), nomgr)
    call jecroc(jexnom(nvnogr, nomgr))
    imax=0
    do 11,ie = 1,nbe
    nue = zi(jgroup-1+ie)
    lent(nue)=lent(nue)+1
    imax=max(imax,lent(nue))
11  continue
!
    lnew=nbe
    if (imax .gt. 1) then
!         -- IL Y A DES DOUBLONS A ELIMINER :
        nelim=0
        do 31,k=1,nbent
        if (lent(k) .gt. 1) nelim=nelim+1
31      continue
        ASSERT(nelim.gt.0)
        lnew=nbe-nelim
        ASSERT(lnew.gt.0)
        AS_ALLOCATE(vi=vlnew, size=lnew)
        je=0
        do 34,ie=1,nbe
        nue = zi(jgroup-1+ie)
        kk=lent(nue)
        ASSERT(kk.ne.0)
        if (kk .eq. 1) then
!             -- ENTITE NON DOUBLONNEE :
            je=je+1
            ASSERT(je.ge.1.and.je.le.lnew)
            vlnew(je)=nue
        else if (kk.gt.1) then
!             -- ENTITE DOUBLONNEE VUE LA 1ERE FOIS:
            je=je+1
            ASSERT(je.ge.1.and.je.le.lnew)
            vlnew(je)=nue
            lent(nue)=-1
        else if (kk.eq.-1) then
!             -- ENTITE DOUBLONNEE DEJA VUE :
        else
            ASSERT(.false.)
        endif
34      continue
    endif
!
    call jeecra(jexnom(nvnogr, nomgr), 'LONMAX', max(1,lnew))
    call jeecra(jexnom(nvnogr, nomgr), 'LONUTI', lnew)
    call jeveuo(jexnom(nvnogr, nomgr), 'E', jgrou2)
!
    if (imax .gt. 1) then
        do 35,k=1,lnew
        zi(jgrou2-1+k)=vlnew(k)
35      continue
        AS_DEALLOCATE(vi=vlnew)
    else
        do 36,k=1,lnew
        zi(jgrou2-1+k)=zi(jgroup-1+k)
36      continue
    endif
!
!
!       -- REMISE A ZERO DE .LENT :
    do 32,k=1,nbent
    lent(k)=0
32  continue
!
    21 end do
!
    AS_DEALLOCATE(vi=lent)
!
    call jelira(ma//'.'//typgr, 'CLAS', cval=clas)
    call jedetr(ma//'.'//typgr)
    call jedetr(ma//'.'//ptrn)
    call cpclma('&&GRPDBL', ma, typgr, clas(1:1))
    call jedetr(nvnogr)
    call jedetr(nvptno)
!
!
    call jedema()
end subroutine
