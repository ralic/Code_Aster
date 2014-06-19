subroutine oriem1(ma,kdim,numa2d,numa3d)
    implicit   none
! person_in_charge: jacques.pellet at edf.fr
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/indiis.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jelira.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/provec.h"
#include "asterfort/utmess.h"
#include "blas/ddot.h"
    character(len=8),intent(in) :: ma
    character(len=2),intent(in) :: kdim
    integer,intent(in) :: numa2d
    integer,intent(inout) :: numa3d
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
! But :  regarder si la maille numa3d est bien du cote "-" de la normale
!        de la maille de peau numa2d.
!        Si ce n'est pas le cas, met a zero numa3d
!
! Pour determiner la disposition relative des mailles :
!        On NE se sert PAS de l'orientation des faces des mailles
!        volumiques (qui pourraient etre "retournees").
!        On se sert de la position geometrique des noeuds.
!        Si les mailles volumiques sont degenerees (sans volume), l'algorithme
!        echoue et l'on met numa3d a zero.
! ======================================================================
! in   : kdim   : '3D' / '2D'
! in   : ma     : nom du maillage
! in   : numa2d : numero d'une maille de peau
! inout: numa3d : numero d'une maille "volumique"
! ======================================================================
!
    integer :: ino, n1, n2, n3, ic, nbno1, nbno3, indi
    real(kind=8) :: nor1(3), n1n2(3), n1n3(3), ps1
    integer, pointer :: lino1(:) => null()
    integer, pointer :: lino3(:) => null()
    real(kind=8), pointer :: coor(:) => null()
    character(len=24) :: valk(2)

!
! ========================= DEBUT DU CODE EXECUTABLE ==================

    call jemarq()

    call jeveuo(ma//'.COORDO    .VALE','L',vr=coor)

    call jeveuo(jexnum(ma//'.CONNEX',numa2d),'L',vi=lino3)
    call jelira(jexnum(ma//'.CONNEX',numa2d),'LONMAX',nbno3)

    call jeveuo(jexnum(ma//'.CONNEX',numa3d),'L',vi=lino1)
    call jelira(jexnum(ma//'.CONNEX',numa3d),'LONMAX',nbno1)


!   -- 1. Calcul de la normale de la maille de peau: nor1
!   ------------------------------------------------------
    n1 = lino3(1)
    n2 = lino3(2)

!   -- cas 3D :
    if (kdim .eq. '3D') then
        n3 = lino3(3)
        do 19 ic = 1, 3
            n1n2(ic)=coor(3*(n2-1)+ic)-coor(3*(n1-1)+ic)
            n1n3(ic)=coor(3*(n3-1)+ic)-coor(3*(n1-1)+ic)
19      continue
        call provec(n1n2, n1n3, nor1)

!   -- cas 2D :
    else
        do 21 ic = 1, 3
            n1n2(ic)=coor(3*(n2-1)+ic)-coor(3*(n1-1)+ic)
21      continue
        ASSERT(n1n2(3).eq.0.d0)

!       -- on tourne n1n2 de +90 degres :
        nor1(1)=-n1n2(2)
        nor1(2)=n1n2(1)
        nor1(3)=0.d0
    endif
    ASSERT(ddot(3,nor1,1,nor1,1).gt.0)


!   -- 2. position de la maille numa3d par rapport a la maille de peau  => ps1
!   --------------------------------------------------------------------------
    do 30 ino = 1, nbno1
!        -- on cherche un noeud de lino1 (n2) qui ne soit pas un noeud
!           de la peau :
        indi=indiis(lino3,lino1(ino),1,nbno3)
        if (indi .eq. 0) then
            n2=lino1(ino)
            n1=lino3(1)
            do 20 ic = 1, 3
                n1n2(ic)=coor(3*(n2-1)+ic)-coor(3*(n1-1)+ic)
20          continue
!           -- ps1 > 0 <=> la normale de la peau est orientee comme la
!                         la normale exterieure de la maille 1
            ps1=ddot(3,n1n2,1,nor1,1)
            goto 40
!
        endif
30  end do
    ASSERT(.false.)
40  continue


!   -- si numa3d est degeneree ou du cote "+", on la supprime :
    if (ps1.ge.0.d0) then
       call jenuno(jexnum(ma//'.NOMMAI', numa3d), valk(1))
       call jenuno(jexnum(ma//'.NOMMAI', numa2d), valk(2))
       call utmess('A','CALCULEL3_47',nk=2,valk=valk)
       numa3d=0
    endif

    call jedema()
end subroutine
