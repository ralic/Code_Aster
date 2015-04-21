subroutine modi_alea(ma,alea)

    implicit none
    character(len=*) :: ma
    real(kind=8) :: alea
! ----------------------------------------------------------------------
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
!
!     but : perturber aleatoirement les coordonnees des noeuds
!           d'un maillage pour lui faire perdre ses symetries.
!           cela permet par exemple d'eviter des modes multiples.
!
!     in :
!            ma     : maillage
!            alea   : coefficient parametrant la perturbation
!     out:
!            ma     : les coordonnees du maillage sont modifiees
! ----------------------------------------------------------------------
! La perturbation aleatoire des noeuds consiste a ajouter a chaque coordonnee
! la quantite   alea*random([0,1])*dim1
! ou dim1 est la "dimension" du maillage dans la direction concernee.
! Par exemple, pour la coordonnee Y, dim1= Y_max - Y_min
! ----------------------------------------------------------------------
!
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/assert.h"
#include "asterfort/ggubs.h"
    integer :: n1, jcoor,nno,ino
    character(len=8) :: ma8
    character(len=24) :: coorjv
    real(kind=8) :: xmin,xmax,ymin,ymax,zmin,zmax,xdim(3),x,y,z
    real(kind=8) :: rand1(1),dseed ,r1
! ----------------------------------------------------------------------
!
    call jemarq()
    ma8=ma
    coorjv=ma8//'.COORDO    .VALE'
    call jeveuo(coorjv, 'E', jcoor)
    call jelira(coorjv, 'LONMAX', n1)
    nno=n1/3



!   1. calcul de xmin, ..., zmax et xdim :
!   --------------------------------------
    xmin=zr(jcoor-1+1)
    ymin=zr(jcoor-1+2)
    zmin=zr(jcoor-1+3)
    xmax=zr(jcoor-1+1)
    ymax=zr(jcoor-1+2)
    zmax=zr(jcoor-1+3)
    do ino =2,nno
        x=zr(jcoor-1+3*(ino-1)+1)
        y=zr(jcoor-1+3*(ino-1)+2)
        z=zr(jcoor-1+3*(ino-1)+3)
        xmin=min(xmin,x)
        ymin=min(ymin,y)
        zmin=min(zmin,z)
        xmax=max(xmax,x)
        ymax=max(ymax,y)
        zmax=max(zmax,z)
    enddo
    xdim(1)=alea*(xmax-xmin)
    xdim(2)=alea*(ymax-ymin)
    xdim(3)=alea*(zmax-zmin)


!   2. modification des corrdonnees :
!   --------------------------------------
    dseed = 24331.d0

    do ino =1,nno
        x=zr(jcoor-1+3*(ino-1)+1)
        y=zr(jcoor-1+3*(ino-1)+2)
        z=zr(jcoor-1+3*(ino-1)+3)

        call ggubs(dseed, 1, rand1(1))
        r1=rand1(1)
        ! on passe de [0,1] a [-1,+1] :
        r1=2.d0*(r1-0.5)
        x=x+r1*xdim(1)

        call ggubs(dseed, 1, rand1(1))
        r1=rand1(1)
        r1=2.d0*(r1-0.5)
        y=y+r1*xdim(2)

        call ggubs(dseed, 1, rand1(1))
        r1=rand1(1)
        r1=2.d0*(r1-0.5)
        z=z+r1*xdim(3)

        zr(jcoor-1+3*(ino-1)+1)=x
        zr(jcoor-1+3*(ino-1)+2)=y
        zr(jcoor-1+3*(ino-1)+3)=z
    enddo


    call jedema()
!
end subroutine
