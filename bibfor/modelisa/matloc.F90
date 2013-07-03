subroutine matloc(noma, ncncin, motfac, ioc, ino,&
                  nbma, listma, pgl)
    implicit none
#include "jeveux.h"
!
#include "asterc/getvr8.h"
#include "asterc/r8dgrd.h"
#include "asterfort/angvx.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/matrot.h"
#include "asterfort/u2mess.h"
    integer :: ioc, ino, nbma, listma(*)
    real(kind=8) :: pgl(3, 3)
    character(len=8) :: noma
    character(len=16) :: motfac
    character(len=24) :: ncncin
! ---------------------------------------------------------------------
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
! IN  : MOTFAC : MOT CLE FACTEUR
! IN  : IOC    : NOMERO D'OCCURRENCE
! IN  : INO    : NOMERO DU NOEUD TRAITE
! OUT : PGL    : MATRICE DE PASSAGE GLOBAL VERS LOCAL
! ---------------------------------------------------------------------
!
    integer :: n2, n3, i, j, nbm, adrm, iatyma, numa, adrvlc, ino1, ino2, acoord
    real(kind=8) :: vx(3), vy(3), vz(3), vecty(3), vxn, vyn, vyp, dgrd, angl(3)
    real(kind=8) :: alpha, beta, gamma
    character(len=8) :: k8b, typm
    character(len=24) :: connex, typmai, coordo
    integer :: iarg
! ----------------------------------------------------------------------
!
    call jemarq()
!
    dgrd = r8dgrd()
!
    call jelira(jexnum(ncncin, ino), 'LONMAX', nbm, k8b)
    call jeveuo(jexnum(ncncin, ino), 'L', adrm)
!
    connex = noma//'.CONNEX         '
    typmai = noma//'.TYPMAIL        '
    coordo = noma//'.COORDO    .VALE'
    call jeveuo(typmai, 'L', iatyma)
    call jeveuo(coordo, 'L', acoord)
!
! --- SI 1 MAILLE SUR LE NOEUD, OK
!
    if (nbm .eq. 1) then
        numa = zi(adrm)
!
! --- SI PLUSIEURS MAILLES SUR LE NOEUD, RECHERCHE DE LA MAILLE
!
    else
        if (nbma .eq. 0) then
            call u2mess('F', 'MODELISA5_32')
        endif
        if (nbma .ne. 1) then
            call u2mess('F', 'MODELISA5_33')
        endif
        do 20 i = 1, nbma
            numa = listma(i)
            do 22 j = 1, nbm
                if (zi(adrm+j-1) .eq. numa) goto 24
22          continue
20      continue
        call u2mess('F', 'MODELISA5_34')
24      continue
    endif
!
    call jenuno(jexnum('&CATA.TM.NOMTM', zi(iatyma+numa-1)), typm)
    if (typm(1:3) .ne. 'SEG') then
        call u2mess('F', 'MODELISA5_35')
    endif
    call jeveuo(jexnum(connex, numa), 'L', adrvlc)
    if (ino .eq. zi(adrvlc)) then
        ino1 = zi(adrvlc+1)
        ino2 = zi(adrvlc)
    else
        ino1 = zi(adrvlc)
        ino2 = zi(adrvlc+1)
    endif
!
! --- SI VECT_Y
!
    call getvr8(motfac, 'VECT_Y', ioc, iarg, 3,&
                vecty, n2)
    if (n2 .ne. 0) then
!        -- VECTEUR COLINEAIRE A LA MAILLE
        vx(1) = zr(acoord+3*(ino2-1) ) - zr(acoord+3*(ino1-1) )
        vx(2) = zr(acoord+3*(ino2-1)+1) - zr(acoord+3*(ino1-1)+1)
        vx(3) = zr(acoord+3*(ino2-1)+2) - zr(acoord+3*(ino1-1)+2)
        vxn = sqrt( vx(1)**2 + vx(2)**2 + vx(3)**2 )
        vx(1) = vx(1) / vxn
        vx(2) = vx(2) / vxn
        vx(3) = vx(3) / vxn
!        -- VECTEUR VECT_Y FOURNI
        vy(1) = vecty(1)
        vy(2) = vecty(2)
        vy(3) = vecty(3)
!        -- PROJECTION / NORMALISATION
        vyp = vx(1)*vy(1) + vx(2)*vy(2) + vx(3)*vy(3)
        vy(1) = vy(1) - vyp*vx(1)
        vy(2) = vy(2) - vyp*vx(2)
        vy(3) = vy(3) - vyp*vx(3)
        vyn = sqrt(vy(1)**2+vy(2)**2+vy(3)**2)
        vy(1) = vy(1) / vyn
        vy(2) = vy(2) / vyn
        vy(3) = vy(3) / vyn
!        -- VECTEUR TANGENT
        vz(1) = vx(2)*vy(3) - vy(2)*vx(3)
        vz(2) = vx(3)*vy(1) - vy(3)*vx(1)
        vz(3) = vx(1)*vy(2) - vy(1)*vx(2)
        do 30 i = 1, 3
            pgl(1,i) = vx(i)
            pgl(2,i) = vy(i)
            pgl(3,i) = vz(i)
30      continue
        goto 9999
    endif
!
! --- SI ANGL_VRIL
!
    call getvr8(motfac, 'ANGL_VRIL', ioc, iarg, 1,&
                gamma, n3)
    if (n3 .ne. 0) then
!        -- VECTEUR COLINEAIRE A LA MAILLE
        vx(1) = zr(acoord+3*(ino2-1) ) - zr(acoord+3*(ino1-1) )
        vx(2) = zr(acoord+3*(ino2-1)+1) - zr(acoord+3*(ino1-1)+1)
        vx(3) = zr(acoord+3*(ino2-1)+2) - zr(acoord+3*(ino1-1)+2)
        vxn = sqrt( vx(1)**2 + vx(2)**2 + vx(3)**2 )
        vx(1) = vx(1) / vxn
        vx(2) = vx(2) / vxn
        vx(3) = vx(3) / vxn
        call angvx(vx, alpha, beta)
        angl(1) = alpha
        angl(2) = beta
        angl(3) = gamma * dgrd
        call matrot(angl, pgl)
        goto 9999
    endif
!
9999  continue
    call jedema()
!
end subroutine
