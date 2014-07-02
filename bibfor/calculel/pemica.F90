subroutine pemica(champ, long, vr, nbmail, nummai,&
                  orig, iorig, icage)
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8rddg.h"
#include "asterfort/assert.h"
#include "asterfort/celver.h"
#include "asterfort/digdel.h"
#include "asterfort/jacobi.h"
#include "asterfort/jedema.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/nbelem.h"
#include "asterfort/nbgrel.h"
#include "asterfort/orien2.h"
#include "asterfort/scalai.h"
#include "asterfort/utmess.h"
!
    character(len=*) :: champ
    integer :: long, nbmail, nummai(*), iorig
    real(kind=8) :: vr(*), orig(3)
!     ------------------------------------------------------------------
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
!     FAIRE DES OPERATIONS SUR UN CHAM_ELEM (OU D'UN RESUELEM)
!            (NOTION D'INTEGRALE DU CHAMP SUR LE MODELE)
!     ------------------------------------------------------------------
! IN  : CHAMP  : NOM DU CHAM_ELEM
! IN  : LONG   : LONGUEUR DU VECTEUR VR
! OUT : VR     : VECTEUR CONTENANT LES RESULATTS GLOBAUX
! IN  : NBMAIL : = 0  , CALCUL SUR TOUT LE CHAM_ELEM
!                SINON, CALCUL SUR UN NOMBRE DE MAILLES
! IN  : NUMMAI : NUMEROS DES MAILLES
!     ------------------------------------------------------------------
!     ------------------------------------------------------------------
    integer :: longt, long2, mode, nperm
    integer :: itype, iordre
    real(kind=8) :: masse, ixx, iyy, izz, ixy, ixz, iyz, angl(3)
    character(len=8) :: scal
    character(len=4) :: docu
    character(len=19) :: champ2, ligrel
    aster_logical :: first
    real(kind=8) :: ar(6), br(6), vecpro(3, 3), valpro(3), tol, toldyn
    real(kind=8) :: v1(3), v2(3), v3(3), jacaux(3), ixpr2, iypr2
!     ------------------------------------------------------------------
!-----------------------------------------------------------------------
    integer :: i, ibid, icage, icoef, idecgr, iel, im
    integer :: j, jgr, jligr, k
    integer :: nbgr, nbvec, nel, nitjac
    real(kind=8) :: dx, dy, dz, epsi, pgx, pgy, pgz
    real(kind=8) :: rddg
    character(len=24), pointer :: celk(:) => null()
    integer, pointer :: celd(:) => null()
    real(kind=8), pointer :: celv(:) => null()
!-----------------------------------------------------------------------
    call jemarq()
    champ2 = champ
    rddg = r8rddg()
    epsi = 1.d-12
!
!
!     -- ON VERIFIE QUE LE CHAM_ELEM N'EST PAS TROP DYNAMIQUE :
    call celver(champ2, 'NBVARI_CST', 'STOP', ibid)
    call celver(champ2, 'NBSPT_1', 'STOP', ibid)
!
    call jelira(champ2//'.CELD', 'DOCU', cval=docu)
    if (docu .ne. 'CHML') then
        call utmess('F', 'CALCULEL3_52')
    endif
    call jeveuo(champ2//'.CELK', 'L', vk24=celk)
    ligrel = celk(1)(1:19)
!
    call jeveuo(champ2//'.CELD', 'L', vi=celd)
!
!     --- TYPE DE LA GRANDEUR ---
    scal = scalai(celd(1))
!
!     -- ON VERIFIE LES LONGUEURS:
    ASSERT(long.ge.16)
    first = .true.
    nbgr = nbgrel(ligrel)
    do 10 j = 1, nbgr
        mode=celd(celd(4+j) +2)
        if (mode .ne. 0) then
!           --- NOMBRE D'ELEMENTS DANS LE MODE LOCAL ---
            long2 = digdel(mode)
            icoef=max(1,celd(4))
            long2=long2*icoef
            if (first) then
                longt=long2
            else if (longt.ne.long2) then
                call utmess('F', 'CALCULEL4_53')
            endif
            first = .false.
        endif
 10 end do
    ASSERT(longt.le.long)
!
!     -- ON MET A ZERO LE VECTEUR "VSCAL":
    if (scal(1:1) .eq. 'R') then
        vr(1:long) = 0.d0
    else
        call utmess('F', 'CALCULEL3_74', sk=scal)
    endif
!
    call jeveuo(champ2//'.CELV', 'L', vr=celv)
    if (nbmail .le. 0) then
        do 102 j = 1, nbgr
            mode=celd(celd(4+j) +2)
            if (mode .eq. 0) goto 102
            nel = nbelem(ligrel,j)
            idecgr=celd(celd(4+j)+8)
            do 104 k = 1, nel
!
!              -- MASSE DE LA STRUCTURE ----
                i = 1
                masse = celv(idecgr+(k-1)*longt+i-1)
                vr(1) = vr(1)+ masse
!
!              -- CENTRE DE GRAVITE DE LA STRUCTURE ----
                do 106 i = 2, 4
                    vr(i)=vr(i)+celv(idecgr+(k-1)*longt+i-1)*&
                    masse
106             continue
104         continue
102     continue
    else
        call jeveuo(ligrel//'.LIEL', 'L', jligr)
        do 110 im = 1, nbmail
            do 112 j = 1, nbgr
                mode=celd(celd(4+j) +2)
                if (mode .eq. 0) goto 112
                call jeveuo(jexnum(ligrel//'.LIEL', j), 'L', jgr)
                nel = nbelem(ligrel,j)
                idecgr=celd(celd(4+j)+8)
                do 114 k = 1, nel
                    iel = zi(jgr+k-1)
                    if (iel .ne. nummai(im)) goto 114
!
!                 -- MASSE DE LA STRUCTURE ----
                    i = 1
                    masse = celv(idecgr+(k-1)*longt+i-1)
                    vr(1) = vr(1)+ masse
!
!                 -- CENTRE DE GRAVITE DE LA STRUCTURE ----
                    do 116 i = 2, 4
                        vr(i)=vr(i)+celv(idecgr+(k-1)*longt+i-1)&
                        *masse
116                 continue
                    goto 110
114             continue
112         continue
110     continue
    endif
!
!     --- CENTRE DE GRAVITE ---
    if (abs(vr(1)) .gt. 1.d-6) then
        vr(2) = vr(2) / vr(1)
        vr(3) = vr(3) / vr(1)
        vr(4) = vr(4) / vr(1)
    endif
!
    if (iorig .eq. 1) then
!
!       --- NOEUD P CHOISI PAR L'UTILISATEUR POUR CALCULER LE ---
!       --- TENSEUR D'INERTIE                                 ---
        vr(17) = orig(1)
        vr(18) = orig(2)
        vr(19) = orig(3)
!
!       --- VECTEUR PG ---
        pgx = vr(2) - orig(1)
        pgy = vr(3) - orig(2)
        pgz = vr(4) - orig(3)
    endif
!
    if (nbmail .le. 0) then
        do 202 j = 1, nbgr
            mode=celd(celd(4+j) +2)
            if (mode .eq. 0) goto 202
            nel = nbelem(ligrel,j)
            idecgr=celd(celd(4+j)+8)
            do 204 k = 1, nel
!
                masse = celv(idecgr+(k-1)*longt)
!
                dx = celv(idecgr+(k-1)*longt+1) - vr(2)
                dy = celv(idecgr+(k-1)*longt+2) - vr(3)
                dz = celv(idecgr+(k-1)*longt+3) - vr(4)
!
!              --- INERTIES DE LA STRUCTURE ---
                ixx = celv(idecgr+(k-1)*longt+4)
                iyy = celv(idecgr+(k-1)*longt+5)
                izz = celv(idecgr+(k-1)*longt+6)
                ixy = celv(idecgr+(k-1)*longt+7)
                ixz = celv(idecgr+(k-1)*longt+8)
                iyz = celv(idecgr+(k-1)*longt+9)
                vr(5) = vr(5) + ixx + masse*(dy*dy + dz*dz)
                vr(6) = vr(6) + iyy + masse*(dx*dx + dz*dz)
                vr(7) = vr(7) + izz + masse*(dx*dx + dy*dy)
                vr(8) = vr(8) + ixy + masse*dx*dy
                vr(9) = vr(9) + ixz + masse*dx*dz
                vr(10) = vr(10) + iyz + masse*dy*dz
                if (icage .ne. 0) then
                    ixpr2 = celv(idecgr+(k-1)*longt+10)
                    iypr2 = celv(idecgr+(k-1)*longt+11)
                    ASSERT(long.ge.27)
                    vr(26) = vr(26) + ixpr2 + dy*(3.0d0*ixx+iyy) + masse*dy*(dx*dx+dy*dy) + 2.0d0&
                             &*dx*ixy
                    vr(27) = vr(27) + iypr2 + dx*(3.0d0*iyy+ixx) + masse*dx*(dx*dx+dy*dy) + 2.0d0&
                             &*dy*ixy
                endif
!
204         continue
202     continue
    else
        call jeveuo(ligrel//'.LIEL', 'L', jligr)
        do 210 im = 1, nbmail
            do 212 j = 1, nbgr
                mode=celd(celd(4+j) +2)
                if (mode .eq. 0) goto 212
                call jeveuo(jexnum(ligrel//'.LIEL', j), 'L', jgr)
                nel = nbelem(ligrel,j)
                idecgr=celd(celd(4+j)+8)
                do 214 k = 1, nel
                    iel = zi(jgr+k-1)
                    if (iel .ne. nummai(im)) goto 214
!
                    masse = celv(idecgr+(k-1)*longt)
!
                    dx = celv(idecgr+(k-1)*longt+1) - vr(2)
                    dy = celv(idecgr+(k-1)*longt+2) - vr(3)
                    dz = celv(idecgr+(k-1)*longt+3) - vr(4)
!
!                 --- INERTIES DE LA STRUCTURE ---
                    ixx = celv(idecgr+(k-1)*longt+4)
                    iyy = celv(idecgr+(k-1)*longt+5)
                    izz = celv(idecgr+(k-1)*longt+6)
                    ixy = celv(idecgr+(k-1)*longt+7)
                    ixz = celv(idecgr+(k-1)*longt+8)
                    iyz = celv(idecgr+(k-1)*longt+9)
                    vr(5) = vr(5) + ixx + masse*(dy*dy + dz*dz)
                    vr(6) = vr(6) + iyy + masse*(dx*dx + dz*dz)
                    vr(7) = vr(7) + izz + masse*(dx*dx + dy*dy)
                    vr(8) = vr(8) + ixy + masse*dx*dy
                    vr(9) = vr(9) + ixz + masse*dx*dz
                    vr(10) = vr(10) + iyz + masse*dy*dz
                    if (icage .ne. 0) then
                        ixpr2 = celv(idecgr+(k-1)*longt+10)
                        iypr2 = celv(idecgr+(k-1)*longt+11)
                        ASSERT(long.ge.27)
                        vr(26) = vr(26) + ixpr2 + dy*(3.0d0*ixx+iyy) + masse*dy*(dx*dx+dy*dy) + 2&
                                 &.0d0*dx*ixy
                        vr(27) = vr(27) + iypr2 + dx*(3.0d0*iyy+ixx) + masse*dx*(dx*dx+dy*dy) + 2&
                                 &.0d0*dy*ixy
                    endif
                    goto 210
214             continue
212         continue
210     continue
    endif
!
    if (iorig .eq. 1) then
!
!     --- INERTIES DE LA STRUCTURE AU NOEUD UTILISATEUR P  ---
        ASSERT(long.ge.25)
        vr(20) = vr(5) + vr(1)*(pgy*pgy + pgz*pgz)
        vr(21) = vr(6) + vr(1)*(pgx*pgx + pgz*pgz)
        vr(22) = vr(7) + vr(1)*(pgx*pgx + pgy*pgy)
        vr(23) = vr(8) + vr(1)*pgx*pgy
        vr(24) = vr(9) + vr(1)*pgx*pgz
        vr(25) = vr(10) + vr(1)*pgy*pgz
    endif
!
    nbvec = 3
    if (abs(vr(5)) .lt. epsi .and. abs(vr(6)) .lt. epsi .and. abs(vr(7)) .lt. epsi .and.&
        abs(vr(8)) .lt. epsi .and. abs(vr(9)) .lt. epsi .and. abs(vr(10)) .lt. epsi) then
        vr(11) = 0.d0
        vr(12) = 0.d0
        vr(13) = 0.d0
        vr(14) = 0.d0
        vr(15) = 0.d0
        vr(16) = 0.d0
        if (icage .ne. 0) then
            ASSERT(long.ge.29)
            vr(26) = 0.d0
            vr(27) = 0.d0
            vr(28) = 0.d0
            vr(29) = 0.d0
        endif
    else
!        LORS DE LA CONSTRUCTION DE LA MATRICE D'INERTIE,
!        ON RAJOUTE DES MOINS SUR LES TERMES EXTRA_DIAGONAUX
        ar(1) = vr(5)
        ar(2) = - vr(8)
        ar(3) = - vr(9)
        ar(4) = vr(6)
        ar(5) = - vr(10)
        ar(6) = vr(7)
        br(1) = 1.d0
        br(2) = 0.d0
        br(3) = 0.d0
        br(4) = 1.d0
        br(5) = 0.d0
        br(6) = 1.d0
        nperm = 12
        tol = 1.d-10
        toldyn = 1.d-2
        itype = 0
        iordre = 0
        call jacobi(nbvec, nperm, tol, toldyn, ar,&
                    br, vecpro, valpro, jacaux, nitjac,&
                    itype, iordre)
        v1(1) = 0.d0
        v1(2) = 0.d0
        v1(3) = 0.d0
        v2(1) = vecpro(1,1)
        v2(2) = vecpro(2,1)
        v2(3) = vecpro(3,1)
        v3(1) = vecpro(1,2)
        v3(2) = vecpro(2,2)
        v3(3) = vecpro(3,2)
        call orien2(v1, v2, v3, angl)
        vr(11) = valpro(1)
        vr(12) = valpro(2)
        vr(13) = valpro(3)
        vr(14) = angl(1) * rddg
        vr(15) = angl(2) * rddg
        vr(16) = angl(3) * rddg
        if (icage .ne. 0) then
            ASSERT(long.ge.29)
            vr(28) = -sin(angl(1))*vr(27) + cos(angl(1))*vr(26)
            vr(29) = cos(angl(1))*vr(27) + sin(angl(1))*vr(26)
        endif
    endif
!
    call jedema()
end subroutine
