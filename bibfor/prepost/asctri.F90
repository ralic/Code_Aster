subroutine asctri(mailla, rm)
    implicit   none
#include "jeveux.h"
!
#include "asterc/r8maem.h"
#include "asterc/r8pi.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnom.h"
    character(len=8) :: mailla
    real(kind=8) :: rm
!-----------------------------------------------------------------------
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
!
!     TRI DES GROUPES PETIT AXE ET GRAND AXE FISSURE
!
!
!-----------------DONNEES FOURNIES PAR L'UTILISATEUR--------------------
!
!     RM     = RAYON MOYEN DU COUDE
!     MAILLA = S.D. MAILLAGE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
    integer :: jgrn1, jgrn2, icoor, j, k, ibid, kmin, kmax, nbnop, nbnog, iret1
    integer :: iret2, jgrn3, jgrn4, iret3, iret4, iret5
    real(kind=8) :: x, y, z, xmin, zmax, ymax, pi, zinf, zsup, rnorm, xor, yor
    real(kind=8) :: zor, xex, yex, zex
    character(len=8) :: k8b
    character(len=24) :: grpnoe, coord, nogrn1, nogrn2, nogrn3, nogrn4, nogrn5
!     ------------------------------------------------------------------
!
    call jemarq()
!
    pi = r8pi()
    grpnoe = mailla//'.GROUPENO'
    coord = mailla//'.COORDO    .VALE'
!
    call jeveuo(coord, 'E', icoor)
!
    nogrn1 = 'P_AXE_1'
    nogrn2 = 'P_AXE_2'
    call jeexin(jexnom(grpnoe, nogrn1), iret1)
    call jeexin(jexnom(grpnoe, nogrn2), iret2)
!
    if (iret1 .ne. 0 .or. iret2 .ne. 0) then
!
        if (iret1 .ne. 0) then
            call jelira(jexnom(grpnoe, nogrn1), 'LONUTI', nbnop, k8b)
            call jeveuo(jexnom(grpnoe, nogrn1), 'E', jgrn1)
        endif
        if (iret2 .ne. 0) then
            call jelira(jexnom(grpnoe, nogrn2), 'LONUTI', nbnop, k8b)
            call jeveuo(jexnom(grpnoe, nogrn2), 'E', jgrn2)
        endif
!
!  TRI POINTS DES PETIT AXE (DE LA PEAU INTERNE VERS LA PEAU EXTERNE)
!   GROUPES P_AXE_1 ET P_AXE_2
!
        do 10 j = 1, nbnop-1
!
            if (iret1 .ne. 0) then
                xmin = r8maem()
                do 20 k = j, nbnop
                    x = zr(icoor+3*(zi(jgrn1+k-1)-1))
                    if (x .lt. xmin) then
                        kmin = k
                        xmin = x
                    endif
20              continue
                ibid = zi(jgrn1+j-1)
                zi(jgrn1+j-1) = zi(jgrn1+kmin-1)
                zi(jgrn1+kmin-1) = ibid
            endif
!
            if (iret2 .ne. 0) then
                xmin = r8maem()
                do 30 k = j, nbnop
                    x = zr(icoor+3*(zi(jgrn2+k-1)-1))
                    if (x .lt. xmin) then
                        kmin = k
                        xmin = x
                    endif
30              continue
                ibid = zi(jgrn2+j-1)
                zi(jgrn2+j-1) = zi(jgrn2+kmin-1)
                zi(jgrn2+kmin-1) = ibid
            endif
!
10      continue
!
    endif
!
    nogrn1 = 'NOLIG1'
    nogrn2 = 'NOLIG2'
    call jeexin(jexnom(grpnoe, nogrn1), iret1)
    call jeexin(jexnom(grpnoe, nogrn2), iret2)
!
    if (iret1 .ne. 0 .or. iret2 .ne. 0) then
!
        if (iret1 .ne. 0) then
            call jelira(jexnom(grpnoe, nogrn1), 'LONUTI', nbnop, k8b)
            call jeveuo(jexnom(grpnoe, nogrn1), 'E', jgrn1)
        endif
        if (iret2 .ne. 0) then
            call jelira(jexnom(grpnoe, nogrn2), 'LONUTI', nbnop, k8b)
            call jeveuo(jexnom(grpnoe, nogrn2), 'E', jgrn2)
        endif
!
!  TRI POINTS DE NOLIG1 ET NOLIG2 (PETIT AXE + PTS DANS LE PROLONGEMENT)
!  (TRIES DE LA PEAU INTERNE VERS LA PEAU EXTERNE)
!
        do 11 j = 1, nbnop-1
!
            if (iret1 .ne. 0) then
                xmin = r8maem()
                do 21 k = j, nbnop
                    x = zr(icoor+3*(zi(jgrn1+k-1)-1))
                    if (x .lt. xmin) then
                        kmin = k
                        xmin = x
                    endif
21              continue
                ibid = zi(jgrn1+j-1)
                zi(jgrn1+j-1) = zi(jgrn1+kmin-1)
                zi(jgrn1+kmin-1) = ibid
            endif
!
            if (iret2 .ne. 0) then
                xmin = r8maem()
                do 31 k = j, nbnop
                    x = zr(icoor+3*(zi(jgrn2+k-1)-1))
                    if (x .lt. xmin) then
                        kmin = k
                        xmin = x
                    endif
31              continue
                ibid = zi(jgrn2+j-1)
                zi(jgrn2+j-1) = zi(jgrn2+kmin-1)
                zi(jgrn2+kmin-1) = ibid
            endif
!
11      continue
!
    endif
!
!   TRI POINTS DES GRAND AXE
!   GROUPES G_AXE_1 ET G_AXE_2
!
    nogrn1 = 'G_AXE_1'
    nogrn2 = 'G_AXE_2'
    call jeexin(jexnom(grpnoe, nogrn1), iret1)
    call jeexin(jexnom(grpnoe, nogrn2), iret2)
!
    if (iret1 .ne. 0 .or. iret2 .ne. 0) then
!
        if (iret1 .ne. 0) then
            call jelira(jexnom(grpnoe, nogrn1), 'LONUTI', nbnog, k8b)
            call jeveuo(jexnom(grpnoe, nogrn1), 'E', jgrn1)
        endif
        if (iret2 .ne. 0) then
            call jelira(jexnom(grpnoe, nogrn2), 'LONUTI', nbnog, k8b)
            call jeveuo(jexnom(grpnoe, nogrn2), 'E', jgrn2)
        endif
!
! -- AUTRES ORIENTATION DE FISSURE : TRI DE Z<0 A Z>0
!
        do 90 j = 1, nbnog-1
!
            if (iret1 .ne. 0) then
                zmax = r8maem()
                do 100 k = j, nbnog
                    z = zr(icoor+3*(zi(jgrn1+k-1)-1)+2)
                    if (z .lt. zmax) then
                        kmax = k
                        zmax = z
                    endif
100              continue
                ibid = zi(jgrn1+j-1)
                zi(jgrn1+j-1) = zi(jgrn1+kmax-1)
                zi(jgrn1+kmax-1) = ibid
            endif
            if (iret2 .ne. 0) then
                zmax = r8maem()
                do 110 k = j, nbnog
                    z = zr(icoor+3*(zi(jgrn2+k-1)-1)+2)
                    if (z .lt. zmax) then
                        kmax = k
                        zmax = z
                    endif
110              continue
                ibid = zi(jgrn2+j-1)
                zi(jgrn2+j-1) = zi(jgrn2+kmax-1)
                zi(jgrn2+kmax-1) = ibid
            endif
!
90      continue
!
        zinf = zr(icoor+3*(zi(jgrn2+1-1)-1)+2)
        zsup = zr(icoor+3*(zi(jgrn2+nbnog-1)-1)+2)
!
        if (abs(zsup-zinf) .lt. 1.d-5) then
!
! -- FISSURE CIRCONFERENTIELLE (90 DEGRE) : TRI DE Y<0 A Y>0
!
            do 60 j = 1, nbnog-1
!
                if (iret1 .ne. 0) then
                    ymax = pi*rm
                    do 70 k = j, nbnog
                        y = zr(icoor+3*(zi(jgrn1+k-1)-1)+1)
                        if (y .lt. ymax) then
                            kmax = k
                            ymax = y
                        endif
70                  continue
                    ibid = zi(jgrn1+j-1)
                    zi(jgrn1+j-1) = zi(jgrn1+kmax-1)
                    zi(jgrn1+kmax-1) = ibid
                endif
!
                if (iret2 .ne. 0) then
                    ymax = pi*rm
                    do 80 k = j, nbnog
                        y = zr(icoor+3*(zi(jgrn2+k-1)-1)+1)
                        if (y .lt. ymax) then
                            kmax = k
                            ymax = y
                        endif
80                  continue
                    ibid = zi(jgrn2+j-1)
                    zi(jgrn2+j-1) = zi(jgrn2+kmax-1)
                    zi(jgrn2+kmax-1) = ibid
                endif
!
60          continue
!
        endif
!
    endif
!
!   TRI POINTS DU FOND DE FISSURE
!
    nogrn1 = 'FONDFISS'
    call jeexin(jexnom(grpnoe, nogrn1), iret1)
!
    if (iret1 .ne. 0) then
!
        call jelira(jexnom(grpnoe, nogrn1), 'LONUTI', nbnog, k8b)
        call jeveuo(jexnom(grpnoe, nogrn1), 'E', jgrn1)
!
! -- AUTRES ORIENTATION DE FISSURE : TRI DE Z<0 A Z>0
!
        do 130 j = 1, nbnog-1
!
            zmax = r8maem()
            do 120 k = j, nbnog
                z = zr(icoor+3*(zi(jgrn1+k-1)-1)+2)
                if (z .lt. zmax) then
                    kmax = k
                    zmax = z
                endif
120          continue
            ibid = zi(jgrn1+j-1)
            zi(jgrn1+j-1) = zi(jgrn1+kmax-1)
            zi(jgrn1+kmax-1) = ibid
!
130      continue
!
        zinf = zr(icoor+3*(zi(jgrn1+1-1)-1)+2)
        zsup = zr(icoor+3*(zi(jgrn1+nbnog-1)-1)+2)
!
        if (abs(zsup-zinf) .lt. 1.d-5) then
!
! -- FISSURE CIRCONFERENTIELLE (90 DEGRE) : TRI DE Y<0 A Y>0
!
            do 140 j = 1, nbnog-1
!
                ymax = pi*rm
                do 150 k = j, nbnog
                    y = zr(icoor+3*(zi(jgrn1+k-1)-1)+1)
                    if (y .lt. ymax) then
                        kmax = k
                        ymax = y
                    endif
150              continue
                ibid = zi(jgrn1+j-1)
                zi(jgrn1+j-1) = zi(jgrn1+kmax-1)
                zi(jgrn1+kmax-1) = ibid
!
140          continue
!
        endif
!
    endif
!
!  -- CALCUL DE LA POSITION DU CHAMP THETA A L'ORIGINE ET A
!     L'EXTREMITE DU FOND DE FISSURE
!
    nogrn3 = 'THOR'
    nogrn4 = 'THEX'
    nogrn5 = 'MED1'
    call jeexin(jexnom(grpnoe, nogrn3), iret3)
    call jeexin(jexnom(grpnoe, nogrn4), iret4)
    call jeexin(jexnom(grpnoe, nogrn5), iret5)
!
    if (iret3 .ne. 0 .and. iret4 .ne. 0) then
!
        call jeveuo(jexnom(grpnoe, nogrn3), 'L', jgrn3)
        call jeveuo(jexnom(grpnoe, nogrn4), 'L', jgrn4)
!
        if (iret5 .ne. 0) then
!
            xor = zr(icoor+3*(zi(jgrn1)-1))
            yor = zr(icoor+3*(zi(jgrn1)-1)+1)
            zor = zr(icoor+3*(zi(jgrn1)-1)+2)
            xex = zr(icoor+3*(zi(jgrn1+nbnog-1)-1))
            yex = zr(icoor+3*(zi(jgrn1+nbnog-1)-1)+1)
            zex = zr(icoor+3*(zi(jgrn1+nbnog-1)-1)+2)
!
            rnorm = sqrt( (xex-xor)**2 + (yex-yor)**2 + (zex-zor)**2 )
!
            zr(icoor+3*(zi(jgrn3)-1)) = xor + (xor - xex)/rnorm
            zr(icoor+3*(zi(jgrn3)-1)+1)= yor + (yor - yex)/rnorm
            zr(icoor+3*(zi(jgrn3)-1)+2)= zor + (zor - zex)/rnorm
!
            zr(icoor+3*(zi(jgrn4)-1)) = xex + (xex - xor)/rnorm
            zr(icoor+3*(zi(jgrn4)-1)+1)= yex + (yex - yor)/rnorm
            zr(icoor+3*(zi(jgrn4)-1)+2)= zex + (zex - zor)/rnorm
!
        else
!
            call jelira(jexnom(grpnoe, 'P_AXE_2'), 'LONUTI', nbnog, k8b)
            call jeveuo(jexnom(grpnoe, 'P_AXE_2'), 'L', jgrn1)
!
            xor = zr(icoor+3*(zi(jgrn1)-1))
            yor = zr(icoor+3*(zi(jgrn1)-1)+1)
            zor = zr(icoor+3*(zi(jgrn1)-1)+2)
            xex = zr(icoor+3*(zi(jgrn1+nbnog-1)-1))
            yex = zr(icoor+3*(zi(jgrn1+nbnog-1)-1)+1)
            zex = zr(icoor+3*(zi(jgrn1+nbnog-1)-1)+2)
!
            rnorm = sqrt( (xex-xor)**2 + (yex-yor)**2 + (zex-zor)**2 )
!
            zr(icoor+3*(zi(jgrn3)-1)) = xor + (xor - xex)/rnorm
            zr(icoor+3*(zi(jgrn3)-1)+1)= yor + (yor - yex)/rnorm
            zr(icoor+3*(zi(jgrn3)-1)+2)= zor + (zor - zex)/rnorm
!
            call jelira(jexnom(grpnoe, 'G_AXE_2'), 'LONUTI', nbnog, k8b)
            call jeveuo(jexnom(grpnoe, 'G_AXE_2'), 'L', jgrn1)
!
            xor = zr(icoor+3*(zi(jgrn1)-1))
            yor = zr(icoor+3*(zi(jgrn1)-1)+1)
            zor = zr(icoor+3*(zi(jgrn1)-1)+2)
            xex = zr(icoor+3*(zi(jgrn1+nbnog-1)-1))
            yex = zr(icoor+3*(zi(jgrn1+nbnog-1)-1)+1)
            zex = zr(icoor+3*(zi(jgrn1+nbnog-1)-1)+2)
!
            rnorm = sqrt( (xex-xor)**2 + (yex-yor)**2 + (zex-zor)**2 )
!
            zr(icoor+3*(zi(jgrn4)-1)) = xex + (xex - xor)/rnorm
            zr(icoor+3*(zi(jgrn4)-1)+1)= yex + (yex - yor)/rnorm
            zr(icoor+3*(zi(jgrn4)-1)+2)= zex + (zex - zor)/rnorm
!
        endif
!
    endif
!
    call jedema()
!
end subroutine
