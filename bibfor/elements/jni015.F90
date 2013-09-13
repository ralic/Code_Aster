subroutine jni015(elrefe, nmaxob, liobj, nbobj)
    implicit none
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
! person_in_charge: jacques.pellet at edf.fr
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/clffch.h"
#include "asterfort/gausch.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/utmess.h"
#include "asterfort/wkvect.h"
    character(len=8) :: elrefe
    integer :: nmaxob, nbobj
    character(len=24) :: liobj(nmaxob)
!
! BUT :  INITIALISATION DES ELEMENTS HOMOGENEISES
!.......................................................................
    character(len=24) :: carac, ff
    character(len=6) :: flui, pou, geom, alias
    parameter     (flui = 'FLUIDE',pou='POUTRE',geom='HEXA8 ')
    integer :: npg1(2, 2), npg2(2, 2), lcarac, nno1, nno2, iret, icarac, npg
    integer :: lff, lfft, n, npgi(3)
    integer :: iff, ifft, ipoids, ivf1, ivf12, ivf2, idpdx1, idpdy1, idpdz1
    integer :: idsdx1, idsdy1, idsdz1, idsxy1, idsxz1, idsyz1, icopg, i, j, ider
    integer :: idpdx2, idpdy2, idpdz2, idsdx2, idsdy2, idsdz2, idsxy2, idsxz2
    integer :: idsyz2
    real(kind=8) :: xin(20), yin(20), zin(20), xg, yg, zg, bid(1)
!
! DEB ------------------------------------------------------------------
!
!
    carac='&INEL.'//elrefe//'.CARAC'
    ff   ='&INEL.'//elrefe//'.FF'
!
!
    nbobj = 2
    ASSERT(nmaxob.gt.nbobj)
    liobj(1) = carac
    liobj(2) = ff
!
!
    lcarac = 10
!
    if (elrefe .eq. 'POHOH20') then
!       1 ERE FAMILLE (DX, DRY)
!       2 EME FAMILLE (PRES)
!       --- NOMBRE DE NOEUDS
        nno1 = 8
        nno2 = 20
        alias = 'HEXA20'
!       --- NOMBRE DE POINTS DE GAUSS ASSOCIES A LA PREMIERE FAMILLE
!       --->       MATRICE DE RAIDEUR : INTEGRATION SELON X ET Y
        npg1(1,1) = 2
!       --->       MATRICE DE RAIDEUR : INTEGRATION SELON Z
        npg1(1,2) = 2
!       --->       MATRICE DE MASSE : INTEGRATION SELON X ET Y
        npg1(2,1) = 2
!       --->       MATRICE DE MASSE : INTEGRATION SELON Z
        npg1(2,2) = 4
!       --- NOMBRE DE POINTS DE GAUSS ASSOCIES A LA DEUXIEME FAMILLE
!       ---        MATRICE DE MASSE : CONTRIBUTION DE P (MATRICE A)
!       --->       INTEGRATION SELON X ET Y
        npg2(1,1) = 3
!       --->       INTEGRATION SELON Z
        npg2(1,2) = 3
!       ---        MATRICE DE MASSE : COUPLAGE FLUIDE-STRUCTURE
!       --->       INTEGRATION SELON X ET Y
        npg2(2,1) = 2
!       --->       INTEGRATION SELON Z
        npg2(2,2) = 3
!
    else if (elrefe.eq.'POHOH8') then
        nno1 = 8
        nno2 = 8
        alias = 'HEXA8'
        npg1(1,1) = 2
        npg1(1,2) = 2
        npg1(2,1) = 2
        npg1(2,2) = 4
        npg2(1,1) = 2
        npg2(1,2) = 2
        npg2(2,1) = 2
        npg2(2,2) = 3
!
    else
        call utmess('F', 'ELEMENTS2_29')
    endif
!
    call jeexin(carac, iret)
    if (iret .ne. 0) goto 9998
!
    call wkvect(carac, 'V V I', lcarac, icarac)
    zi(icarac ) = nno1
    zi(icarac+1) = nno2
    n = 1
    do 1 i = 1, 2
        do 1 j = 1, 2
            n = n+1
            zi(icarac+n)= npg1(i,j)
            zi(icarac+n+4) = npg2(i,j)
 1      continue
! --- PLACE MEMOIRE POUR VALEURS DES FONCTIONS DE FORME ET DE LEURS ---
! ---              DERIVEES AU DIFFERENTS POINTS DE GAUSS           ---
    npg = npg1(1,1)*npg1(1,1)*npg1(1,2)
    lff = npg + 10 * ( npg * 2*nno1 ) + 10 * npg * nno1
    npg = npg1(2,1)*npg1(2,1)*npg1(2,2)
    lff = lff + npg + npg * 2*nno1 + 4 * npg * nno1
    npg = npg2(1,1)*npg2(1,1)*npg2(1,2)
    lff = lff + npg + 4 * npg * nno2 + 4 * npg * nno1
    npg = npg2(2,1)*npg2(2,1)*npg2(2,2)
    lff = lff + npg + npg * 2*nno1 + 4 * npg * nno2 + 4 * npg * nno1
!
    call wkvect(ff, 'V V R', lff, iff)
! --- PLACE MEMOIRE POUR COORDONNEES DES DIFFERENTS POINTS DE GAUSS ---
    lfft = 3*(&
           npg1(1,1)*npg1(1,1)*npg1(1,2) + npg1(2,1)*npg1(2,1)*npg1(2,2) + npg2(1,1)*npg2(1,1)*np&
           &g2(1,2) + npg2(2,1)*npg2(2,1)*npg2(2,2)&
           )
    call wkvect('&&JNI014.FFT', 'V V R', lfft, ifft)
!     --------------------------------------------
!     --- COORDONNEES INTRINSEQUES DE L'HEXA8 ----
!     --------------------------------------------
    do 500 i = 1, 4
        zin(i) = -1.d0
        zin(4+i) = 1.d0
500  end do
    do 510 i = 1, 2
        xin(1+4*(i-1)) = -1.d0
        xin(2+4*(i-1)) = 1.d0
        xin(3+4*(i-1)) = 1.d0
        xin(4+4*(i-1)) = -1.d0
510  end do
    do 520 i = 1, 2
        yin(1+4*(i-1)) = -1.d0
        yin(2+4*(i-1)) = -1.d0
        yin(3+4*(i-1)) = 1.d0
        yin(4+4*(i-1)) = 1.d0
520  end do
!     --------------------------------------------
!     --- COORDONNEES INTRINSEQUES DE L'EXA20 ----
!     --------------------------------------------
    do 530 i = 9, 12
        zin(i) = -1.d0
        zin(4+i) = 0.d0
        zin(8+i) = 1.d0
530  end do
    xin(9) = 0.d0
    xin(10) = 1.d0
    xin(11) = 0.d0
    xin(12) = -1.d0
    xin(13) = -1.d0
    xin(14) = 1.d0
    xin(15) = 1.d0
    xin(16) = -1.d0
    xin(17) = 0.d0
    xin(18) = 1.d0
    xin(19) = 0.d0
    xin(20) = -1.d0
    yin(9) = -1.d0
    yin(10) = 0.d0
    yin(11) = 1.d0
    yin(12) = 0.d0
    yin(13) = -1.d0
    yin(14) = -1.d0
    yin(15) = 1.d0
    yin(16) = 1.d0
    yin(17) = -1.d0
    yin(18) = 0.d0
    yin(19) = 1.d0
    yin(20) = 0.d0
!     --------------------------------------------
!     --- PREMIERE FAMILLE DE POINTS DE GAUSS ----
!     --- MATRICE DE RAIDEUR : POUTRE         ----
!     --------------------------------------------
    npg = npg1(1,1) * npg1(1,1) * npg1(1,2)
    ipoids = iff
    ivf1 = ipoids + npg
    idpdx1 = ivf1 + npg * 2 * nno1
    idpdy1 = idpdx1 + npg * 2 * nno1
    idpdz1 = idpdy1 + npg * 2 * nno1
    idsdx1 = idpdz1 + npg * 2 * nno1
    idsdy1 = idsdx1 + npg * 2 * nno1
    idsdz1 = idsdy1 + npg * 2 * nno1
    idsxy1 = idsdz1 + npg * 2 * nno1
    idsxz1 = idsxy1 + npg * 2 * nno1
    idsyz1 = idsxz1 + npg * 2 * nno1
    ivf2 = idsyz1 + npg * 2 * nno1
    idpdx2 = ivf2 + npg * nno1
    idpdy2 = idpdx2 + npg * nno1
    idpdz2 = idpdy2 + npg * nno1
    idsdx2 = idpdz2 + npg * nno1
    idsdy2 = idsdx2 + npg * nno1
    idsdz2 = idsdy2 + npg * nno1
    idsxy2 = idsdz2 + npg * nno1
    idsxz2 = idsxy2 + npg * nno1
    idsyz2 = idsxz2 + npg * nno1
    icopg = ifft
! --- POIDS ET COORDONNEES DES POINTS DE GAUSS
    npgi(1) = npg1(1,1)
    npgi(2) = npg1(1,1)
    npgi(3) = npg1(1,2)
    call gausch(npgi, zr(icopg), zr(icopg+npg), zr(icopg+npg+npg), zr(ipoids))
! --- FONCTIONS DE FORME ET DERIVEES PREMIERES ET SECONDES
!                 AU POINT DE GAUSS
    do 100 i = 1, npg
        xg = zr(icopg+i-1)
        yg = zr(icopg+i-1+npg)
        zg = zr(icopg+i-1+npg+npg)
! --- POUR LES POUTRES
        ider = 2
        call clffch(alias, pou, nno1, xg, yg,&
                    zg, xin, yin, zin, zr(ivf1 +(i-1)*2*nno1),&
                    zr(idpdx1+(i-1)*2*nno1), zr(idpdy1+(i- 1)*2*nno1), zr(idpdz1+(i-1)*2*nno1),&
                    zr(idsdx1+(i-1)*2*nno1), zr(idsdy1+(i-1)*2*nno1), zr(idsdz1+(i-1)*2*nno1),&
                    zr(idsxy1+( i-1)*2*nno1), zr(idsxz1+(i-1)*2*nno1), zr(idsyz1+(i-1)*2*nno1),&
                    ider)
! --- POUR LA GEOMETRIE
        ider = 2
        call clffch(geom, flui, nno1, xg, yg,&
                    zg, xin, yin, zin, zr(ivf2 +(i-1)*nno1),&
                    zr(idpdx2+(i-1)*nno1), zr(idpdy2+(i-1)* nno1), zr(idpdz2+(i-1)*nno1),&
                    zr(idsdx2+(i-1)*nno1), zr( idsdy2+(i-1)*nno1), zr(idsdz2+(i-1)*nno1),&
                    zr(idsxy2+(i-1)* nno1), zr(idsxz2+(i-1)*nno1), zr(idsyz2+(i-1)*nno1), ider)
!
100  end do
!     --------------------------------------------
!     --- DEUXIEME FAMILLE DE POINTS DE GAUSS ----
!     --- MATRICE DE MASSE : POUTRE           ----
!     --------------------------------------------
    icopg = icopg + 3*npg
    ipoids = idsyz2 + npg * nno1
    npg = npg1(2,1) * npg1(2,1) * npg1(2,2)
    ivf1 = ipoids + npg
    ivf2 = ivf1 + npg * 2 * nno1
    idpdx2 = ivf2 + npg * nno1
    idpdy2 = idpdx2 + npg * nno1
    idpdz2 = idpdy2 + npg * nno1
! --- POIDS ET COORDONNEES DES POINTS DE GAUSS
    npgi(1) = npg1(2,1)
    npgi(2) = npg1(2,1)
    npgi(3) = npg1(2,2)
    call gausch(npgi, zr(icopg), zr(icopg + npg), zr(icopg + npg + npg), zr(ipoids))
! --- FONCTIONS DE FORME ET DERIVEES PREMIERES AU POINT DE GAUSS
    do 200 i = 1, npg
        xg = zr(icopg+i-1)
        yg = zr(icopg+i-1+npg)
        zg = zr(icopg+i-1+npg+npg)
! --- POUR LES POUTRES
        ider = 0
        call clffch(alias, pou, nno1, xg, yg,&
                    zg, xin, yin, zin, zr(ivf1+(i-1)*2*nno1),&
                    bid, bid, bid, bid, bid,&
                    bid, bid, bid, bid, ider)
! --- POUR LA GEOMETRIE
        ider = 1
        call clffch(geom, flui, nno1, xg, yg,&
                    zg, xin, yin, zin, zr(ivf2 +(i-1)*nno1),&
                    zr(idpdx2+(i-1)*nno1), zr(idpdy2+(i-1)* nno1), zr(idpdz2+(i-1)*nno1), bid,&
                    bid, bid, bid, bid, bid,&
                    ider)
!
200  continue
!     --------------------------------------------
!     --- TROISIEME FAMILLE DE POINTS DE GAUSS ---
!     --- MATRICE DE MASSE : FLUIDE / FLUIDE   ---
!     --------------------------------------------
    icopg = icopg + 3*npg
    ipoids = idpdz2 + npg * nno1
    npg = npg2(1,1) * npg2(1,1) * npg2(1,2)
    ivf1 = ipoids + npg
    idpdx1 = ivf1 + npg * nno2
    idpdy1 = idpdx1 + npg * nno2
    idpdz1 = idpdy1 + npg * nno2
    ivf2 = idpdz1 + npg * nno2
    idpdx2 = ivf2 + npg * nno1
    idpdy2 = idpdx2 + npg * nno1
    idpdz2 = idpdy2 + npg * nno1
! --- POIDS ET COORDONNEES DES POINTS DE GAUSS
    npgi(1) = npg2(1,1)
    npgi(2) = npg2(1,1)
    npgi(3) = npg2(1,2)
    call gausch(npgi, zr(icopg), zr(icopg + npg), zr(icopg + npg + npg), zr(ipoids))
! --- FONCTIONS DE FORME ET DERIVEES PREMIERES AU POINT DE GAUSS
    do 300 i = 1, npg
        xg = zr(icopg+i-1)
        yg = zr(icopg+i-1+npg)
        zg = zr(icopg+i-1+npg+npg)
! --- POUR LE FLUIDE
        ider = 1
        call clffch(alias, flui, nno2, xg, yg,&
                    zg, xin, yin, zin, zr(ivf1 +(i-1)*nno2),&
                    zr(idpdx1+(i-1)*nno2), zr(idpdy1+(i-1)* nno2), zr(idpdz1+(i-1)*nno2), bid,&
                    bid, bid, bid, bid, bid,&
                    ider)
! --- POUR LA GEOMETRIE
        ider = 1
        call clffch(geom, flui, nno1, xg, yg,&
                    zg, xin, yin, zin, zr(ivf2 +(i-1)*nno1),&
                    zr(idpdx2+(i-1)*nno1), zr(idpdy2+(i-1)* nno1), zr(idpdz2+(i-1)*nno1), bid,&
                    bid, bid, bid, bid, bid,&
                    ider)
!
300  end do
!     --------------------------------------------
!     --- QUATRIEME FAMILLE DE POINTS DE GAUSS ---
!     --- MATRICE DE MASSE : SOLIDE / FLUIDE  ----
!     --------------------------------------------
    icopg = icopg + 3*npg
    ipoids = idpdz2 + npg * nno1
    npg = npg2(2,1) * npg2(2,1) * npg2(2,2)
    ivf1 = ipoids + npg
    ivf12 = ivf1 + npg * 2 * nno1
    idpdx1 = ivf12 + npg * nno2
    idpdy1 = idpdx1 + npg * nno2
    idpdz1 = idpdy1 + npg * nno2
    ivf2 = idpdz1 + npg * nno2
    idpdx2 = ivf2 + npg * nno1
    idpdy2 = idpdx2 + npg * nno1
    idpdz2 = idpdy2 + npg * nno1
! --- POIDS ET COORDONNEES DES POINTS DE GAUSS
    npgi(1) = npg2(2,1)
    npgi(2) = npg2(2,1)
    npgi(3) = npg2(2,2)
    call gausch(npgi, zr(icopg), zr(icopg + npg), zr(icopg + npg + npg), zr(ipoids))
! --- FONCTIONS DE FORME ET DERIVEES PREMIERES AU POINT DE GAUSS
    do 400 i = 1, npg
        xg = zr(icopg+i-1)
        yg = zr(icopg+i-1+npg)
        zg = zr(icopg+i-1+npg+npg)
! --- POUR LES POUTRES
        ider = 0
        call clffch(alias, pou, nno1, xg, yg,&
                    zg, xin, yin, zin, zr(ivf1+(i-1)*2*nno1),&
                    bid, bid, bid, bid, bid,&
                    bid, bid, bid, bid, ider)
! --- POUR LE FLUIDE
        ider = 1
        call clffch(alias, flui, nno2, xg, yg,&
                    zg, xin, yin, zin, zr(ivf12 +(i-1)*nno2),&
                    zr(idpdx1+(i-1)*nno2), zr(idpdy1+(i-1)* nno2), zr(idpdz1+(i-1)*nno2), bid,&
                    bid, bid, bid, bid, bid,&
                    ider)
! --- POUR LA GEOMETRIE
        ider = 1
        call clffch(geom, flui, nno1, xg, yg,&
                    zg, xin, yin, zin, zr(ivf2 +(i-1)*nno1),&
                    zr(idpdx2+(i-1)*nno1), zr(idpdy2+(i-1)* nno1), zr(idpdz2+(i-1)*nno1), bid,&
                    bid, bid, bid, bid, bid,&
                    ider)
!
400  continue
!
    call jedetr('&&JNI014.FFT')
!
9998  continue
!
!
end subroutine
