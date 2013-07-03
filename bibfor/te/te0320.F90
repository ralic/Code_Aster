subroutine te0320(option, nomte)
    implicit   none
#include "jeveux.h"
!
#include "asterc/r8vide.h"
#include "asterfort/elref4.h"
#include "asterfort/jevech.h"
#include "asterfort/rcvalb.h"
#include "asterfort/u2mess.h"
    character(len=16) :: option, nomte
! ......................................................................
! ======================================================================
! COPYRIGHT (C) 2007 - 2013  EDF R&D                WWW.CODE-ASTER.ORG
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
!    - FONCTION REALISEE: INITIALISATION DU CALCUL DE Z EN 2D ET AXI
!                         OPTION :'META_INIT_ELNO'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
!
!
    character(len=24) :: nomres
    character(len=16) :: compor(3)
    character(len=8) :: fami, poum
    integer :: icodre
    real(kind=8) :: zero, metapg(63), ms0, zalpha, zbeta
    real(kind=8) :: tno0
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfde, jgano
    integer :: icompo, j, kn, kpg, spt
    integer :: imate, itempe, iphasi, iphasn, nval
!     -----------------------------------------------------------------
!
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
!     PARAMETRES EN ENTREE
!    ---------------------
    call jevech('PMATERC', 'L', imate)
    call jevech('PCOMPOR', 'L', icompo)
    call jevech('PTEMPER', 'L', itempe)
    call jevech('PPHASIN', 'L', iphasi)
!
    compor(1)=zk16(icompo)
!
!     PARAMETRES EN SORTIE
!    ----------------------
    call jevech('PPHASNOU', 'E', iphasn)
!
    zero=0.d0
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
!     -- ON VERIFIE QUE LES VALEURS INITIALES SONT BIEN INITIALISEES:
    if (compor(1) .eq. 'ACIER') then
        nval=5
        do 10, j=1,nval
        if (zr(iphasi-1+j) .eq. r8vide()) call u2mess('F', 'ELEMENTS5_44')
10      continue
    else if (compor(1).eq.'ZIRC') then
        if (zr(iphasi-1+1) .eq. r8vide()) call u2mess('F', 'ELEMENTS5_44')
        if (zr(iphasi-1+2) .eq. r8vide()) call u2mess('F', 'ELEMENTS5_44')
        if (zr(iphasi-1+4) .eq. r8vide()) call u2mess('F', 'ELEMENTS5_44')
    endif
!
!
    if (compor(1) .eq. 'ACIER') then
!        MATERIAU FERRITIQUE
!        ---------------------
!     ON RECALCULE DIRECTEMENT A PARTIR DES TEMPERATURES AUX NOEUDS
        nomres = 'MS0'
        call rcvalb(fami, kpg, spt, poum, zi(imate),&
                    ' ', 'META_ACIER', 1, 'INST', 0.d0,&
                    1, nomres, ms0, icodre, 1)
        tno0 = zero
        do 101 kn = 1, nno
            tno0 = zr(itempe+kn-1)
!
            do 201 j = 0, 4
                metapg(1+7*(kn-1)+j)=zr(iphasi+j)
201          continue
!
            metapg(1+7*(kn-1)+6)=ms0
            metapg(1+7*(kn-1)+5)=tno0
!
            do 86 j = 1, 7
                zr(iphasn+7*(kn-1)+j-1) = metapg(1+7*(kn-1)+j-1)
86          continue
101      continue
!
!
    else if (compor(1)(1:4) .eq. 'ZIRC') then
!
        do 102 kn = 1, nno
!
            tno0 = zr(itempe+kn-1)
!
! ----------PROPORTION TOTALE DE LA PHASE ALPHA
!
            metapg(1+4* (kn-1)) = zr(iphasi-1+1)
            metapg(1+4* (kn-1)+1) = zr(iphasi-1+2)
            metapg(1+4* (kn-1)+2) = tno0
            metapg(1+4* (kn-1)+3) = zr(iphasi-1+4)
!
            zalpha = metapg(1+4* (kn-1)+1) + metapg(1+4* (kn-1))
!
!-----------DECOMPOSITION DE LA PHASE ALPHA POUR LA MECANIQUE
!
            zbeta=1-zalpha
            if (zbeta .gt. 0.1d0) then
                metapg(1+4*(kn-1)) =0.d0
            else
                metapg(1+4*(kn-1))= 10.d0*(zalpha-0.9d0)*zalpha
            endif
            metapg(1+4* (kn-1)+1) = zalpha - metapg(1+4* (kn-1))
!
            do 87 j = 1, 4
                zr(iphasn+4*(kn-1)+j-1) = metapg(1+4*(kn-1)+j-1)
87          continue
!
102      continue
!
    endif
!
!
end subroutine
