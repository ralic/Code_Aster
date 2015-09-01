subroutine arlapl(ndim  ,nns ,ndml1   ,ndml2 ,nomte, &
                  npgs,ipoids,ivfs,idfdes)



! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================


    implicit none

#include "jeveux.h"
#include "asterfort/jevech.h"
#include "asterfort/arltem.h"

!     ARGUMENTS:
!     ----------
    integer :: ndim,nns,ndml1,ndml2
    character(len=16) :: nomte
    integer :: npgs,ipoids,ivfs,idfdes

! ----------------------------------------------------------------------
!
! CALCUL DES MATRICES DE COUPLAGE ARLEQUIN (OPTION ARLQ_MATR)
! CREATION DES MATRICES DE COUPLAGE POUR LES ELEMENTS 1D ET 3D
!
! ----------------------------------------------------------------------

! IN  NDIM   : DIMENSION DU PROBLEME
! IN  NNS    : NOMBRE DE NOEUDS DE LA MAILLE SUPPORT
! IN  NDML1  : NOMBRE DE NOEUDS DE LA MAILLE 1
! IN  NDML2  : NOMBRE DE NOEUDS DE LA MAILLE 2
! IN  NOMTE  : NOM DU TYPE_ELEMENT MAILLE SUPPORT S
! IN  NPGS   : NOMBRE DE POINTS DE GAUSS DE LA MAILLE SUPPORT S
! IN  IPOIDS : POINTEUR VERS POIDS DE GAUSS DE LA MAILLE SUPPORT S
! IN  IVFS   : POINTEUR VERS FONCTIONS DE FORME DE LA MAILLE SUPPORT S
! IN  IDFDES : POINTEUR VERS DER. FONCTIONS DE FORME DE LA MAILLE S

    integer          ::  nliai,nddl
    parameter    (nliai=12,nddl=nliai*nliai)
    integer :: jrefe1,jrefe2,jcoor1,jcoor2,jinfor,jcoors
    real(kind=8) ::  mcplc1(2*ndim*ndml2,ndim*ndml1)
    real(kind=8) ::  mcplc2(2*ndim*ndml2,2*ndim*ndml2)
    character(len=8) :: elrf1,elrf2
    integer :: iaux,jaux,ijkl1,ijkl2,imatu1,imatu2

! ----------------------------------------------------------------------

! --- ACCES INFOS MAILLES COUPLEES

    call jevech('PGEOMER','L',jcoors)
    call jevech('PREFE1K','L',jrefe1)
    elrf1 = zk8(jrefe1)
    call jevech('PCOOR1R','L',jcoor1)
    call jevech('PREFE2K','L',jrefe2)
    elrf2 = zk8(jrefe2)
    call jevech('PCOOR2R','L',jcoor2)
    call jevech('PINFORR','L',jinfor)

! --- CALCUL DES TERMES COMPOSANT LES MATRICES DE COUPLAGE

    call arltem(ndim  ,nomte , &
                nns   ,jcoors, &
                npgs  , ivfs  ,idfdes,ipoids, &
                elrf1 , ndml1   ,jcoor1, &
                elrf2 , ndml2   ,jcoor2, &
                mcplc1,mcplc2)

! --- RECOPIE DES TERMES DES MATRICES DE COUPLAGE

    call jevech('PMATUN1','E',imatu1)
    call jevech('PMATUN2','E',imatu2)
    ijkl1 = 0
    ijkl2 = 0
    if (elrf1 == 'H20' .and. elrf2 == 'SE2') then
       do 130 iaux = 1,2*ndim*ndml2
           do 140 jaux = 1,ndim*ndml1
               ijkl1 = ijkl1 + 1
               zr(imatu1-1+ijkl1) = mcplc1(iaux,jaux)
           140 end do
       130 end do
       do 150 iaux = 1,2*ndim*ndml2
           do 160 jaux = 1,2*ndim*ndml2
               ijkl2 = ijkl2 + 1
               zr(imatu2-1+ijkl2) = mcplc2(iaux,jaux)
           160 end do
       150 end do
       zr(imatu2-1+nddl+1) = zr(jinfor+1)
       zr(imatu2-1+nddl+2) = zr(jinfor+2)
       zr(imatu2-1+nddl+3) = zr(jinfor+3)
       zr(imatu2-1+nddl+4) = zr(jinfor+4)
       zr(imatu2-1+nddl+5)  = zr(jcoor1+ndim*ndml1-1+1)
       zr(imatu2-1+nddl+6)  = zr(jcoor1+ndim*ndml1-1+2)
       zr(imatu2-1+nddl+7)  = zr(jcoor1+ndim*ndml1-1+3)
       zr(imatu2-1+nddl+8)  = zr(jcoor1+ndim*ndml1-1+4)
       zr(imatu2-1+nddl+9)  = zr(jcoor1+ndim*ndml1-1+5)
       zr(imatu2-1+nddl+10) = zr(jcoor1+ndim*ndml1-1+6)
       zr(imatu2-1+nddl+11) = zr(jcoor1+ndim*ndml1-1+7)
       zr(imatu2-1+nddl+12) = zr(jcoor1+ndim*ndml1-1+8)
       zr(imatu2-1+nddl+13) = zr(jcoor1+ndim*ndml1-1+9)
       zr(imatu2-1+nddl+14) = zr(jcoor1+ndim*ndml1-1+10)
       zr(imatu2-1+nddl+15) = zr(jcoor1+ndim*ndml1-1+11)
       zr(imatu2-1+nddl+16) = zr(jcoor1+ndim*ndml1-1+12)
       zr(imatu2-1+nddl+17) = zr(jcoor1+ndim*ndml1-1+13)
       zr(imatu2-1+nddl+18) = zr(jcoor1+ndim*ndml1-1+14)
       zr(imatu2-1+nddl+19) = zr(jcoor1+ndim*ndml1-1+15)
       zr(imatu2-1+nddl+20) = zr(jcoor1+ndim*ndml1-1+16)
       zr(imatu2-1+nddl+21) = zr(jcoor1+ndim*ndml1-1+17)
       zr(imatu2-1+nddl+22) = zr(jcoor1+ndim*ndml1-1+18)
       zr(imatu2-1+nddl+23) = zr(jcoor1+ndim*ndml1-1+19)
       zr(imatu2-1+nddl+24) = zr(jcoor1+ndim*ndml1-1+20)
       zr(imatu2-1+nddl+25) = zr(jcoor2+ndim*ndml2-1+1)
       zr(imatu2-1+nddl+26) = zr(jcoor2+ndim*ndml2-1+2)
    elseif (elrf1 == 'HE8' .and. elrf2 == 'SE2') then
       do 131 iaux = 1,2*ndim*ndml2
           do 141 jaux = 1,ndim*ndml1
               ijkl1 = ijkl1 + 1
               zr(imatu1-1+ijkl1) = mcplc1(iaux,jaux)
           141 end do
       131 end do
       do 151 iaux = 1,2*ndim*ndml2
           do 161 jaux = 1,2*ndim*ndml2
               ijkl2 = ijkl2 + 1
               zr(imatu2-1+ijkl2) = mcplc2(iaux,jaux)
           161 end do
       151 end do
       zr(imatu2-1+nddl+1) = zr(jinfor+1)
       zr(imatu2-1+nddl+2) = zr(jinfor+2)
       zr(imatu2-1+nddl+3) = zr(jinfor+3)
       zr(imatu2-1+nddl+4) = zr(jinfor+4)
       zr(imatu2-1+nddl+5)  = zr(jcoor1+ndim*ndml1-1+1)
       zr(imatu2-1+nddl+6)  = zr(jcoor1+ndim*ndml1-1+2)
       zr(imatu2-1+nddl+7)  = zr(jcoor1+ndim*ndml1-1+3)
       zr(imatu2-1+nddl+8)  = zr(jcoor1+ndim*ndml1-1+4)
       zr(imatu2-1+nddl+9)  = zr(jcoor1+ndim*ndml1-1+5)
       zr(imatu2-1+nddl+10) = zr(jcoor1+ndim*ndml1-1+6)
       zr(imatu2-1+nddl+11) = zr(jcoor1+ndim*ndml1-1+7)
       zr(imatu2-1+nddl+12) = zr(jcoor1+ndim*ndml1-1+8)
       zr(imatu2-1+nddl+13) = zr(jcoor2+ndim*ndml2-1+1)
       zr(imatu2-1+nddl+14) = zr(jcoor2+ndim*ndml2-1+2)
    elseif (elrf1 == 'P15' .and. elrf2 == 'SE2') then
       do 132 iaux = 1,2*ndim*ndml2
           do 142 jaux = 1,ndim*ndml1
               ijkl1 = ijkl1 + 1
               zr(imatu1-1+ijkl1) = mcplc1(iaux,jaux)
           142 end do
       132 end do
       do 152 iaux = 1,2*ndim*ndml2
           do 162 jaux = 1,2*ndim*ndml2
               ijkl2 = ijkl2 + 1
               zr(imatu2-1+ijkl2) = mcplc2(iaux,jaux)
           162 end do
       152 end do
       zr(imatu2-1+nddl+1) = zr(jinfor+1)
       zr(imatu2-1+nddl+2) = zr(jinfor+2)
       zr(imatu2-1+nddl+3) = zr(jinfor+3)
       zr(imatu2-1+nddl+4) = zr(jinfor+4)
       zr(imatu2-1+nddl+5)  = zr(jcoor1+ndim*ndml1-1+1)
       zr(imatu2-1+nddl+6)  = zr(jcoor1+ndim*ndml1-1+2)
       zr(imatu2-1+nddl+7)  = zr(jcoor1+ndim*ndml1-1+3)
       zr(imatu2-1+nddl+8)  = zr(jcoor1+ndim*ndml1-1+4)
       zr(imatu2-1+nddl+9)  = zr(jcoor1+ndim*ndml1-1+5)
       zr(imatu2-1+nddl+10) = zr(jcoor1+ndim*ndml1-1+6)
       zr(imatu2-1+nddl+11) = zr(jcoor1+ndim*ndml1-1+7)
       zr(imatu2-1+nddl+12) = zr(jcoor1+ndim*ndml1-1+8)
       zr(imatu2-1+nddl+13) = zr(jcoor1+ndim*ndml1-1+9)
       zr(imatu2-1+nddl+14) = zr(jcoor1+ndim*ndml1-1+10)
       zr(imatu2-1+nddl+15) = zr(jcoor1+ndim*ndml1-1+11)
       zr(imatu2-1+nddl+16) = zr(jcoor1+ndim*ndml1-1+12)
       zr(imatu2-1+nddl+17) = zr(jcoor1+ndim*ndml1-1+13)
       zr(imatu2-1+nddl+18) = zr(jcoor1+ndim*ndml1-1+14)
       zr(imatu2-1+nddl+19) = zr(jcoor1+ndim*ndml1-1+15)
       zr(imatu2-1+nddl+20) = zr(jcoor2+ndim*ndml2-1+1)
       zr(imatu2-1+nddl+21) = zr(jcoor2+ndim*ndml2-1+2)
    elseif (elrf1 == 'PE6' .and. elrf2 == 'SE2') then
       do 133 iaux = 1,2*ndim*ndml2
           do 143 jaux = 1,ndim*ndml1
               ijkl1 = ijkl1 + 1
               zr(imatu1-1+ijkl1) = mcplc1(iaux,jaux)
           143 end do
       133 end do
       do 153 iaux = 1,2*ndim*ndml2
           do 163 jaux = 1,2*ndim*ndml2
               ijkl2 = ijkl2 + 1
               zr(imatu2-1+ijkl2) = mcplc2(iaux,jaux)
           163 end do
       153 end do
       zr(imatu2-1+nddl+1) = zr(jinfor+1)
       zr(imatu2-1+nddl+2) = zr(jinfor+2)
       zr(imatu2-1+nddl+3) = zr(jinfor+3)
       zr(imatu2-1+nddl+4) = zr(jinfor+4)
       zr(imatu2-1+nddl+5)  = zr(jcoor1+ndim*ndml1-1+1)
       zr(imatu2-1+nddl+6)  = zr(jcoor1+ndim*ndml1-1+2)
       zr(imatu2-1+nddl+7)  = zr(jcoor1+ndim*ndml1-1+3)
       zr(imatu2-1+nddl+8)  = zr(jcoor1+ndim*ndml1-1+4)
       zr(imatu2-1+nddl+9)  = zr(jcoor1+ndim*ndml1-1+5)
       zr(imatu2-1+nddl+10) = zr(jcoor1+ndim*ndml1-1+6)
       zr(imatu2-1+nddl+11) = zr(jcoor2+ndim*ndml2-1+1)
       zr(imatu2-1+nddl+12) = zr(jcoor2+ndim*ndml2-1+2)
    elseif (elrf1 == 'T10' .and. elrf2 == 'SE2') then
       do 134 iaux = 1,2*ndim*ndml2
           do 144 jaux = 1,ndim*ndml1
               ijkl1 = ijkl1 + 1
               zr(imatu1-1+ijkl1) = mcplc1(iaux,jaux)
           144 end do
       134 end do
       do 154 iaux = 1,2*ndim*ndml2
           do 164 jaux = 1,2*ndim*ndml2
               ijkl2 = ijkl2 + 1
               zr(imatu2-1+ijkl2) = mcplc2(iaux,jaux)
           164 end do
       154 end do
       zr(imatu2-1+nddl+1) = zr(jinfor+1)
       zr(imatu2-1+nddl+2) = zr(jinfor+2)
       zr(imatu2-1+nddl+3) = zr(jinfor+3)
       zr(imatu2-1+nddl+4) = zr(jinfor+4)
       zr(imatu2-1+nddl+5)  = zr(jcoor1+ndim*ndml1-1+1)
       zr(imatu2-1+nddl+6)  = zr(jcoor1+ndim*ndml1-1+2)
       zr(imatu2-1+nddl+7)  = zr(jcoor1+ndim*ndml1-1+3)
       zr(imatu2-1+nddl+8)  = zr(jcoor1+ndim*ndml1-1+4)
       zr(imatu2-1+nddl+9)  = zr(jcoor1+ndim*ndml1-1+5)
       zr(imatu2-1+nddl+10) = zr(jcoor1+ndim*ndml1-1+6)
       zr(imatu2-1+nddl+11) = zr(jcoor1+ndim*ndml1-1+7)
       zr(imatu2-1+nddl+12) = zr(jcoor1+ndim*ndml1-1+8)
       zr(imatu2-1+nddl+13) = zr(jcoor1+ndim*ndml1-1+9)
       zr(imatu2-1+nddl+14) = zr(jcoor1+ndim*ndml1-1+10)
       zr(imatu2-1+nddl+15) = zr(jcoor2+ndim*ndml2-1+1)
       zr(imatu2-1+nddl+16) = zr(jcoor2+ndim*ndml2-1+2)
    elseif (elrf1 == 'TE4' .and. elrf2 == 'SE2') then
       do 135 iaux = 1,2*ndim*ndml2
           do 145 jaux = 1,ndim*ndml1
               ijkl1 = ijkl1 + 1
               zr(imatu1-1+ijkl1) = mcplc1(iaux,jaux)
           145 end do
       135 end do
       do 155 iaux = 1,2*ndim*ndml2
           do 165 jaux = 1,2*ndim*ndml2
               ijkl2 = ijkl2 + 1
               zr(imatu2-1+ijkl2) = mcplc2(iaux,jaux)
           165 end do
       155 end do
       zr(imatu2-1+nddl+1) = zr(jinfor+1)
       zr(imatu2-1+nddl+2) = zr(jinfor+2)
       zr(imatu2-1+nddl+3) = zr(jinfor+3)
       zr(imatu2-1+nddl+4) = zr(jinfor+4)
       zr(imatu2-1+nddl+5)  = zr(jcoor1+ndim*ndml1-1+1)
       zr(imatu2-1+nddl+6)  = zr(jcoor1+ndim*ndml1-1+2)
       zr(imatu2-1+nddl+7)  = zr(jcoor1+ndim*ndml1-1+3)
       zr(imatu2-1+nddl+8)  = zr(jcoor1+ndim*ndml1-1+4)
       zr(imatu2-1+nddl+9)  = zr(jcoor2+ndim*ndml2-1+1)
       zr(imatu2-1+nddl+10) = zr(jcoor2+ndim*ndml2-1+2)
    endif

end subroutine
