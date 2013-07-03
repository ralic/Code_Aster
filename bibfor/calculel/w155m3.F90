subroutine w155m3(numa, jce2d, jce2l, jce2v, isp,&
                  nucou, nusec, nufib, posic, posis)
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
! ======================================================================
    implicit none
#include "jeveux.h"
!
#include "asterfort/assert.h"
#include "asterfort/cesexi.h"
    integer :: numa, nucou, nusec, nufib, posic, posis, isp
    integer :: jce2l, jce2d, jce2v
    integer :: nbcou, nbsec, nbfib, isec, icou
    integer :: iad1, iad2, iad3, iad4
    character(len=8) :: typma
!
! ----------------------------------------------------------------------
! BUT : DETERMINER NUCOU, NUSEC, NUFIB, POSIC ET POSIS
!       A PARTIR DU NUMERO DE SOUS-POINT ISP
! ----------------------------------------------------------------------
!
!
!     -- ON DETERMINE SI LA MAILLE EST DE TYPE COQUE, PMF, GRILLE
!        OU TUYAU :
!       -- CMP1 = COQ_NCOU
    call cesexi('C', jce2d, jce2l, numa, 1,&
                1, 1, iad1)
!       -- CMP2 = TUY_NCOU
    call cesexi('C', jce2d, jce2l, numa, 1,&
                1, 2, iad2)
!       -- CMP3 = TUY_NSEC
    call cesexi('C', jce2d, jce2l, numa, 1,&
                1, 3, iad3)
!       -- CMP4 = NBFIBR
    call cesexi('C', jce2d, jce2l, numa, 1,&
                1, 4, iad4)
!
!
    if (iad4 .gt. 0) then
        typma='PMF'
        nbfib=zi(jce2v-1+iad4)
    else if (iad2.gt.0) then
        typma='TUY'
        call assert(iad3.gt.0)
        nbcou=zi(jce2v-1+iad2)
        nbsec=zi(jce2v-1+iad3)
    else if (iad1.gt.0) then
        typma='COQ'
        nbcou=zi(jce2v-1+iad1)
    else
        call assert(.false.)
    endif
!
    nucou=-999
    nusec=-999
    nufib=-999
    posic=-999
    posis=-999
!
!
    if (typma .eq. 'GRI') then
!       -------------------------
        call assert(isp.le.nbcou)
        nucou=isp
!
    else if (typma.eq.'PMF') then
!       -------------------------
        call assert(isp.le.nbfib)
        nufib=isp
!
    else if (typma.eq.'COQ') then
!       -------------------------
        call assert(isp.le.nbcou*3)
        nucou=(isp+2)/3
        posic=mod(isp+1,3)-1
!
    else if (typma.eq.'TUY') then
!       -------------------------
        call assert(isp.le.(2*nbcou+1)*(2*nbsec+1))
        icou=(isp-1)/(2*nbsec+1)+1
        isec=isp-(icou-1)*(2*nbsec+1)
        if (icou .eq. 1) then
            nucou=1
            posic=-1
        else
            nucou=icou/2
            posic=icou-2*nucou
        endif
        if (isec .eq. 1) then
            nusec=1
            posis=-1
        else
            nusec=isec/2
            posis=isec-2*nusec
        endif
!
    else
        call assert(.false.)
    endif
!
end subroutine
