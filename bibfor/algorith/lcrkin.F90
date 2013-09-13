subroutine lcrkin(ndim, opt, comp, materf, nbcomm,&
                  cpmono, nmat, mod, nvi, sigd,&
                  sigf, vind, vinf, nbphas, iret)
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
!     ----------------------------------------------------------------
!     INITIALISATIONS POUR RUNGE-KUTTA
!     ----------------------------------------------------------------
!     IN
!          NDIM   :  2 OU 3
!          OPT    :  OPTION DE CALCUL : RIGI_MECA, FULL_MECA, RAPH_MECA
!          COMP   :  NOM MODELE DE COMPORTEMENT
!          MATERF :  COEF MATERIAU
!          NBCOMM :  INDICES DES COEF MATERIAU
!          NMAT   :  DIMENSION MATER
!          MOD    :  TYPE DE MODELISATION
!          NVI    :  NOMBRE DE VARIABLES INTERNES
!          VIND   :  VARIABLES INTERNES A T
!          SIGD   :  CONTRAINTES A T
!     VAR  NVI    :  NOMBRE DE VARIABLES INTERNES
!          SIGF   :  CONTRAINTES A T+DT
!          VINF   :  VARIABLES INTERNES A T+DT
!     OUT  IRET   :  CODE RETOUR
!     ----------------------------------------------------------------
#include "asterfort/assert.h"
#include "asterfort/lcinma.h"
#include "asterfort/lcopli.h"
#include "asterfort/utmess.h"
#include "blas/daxpy.h"
#include "blas/dcopy.h"
    integer :: ndt, nvi, nmat, ndi, nbcomm(nmat, 3), icp, ndim, iret, ifl
    integer :: indfa, nuecou
    integer :: nbphas, ifa, indpha, iphas, nbsys, nsfv, nbfsys
    real(kind=8) :: materf(nmat, 2), vind(*), vinf(*), id(3, 3), sigd(*)
    real(kind=8) :: sigf(*)
    real(kind=8) :: dsde(6, 6), maxdom, endoc, fp(3, 3)
    character(len=16) :: loi, comp(*), opt, necoul
    character(len=24) :: cpmono(5*nmat+1)
    character(len=8) :: mod
    common/tdim/ ndt,ndi
    integer :: irr, decirr, nbsyst, decal, gdef
    common/polycr/irr,decirr,nbsyst,decal,gdef
    parameter (maxdom=0.99d0)
    data id/1.d0,0.d0,0.d0, 0.d0,1.d0,0.d0, 0.d0,0.d0,1.d0/
!     ----------------------------------------------------------------
!
    loi = comp(1)
    iret=0
    call lcinma(0.d0, dsde)
!
    if (materf(nmat,1) .eq. 0) then
        call lcopli('ISOTROPE', mod, materf(1, 1), dsde)
    else if (materf(nmat,1).eq.1) then
        call lcopli('ORTHOTRO', mod, materf(1, 1), dsde)
    endif
!
! --    DEBUT TRAITEMENT DE VENDOCHAB --
!     ROUTINE DE DECROISSANCE DES CONTRAINTES QUAND D>MAXDOM
    if (loi(1:9) .eq. 'VENDOCHAB') then
!
        if (opt .eq. 'RIGI_MECA_TANG') then
            call utmess('F', 'ALGORITH8_91')
        endif
        if (vind(9) .ge. maxdom) then
!
            if (vind(9) .eq. 1.0d0) then
                do 4 icp = 1, 2*ndim
                    sigf(icp)=sigd(icp)*(0.01d0)
 4              continue
                materf(1,1)=0.01d0*materf(1,1)
                call lcopli('ISOTROPE', mod, materf(1, 1), dsde)
            else
                do 5 icp = 1, 2*ndim
                    sigf(icp)=sigd(icp)*(0.1d0)
 5              continue
                endoc=(1.0d0-max(maxdom,vind(9)))*0.1d0
                materf(1,1)=endoc*materf(1,1)
                call lcopli('ISOTROPE', mod, materf(1, 1), dsde)
                materf(1,1)=materf(1,1)/endoc
            endif
            do 6 icp = 1, nvi
                vinf(icp)=vind(icp)
 6          continue
            vinf(9)=1.0d0
            iret=9
            goto 9999
        endif
    endif
! --  FIN   TRAITEMENT DE VENDOCHAB --
!
    call dcopy(nvi, vind, 1, vinf, 1)
!
    if (loi(1:9) .eq. 'VENDOCHAB') then
!        INITIALISATION DE VINF(8) A UNE VALEUR NON NULLE
!        POUR EVITER LES 1/0 DANS RKDVEC
        if (vinf(8) .le. (1.0d-8)) then
            vinf(8)=1.0d-8
        endif
    endif
!
!     COMPTAGE
    irr=0
    decirr=0
    nbsyst=0
    decal=0
    if (loi(1:8) .eq. 'MONOCRIS') then
        if (gdef .eq. 1) then
            if (opt .ne. 'RAPH_MECA') then
                call utmess('F', 'ALGORITH8_91')
            endif
            call dcopy(9, vind(nvi-3-18+1), 1, fp, 1)
            call daxpy(9, 1.d0, id, 1, fp,&
                       1)
            call dcopy(9, fp, 1, vinf(nvi-3-18+1), 1)
            nvi=nvi-9
        endif
        if (materf(nbcomm(1,1),2) .ge. 4) then
!           UNE SEULE FAMILLE
            ASSERT(nbcomm(nmat, 2).eq.1)
            necoul=cpmono(3)
            if (necoul .eq. 'MONO_DD_CC_IRRA') then
                irr=1
                decirr=6+3*12
            endif
            if (necoul .eq. 'MONO_DD_CFC_IRRA') then
                irr=1
                decirr=6+3*12
            endif
        endif
        nvi = nvi-3
!        NE PAS DIMINUER NVI DAVANTAGE A CAUSE DES GDEF
    endif
!      POUR POLYCRISTAL
!     INITIALISATION DE NBPHAS
    nbphas=nbcomm(1,1)
    if (loi(1:8) .eq. 'POLYCRIS') then
!        RECUPERATION DU NOMBRE DE PHASES
        nbphas=nbcomm(1,1)
        nsfv=7+6*nbphas
        do 33 iphas = 1, nbphas
            indpha=nbcomm(1+iphas,1)
            nbfsys=nbcomm(indpha,1)
            do 32 ifa = 1, nbfsys
!              indice de la famille IFA
                indfa=indpha+ifa
                ifl=nbcomm(indfa,1)
                nuecou=nint(materf(ifl,2))
!              IRRADIATION
                if (nuecou .eq. 7) then
                    if (nint(materf(ifl+21,2)) .eq. 1) then
                        irr=1
                    endif
                endif
                nbsys=12
                nsfv=nsfv+nbsys*3
32          continue
33      continue
        decirr=nsfv
    endif
!
!
9999  continue
end subroutine
