subroutine te0354(option, nomte)
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
    implicit none
#include "jeveux.h"
#include "asterfort/dfdm2d.h"
#include "asterfort/dfdm3d.h"
#include "asterfort/elref1.h"
#include "asterfort/elref4.h"
#include "asterfort/foderi.h"
#include "asterfort/fointe.h"
#include "asterfort/jevech.h"
#include "asterfort/lteatt.h"
#include "blas/daxpy.h"
#include "blas/ddot.h"
    character(len=16) :: option, nomte
!
! ----------------------------------------------------------------------
!                   SOURCE THERMIQUE NON LINEAIRE
!
!    - FONCTION REALISEE:  CALCUL DES MATRICES ELEMENTAIRES
!                          OPTION : 'CHAR_THER_SOURNL'
!                                   'RESI_THER_SOURNL'
!                                   'MTAN_THER_SOURNL'
!
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ......................................................................
!
    integer :: nnomax
    parameter (nnomax=27)
!
    logical :: axi, resi
!      INTEGER NDIM,NNO,NPG,NNOS,G,I,OS,OSM,M,N,IW,IVF,IDFDE,IRET,JGANO
    integer :: ndim, nno, npg, nnos, g, os, osm, m, n, iw, ivf, idfde, iret
    integer :: jgano
    integer :: igeom, itemps, ivect, imatr, isour, ither
    real(kind=8) :: dfdx(nnomax), dfdy(nnomax), dfdz(nnomax), w, rg, theta, sour
    real(kind=8) :: tg
    real(kind=8) :: dsdt, coef, coefop
!      CHARACTER*8 ELREFE,FCT
    character(len=8) :: elrefe
!
!
!    LECTURE DES PARAMETRES COMMUNS
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PTEMPSR', 'L', itemps)
    call jevech('PSOURNL', 'L', isour)
    if (zk8(isour)(1:7) .eq. '&FOZERO') goto 9999
    theta = zr(itemps+2)
!
!    LECTURE DES PARAMETRES SPECIFIQUES A CHAQUE OPTION
    if (option(1:4) .eq. 'CHAR') then
        resi = .true.
        coefop = 1-theta
        call jevech('PTEMPER', 'L', ither)
        call jevech('PVECTTR', 'E', ivect)
    else if (option(1:4).eq.'RESI') then
        resi = .true.
        coefop = -theta
        call jevech('PTEMPEI', 'L', ither)
        call jevech('PRESIDU', 'E', ivect)
    else
        resi = .false.
        coefop = -theta
        call jevech('PTEMPEI', 'L', ither)
        call jevech('PMATTTR', 'E', imatr)
    endif
!
!    ACCES AUX CARACTERISTIQUES DE L'ELEMENT FINI
    call elref1(elrefe)
    call elref4(elrefe, 'RIGI', ndim, nno, nnos,&
                npg, iw, ivf, idfde, jgano)
    axi = lteatt(' ','AXIS','OUI')
!
    do 100 g = 1, npg
        os = (g-1)*nno
!
!      CALCUL DU POIDS DU POINT DE GAUSS
        if (ndim .eq. 2) then
            call dfdm2d(nno, g, iw, idfde, zr(igeom),&
                        dfdx, dfdy, w)
            if (axi) then
                rg = ddot(nno,zr(igeom),2,zr(ivf+os),1)
                w = w*rg
            endif
        else
            call dfdm3d(nno, g, iw, idfde, zr(igeom),&
                        dfdx, dfdy, dfdz, w)
        endif
!
!      CALCUL DE LA TEMPERATURE AU POINT DE GAUSS
        tg = ddot(nno,zr(ither),1,zr(ivf+os),1)
!
!      CALCUL DU RESIDU
        if (resi) then
!
!        CALCUL DE LA SOURCE
            call fointe('FM', zk8(isour), 1, 'TEMP', tg,&
                        sour, iret)
            coef = w*sour*coefop
!
!        CONTRIBUTION AU RESIDU
            call daxpy(nno, coef, zr(ivf+os), 1, zr(ivect),&
                       1)
!
!      CALCUL DE LA MATRICE TANGENTE (STOCKAGE SYMETRIQUE)
        else
!
!        CALCUL DE LA DERIVEE DE LA SOURCE PAR RAPPORT A LA TEMPERATURE
            call foderi(zk8(isour), tg, sour, dsdt)
            coef = w*dsdt*coefop
!
!        CONTRIBUTION A LA MATRICE
            osm = 0
            do 110 n = 0, nno-1
                do 115 m = 0, n
                    zr(imatr+osm)=zr(imatr+osm)+coef*zr(ivf+os+n)*zr(&
                    ivf+os+m)
                    osm = osm + 1
115              continue
110          continue
        endif
100  end do
!
9999  continue
end subroutine
