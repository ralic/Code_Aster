subroutine rcvad2(fami, kpg, ksp, poum, jmat,&
                  phenom, nbres, nomres, valres, devres,&
                  icodre)
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
    implicit none
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/rcfode.h"
#include "asterfort/rcvarc.h"
#include "asterfort/utmess.h"
    integer :: kpg, ksp, imat, nbres, jmat
    character(len=*) :: fami, poum
    integer :: icodre(nbres)
    character(len=16) :: nomres(nbres)
    character(len=*) :: phenom
    real(kind=8) :: temp, valres(nbres), devres(nbres)
! ......................................................................
!     OBTENTION DE LA VALEUR DES COEFFICIENTS DU MATERIAU ET DE LEURS
!     DERIVEES PAR RAPPORT A LA TEMPERATURE
!
! IN   IMAT   : ADRESSE DU MATERIAU CODE
! IN   PHENOM : PHENOMENE
! IN   TEMP   : TEMPERATURE AU POINT DE GAUSS CONSIDERE
! IN   NBRES  : NOMBRE DES COEFFICIENTS
! IN   NOMRES : NOM DES COEFFICIENTS
!
! OUT  VALRES : VALEURS DES COEFFICIENTS
! OUT  DEVRES : DERIVEE DES COEFFICIENTS
! OUT  ICODRE : POUR CHAQUE RESULTAT, 0 SI ON A TROUVE, 1 SINON
! ......................................................................
!
!
!
!
    integer :: nbobj, nbf, nbr, ivalr, ivalk, ir, idf, ires
    integer :: lmat, lfct, icomp, ipi, ifon, ik, nbmat, iret
    character(len=10) :: phen
!
! ----------------------------------------------------------------------
! PARAMETER ASSOCIE AU MATERIAU CODE
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    parameter  ( lmat = 9 , lfct = 10 )
! DEB ------------------------------------------------------------------
!
!
    phen = phenom
    nbmat=zi(jmat)
!     UTILISABLE SEULEMENT AVEC UN MATERIAU PAR MAILLE
    ASSERT(nbmat.eq.1)
    imat = jmat+zi(jmat+nbmat+1)
!
    do 30 ires = 1, nbres
        icodre(ires) = 1
30  end do
!
    do 40 icomp = 1, zi(imat+1)
        if (phen .eq. zk32(zi(imat)+icomp-1)(1:10)) then
            ipi = zi(imat+2+icomp-1)
            goto 888
        endif
40  end do
    call utmess('F', 'ELEMENTS2_63')
    goto 999
888  continue
!
    nbobj = 0
    nbr = zi(ipi)
    ivalk = zi(ipi+3)
    ivalr = zi(ipi+4)
    do 150 ir = 1, nbr
        do 140 ires = 1, nbres
            if (nomres(ires) .eq. zk16(ivalk+ir-1)) then
                valres(ires) = zr(ivalr-1+ir)
                devres(ires) = 0.d0
                icodre(ires) = 0
                nbobj = nbobj + 1
            endif
140      continue
150  end do
!
    if (nbobj .ne. nbres) then
        idf = zi(ipi)+zi(ipi+1)
        nbf = zi(ipi+2)
        call rcvarc(' ', 'TEMP', poum, fami, kpg,&
                    ksp, temp, iret)
        if (iret .eq. 0) then
            do 170 ires = 1, nbres
                do 160 ik = 1, nbf
                    if (nomres(ires) .eq. zk16(ivalk+idf+ik-1)) then
                        ifon = ipi+lmat-1+lfct*(ik-1)
                        call rcfode(ifon, temp, valres(ires), devres( ires))
                        icodre(ires) = 0
                    endif
160              continue
170          continue
        else
            do 180 ires = 1, nbres
                do 190 ik = 1, nbf
                    if (nomres(ires) .eq. zk16(ivalk+idf+ik-1)) then
                        ifon = ipi+lmat-1+lfct*(ik-1)
                        call rcfode(ifon, 0.d0, valres(ires), devres( ires))
                        icodre(ires) = 0
                    endif
190              continue
180          continue
        endif
    endif
!
999  continue
! FIN ------------------------------------------------------------------
end subroutine
