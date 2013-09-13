subroutine refthm(fnoevo, dt, perman, nno, nnos,&
                  nnom, npi, npg, ipoids, ipoid2,&
                  ivf, ivf2, idfde, idfde2, geom,&
                  b, dfdi, dfdi2, r, vectu,&
                  imate, mecani, press1, press2, tempe,&
                  dimdef, dimcon, dimuel, nddls, nddlm,&
                  nmec, np1, np2, ndim, axi)
!
! aslint: disable=W1504
    implicit none
#include "asterc/r8miem.h"
#include "asterc/r8vide.h"
#include "asterfort/assert.h"
#include "asterfort/fnothm.h"
#include "asterfort/r8inir.h"
#include "asterfort/terefe.h"
#include "asterfort/utmess.h"
#include "blas/daxpy.h"
    logical :: fnoevo, perman, axi
    integer :: nno, nnos, npi, ipoids, ipoid2, ivf, ivf2, nnom
    integer :: idfde, idfde2, imate, dimdef, dimcon, dimuel, npg
    integer :: mecani(5), press1(7), press2(7), tempe(5)
    integer :: nddls, nddlm
    integer :: nmec, np1, np2, ndim
    real(kind=8) :: geom(ndim, nno), b(dimdef, dimuel), dfdi(nno, 3)
    real(kind=8) :: dfdi2(nnos, 3)
    real(kind=8) :: r(1:dimdef+1), vectu(dimuel), dt
! =====================================================================
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! =====================================================================
    integer :: indice, i, j, k, parsig, partmp, parbsi
    parameter (parsig = 27*36 ,partmp = 27*6 ,parbsi = 27*6 )
    real(kind=8) :: sigtm(parsig), ftemp(partmp), bsigm(parbsi)
    real(kind=8) :: sigref, fh1ref, fh2ref, fthref, contm(4)
! =====================================================================
! --- INITIALISATIONS --------------------------------------------------
! ======================================================================
    call r8inir(dimcon*npi, 0.d0, sigtm(1), 1)
    call r8inir(dimuel, 0.d0, ftemp(1), 1)
! ======================================================================
! --- TESTS DE COHERENCE -----------------------------------------------
! ======================================================================
! --- CES VERIFICATIONS ONT POUR OBJETIF DE CONTROLER LES DIMENSIONS ---
! --- DE NDDL, NNO ET DIMCON QUI DIMENSIONNENT LES VECTEURS SIGTM, -----
! --- FTEMP ET BSIGM ---------------------------------------------------
! ======================================================================
! --- LA DIMENSION MAX DE NDDL = NMEC + NP1 + NP2 + NT = 6 -------------
! --- LA DIMENSION MAX DE NNO CORRESPOND AU NOMBRE MAX DE NOEUD PAR ELT-
! --- LA DIMENSION MAX DE NPGU AU NOMBRE MAX DE PTS DE GAUSS PAR ELT ---
! --- LA DIMENSION MAX DE DIMCON (ROUTINE TE0600) ----------------------
! ======================================================================
    ASSERT(nddls .le. 6)
    ASSERT(nno .le. 27)
    ASSERT(npi .le. 27)
    ASSERT(dimcon .le. 31 + 5)
! ======================================================================
! --- CES VERIFICATIONS ONT POUR OBJECTIF DE CONTROLER LA PRESENCE -----
! --- DES DIFFERENTS PARAMETRES DE REFERENCE ---------------------------
! ======================================================================
    if (mecani(1) .ne. 0) then
        call terefe('SIGM_REFE', 'THM', sigref)
        indice = 1
        contm(indice) = sigref
    endif
    if (press1(1) .ne. 0) then
        call terefe('FLUX_HYD1_REFE', 'THM', fh1ref)
        indice = 2
        contm(indice) = fh1ref
    endif
    if (press2(1) .ne. 0) then
        call terefe('FLUX_HYD2_REFE', 'THM', fh2ref)
        indice = 3
        contm(indice) = fh2ref
    endif
    if (tempe(1) .ne. 0) then
        call terefe('FLUX_THER_REFE', 'THM', fthref)
        indice = 4
        contm(indice) = fthref
    endif
! ======================================================================
! --- TESTS DE COHERENCE -----------------------------------------------
! ======================================================================
    do 200 i = 1, npi
        do 210 j = 1, dimcon
            if (j .le. mecani(5)) then
                indice = 1
            else if (j.le.(mecani(5)+press1(2)*press1(7))) then
                indice = 2
                if (tempe(5) .gt. 0) then
! ======================================================================
! --- ON NE FAIT RIEN DANS LE CAS DE L'ENTHALPIE -----------------------
! ======================================================================
                    if (j .eq. (mecani(5)+press1(7)) .or. j .eq.&
                        (mecani( 5)+press1(2)*press1(7))) then
                        goto 210
                    endif
                endif
                else if ( j.le. (mecani(5)+press1(2)*press1(7)+press2(2)*&
            press2(7)) ) then
                indice = 3
                if (tempe(5) .gt. 0) then
! ======================================================================
! --- ON NE FAIT RIEN DANS LE CAS DE L'ENTHALPIE -----------------------
! ======================================================================
                    if (j .eq. (mecani(5)+ press1(2)*press1(7)+press2( 7)) .or. j .eq.&
                        (mecani(5)+ press1(2)*press1(7)+ press2(2)*press2(7))) then
                        goto 210
                    endif
                endif
            else if (j.le.(mecani(5)+tempe(5))) then
                indice = 4
            endif
!
            if (contm(indice) .eq. r8vide()) goto 210
!
            sigtm(j+dimcon*(i-1)) = contm(indice)
            call fnothm(fnoevo, dt, perman, nno, nnos,&
                        nnom, npi, npg, ipoids, ipoid2,&
                        ivf, ivf2, idfde, idfde2, geom,&
                        sigtm, b, dfdi, dfdi2, r,&
                        bsigm(1), imate, mecani, press1, press2,&
                        tempe, dimdef, dimcon, nddls, nddlm,&
                        dimuel, nmec, np1, np2, ndim,&
                        axi)
!
            do 220 k = 1, dimuel
                ftemp(k) = ftemp(k) + abs(bsigm(k))
220          continue
            sigtm(j+dimcon*(i-1)) = 0.0d0
!
210      continue
200  continue
!
    call daxpy(dimuel, 1.d0/npi, ftemp(1), 1, vectu(1),&
               1)
!
    do 230 k = 1, dimuel
        if (abs(vectu(k)) .lt. r8miem()) then
            call utmess('F', 'ALGORITH10_40')
        endif
230  continue
!
end subroutine
