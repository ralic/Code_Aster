subroutine te0401(optioz, nomtz)
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
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/bsthco.h"
#include "asterfort/jevech.h"
#include "asterfort/jevete.h"
#include "asterfort/matpgl.h"
#include "asterfort/r8inir.h"
#include "asterfort/tranlg.h"
#include "asterfort/utvtsv.h"
#include "asterfort/vdxrig.h"
    character(len=*) :: optioz, nomtz
    character(len=16) :: option, nomte
!     ----------------------------------------------------------------
!     CALCUL DES OPTIONS DES ELEMENTS DE COQUE : COQUE_3D
!     ----------------------------------------------------------------
!
    integer :: nb1, nb2, nddlet
    integer :: lzr
    integer :: jgeom, jener
    integer :: i, j, kompt
    integer :: iu, imatuu
    real(kind=8) :: matloc(51, 51), plg(9, 3, 3)
    real(kind=8) :: vrs (1326)
    real(kind=8) :: bsigth(51), enerth
    logical :: indith
! DEB
!
    option = optioz
    nomte = nomtz
!
    enerth = 0.0d0
!
    call jevech('PGEOMER', 'L', jgeom)
!
    if (option .eq. 'RIGI_MECA') call jevech('PMATUUR', 'E', imatuu)
!
    if (option .eq. 'RIGI_MECA' .or. option .eq. 'EPOT_ELEM') then
!
        call vdxrig(nomte, zr(jgeom), matloc, nb1, 0,&
                    0)
!
!     CONSTRUCTION DE LA MATRICE DE PASSAGE REPERE GLOBAL REPERE LOCAL
!
        call jevete('&INEL.'//nomte(1:8)//'.DESR', ' ', lzr)
!
        nb2 = nb1 + 1
        call matpgl(nb2, zr(lzr), plg)
!
        call r8inir(1326, 0.d0, vrs, 1)
!
        nddlet = 6*nb1 + 3
!
        call tranlg(nb1, 51, nddlet, plg, matloc,&
                    vrs)
!
    else
        ASSERT(.false.)
    endif
!
!
    if (option .eq. 'RIGI_MECA') then
!
!--------- STOCKAGE
!
!--------- COMPTEUR DE POSITION
!
        kompt = 0
!
        do 100 j = 1, 6 * nb1 + 3
            do 110 i = 1, j
                kompt = kompt + 1
                zr ( imatuu - 1 + kompt ) = vrs ( kompt )
110          continue
100      continue
!
    endif
!
!---- ENERGIES DE DEFORMATION ELASTIQUE
!
    if (option .eq. 'EPOT_ELEM') then
!
!------- LECTURE DE L'ADRESSE
!
        call jevech('PENERDR', 'E', jener)
!
!------- ADRESSE DES DEPLACEMENTS
!
        call jevech('PDEPLAR', 'L', iu)
!
!
!------- ENERGIE DE DEFORMATION TOTALE
!
        call utvtsv('ZERO', 6 * nb1 + 3, vrs, zr ( iu ), zr ( jener ))
!
        zr ( jener ) = 0.5d0 * zr ( jener )
!
        call bsthco(nomte, bsigth, indith)
!
        if (indith) then
            do 120 i = 1, 6 * nb1 + 3
                enerth = enerth + bsigth(i)*zr(iu+i-1)
120          continue
            zr(jener) = zr(jener) - enerth
        endif
!
        if (abs ( zr ( jener ) ) .gt. 1.d-6) then
!
!--------- ENERGIE DE DEFORMATION DE MEMBRANE
!
            call vdxrig(nomte, zr(jgeom), matloc, nb1, 1,&
                        0)
!
            call r8inir(1326, 0.d0, vrs, 1)
!
            call tranlg(nb1, 51, nddlet, plg, matloc,&
                        vrs)
!
            call utvtsv('ZERO', 6 * nb1 + 3, vrs, zr ( iu ), zr ( jener + 1 ))
!
            zr ( jener + 1 ) = 0.5d0 * zr ( jener + 1 )
!
!
!--------- ENERGIE DE DEFORMATION DE FLEXION
!
            call vdxrig(nomte, zr(jgeom), matloc, nb1, 0,&
                        1)
!
            call r8inir(1326, 0.d0, vrs, 1)
!
            call tranlg(nb1, 51, nddlet, plg, matloc,&
                        vrs)
!
            call utvtsv('ZERO', 6 * nb1 + 3, vrs, zr ( iu ), zr ( jener + 2 ))
!
            zr ( jener + 2 ) = 0.5d0 * zr ( jener + 2 )
!
!--------- VALEURS RELATIVES
!
            zr ( jener + 1 ) = zr ( jener + 1 ) / zr ( jener )
            zr ( jener + 2 ) = zr ( jener + 2 ) / zr ( jener )
!
        else
!
            call r8inir(2, 0.d0, zr ( jener + 1 ), 1)
!
        endif
!
!
    endif
!
!
end subroutine
