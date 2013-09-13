subroutine verdis(model, nomail, foue, i3d, i2d,&
                  ndim, ier)
    implicit none
#include "asterfort/codent.h"
#include "asterfort/dismoi.h"
#include "asterfort/modexi.h"
#include "asterfort/utmess.h"
    character(len=1) :: foue
    character(len=8) :: model, nomail
    integer :: i3d, i2d, ndim, ier
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
! person_in_charge: jean-luc.flejou at edf.fr
! --- ------------------------------------------------------------------
!
!      VERIFICATION DE LA COHERENCE ENTRE LA DIMENSION DES DISCRETS
!      ET LA DIMENSION DU MODELE
!
! --- ------------------------------------------------------------------
!
! IN
!     MODEL : NOM DU MODELE
!     FOUE  : ARRET <F> OU <E>
!     IER   : NOMBRE D'ERREUR PRECEDENTE
! OUT
!     I3D   : =0 PAS DE DISCRET 3D, =1 DISCRET 3D
!     I2D   : =0 PAS DE DISCRET 2D, =1 DISCRET 2D
!     NDIM  : DIMENSION DU MODELE
!     IER   : ERREUR PRECEDENTE + NOUVELLE ERREUR
!
! --- ------------------------------------------------------------------
    character(len=8) :: k8bid, kmess
    integer :: ierd, ibid, dimmai, dimmod
! --- ------------------------------------------------------------------
!
! --- RECUPERATION DE LA DIMENSION DU MAILLAGE
    call dismoi('F', 'DIM_GEOM_B', nomail, 'MAILLAGE', dimmai,&
                k8bid, ierd)
! --- RECUPERATION DE LA DIMENSION DU MODELE
    call dismoi('F', 'DIM_GEOM', model, 'MODELE', dimmod,&
                k8bid, ierd)
!     SI DIMMAI=DIMMOD
    if (dimmai .eq. dimmod) then
        ndim = dimmai
    else
        ibid = dimmod
!        IBID  =   1  : 1D
!              =   2  : 2D
!              =   3  : 3D
!              = 120  : 1D+2D     MELANGE
!              = 103  : 1D+3D     MELANGE
!              =  23  : 2D+3D     MELANGE
!              = 123  : 1D+2D+3D  MELANGE
!        IBID>3 ==> MELANGE DE MODELISATIONS ==> MESSAGE AFFE_MODELE
        if (ibid .gt. 3) then
            call utmess('A', 'MODELISA4_4')
        endif
!
        ndim = ibid
        if (ibid .ge. 100) then
            ibid = ibid - 100
            ndim = 1
        endif
        if (ibid .ge. 20) then
            ibid = ibid - 20
            ndim = 2
        endif
        if (ibid .eq. 3) ndim = 3
    endif
!     LA DIMENSION C'EST 2D OU 3D : TOUS LES AUTRES CAS SONT EXCLUS
    if ((ndim.ne.2) .and. (ndim.ne.3)) then
        call codent(dimmod, 'G', kmess)
        call utmess(foue, 'DISCRETS_20', sk=kmess)
        ier = ier + 1
    endif
!
! --- LE MODELE COMPORTE T-IL DES ELEMENTS DISCRETS 3D
    call modexi(model, 'DIS_', i3d)
! --- LE MODELE COMPORTE T-IL DES ELEMENTS DISCRETS 2D
    call modexi(model, '2D_DIS_', i2d)
!
! --- IL FAUT DES DISCRETS DANS LA MODELISATION
    if ((i3d.eq.0) .and. (i2d.eq.0)) then
        call utmess(foue, 'DISCRETS_17')
        ier = ier + 1
    endif
! --- PAS DE DISCRET 2D ET 3D SUR UN MODELE
    if ((i3d.eq.1) .and. (i2d.eq.1)) then
        call utmess(foue, 'DISCRETS_16')
        ier = ier + 1
    endif
!
end subroutine
