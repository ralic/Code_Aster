subroutine te0134(option, nomte)
!
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
!
! --------------------------------------------------------------------------------------------------
!
!           CALCUL DU REPERE LOCAL DONNE PAR L'UTILISATEUR
!
!   En sortie :
!       - les 3 champs de vecteurs correspondant au repère
!       - la matrice de passage du repère global au repère local
!
!   Types d'élements concernés :
!       DKT , DKTG , DST , Q4G , Q4GG , COQUE_3D , GRILLE
!
! --------------------------------------------------------------------------------------------------
!
    implicit none
    character(len=16) :: option, nomte
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterc/r8dgrd.h"
#include "asterfort/assert.h"
#include "asterfort/coqrep.h"
#include "asterfort/dxqpgl.h"
#include "asterfort/dxtpgl.h"
#include "asterfort/elrefe_info.h"
#include "asterfort/jevech.h"
#include "asterfort/tecach.h"
#include "asterfort/utpvlg.h"
!
! --------------------------------------------------------------------------------------------------
!
    integer :: jgeom, jrepl1, jrepl2, jrepl3, jmatpas, jcacoq
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfdx, jgano
    integer :: ii, iret
    real(kind=8) :: pgl(3, 3), t2iu(4), t2ui(4)
    real(kind=8) :: pulx(3), puly(3), pulz(3), ux(3), uy(3), uz(3)
    real(kind=8) :: coor(12), alpha, beta, c, s
    aster_logical :: vecteur, matrice
!
! --------------------------------------------------------------------------------------------------
!
    call elrefe_info(fami='RIGI',ndim=ndim,nno=nno,nnos=nnos,&
                     npg=npg,jpoids=ipoids,jvf=ivf,jdfde=idfdx,jgano=jgano)
    ASSERT( (nnos.eq.3).or.(nnos.eq.4) )
!
    call jevech('PGEOMER', 'L', jgeom)
    call jevech('PCACOQU', 'L', jcacoq)
    vecteur = .False.
    matrice = .False.
!   Si on demande les vecteurs, c'est les 3
    call tecach('NNO', 'PREPLO1', 'E', iret, iad=jrepl1)
    if ( iret.eq. 0 ) then
        vecteur = .true.
        call jevech('PREPLO2', 'E', jrepl2)
        call jevech('PREPLO3', 'E', jrepl3)
    else
        jrepl2 = 1
        jrepl3 = 1
    endif
!   Si on demande la matrice de passage
    call tecach('NNO', 'PMATPASS', 'E', iret, iad=jmatpas)
    if ( iret.eq. 0 ) then
        matrice = .true.
    else
        jmatpas = 1
    endif
!
! --------------------------------------------------------------------------------------------------
    do ii = 1, nnos*3
        coor(ii) = zr(jgeom-1+ii)
    enddo
!
! --------------------------------------------------------------------------------------------------
!   CALCUL DE LA MATRICE DE PASSAGE GLOBAL -> LOCAL(INTRINSEQUE)
    if      (nnos .eq. 3) then
        call dxtpgl(coor, pgl)
    else if (nnos .eq. 4) then
        call dxqpgl(coor, pgl, 'S', iret)
    endif
    alpha = zr(jcacoq+1)*r8dgrd()
    beta  = zr(jcacoq+2)*r8dgrd()
! --------------------------------------------------------------------------------------------------
    call coqrep(pgl, alpha, beta, t2iu, t2ui, c, s)
!
!   T2IU : LA MATRICE DE PASSAGE (2X2) : UTILISATEUR -> INTRINSEQUE
!   PUL  : LA MATRICE DE PASSAGE (3X3) : UTILISATEUR -> INTRINSEQUE
!
!       ( T2IU(1) , T2IU(3) , 0 )
!   PUL=( T2IU(2) , T2IU(4) , 0 )
!       (   0     ,    0    , 1 )
!
!   PUL = (PULX,PULY,PULZ)
!
    pulx(1) = t2iu(1)
    pulx(2) = t2iu(2)
    pulx(3) = 0.0d0
!
    puly(1) = t2iu(3)
    puly(2) = t2iu(4)
    puly(3) = 0.0d0
!
    pulz(1) = 0.0d0
    pulz(2) = 0.0d0
    pulz(3) = 1.0d0
!
!     (UX,UY,UZ) : VECTEUR LOCAUX UTILISATEUR DANS LE BASE GLOBALE
!     (UX,UY,UZ) = INV(PGL) * PUL
!
    call utpvlg(1, 3, pgl, pulx, ux)
    call utpvlg(1, 3, pgl, puly, uy)
    call utpvlg(1, 3, pgl, pulz, uz)
!
    if ( vecteur ) then
        do  ii = 1, 3
            zr(jrepl1-1+ii)=ux(ii)
            zr(jrepl2-1+ii)=uy(ii)
            zr(jrepl3-1+ii)=uz(ii)
        enddo
    endif
    if ( matrice ) then
!       matrice de passage du repère GLOBAL au repère LOCAL, constante par élément
        zr(jmatpas ) = 0.5
!       Stockage en colonne
        zr(jmatpas +1 )= ux(1)
        zr(jmatpas +2 )= ux(2)
        zr(jmatpas +3 )= ux(3)
!
        zr(jmatpas +4 )= uy(1)
        zr(jmatpas +5 )= uy(2)
        zr(jmatpas +6 )= uy(3)
!
        zr(jmatpas +7 )= uz(1)
        zr(jmatpas +8 )= uz(2)
        zr(jmatpas +9 )= uz(3)
    endif
!
end subroutine
