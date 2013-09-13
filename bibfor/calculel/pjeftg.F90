subroutine pjeftg(igeom, geomi, nomai, motfac, iocc)
! ----------------------------------------------------------------------
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
! BUT : TRANSFORMER LA GEOMETRIE DES NOEUDS DU MAILLAGE_2 AVANT LA
!       LA PROJECTION (MOT CLE TRANSF_GEOM_2).
!       CELA PERMET PAR EXEMPLE DE PROJETER :
!       - UN MAILLLAGE SUR UN AUTRE MAILLAGE HOMOTHETIQUE
!       - UN MAILLAGE 2D SUR UN MAILLAGE 3D "ECRASE" DANS UN PLAN
!         (2D AXIS -> 3D AXIS)
!
! OUT : GEOMI (K24) : NOM DE L'OBJET CONTENANT LA GEOMETRIE TRANSFORMEE
!       DES NOEUDS DU MAILLAGE_[1|2]
!       SI PAS DE GEOMETRIE TRANSFORMEE GEOMI=' '
!
! ----------------------------------------------------------------------
    implicit none
!
! 0.1. ==> ARGUMENTS
!
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/fointe.h"
#include "asterfort/getvid.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jedupo.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
    integer :: igeom
    character(len=8) :: nomai
    character(len=24) :: geomi
    character(len=*) :: motfac
    integer :: iocc
!
! 0.2. ==> COMMUNS
!
!
!
! 0.3. ==> VARIABLES LOCALES
!
!
    character(len=8) :: lfonc(3), lparx(3)
    integer :: n1, nbnoi, ifonc
    integer :: nfonc, jgeomi, inoi, ier
!
    real(kind=8) :: vx(3)
!----------------------------------------------------------------------
! DEB ------------------------------------------------------------------
    call jemarq()
!
    ASSERT((igeom.eq.1).or.(igeom.eq.2))
!
!     PRISE EN COMPTE DU MOT-CLE TRANSF_GEOM_[1|2] : CALCUL DE GEOMI
!     --------------------------------------------------------------
    if (igeom .eq. 1) then
        call getvid(motfac, 'TRANSF_GEOM_1', iocc=iocc, nbval=3, vect=lfonc,&
                    nbret=nfonc)
    else
        call getvid(motfac, 'TRANSF_GEOM_2', iocc=iocc, nbval=3, vect=lfonc,&
                    nbret=nfonc)
    endif
    ASSERT(nfonc.ge.0)
    if (nfonc .gt. 0) then
        ASSERT(nfonc.eq.2 .or. nfonc.eq.3)
        if (nfonc .eq. 2) lfonc(3)='&FOZERO'
        if (igeom .eq. 1) then
            geomi='&&PJEFTG.GEOM1'
        else
            geomi='&&PJEFTG.GEOM2'
        endif
        call jedetr(geomi)
        call jedupo(nomai//'.COORDO    .VALE', 'V', geomi, .false.)
        call jelira(geomi, 'LONMAX', n1)
        call jeveuo(geomi, 'E', jgeomi)
        nbnoi=n1/3
        ASSERT(n1.eq.nbnoi*3)
        lparx(1)='X'
        lparx(2)='Y'
        lparx(3)='Z'
        do 1, inoi=1,nbnoi
        do 2, ifonc=1,3
        call fointe('F', lfonc(ifonc), 3, lparx, zr(jgeomi-1+3*( inoi-1)+1),&
                    vx(ifonc), ier)
        ASSERT(ier.eq.0)
 2      continue
        zr(jgeomi-1+3*(inoi-1)+1)=vx(1)
        zr(jgeomi-1+3*(inoi-1)+2)=vx(2)
        zr(jgeomi-1+3*(inoi-1)+3)=vx(3)
 1      continue
    else
        geomi = ' '
    endif
!
    call jedema()
end subroutine
