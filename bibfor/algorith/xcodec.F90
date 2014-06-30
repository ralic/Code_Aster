subroutine xcodec(noma, modelx, ndim, crimax, linter)
!
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
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/xaint2.h"
#include "asterfort/xconno.h"
#include "asterfort/xfisco.h"
#include "asterfort/xfisno.h"
#include "asterfort/xoripe.h"
#include "asterfort/xpheop.h"
#include "asterfort/xstan2.h"
#include "asterfort/xtopoc.h"
#include "asterfort/xtopoi.h"
    character(len=8) :: noma, modelx
    integer :: ndim
    real(kind=8) :: crimax
    logical(kind=1) :: linter
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (MODIF. DU MODELE)
!
! CALCUL DU DÉCOUPAGE EN SOUS-TETRAS, DES FACETTES DE CONTACT
! ET VERIFICATION DES CRITERES DE CONDITIONNEMENT
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELX : NOM DU MODELE XFEM MODIFIE
! IN  NDIM   : DIMENSION DU PROBLEME
! IN  CRIMAX : CRITERE MAXIMUM
!
!
!
!
    character(len=19) :: ltno, lnno, stno, basloc, grlnno, grltno
    character(len=9) :: optrig
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CONCATENER LES CHAMPS NODAUX POUR DES FISSURES DU MODELE
!
    optrig = xpheop(modelx)
    ltno = modelx(1:8)//'.LTNO'
    lnno = modelx(1:8)//'.LNNO'
    stno = modelx(1:8)//'.STNO'
    basloc = modelx(1:8)//'.BASLOC'
    grlnno = modelx(1:8)//'.GRLNNO'
    grltno = modelx(1:8)//'.GRLTNO'
    call xconno(modelx, '.STNO      ', 'G', optrig, 'PSTANO',&
                stno)
    call xconno(modelx, '.LNNO      ', 'G', optrig, 'PLSN',&
                lnno)
    call xconno(modelx, '.LTNO      ', 'G', optrig, 'PLST',&
                ltno)
    call xconno(modelx, '.BASLOC    ', 'G', optrig, 'PBASLOR',&
                basloc)
    call xconno(modelx, '.GRLNNO    ', 'V', 'TOPOFA', 'PGRADLN',&
                grlnno)
    call xconno(modelx, '.GRLTNO    ', 'V', 'TOPOFA', 'PGRADLT',&
                grltno)
!
! --- UTILE POUR LES INTERSECTIONS
!
    if (linter) then
        call xfisno(noma, modelx)
        call xfisco(noma, modelx)
    endif
!
! --- CALCUL DES DONNEES UTILES POUR L'INTÉGRATION (SOUS-TETRAS...)
!
    call xtopoi(noma, modelx)
!
! --- CALCUL DE LA TOPOLOGIE DES FACETTES DE CONTACT
!
    call xtopoc(modelx)
!
! --- ON MODIFIE MODELX(1:8)//'TOPOFAC.AI' POUR LE MULTI-HEAVISIDE
!
    if (linter) then
        call xaint2(noma, modelx)
    endif
!
! --- ON MODIFIE MODELX(1:8)//'.STNO' POUR LE CONDITIONNEMENT HEAVISIDE
!
    call xstan2(crimax, noma, modelx)
!
! --- ORIENTATION DES FACETTES DE PEAU X-FEM (COMME ORIE_PEAU)
!
    call xoripe(modelx)
!
!
    call jedema()
end subroutine
