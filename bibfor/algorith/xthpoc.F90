subroutine xthpoc(modele, chtn, chtpg)
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
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/megeom.h"
    character(len=19) :: chtn, chtpg
    character(len=24) :: modele
! ----------------------------------------------------------------------
!
! THER_LINEAIRE + XFEM : APPEL A CALCUL POUR L'OPTION 'TEMP_ELGA'
!
! IN  MODELE  : NOM DU MODELE
! IN  CHTN    : CHAMP DE TEMPERATURE AUX NOEUDS
! OUT CHTPG   : CHAMP DE TEMPERATURE AUX PG
!
! ----------------------------------------------------------------------
    integer :: nbin, nbout
    parameter    (nbin=9)
    parameter    (nbout=1)
    character(len=8) :: lpain(nbin), lpaout(nbout)
    character(len=16) :: option
    character(len=19) :: ligrmo, pintto, cnseto, heavto, loncha, basloc, lsn
    character(len=19) :: lst
    character(len=24) :: lchin(nbin), lchout(nbout), chgeom
! ----------------------------------------------------------------------
!
    call jemarq()
!
    call megeom(modele, chgeom)
    pintto = modele(1:8)//'.TOPOSE.PIN'
    cnseto = modele(1:8)//'.TOPOSE.CNS'
    heavto = modele(1:8)//'.TOPOSE.HEA'
    loncha = modele(1:8)//'.TOPOSE.LON'
    basloc = modele(1:8)//'.BASLOC'
    lsn = modele(1:8)//'.LNNO'
    lst = modele(1:8)//'.LTNO'
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom
    lpain(2) = 'PTEMPER'
    lchin(2) = chtn
    lpain(3) = 'PPINTTO'
    lchin(3) = pintto
    lpain(4) = 'PCNSETO'
    lchin(4) = cnseto
    lpain(5) = 'PHEAVTO'
    lchin(5) = heavto
    lpain(6) = 'PLONCHA'
    lchin(6) = loncha
    lpain(7) = 'PBASLOR'
    lchin(7) = basloc
    lpain(8) = 'PLSN'
    lchin(8) = lsn
    lpain(9) = 'PLST'
    lchin(9) = lst
!
    lpaout(1) = 'PTEMPPG'
    lchout(1) = chtpg
!
    option = 'TEMP_ELGA'
    ligrmo = modele(1:8)//'.MODELE'
!
!     RQ : LIGRMO CONTIENT TOUS LES EF DU MODELE, MAIS SEULS LES EF
!     ---  X-FEM SAVENT CALCULER L'OPTION 'TEMP_ELGA'
    call calcul('S', option, ligrmo, nbin, lchin,&
                lpain, nbout, lchout, lpaout, 'V',&
                'OUI')
!
    call jedema()
!
end subroutine
