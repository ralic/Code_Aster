subroutine xreacl(mesh, model, hval_incr, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/copisd.h"
#include "asterfort/dbgcal.h"
#include "asterfort/dismoi.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/nmchex.h"
#include "asterfort/xmchex.h"
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
! person_in_charge: samuel.geniaut at edf.fr
!
    character(len=8), intent(in) :: mesh
    character(len=8), intent(in) :: model
    character(len=19), intent(in) :: hval_incr(*)
    type(NL_DS_Contact), intent(in) :: ds_contact
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (METHODE XFEM - ALGORITHME)
!
! MISE À JOUR DU SEUIL DE FROTTEMENT
!
! ----------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  ds_contact       : datastructure for contact management
! In  hval_incr        : hat-variable for incremental values fields
!
!
!
    integer, parameter :: nbout = 1
    integer, parameter :: nbin  = 10
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: nbma
    character(len=19) :: ligrmo, xdonco, xseuco, cseuil
    character(len=19) :: lnno, ltno
    character(len=16) :: option
    character(len=24) :: ainter, cface, faclon, pinter, chgeom, baseco
    character(len=19) :: depplu
    aster_logical :: debug, lcontx
    integer :: ifm, niv, ifmdbg, nivdbg
    integer, pointer :: xfem_cont(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    ligrmo = model(1:8)//'.MODELE'
    cseuil = '&&XREACL.SEUIL'
    xdonco = ds_contact%sdcont_solv(1:14)//'.XFDO'
    xseuco = ds_contact%sdcont_solv(1:14)//'.XFSE'
    option = 'XREACL'
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
    call dismoi('NB_MA_MAILLA', mesh, 'MAILLAGE', repi=nbma)
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', depplu)
!
! --- SI PAS DE CONTACT ALORS ON ZAPPE LA VÉRIFICATION
!
    call jeveuo(model(1:8)//'.XFEM_CONT', 'L', vi=xfem_cont)
    lcontx = xfem_cont(1) .ge. 1
    if (.not.lcontx) then
        goto 999
    endif
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- RECUPERATION DES DONNEES XFEM
!
    lnno = model(1:8)//'.LNNO'
    ltno = model(1:8)//'.LTNO'
    ainter = model(1:8)//'.TOPOFAC.AI'
    cface = model(1:8)//'.TOPOFAC.CF'
    faclon = model(1:8)//'.TOPOFAC.LO'
    pinter = model(1:8)//'.TOPOFAC.OE'
    baseco = model(1:8)//'.TOPOFAC.BA'
!
! --- CREATION DU CHAM_ELEM_S VIERGE
!
    call xmchex(mesh, nbma, xseuco, cseuil)
!
! --- RECUPERATION DES COORDONNEES DES NOEUDS
!
    chgeom = mesh(1:8)//'.COORDO'
!
! --- CREATION DES LISTES DES CHAMPS IN
!
    lpain(1) = 'PDEPL_P'
    lchin(1) = depplu(1:19)
    lpain(2) = 'PAINTER'
    lchin(2) = ainter(1:19)
    lpain(3) = 'PCFACE'
    lchin(3) = cface(1:19)
    lpain(4) = 'PLONGCO'
    lchin(4) = faclon(1:19)
    lpain(5) = 'PDONCO'
    lchin(5) = xdonco(1:19)
    lpain(6) = 'PPINTER'
    lchin(6) = pinter(1:19)
    lpain(7) = 'PGEOMER'
    lchin(7) = chgeom(1:19)
    lpain(8) = 'PLSN'
    lchin(8) = lnno(1:19)
    lpain(9) = 'PLST'
    lchin(9) = ltno(1:19)
    lpain(10) = 'PBASECO'
    lchin(10) = baseco(1:19)
!
!
! --- CREATION DES LISTES DES CHAMPS OUT
!
    lpaout(1) = 'PSEUIL'
    lchout(1) = cseuil(1:19)
!
! --- APPEL A CALCUL
!
    call calcul('S', option, ligrmo, nbin, lchin,&
                lpain, nbout, lchout, lpaout, 'V',&
                'OUI')
!
    if (debug) then
        call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                    nbout, lpaout, lchout)
    endif
!
! --- ON COPIE CSEUIL DANS RESOCO.SE
!
    call copisd('CHAMP_GD', 'V', lchout(1), xseuco)
!
999 continue
!
    call jedema()
end subroutine
