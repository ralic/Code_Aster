subroutine xmmbca(mesh, model, mate, hval_incr, ds_contact)
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
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mesomm.h"
#include "asterfort/mmbouc.h"
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
    character(len=24), intent(in) :: mate
    character(len=19), intent(in) :: hval_incr(*)
    type(NL_DS_Contact), intent(inout) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! XFEM - Management of contact loop
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  model            : name of model
! In  mate             : name of material characteristics (field)
! In  hval_incr        : hat-variable for incremental values fields
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbout = 4
    integer, parameter :: nbin  = 20
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: sinco(1)
    integer :: jfiss
    character(len=19) :: xdonco, xindco, xmemco, xgliss, xcohes, ccohes
    character(len=16) :: option
    character(len=19) :: ligrmo, cicoca, cindoo, cmemco, ltno
    character(len=19) :: pinter, ainter, cface, faclon, baseco, xcoheo
    character(len=19) :: fissno, heavno, heavfa, hea_no, hea_fa
    aster_logical :: debug, lcontx
    integer :: ifm, niv, ifmdbg, nivdbg
    character(len=19) :: oldgeo, depmoi, depplu
    aster_logical :: loop_cont_conv
    integer, pointer :: xfem_cont(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    oldgeo = mesh(1:8)//'.COORDO'
    ligrmo = model(1:8)//'.MODELE'
    cicoca = '&&XMMBCA.CICOCA'
    cindoo = '&&XMMBCA.INDOUT'
    cmemco = '&&XMMBCA.MEMCON'
    ccohes = '&&XMMBCA.COHES'
!
    xindco = ds_contact%sdcont_solv(1:14)//'.XFIN'
    xdonco = ds_contact%sdcont_solv(1:14)//'.XFDO'
    xmemco = ds_contact%sdcont_solv(1:14)//'.XMEM'
    xgliss = ds_contact%sdcont_solv(1:14)//'.XFGL'
    xcohes = ds_contact%sdcont_solv(1:14)//'.XCOH'
    xcoheo = ds_contact%sdcont_solv(1:14)//'.XCOP'
!
    loop_cont_conv = .false.
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(hval_incr, 'VALINC', 'DEPMOI', depmoi)
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', depplu)
!
! --- SI PAS DE CONTACT ALORS ON ZAPPE LA VÉRIFICATION
!
    call jeveuo(model(1:8)//'.XFEM_CONT', 'L', vi=xfem_cont)
    lcontx = xfem_cont(1) .ge. 1
    if (.not.lcontx) then
        loop_cont_conv = .true.
        goto 999
    endif
!
! --- DETERMINATION DE L OPTION
!
    if(xfem_cont(1).eq.1.or.xfem_cont(1).eq.3) option='XCVBCA'
    if(xfem_cont(1).eq.2) option='XCVBCA_MORTAR' 
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- ACCES A LA SD FISS_XFEM
!
    call jeveuo(model(1:8)//'.FISS', 'L', jfiss)
!
! --- RECUPERATION DES DONNEES XFEM
!
    ltno = model(1:8)//'.LTNO'
    pinter = model(1:8)//'.TOPOFAC.OE'
    ainter = model(1:8)//'.TOPOFAC.AI'
    cface = model(1:8)//'.TOPOFAC.CF'
    faclon = model(1:8)//'.TOPOFAC.LO'
    baseco = model(1:8)//'.TOPOFAC.BA'
    fissno = model(1:8)//'.FISSNO'
    heavno = model(1:8)//'.HEAVNO'
    heavfa = model(1:8)//'.TOPOFAC.HE'
    hea_no = model(1:8)//'.TOPONO.HNO'
    hea_fa = model(1:8)//'.TOPONO.HFA'
!
! --- CREATION DU CHAM_ELEM_S VIERGE  INDIC. CONTACT ET MEMOIRE CONTACT
!
    call xmchex(mesh, xindco, cindoo)
    call xmchex(mesh, xmemco, cmemco)
    if (xfem_cont(1).eq.1.or.xfem_cont(1).eq.3) then
        call xmchex(mesh, xcohes, ccohes)
    endif
!
! --- CREATION DES LISTES DES CHAMPS IN
!
    lpain(1) = 'PGEOMER'
    lchin(1) = oldgeo(1:19)
    lpain(2) = 'PDEPL_M'
    lchin(2) = depmoi(1:19)
    lpain(3) = 'PDEPL_P'
    lchin(3) = depplu(1:19)
    lpain(4) = 'PINDCOI'
    lchin(4) = xindco
    lpain(5) = 'PLST'
    lchin(5) = ltno
    lpain(6) = 'PPINTER'
    lchin(6) = pinter
    lpain(7) = 'PAINTER'
    lchin(7) = ainter
    lpain(8) = 'PCFACE'
    lchin(8) = cface
    lpain(9) = 'PLONGCO'
    lchin(9) = faclon
    lpain(10) = 'PDONCO'
    lchin(10) = xdonco
    lpain(11) = 'PGLISS'
    lchin(11) = xgliss
    lpain(12) = 'PMEMCON'
    lchin(12) = xmemco
    lpain(13) = 'PCOHES'
    lchin(13) = xcohes
    lpain(14) = 'PBASECO'
    lchin(14) = baseco
    lpain(15) = 'PMATERC'
    lchin(15) = mate(1:19)
    lpain(16) = 'PFISNO'
    lchin(16) = fissno
    lpain(17) = 'PHEAVNO'
    lchin(17) = heavno
    lpain(18) = 'PHEAVFA'
    lchin(18) = heavfa
    lpain(19) = 'PHEA_NO'
    lchin(19) = hea_no
    lpain(20) = 'PHEA_FA'
    lchin(20) = hea_fa
!
! --- CREATION DES LISTES DES CHAMPS OUT
!
    lpaout(1) = 'PINCOCA'
    lchout(1) = cicoca
    lpaout(2) = 'PINDCOO'
    lchout(2) = cindoo
    lpaout(3) = 'PINDMEM'
    lchout(3) = cmemco
    lpaout(4) = 'PCOHESO'
    lchout(4) = ccohes
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
! --- ON FAIT sinco(1) = SOMME DES CICOCA SUR LES ÉLTS DU LIGRMO
!
    call mesomm(cicoca, 1, vi=sinco(1))
!
! --- SI sinco(1) EST STRICTEMENT POSITIF, ALORS ON A EU UN CODE RETOUR
! --- SUPERIEUR A ZERO SUR UN ELEMENT ET DONC ON A PAS CONVERGÉ
!
    if (sinco(1) .gt. 0) then
        loop_cont_conv = .false.
    else
        loop_cont_conv = .true.
    endif
!
! --- ON COPIE CINDO DANS RESOCO.XFIN
!
    call copisd('CHAMP_GD', 'V', lchout(2), xindco)
!
! --- ON COPIE CMEMCO DANS RESOCO.XMEM
!
    call copisd('CHAMP_GD', 'V', lchout(3), xmemco)
    call copisd('CHAMP_GD', 'V', lchout(4), xcohes)
!
999 continue
!
! - Set loop values
!
    if (loop_cont_conv) then
        call mmbouc(ds_contact, 'Cont', 'Set_Convergence')
    else
        call mmbouc(ds_contact, 'Cont', 'Set_Divergence')
    endif
!
    call jedema()
end subroutine
