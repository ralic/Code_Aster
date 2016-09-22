subroutine merimo(base           , model , cara_elem, mate  , varc_refe,&
                  ds_constitutive, iterat, acti_func, sddyna, hval_incr,&
                  hval_algo      , merigi, vefint   , optioz, tabret)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/calcul.h"
#include "asterfort/dbgcal.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/isfonc.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/memare.h"
#include "asterfort/merimp.h"
#include "asterfort/nmchex.h"
#include "asterfort/nmiret.h"
#include "asterfort/reajre.h"
#include "asterfort/redetr.h"
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2016  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: mickael.abbas at edf.fr
!
    character(len=1), intent(in) :: base
    integer, intent(in) :: iterat
    character(len=*), intent(in) :: mate
    character(len=19), intent(in) :: sddyna
    character(len=24), intent(in) :: model
    character(len=24), intent(in) :: cara_elem
    type(NL_DS_Constitutive), intent(in) :: ds_constitutive
    character(len=24), intent(in) :: varc_refe
    integer, intent(in) :: acti_func(*)
    character(len=19), intent(in) :: hval_incr(*)
    character(len=19), intent(in) :: hval_algo(*)
    character(len=*), intent(in) :: optioz
    character(len=19), intent(in) :: merigi
    character(len=19), intent(in) :: vefint
    aster_logical, intent(out) :: tabret(0:10)
!
! --------------------------------------------------------------------------------------------------
!
! Nonlinear mechanics (algorithm)
!
! Computation of rigidity matrix and internal forces
!
! --------------------------------------------------------------------------------------------------
!
! In  ds_constitutive  : datastructure for constitutive laws management
!
! --------------------------------------------------------------------------------------------------
!
    integer :: mxchout, mxchin
    parameter    (mxchout=9, mxchin=57)
    character(len=8) :: lpaout(mxchout), lpain(mxchin)
    character(len=19) :: lchout(mxchout), lchin(mxchin)
!
    aster_logical :: l_macr_elem
    aster_logical :: matrix, vector, codint, conext
    integer :: ires, iret, nbin, nbout
    character(len=24) :: caco3d
    character(len=24) :: ligrmo
    character(len=19) :: sigext, sigplu, varplu, strplu
    character(len=16) :: option
    aster_logical :: debug
    integer :: ifmdbg, nivdbg
    integer :: ich_matrixs, ich_matrixn, ich_vector, ich_codret
    character(len=24), pointer :: rerr(:) => null()
!
! --------------------------------------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! - Initializations
!
    option = optioz
    ligrmo = model(1:8)//'.MODELE'
    caco3d = '&&MERIMO.CARA_ROTA_FICTI'
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
    do iret = 0, 10
        tabret(iret) = .false.
    end do
!
! - Active functionnalites
!
    l_macr_elem = isfonc(acti_func, 'MACR_ELEM_STAT')
!
! - Get fields from hat-variables
!
    call nmchex(hval_incr, 'VALINC', 'SIGEXT', sigext)
    call nmchex(hval_incr, 'VALINC', 'SIGPLU', sigplu)
    call nmchex(hval_incr, 'VALINC', 'VARPLU', varplu)
    call nmchex(hval_incr, 'VALINC', 'STRPLU', strplu)
!
! - Input fields
!
    call merimp(model    , cara_elem, mate  , varc_refe, ds_constitutive,&
                acti_func, iterat   , sddyna, hval_incr, hval_algo      ,&
                caco3d   , mxchin   , nbin  , lpain    , lchin)
!
! - Output fields
!
    if (option(1:9) .eq. 'FULL_MECA') then
        matrix = .true.
        vector = .true.
        codint = .true.
        conext = .false.
    else if (option(1:10).eq.'RIGI_MECA ') then
        matrix = .true.
        vector = .false.
        codint = .false.
        conext = .false.
    else if (option(1:16).eq.'RIGI_MECA_IMPLEX') then
        matrix = .true.
        vector = .false.
        codint = .false.
        conext = .true.
    else if (option(1:10).eq.'RIGI_MECA_') then
        matrix = .true.
        vector = .false.
        codint = .false.
        conext = .false.
    else if (option(1:9).eq.'RAPH_MECA') then
        matrix = .false.
        vector = .true.
        codint = .true.
        conext = .false.
    else
        ASSERT(.false.)
    endif
!
    if (nivdbg .ge. 2) then
        write (ifmdbg,*) '<CALCUL> ... OPTION: ',option
        if (matrix) then
            write (ifmdbg,*) '<CALCUL> ... CALCUL DES MATR_ELEM DE RIGIDITE '
        endif
        if (vector) then
            write (ifmdbg,*) '<CALCUL> ... CALCUL DES VECT_ELEM DES FORCES INTERNES '
        endif
        if (conext) then
            write (ifmdbg,*) '<CALCUL> ... CALCUL DES CONTRAINTES EXTRAPOLEES POUR IMPLEX '
        endif
    endif
!
! - Prepare vector and matrix
!
    if (matrix) then
        call jeexin(merigi//'.RELR', iret)
        if (iret .eq. 0) then
            call jeexin(merigi//'.RERR', ires)
            if (ires .eq. 0) then
                call memare(base, merigi, model(1:8), mate, cara_elem,&
                            'RIGI_MECA')
            endif
            if (l_macr_elem) then
                call jeveuo(merigi//'.RERR', 'E', vk24=rerr)
                rerr(3) = 'OUI_SOUS_STRUC'
            endif
        endif
        call jedetr(merigi//'.RELR')
        call reajre(merigi, ' ', base)
    endif
!
    if (vector) then
        call jeexin(vefint//'.RELR', iret)
        if (iret .eq. 0) then
            call memare(base, vefint, model(1:8), mate, cara_elem,&
                        'CHAR_MECA')
        endif
        call jedetr(vefint//'.RELR')
        call reajre(vefint, ' ', base)
    endif
!
    lpaout(1) = 'PCONTPR'
    lchout(1) = sigplu(1:19)
    lpaout(2) = 'PVARIPR'
    lchout(2) = varplu(1:19)
    lpaout(3) = 'PCACO3D'
    lchout(3) = caco3d(1:19)
    lpaout(4) = 'PSTRXPR'
    lchout(4) = strplu(1:19)
    nbout = 4
    if (matrix) then
        nbout = nbout+1
        lpaout(nbout) = 'PMATUUR'
        lchout(nbout) = merigi(1:15)//'.M01'
        ich_matrixs = nbout
        nbout = nbout+1
        lpaout(nbout) = 'PMATUNS'
        lchout(nbout) = merigi(1:15)//'.M02'
        ich_matrixn = nbout
    endif
    if (vector) then
        nbout = nbout+1
        lpaout(nbout) = 'PVECTUR'
        lchout(nbout) = vefint(1:15)//'.R01'
        ich_vector = nbout
    endif
    if (codint) then
        nbout = nbout+1
        lpaout(nbout) = 'PCODRET'
        lchout(nbout) = ds_constitutive%comp_error(1:19)
        ich_codret = nbout
    endif
    if (conext) then
        nbout = nbout+1
        lpaout(nbout) = 'PCONTXR'
        lchout(nbout) = sigext(1:19)
    endif
!
    ASSERT(nbout.le.mxchout)
    ASSERT(nbin.le.mxchin)
!
! - Compute
!
    if (debug) then
        call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                    nbout, lpaout, lchout)
    endif
!
    call calcul('S', option, ligrmo, nbin, lchin,&
                lpain, nbout, lchout, lpaout, base,&
                'NON')
!
! - Save
!
    if (matrix) then
        call reajre(merigi, lchout(ich_matrixs), base)
        call reajre(merigi, lchout(ich_matrixn), base)
        call redetr(merigi)
    endif
!
    if (vector) then
        call reajre(vefint, lchout(ich_vector), base)
    endif
!
! - Errors
!
    if (codint) then
        call nmiret(lchout(ich_codret), tabret)
    endif
!
    call jedema()
end subroutine
