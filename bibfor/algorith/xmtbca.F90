subroutine xmtbca(mesh, hval_incr, mate, ds_contact)
!
use NonLin_Datastructure_type
!
implicit none
!
#include "asterf_types.h"
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfmmvd.h"
#include "asterfort/dbgcal.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mmbouc.h"
#include "asterfort/nmchex.h"
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
    character(len=8), intent(in) :: mesh
    character(len=19), intent(in) :: hval_incr(*)
    character(len=24), intent(in) :: mate
    type(NL_DS_Contact), intent(inout) :: ds_contact
!
! --------------------------------------------------------------------------------------------------
!
! Contact - Solve
!
! XFEM large sliding - Management of contact loop
!
! --------------------------------------------------------------------------------------------------
!
! In  mesh             : name of mesh
! In  hval_incr        : hat-variable for incremental values fields
! IO  ds_contact       : datastructure for contact management
!
! --------------------------------------------------------------------------------------------------
!
    integer, parameter :: nbout = 1
    integer, parameter :: nbin  = 11
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: ntpc, sinco, ipc, ipc2, naret, naret2
    integer :: jtabf
    real(kind=8) :: group
    character(len=24) :: tabfin
    integer :: ztabf
    character(len=19) :: ligrxf, cindco
    character(len=19) :: cpoint, cainte, heavno, hea_fa, hea_no
    character(len=19) :: oldgeo, depplu
    character(len=19) :: basefo, lnno, stano
    character(len=16) :: option
    aster_logical :: debug
    integer :: ifm, niv, ifmdbg, nivdbg
    integer :: igr, igr2, ngrel, iel, iel2, nel, nel2
    integer :: adiel, adiel2, jad, jad2, debgr, debgr2
    integer, pointer :: celd(:) => null()
    integer, pointer :: celv(:) => null()
    aster_logical :: loop_cont_conv
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
    cpoint = ds_contact%sdcont_solv(1:14)//'.XFPO'
    cainte = ds_contact%sdcont_solv(1:14)//'.XFAI'
    heavno = ds_contact%sdcont_solv(1:14)//'.XFPL'
    hea_fa = ds_contact%sdcont_solv(1:14)//'.XFHF'
    hea_no = ds_contact%sdcont_solv(1:14)//'.XFHN'
    basefo  = ds_contact%sdcont_solv(1:14)//'.XFBS'
    lnno  = ds_contact%sdcont_solv(1:14)//'.XFLN'
    stano  = ds_contact%sdcont_solv(1:14)//'.XFST'
    option = 'XCVBCA'
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
!
! - <LIGREL> for contact elements
!
    ligrxf = ds_contact%ligrel_elem_cont
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(hval_incr, 'VALINC', 'DEPPLU', depplu)
!
!----RECUPERATION DE TABFIN -
!
    tabfin = ds_contact%sdcont_solv(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'E', jtabf)
    ztabf = cfmmvd('ZTABF')
!
    cindco = '&&XMTBCA.INDCO'
!
    ntpc = cfdisi(ds_contact%sdcont_defi,'NTPC' )
!
! --- INITIALISATION DE L'INDICATEUR DE CONVERGENCE DE LA BOUCLE
! --- SUR LES CONTRAINTES ACTIVES (CONVERGENCE <=> INCOCA =1)
!
    sinco = 0
    loop_cont_conv = .true.
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- CREATION DES LISTES DES CHAMPS IN
!
    lpain(1) = 'PGEOMER'
    lchin(1) = oldgeo(1:19)
    lpain(2) = 'PDEPL_P'
    lchin(2) = depplu(1:19)
    lpain(3) = 'PCAR_PT'
    lchin(3) = cpoint
    lpain(4) = 'PCAR_AI'
    lchin(4) = cainte
    lpain(5) = 'PHEAVNO'
    lchin(5) = heavno
    lpain(6) = 'PHEA_NO'
    lchin(6) = hea_no
    lpain(7) = 'PHEA_FA'
    lchin(7) = hea_fa
    lpain(8) = 'PBASLOC'
    lchin(8) =  basefo   
    lpain(9) = 'PLSNGG'
    lchin(9) =  lnno
    lpain(10) = 'PSTANO'
    lchin(10) =  stano
    lpain(11)  = 'PMATERC'
    lchin(11)  = mate(1:19)

!
! --- CREATION DES LISTES DES CHAMPS OUT
!
    lpaout(1) = 'PINDCOO'
    lchout(1) = cindco
!
! --- APPEL A CALCUL
!
    call calcul('S', option, ligrxf, nbin, lchin,&
                lpain, nbout, lchout, lpaout, 'V',&
                'OUI')
!
    if (debug) then
        call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                    nbout, lpaout, lchout)
    endif
!
! --- ON RECUPÈRE LES VARIABLES CHANGEMENT DE STATUT/STATUT/MEMCO
!
    call jeveuo(cindco//'.CELV', 'L', vi=celv)
    call jeveuo(cindco//'.CELD', 'L', vi=celd)
    ngrel = celd(2)
!
! --- ON INTERVERTI LE STATUT DES ARETES SI NECESSAIRE
!
    do igr = 1, ngrel
        debgr = celd(4+igr)
        nel = celd(debgr+1)
        call jeveuo(jexnum(ligrxf//'.LIEL', igr), 'L', jad)
        do iel = 1, nel
            adiel = celd(debgr+4+4*(iel-1)+4)
! --- SI PAS DE CHANGEMENT DE STATUT DU POINT D'INTEGRATION, ON SORT
            if (celv(adiel) .eq. 0) goto 20
            ipc = -zi(jad-1+iel)
            group = zr(jtabf+ztabf*(ipc-1)+4)
! --- SI LE POINT D'INTEGRATION N'APPARTIENT PAS À UN GROUPE, ON SORT
            if (group .eq. 0.d0) goto 20
! --- SI LE POINT EST VITAL OU NON CONTACTANT, ON SORT
            if (zr(jtabf+ztabf*(ipc-1)+27) .eq. 1) goto 20
            if (celv(adiel+1) .eq. 0) goto 20
!
! --- SI LE PT D'INTEG EST SUR UNE ARETE NON VITAL ET DEVIENT CONTACTANT
! --- ON REGARDE SI UN PT SUR L'ARETE VITALE DE CE GROUPE DEVIENT OU
! --- RESTE CONTACTANT
            naret = nint(zr(jtabf+ztabf*(ipc-1)+5))
            do igr2 = 1, ngrel
                debgr2 = celd(4+igr2)
                nel2 = celd(debgr2+1)
                call jeveuo(jexnum(ligrxf//'.LIEL', igr2), 'L', jad2)
                do iel2 = 1, nel2
                    adiel2=celd(debgr2+4+4*(iel2-1)+4)
                    ipc2 = -zi(jad2-1+iel2)
                    if (zr(jtabf+ztabf*(ipc2-1)+4) .ne. group) goto 40
                    if (zr(jtabf+ztabf*(ipc2-1)+27) .eq. 0) goto 40
! --- LE PT VITAL EST CONTACTANT
                    if (celv(adiel2+1) .eq. 1) goto 20
! --- ATTENTION,LE PT VITAL EST NON CONTACTNT
                    naret2 = int(zr(jtabf+ztabf*(ipc2-1)+5))
                    do ipc2 = 1, ntpc
! --- ON INTERVERTI LE PT NON VITAL AVEC LE PT VITAL
                        if (zr(jtabf+ztabf*(ipc2-1)+4) .eq. group) then
                            if (zr(jtabf+ztabf*(ipc2-1)+5) .eq. naret2) then
                                zr(jtabf+ztabf*(ipc2-1)+27) = 0
                                elseif (zr(jtabf+ztabf*(ipc2-1)+5)&
                            .eq.naret) then
                                zr(jtabf+ztabf*(ipc2-1)+27) = 1
                            endif
                        endif
                    end do
                    goto 20
                end do
40              continue
            end do
20      continue
        end do
    end do
!
! --- MISE A JOUR DU STATUT DE CONTACT ET DE LA MEMOIRE DE CONTACT,
! --- SINCO = SOMME DES CICOCA SUR LES ÉLTS DU LIGREL
!
    do igr = 1, ngrel
        debgr = celd(4+igr)
        nel = celd(debgr+1)
        call jeveuo(jexnum(ligrxf//'.LIEL', igr), 'L', jad)
        do iel = 1, nel
            adiel = celd(debgr+4+4*(iel-1)+4)
            ipc = -zi(jad-1+iel)
            zr(jtabf+ztabf*(ipc-1)+13) = celv(adiel+1)
            zr(jtabf+ztabf*(ipc-1)+28) = celv(adiel+2)
            sinco = sinco + celv(adiel)
        end do
    end do
!
!
! --- SI SINCO EST STRICTEMENT POSITIF, ON A PAS CONVERGÉ
!
    if (sinco .gt. 0) then
        loop_cont_conv = .false.
    endif
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
