subroutine xmtbca(noma, defico, resoco, valinc, mmcvca)
!
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
! person_in_charge: ionel.nistor at edf.fr
!
    implicit none
#include "asterf_types.h"
#include "jeveux.h"
!
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
#include "asterfort/nmchex.h"
    aster_logical :: mmcvca
    character(len=8) :: noma
    character(len=24) :: defico, resoco
    character(len=19) :: valinc(*)
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (CONTACT - GRANDS GLISSEMENTS)
!
! MISE À JOUR DU STATUT DES POINTS DE CONTACT
! ET RENVOIE MMCVCA (INDICE DE CONVERGENCE DE LA BOUCLE
! SUR LES CONTRAINTES ACTIVES)
!
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DE L'OBJET MAILLAGE
! IN  DEFICO : SD CONTACT (DEFINITION)
! IN  RESOCO : SD CONTACT (RESOLUTION)
! IN  VALINC : VARIABLE CHAPEAU POUR INCREMENTS VARIABLES
! OUT MMCVCA : INDICE DE CONVERGENCE DE LA BOUCLE SUR LES C.A.
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=6)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: ntpc, sinco, ipc, ipc2, naret, naret2
    integer :: jtabf
    real(kind=8) :: group
    character(len=24) :: tabfin
    integer :: ztabf
    character(len=24) :: nosdco
    integer :: jnosdc
    character(len=19) :: ligrxf, cindco
    character(len=19) :: cpoint, cainte, heavno, heavfa
    character(len=19) :: oldgeo, depplu
    character(len=16) :: option
    aster_logical :: debug
    integer :: ifm, niv, ifmdbg, nivdbg
    integer :: igr, igr2, ngrel, iel, iel2, nel, nel2
    integer :: adiel, adiel2, jad, jad2, debgr, debgr2
    integer, pointer :: celd(:) => null()
    integer, pointer :: celv(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    oldgeo = noma(1:8)//'.COORDO'
    cpoint = resoco(1:14)//'.XFPO'
    cainte = resoco(1:14)//'.XFAI'
    nosdco = resoco(1:14)//'.NOSDCO'
    heavno = resoco(1:14)//'.XFPL'
    heavfa = resoco(1:14)//'.XFHF'
    call jeveuo(nosdco, 'L', jnosdc)
    option = 'XCVBCA'
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
!
! --- LIGREL DES ELEMENTS TARDIFS DE CONTACT/FROTTEMENT
!
    ligrxf = zk24(jnosdc+3-1)(1:19)
!
! --- DECOMPACTION DES VARIABLES CHAPEAUX
!
    call nmchex(valinc, 'VALINC', 'DEPPLU', depplu)
!
!----RECUPERATION DE TABFIN -
!
    tabfin = resoco(1:14)//'.TABFIN'
    call jeveuo(tabfin, 'E', jtabf)
    ztabf = cfmmvd('ZTABF')
!
    cindco = '&&XMTBCA.INDCO'
!
    ntpc = cfdisi(defico,'NTPC' )
!
! --- INITIALISATION DE L'INDICATEUR DE CONVERGENCE DE LA BOUCLE
! --- SUR LES CONTRAINTES ACTIVES (CONVERGENCE <=> INCOCA =1)
!
    sinco = 0
    mmcvca = .true.
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
    lpain(6) = 'PHEAVFA'
    lchin(6) = heavfa
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
    do 10 igr = 1, ngrel
        debgr = celd(4+igr)
        nel = celd(debgr+1)
        call jeveuo(jexnum(ligrxf//'.LIEL', igr), 'L', jad)
        do 20 iel = 1, nel
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
            do 30 igr2 = 1, ngrel
                debgr2 = celd(4+igr2)
                nel2 = celd(debgr2+1)
                call jeveuo(jexnum(ligrxf//'.LIEL', igr2), 'L', jad2)
                do 40 iel2 = 1, nel2
                    adiel2=celd(debgr2+4+4*(iel2-1)+4)
                    ipc2 = -zi(jad2-1+iel2)
                    if (zr(jtabf+ztabf*(ipc2-1)+4) .ne. group) goto 40
                    if (zr(jtabf+ztabf*(ipc2-1)+27) .eq. 0) goto 40
! --- LE PT VITAL EST CONTACTANT
                    if (celv(adiel2+1) .eq. 1) goto 20
!              ASSERT(ZI(JVALV-1+ADIEL2+2).EQ.0)
! --- ATTENTION,LE PT VITAL EST NON CONTACTNT
                    naret2 = int(zr(jtabf+ztabf*(ipc2-1)+5))
                    do 50 ipc2 = 1, ntpc
! --- ON INTERVERTI LE PT NON VITAL AVEC LE PT VITAL
                        if (zr(jtabf+ztabf*(ipc2-1)+4) .eq. group) then
                            if (zr(jtabf+ztabf*(ipc2-1)+5) .eq. naret2) then
                                zr(jtabf+ztabf*(ipc2-1)+27) = 0
                                elseif (zr(jtabf+ztabf*(ipc2-1)+5)&
                            .eq.naret) then
                                zr(jtabf+ztabf*(ipc2-1)+27) = 1
                            endif
                        endif
 50                 continue
                    goto 20
 40             continue
 30         continue
!
 20     continue
 10 end do
!
! --- MISE A JOUR DU STATUT DE CONTACT ET DE LA MEMOIRE DE CONTACT,
! --- SINCO = SOMME DES CICOCA SUR LES ÉLTS DU LIGREL
!
    do 60 igr = 1, ngrel
        debgr = celd(4+igr)
        nel = celd(debgr+1)
        call jeveuo(jexnum(ligrxf//'.LIEL', igr), 'L', jad)
        do 70 iel = 1, nel
            adiel = celd(debgr+4+4*(iel-1)+4)
            ipc = -zi(jad-1+iel)
            zr(jtabf+ztabf*(ipc-1)+13) = celv(adiel+1)
            zr(jtabf+ztabf*(ipc-1)+28) = celv(adiel+2)
            sinco = sinco + celv(adiel)
 70     continue
 60 end do
!
!
! --- SI SINCO EST STRICTEMENT POSITIF, ON A PAS CONVERGÉ
!
    if (sinco .gt. 0) mmcvca = .false.
!
    call jedema()
end subroutine
