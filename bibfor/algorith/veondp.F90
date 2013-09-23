subroutine veondp(modele, mate, sddyna, temps, vecelz)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/calcul.h"
#include "asterfort/corich.h"
#include "asterfort/dbgcal.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/memare.h"
#include "asterfort/ndynin.h"
#include "asterfort/ndynkk.h"
#include "asterfort/reajre.h"
    character(len=*) :: vecelz
    character(len=19) :: sddyna
    character(len=24) :: modele, mate
    real(kind=8) :: temps
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES VECTEURS ELEMENTAIRES DES ONDES PLANES
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  MATE   : CHAMP DE MATERIAU
! IN  SDDYNA : SD DYNAMIQUE
! IN  TEMPS  : INSTANT DE CALCUL
! OUT VECELE : NOM DU VECT_ELEM
!
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=5)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: ibid, i, iret, iondp
    character(len=19) :: vecele
    character(len=24) :: chinst
    integer :: jnoma
    character(len=24) :: chgeom, ligrmo
    integer :: nchond
    character(len=19) :: chondp
    logical :: debug
    character(len=16) :: option
    integer :: ifmdbg, nivdbg
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    call ndynkk(sddyna, 'CHONDP', chondp)
    nchond = ndynin(sddyna,'NBRE_ONDE_PLANE')
    vecele = vecelz
    ligrmo = modele(1:8)//'.MODELE'
    call jeveuo(ligrmo(1:19)//'.LGRF', 'L', jnoma)
    chgeom = zk8(jnoma)//'.COORDO'
    option = 'ONDE_PLAN'
    chinst = '&&CHINST'
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
    call jeveuo(chondp, 'L', iondp)
!
! --- CREATION D'UNE CARTE D'INSTANTS
!
    call mecact('V', chinst, 'MODELE', ligrmo, 'INST_R',&
                ncmp=1, nomcmp='INST', sr=temps)
!
! --- CHAMPS D'ENTREE
!
    lpain(1) = 'PGEOMER'
    lchin(1) = chgeom(1:19)
    lpain(2) = 'PMATERC'
    lchin(2) = mate(1:19)
    lpain(3) = 'PTEMPSR'
    lchin(3) = chinst(1:19)
    lpain(4) = 'PONDPLA'
    lpain(5) = 'PONDPLR'
!
! --- CHAMPS DE SORTIE
!
    lpaout(1) = 'PVECTUR'
    lchout(1) = vecele
!
    call detrsd('VECT_ELEM', vecele)
    call memare('V', vecele, modele(1:8), ' ', ' ',&
                'CHAR_MECA')
!
! -- CALCUL
!
    do 30 i = 1, nchond
        call exisd('CARTE', zk8(iondp+i-1)//'.CHME.ONDPL', iret)
        call exisd('CARTE', zk8(iondp+i-1)//'.CHME.ONDPR', ibid)
!
        if (iret .ne. 0 .and. ibid .ne. 0) then
            lchin(4) = zk8(iondp+i-1)//'.CHME.ONDPL'
            lchin(5) = zk8(iondp+i-1)//'.CHME.ONDPR'
!
            call calcul('S', option, ligrmo, nbin, lchin,&
                        lpain, nbout, lchout, lpaout, 'V',&
                        'OUI')
            call corich('E', lchout(1), -1, ibid)
!
            if (debug) then
                call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                            nbout, lpaout, lchout)
            endif
!
            call reajre(vecele, lchout(1), 'V')
        endif
30  end do
!
    call jedema()
end subroutine
