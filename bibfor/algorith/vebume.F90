subroutine vebume(modelz, matasz, deplaz, lischa, vecelz)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
!
#include "asterfort/calcul.h"
#include "asterfort/conlag.h"
#include "asterfort/corich.h"
#include "asterfort/dbgcal.h"
#include "asterfort/detrsd.h"
#include "asterfort/exisd.h"
#include "asterfort/gcnco2.h"
#include "asterfort/infdbg.h"
#include "asterfort/inical.h"
#include "asterfort/jedema.h"
#include "asterfort/jeexin.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/mecact.h"
#include "asterfort/memare.h"
#include "asterfort/reajre.h"
    character(len=*) :: modelz, matasz, deplaz, vecelz
    character(len=19) :: lischa
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (CALCUL)
!
! CALCUL DES VECTEURS ELEMENTAIRES B.U - ELEMENTS DE LAGRANGE
!
! ----------------------------------------------------------------------
!
!
! IN  MODELE : NOM DU MODELE
! IN  DEPLA  : CHAMP DE DEPLACEMENTS
! IN  LISCHA : SD L_CHARGES
! OUT VECELE : VECTEURS ELEMENTAIRES
!
!
!
!
    integer :: nbout, nbin
    parameter    (nbout=1, nbin=3)
    character(len=8) :: lpaout(nbout), lpain(nbin)
    character(len=19) :: lchout(nbout), lchin(nbin)
!
    integer :: iret, nchar, ndir, icha, ibid
    integer :: jchar, jinf, ifmdbg, nivdbg
    real(kind=8) :: alpha
    character(len=8) :: nomcha, k8bid, masque, modele
    character(len=16) :: option
    character(len=19) :: depla, vecele, matass
    character(len=24) :: ligrch, chalph
    complex(kind=8) :: cbid
    logical :: debug
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('PRE_CALCUL', ifmdbg, nivdbg)
!
! --- INITIALISATIONS
!
    masque = '.0000000'
    vecele = vecelz
    matass = matasz
    modele = modelz
    depla = deplaz
    option = 'MECA_BU_R'
    if (nivdbg .ge. 2) then
        debug = .true.
    else
        debug = .false.
    endif
!
! --- INITIALISATION DES CHAMPS POUR CALCUL
!
    call inical(nbin, lpain, lchin, nbout, lpaout,&
                lchout)
!
! --- ACCES AUX CHARGES
!
    call jeexin(lischa(1:19)//'.LCHA', iret)
    if (iret .eq. 0) goto 9999
    call jelira(lischa(1:19)//'.LCHA', 'LONMAX', nchar, k8bid)
    call jeveuo(lischa(1:19)//'.LCHA', 'L', jchar)
    call jeveuo(lischa(1:19)//'.INFC', 'L', jinf)
!
! --- ALLOCATION DE LA CARTE DU CONDITIONNEMENT DES LAGRANGES
!
!      CALL UTIMSD(6,0,.TRUE.,.FALSE.,'&&OP0070',1,'V')
    call conlag(matass, alpha)
    chalph = '&&VEBUME.CH_NEUT_R'
    call mecact('V', chalph, 'MODELE', modele, 'NEUT_R  ',&
                1, 'X1', ibid, alpha, cbid,&
                k8bid)
!
! --- ALLOCATION DU VECT_ELEM RESULTAT :
!
    call detrsd('VECT_ELEM', vecele)
    call memare('V', vecele, modele(1:8), ' ', ' ',&
                'CHAR_MECA')
    call reajre(vecele, ' ', 'V')
!
! --- CALCUL DE L'OPTION B.U
!
    ndir = 0
    do 10 icha = 1, nchar
        if (zi(jinf+icha) .le. 0) goto 10
        nomcha = zk24(jchar+icha-1) (1:8)
        ligrch = nomcha//'.CHME.LIGRE'
        call jeexin(nomcha//'.CHME.LIGRE.LIEL', iret)
        if (iret .le. 0) goto 10
        call exisd('CHAMP_GD', nomcha//'.CHME.CMULT', iret)
        if (iret .le. 0) goto 10
!
        lpain(1) = 'PDDLMUR'
        lchin(1) = nomcha(1:8)//'.CHME.CMULT'
        lpain(2) = 'PDDLIMR'
        lchin(2) = depla
        lpain(3) = 'PALPHAR'
        lchin(3) = chalph
        lpaout(1) = 'PVECTUR'
        lchout(1) = '&&VEBUME.???????'
        call gcnco2(masque)
        lchout(1)(10:16) = masque(2:8)
        call corich('E', lchout(1), icha, ibid)
!
        call calcul('S', option, ligrch, nbin, lchin,&
                    lpain, nbout, lchout, lpaout, 'V',&
                    'OUI')
!
        if (debug) then
            call dbgcal(option, ifmdbg, nbin, lpain, lchin,&
                        nbout, lpaout, lchout)
        endif
!
        ndir = ndir + 1
        call reajre(vecele, lchout(1), 'V')
10  end do
!
!
9999  continue
!
    call jedema()
end subroutine
