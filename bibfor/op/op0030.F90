subroutine op0030()
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
!
! ----------------------------------------------------------------------
!
! COMMANDE:  DEFI_CONTACT
!
! ----------------------------------------------------------------------
!
!
!
!
#include "jeveux.h"
#include "asterc/getres.h"
#include "asterfort/adalig.h"
#include "asterfort/assert.h"
#include "asterfort/cagene.h"
#include "asterfort/calico.h"
#include "asterfort/caliun.h"
#include "asterfort/cfdisl.h"
#include "asterfort/chveno.h"
#include "asterfort/copisd.h"
#include "asterfort/cormgi.h"
#include "asterfort/detrsd.h"
#include "asterfort/dismoi.h"
#include "asterfort/getvtx.h"
#include "asterfort/infdbg.h"
#include "asterfort/infmaj.h"
#include "asterfort/initel.h"
#include "asterfort/jedema.h"
#include "asterfort/jeecra.h"
#include "asterfort/jeexin.h"
#include "asterfort/jemarq.h"
#include "asterfort/lgtlgr.h"
#include "asterfort/wkvect.h"
    integer :: ifm, niv
    integer :: iret, noc, ndim, iatype
    character(len=4) :: k4bid
    character(len=8) :: noma, nomo, char
    character(len=16) :: k16bid, pheno, oper
    character(len=16) :: formul
    character(len=19) :: ligrmo, ligret, ligrel, ligrch
    integer :: iform
    logical :: lallv
    character(len=24) :: defico
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! -- TITRE
!
    call infmaj()
    call infdbg('CONTACT', ifm, niv)
!
! --- INITIALISATIONS
!
    formul = ' '
    iform = 0
    oper = 'XXXXXXXXXXMEXX'
    ligret = '&&OP0030.LIGRET'
    ligrel = '&&OP0030.LIGREL'
!
! --- CONCEPT RESULTAT
!
    call getres(char, k16bid, k16bid)
    defico = char(1:8)//'.CONTACT'
!
! --- NOMS DE LIGREL, MAILLAGE , DIMENSION DU PB
!
    call cagene(char, oper, ligrmo, noma, ndim)
    nomo = ligrmo(1:8)
!
! --- LIGREL DE CHARGE
!
    ligrch = char//'.CHME.LIGRE'
    call wkvect(char//'.TYPE', 'G V K8', 1, iatype)
    zk8(iatype) = 'MECA_RE'
!
! --- VERIFICATION QUE LE MODELE EST DE TYPE MECANIQUE
!
    call dismoi('PHENOMENE', nomo, 'MODELE', repk=pheno)
!
! --- RECUPERATION DE LA FORMULATION (UNIQUE !)
!
    call getvtx(' ', 'FORMULATION', scal=formul, nbret=noc)
    if (noc .eq. 0) then
        ASSERT(.false.)
    endif
!
    if (formul .eq. 'DISCRETE') then
        iform = 1
    else if (formul.eq.'CONTINUE') then
        iform = 2
    else if (formul.eq.'XFEM') then
        iform = 3
    else if (formul.eq.'LIAISON_UNIL') then
        iform = 4
    else
        ASSERT(.false.)
    endif
!
! --- LECTURE DES DONNEES
!
    if (iform .eq. 4) then
        call caliun(char, noma, nomo)
    else
        call calico(char, noma, nomo, ndim, iform,&
                    ligret)
    endif
!
! --- TYPES DE CONTACT
!
    lallv = cfdisl(defico,'ALL_VERIF')
!
! --- AJOUT LIGREL ELEMENTS TARDIFS METHODE CONTINUE
!
    if (iform .eq. 2) then
        if (.not.lallv) then
! ---   CREATION DU LIGREL A PARTIR DU LIGRET
            call lgtlgr('V', ligret, ligrel)
            call detrsd('LIGRET', ligret)
! ---   ON COPIE LE LIGREL
            call copisd('LIGREL', 'G', ligrel, ligrch)
            call detrsd('LIGREL', ligrel)
        endif
    endif
!
! --- MISE A JOUR DU LIGREL DE CHARGE SI IL EXISTE EN FONCTION
!     DE LA TAILLE MAX DES .RESL
!
    call jeexin(ligrch//'.LGRF', iret)
    if (iret .ne. 0) then
        call adalig(ligrch)
        call cormgi('G', ligrch)
        call jeecra(ligrch//'.LGRF', 'DOCU', cval='MECA')
        call initel(ligrch)
    endif
!
! --- VERIFICATION DE L'ORIENTATION ET DE LA COHERENCE DES NORMALES
!     POUR LES FORMULATIONS MAILLEES
!
    if ((iform.eq.1) .or. (iform.eq.2)) then
        call chveno(k4bid, noma, nomo)
    endif
!
    call jedema()
!
end subroutine
