subroutine xmelem(noma, modele, defico, resoco)
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
    implicit      none
#include "jeveux.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/u2mesi.h"
#include "asterfort/u2mess.h"
#include "asterfort/xmele1.h"
#include "asterfort/xmele2.h"
    character(len=8) :: modele, noma
    character(len=24) :: resoco, defico
!
! ----------------------------------------------------------------------
!
! ROUTINE XFEM (METHODE XFEM - CREATION CHAM_ELEM)
!
! CREATION DES CHAM_ELEM
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  MODELE : NOM DU MODELE
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  RESOCO : SD POUR LA RESOLUTION DE CONTACT
!
!
!
!
!
    integer :: ifm, niv, nfiss, jnfis, nfismx
    character(len=19) :: ligrel
    character(len=19) :: xdonco, xindco, xseuco, xmemco, xgliss, xcohes
    character(len=19) :: xindcp, xmemcp, xseucp, xcohep
    parameter    (nfismx=100)
!
! ----------------------------------------------------------------------
!
    call jemarq()
    call infdbg('XFEM', ifm, niv)
!
! --- AFFICHAGE
!
    if (niv .ge. 2) then
        write (ifm,*) '<XFEM  > CREATION DES CHAM_ELEM'
    endif
!
! --- INITIALISATIONS
!
    ligrel = modele//'.MODELE'
!
! --- NOMBRE DE FISSURES
!
    call jeveuo(modele//'.NFIS', 'L', jnfis)
    nfiss = zi(jnfis)
    if (nfiss .gt. nfismx) then
        call u2mesi('F', 'XFEM_2', 1, nfismx)
    endif
    if (nfiss .le. 0) then
        call u2mess('F', 'XFEM_3')
    endif
!
!
!
    xindco = resoco(1:14)//'.XFIN'
    xmemco = resoco(1:14)//'.XMEM'
    xindcp = resoco(1:14)//'.XFIP'
    xmemcp = resoco(1:14)//'.XMEP'
    xdonco = resoco(1:14)//'.XFDO'
    xseuco = resoco(1:14)//'.XFSE'
    xseucp = resoco(1:14)//'.XFSP'
    xgliss = resoco(1:14)//'.XFGL'
    xcohes = resoco(1:14)//'.XCOH'
    xcohep = resoco(1:14)//'.XCOP'
!
! ---
!
    call xmele1(noma, modele, defico, ligrel, nfiss,&
                xindco, 'PINDCOI', 'RIGI_CONT')
    call xmele1(noma, modele, defico, ligrel, nfiss,&
                xmemco, 'PMEMCON', 'XCVBCA')
    call xmele1(noma, modele, defico, ligrel, nfiss,&
                xindcp, 'PINDCOI', 'RIGI_CONT')
    call xmele1(noma, modele, defico, ligrel, nfiss,&
                xmemcp, 'PMEMCON', 'XCVBCA')
    call xmele1(noma, modele, defico, ligrel, nfiss,&
                xseuco, 'PSEUIL', 'RIGI_CONT')
    call xmele1(noma, modele, defico, ligrel, nfiss,&
                xseucp, 'PSEUIL', 'XREACL')
    call xmele1(noma, modele, defico, ligrel, nfiss,&
                xgliss, 'PGLISS', 'XCVBCA')
    call xmele1(noma, modele, defico, ligrel, nfiss,&
                xcohes, 'PCOHES', 'RIGI_CONT')
    call xmele1(noma, modele, defico, ligrel, nfiss,&
                xcohep, 'PCOHES', 'XCVBCA')
!
! ---
!
    call xmele2(noma, modele, defico, ligrel, nfiss,&
                xdonco)
!
    call jedema()
!
end subroutine
