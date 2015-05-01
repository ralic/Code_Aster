subroutine xmelem(noma, modele, defico, resoco)
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/infdbg.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/utmess.h"
#include "asterfort/xmele1.h"
#include "asterfort/xmele2.h"
#include "asterfort/xmele3.h"
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
    integer :: ifm, niv, nfiss,  nfismx, jxc, contac
    character(len=19) :: ligrel
    character(len=19) :: xdonco, xindco, xseuco, xmemco, xgliss, xcohes
    character(len=19) :: xindcp, xmemcp, xseucp, xcohep
    integer, pointer :: nfis(:) => null()
    integer, pointer :: xfem_cont(:) => null()
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
    call jeveuo(modele//'.NFIS', 'L', vi=nfis)
    nfiss = nfis(1)
    if (nfiss .gt. nfismx) then
        call utmess('F', 'XFEM_2', si=nfismx)
    endif
    if (nfiss .le. 0) then
        call utmess('F', 'XFEM_3')
    endif
!
! --- ON VA CHERCHER LE TYPE DE CONTACT: STANDARD OU MORTAR?
!
    call jeveuo(modele//'.XFEM_CONT','L',vi=xfem_cont)
    contac = xfem_cont(1)
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
    if(contac.eq.1.or.contac.eq.3) then
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
    else if(contac.eq.2) then
        call xmele1(noma, modele, defico, ligrel, nfiss,&
                    xindco, 'PINDCOI', 'RIGI_CONT_M')
        call xmele1(noma, modele, defico, ligrel, nfiss,&
                    xmemco, 'PMEMCON', 'XCVBCA_MORTAR')
        call xmele1(noma, modele, defico, ligrel, nfiss,&
                    xindcp, 'PINDCOI', 'RIGI_CONT_M')
        call xmele1(noma, modele, defico, ligrel, nfiss,&
                    xmemcp, 'PMEMCON', 'XCVBCA_MORTAR')
        call xmele1(noma, modele, defico, ligrel, nfiss,&
                    xseuco, 'PSEUIL', 'RIGI_CONT_M')
        call xmele1(noma, modele, defico, ligrel, nfiss,&
                    xseucp, 'PSEUIL', 'RIGI_CONT_M')
        call xmele1(noma, modele, defico, ligrel, nfiss,&
                    xgliss, 'PGLISS', 'XCVBCA_MORTAR')
    endif
!
! --- SI CONTACT CLASSIQUE, CHAMPS COHESIFS COLLOCATION PTS GAUSS
!
    if(contac.eq.1.or.contac.eq.3) then
        call xmele1(noma, modele, defico, ligrel, nfiss,&
                    xcohes, 'PCOHES', 'RIGI_CONT')
        call xmele1(noma, modele, defico, ligrel, nfiss,&
                    xcohep, 'PCOHES', 'XCVBCA')
!
! --- SI CONTACT MORTAR, CHAMPS ELNO
!
    else if (contac.eq.2) then
        call xmele3(noma, modele, ligrel, nfiss,&
                    xcohes, 'PCOHES', 'RIGI_CONT_M')
        call xmele3(noma, modele, ligrel, nfiss,&
                    xcohep, 'PCOHES', 'XCVBCA_MORTAR')
    else
        ASSERT(.false.)
    endif
!
! ---
!
    call xmele2(noma, modele, defico, ligrel, nfiss,&
                xdonco)
!
    call jedema()
!
end subroutine
