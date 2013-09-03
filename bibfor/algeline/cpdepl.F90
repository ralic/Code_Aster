subroutine cpdepl(melflu, base, nuor, nbm)
    implicit none
!-----------------------------------------------------------------------
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
!-----------------------------------------------------------------------
!  RECOPIE LES CHAMPS DE DEPLACEMENTS PRIS DANS UN CONCEPT MODE_MECA
!  LES DDL DE LAGRANGE SONT ELIMINES
!  APPELANT : FLUST1 , FLUST2
!-----------------------------------------------------------------------
!  IN : MELFLU : NOM DU CONCEPT DE TYPE MELASFLU PRODUIT
!  IN : BASE   : NOM DU CONCEPT DE TYPE MODE_MECA DEFINISSANT LA BASE
!                MODALE DU SYSTEME AVANT PRISE EN COMPTE DU COUPLAGE
!  IN : NUOR   : LISTE DES NUMEROS D'ORDRE DES MODES SELECTIONNES POUR
!                LE COUPLAGE (SUR LESQUELS PORTE L'EXTRACTION)
!  IN : NBM    : NOMBRE DE MODES PRIS EN COMPTE POUR LE COUPLAGE
!-----------------------------------------------------------------------
!
#include "jeveux.h"
#include "asterfort/dismoi.h"
#include "asterfort/extmod.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelibe.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
    integer :: nbm, nuor(nbm)
    character(len=8) :: base
    character(len=19) :: melflu
!
    integer :: iddl(6)
    character(len=8) :: mailla, k8bid
    character(len=14) :: numddl
    character(len=24) :: nomcha, matria, nomnoe
!-----------------------------------------------------------------------
    integer :: ibi,icham,im,imod,iret,lnoe
    integer :: neq
!-----------------------------------------------------------------------
    data iddl    /1,2,3,4,5,6/
!
!-----------------------------------------------------------------------
    call jemarq()
!
    nomcha(1:13) = melflu(1:8)//'.C01.'
    nomcha(17:24) = '001.VALE'
!
    call wkvect('&&CPDEPL.TEMP.NUOR', 'V V I', 1, imod)
!

    call dismoi('F', 'REF_RIGI_PREM', base, 'RESU_DYNA', ibi, matria, iret)
!
    call dismoi('F', 'NOM_NUME_DDL', matria, 'MATR_ASSE', ibi,&
                numddl, iret)
    call dismoi('F', 'NB_EQUA', matria, 'MATR_ASSE', neq,&
                k8bid, iret)
    call dismoi('F', 'NOM_MAILLA', matria, 'MATR_ASSE', ibi,&
                mailla, iret)
    nomnoe = mailla//'.NOMNOE'
    call jelira(nomnoe, 'NOMUTI', lnoe)
!
    do 10 im = 1, nbm
        write(nomcha(14:16),'(I3.3)') nuor(im)
        call jeveuo(nomcha, 'E', icham)
        zi(imod) = nuor(im)
        call extmod(base, numddl, zi(imod), 1, zr(icham),&
                    neq, lnoe, iddl, 6)
        call jelibe(nomcha)
10  continue
!
!     MENAGE
    call jedetr('&&CPDEPL.TEMP.NUOR')
!
    call jedema()
!
end subroutine
