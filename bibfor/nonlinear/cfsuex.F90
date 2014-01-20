subroutine cfsuex(defico, noesup, nbexcl, nzoco)
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
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jelira.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=24) :: noesup, defico
    integer :: nbexcl
    integer :: nzoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - UTILITAIRE)
!
! NOEUD A SUPPRIMER DANS LES VECTEURS IDOINES POUR LE CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  DEFICO : SD POUR LA DEFINITION DE CONTACT
! IN  NOESUP : LISTE DES NOEUDS A ELIMINER
! IN  NBEXCL : LONGUEUR DU VECTEUR NOESUP (NB NOEUDS A ELIMINER)
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
!
!
!
!
    integer ::   jpsans, jsans, jnoes
    integer :: i, izone, ind, jdec, nbold, nbnew, lsansn
    character(len=24) :: psans, sansno
    integer, pointer :: pssnoco(:) => null()
    integer, pointer :: ssnoco(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- SAUVEGARDE ANCIENS VECTEURS
    sansno = defico(1:16)//'.SSNOCO'
    psans = defico(1:16)//'.PSSNOCO'
    call jeveuo(sansno, 'L', jsans)
    call jeveuo(psans, 'E', jpsans)
    call jeveuo(noesup, 'L', jnoes)
    call jelira(sansno, 'LONMAX', ival=lsansn)
    AS_ALLOCATE(vi=ssnoco, size=lsansn)
    AS_ALLOCATE(vi=pssnoco, size=nzoco+1)
    do 10 i = 1, lsansn
        ssnoco(i) = zi(jsans-1+i)
10  end do
    do 20 i = 1, nzoco + 1
        pssnoco(i) = zi(jpsans-1+i)
20  end do
!
! --- DESTRUCTION ANCIENS OBJETS
!
    call jedetr(sansno)
    call jedetr(psans)
!
! --- CREATION NOUVEAUX VECTEURS
!
    call wkvect(sansno, 'G V I', lsansn+nbexcl*nzoco, jsans)
    call wkvect(psans, 'G V I', nzoco+1, jpsans)
    jdec = 1
    zi(jpsans) = 0
    do 50 izone = 1, nzoco
        nbold = pssnoco(izone+1) - pssnoco(izone)
        do 30 ind = 1, nbold
            zi(jsans-1+jdec) = ssnoco(pssnoco(izone)+ind)
            jdec = jdec + 1
30      continue
        do 40 ind = 1, nbexcl
            zi(jsans-1+jdec) = zi(jnoes-1+ind)
            jdec = jdec + 1
40      continue
        nbnew = nbold + nbexcl
        zi(jpsans-1+izone+1) = zi(jpsans-1+izone) + nbnew
50  end do
!
! --- MENAGE
!
    AS_DEALLOCATE(vi=ssnoco)
    AS_DEALLOCATE(vi=pssnoco)
    call jedema()
end subroutine
