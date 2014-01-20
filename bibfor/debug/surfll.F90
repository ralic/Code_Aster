subroutine surfll(defico, noma, ifm, nzoco, nmaco,&
                  nnoco)
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
!
#include "asterfort/cfnbsf.h"
#include "asterfort/cfzone.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/wkvect.h"
#include "asterfort/as_deallocate.h"
#include "asterfort/as_allocate.h"
    character(len=24) :: defico
    character(len=8) :: noma
    integer :: ifm
    integer :: nmaco, nnoco, nzoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - AFFICHAGE DONNEES)
!
! LISTE DES MAILLES ET DES NOEUDS
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  IFM    : UNITE D'IMPRESSION
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
! IN  NMACO  : NOMBRE DE MAILLES DE CONTACT
! IN  NNOCO  : NOMBRE DE NOEUDS DE CONTACT
!
!
!
!
    integer :: nbmail, nbnoeu
    integer :: izone, isuco, ima, ino
    integer :: nbma, nbno
    integer :: jdecma, jdecno
    integer :: numma, numno
!
    character(len=8) :: chain1, chain2
    character(len=24) :: noeuma, mailma
!
    character(len=24) :: psurma, psurno, contma, contno, pzone
    integer :: jsuma, jsuno, jmaco, jnoco, jzone
    character(len=8), pointer :: travma(:) => null()
    character(len=8), pointer :: travno(:) => null()
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- ACCES SD DU MAILLAGE
!
    mailma = noma(1:8)//'.NOMMAI'
    noeuma = noma(1:8)//'.NOMNOE'
!
! --- COMMUNS AVEC FORM. MAILLEES (DISCRET ET CONTINUE MAIS PAS XFEM)
!
    pzone = defico(1:16)//'.PZONECO'
    psurma = defico(1:16)//'.PSUMACO'
    psurno = defico(1:16)//'.PSUNOCO'
    contma = defico(1:16)//'.MAILCO'
    contno = defico(1:16)//'.NOEUCO'
!
    call jeveuo(pzone, 'L', jzone)
    call jeveuo(psurma, 'L', jsuma)
    call jeveuo(psurno, 'L', jsuno)
    call jeveuo(contma, 'L', jmaco)
    call jeveuo(contno, 'L', jnoco)
!
! --- CREATION VECTEURS TEMPORAIRES
!
    AS_ALLOCATE(vk8=travma, size=nmaco)
    AS_ALLOCATE(vk8=travno, size=nnoco)
!
! --- IMPRESSIONS GLOBALES (TOUTES ZONES)
!
!
    write (ifm,*)
    write (ifm,*) '<CONTACT> INFOS SUR LES SURFACES MAILLEES '
    write (ifm,*)
!
    do 10 izone = 1, nzoco
!
        write (ifm,*) '<CONTACT> ZONE : ',izone
!
        nbmail = zi(jsuma+zi(jzone+izone))-zi(jsuma+zi(jzone+izone-1))
        write (ifm,*) '<CONTACT> ... NOMBRE DE MAILLES          : ',&
     &                  nbmail
!
        nbnoeu = zi(jsuno+zi(jzone+izone))-zi(jsuno+zi(jzone+izone-1))
        write (ifm,*) '<CONTACT> ... NOMBRE DE NOEUDS           : ',&
     &                  nbnoeu
!
        write (ifm,*) '<CONTACT> ...... SURFACE MAITRE '
        call cfzone(defico, izone, 'MAIT', isuco)
        call cfnbsf(defico, isuco, 'MAIL', nbma, jdecma)
        call cfnbsf(defico, isuco, 'NOEU', nbno, jdecno)
!
        if (nbma .le. 1) then
            chain1 = ' MAILLE '
        else
            chain1 = ' MAILLES'
        endif
        if (nbno .le. 1) then
            chain2 = ' NOEUD  '
        else
            chain2 = ' NOEUDS '
        endif
!
        write (ifm,1035) nbma, chain1,' ET ',nbno,chain2
!
        do 30 ima = 1, nbma
            numma = zi(jmaco+jdecma+ima-1)
            call jenuno(jexnum(mailma, numma), travma(ima))
30      continue
        write (ifm,1040) '     LISTE DES MAILLES : '
        write (ifm,1050) (travma(ima), ima = 1,nbma)
!
        do 40 ino = 1, nbno
            numno = zi(jnoco+jdecno+ino-1)
            call jenuno(jexnum(noeuma, numno), travno(ino))
40      continue
        write (ifm,1040) '     LISTE DES NOEUDS  : '
        write (ifm,1050) (travno(ino), ino = 1,nbno)
!
        write (ifm,*) '<CONTACT> ...... SURFACE ESCLAVE '
        call cfzone(defico, izone, 'ESCL', isuco)
        call cfnbsf(defico, isuco, 'MAIL', nbma, jdecma)
        call cfnbsf(defico, isuco, 'NOEU', nbno, jdecno)
!
        if (nbma .le. 1) then
            chain1 = ' MAILLE '
        else
            chain1 = ' MAILLES'
        endif
        if (nbno .le. 1) then
            chain2 = ' NOEUD  '
        else
            chain2 = ' NOEUDS '
        endif
!
        write (ifm,1035) nbma, chain1,' ET ',nbno,chain2
!
        do 31 ima = 1, nbma
            numma = zi(jmaco+jdecma+ima-1)
            call jenuno(jexnum(mailma, numma), travma(ima))
31      continue
        write (ifm,1040) '     LISTE DES MAILLES : '
        write (ifm,1050) (travma(ima), ima = 1,nbma)
!
        do 41 ino = 1, nbno
            numno = zi(jnoco+jdecno+ino-1)
            call jenuno(jexnum(noeuma, numno), travno(ino))
41      continue
        write (ifm,1040) '     LISTE DES NOEUDS  : '
        write (ifm,1050) (travno(ino), ino = 1,nbno)
10  end do
!
    1035 format (' <CONTACT> ...... ',i5,a8,a4,i5,a8)
    1040 format (' <CONTACT> ...... ',a25)
    1050 format ((' <CONTACT> ...... ',17x,4(a8,1x)))
!
! --- MENAGE
!
    AS_DEALLOCATE(vk8=travma)
    AS_DEALLOCATE(vk8=travno)
!
    call jedema()
end subroutine
