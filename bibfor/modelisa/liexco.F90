subroutine liexco(char, motfac, noma, nomo, nzoco,&
                  nmaco, nnoco)
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
    implicit     none
#include "jeveux.h"
#include "asterfort/cfnbsf.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lireco.h"
#include "asterfort/wkvect.h"
    character(len=8) :: char
    character(len=16) :: motfac
    character(len=8) :: noma, nomo
    integer :: nzoco, nmaco, nnoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
!
! REMPLISSAGE DE LA LISTE DES MAILLES ET DES NOEUDS
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
! IN  NMACO  : NOMBRE TOTAL DE MAILLES DES SURFACES DE CONTACT
! IN  NNOCO  : NOMBRE TOTAL DE NOEUDS DES SURFACES DE CONTACT
!
!
!
!
    integer :: jdecma, jdecno
    integer :: jlist
    integer :: nbmaes, nbnoes
    integer :: nbmama, nbnoma
    character(len=24) :: defico
    integer :: izone, isuco, ima, ino
    character(len=24) :: listme, listmm
    character(len=24) :: listne, listnm
    character(len=24) :: contma, contno
    integer :: jmaco, jnoco
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    isuco = 1
!
! --- ACCES AUX STRUCTURES DE DONNEES DE CONTACT
!
    contma = defico(1:16)//'.MAILCO'
    contno = defico(1:16)//'.NOEUCO'
!
! --- NOM DES SD TEMPORAIRES
!
    listmm = '&&NBSUCO.MAIL.MAIT'
    listme = '&&NBSUCO.MAIL.ESCL'
    listnm = '&&NBSUCO.NOEU.MAIT'
    listne = '&&NBSUCO.NOEU.ESCL'
!
! --- CREATION DES SD
!
    call wkvect(contma, 'G V I', nmaco, jmaco)
    call wkvect(contno, 'G V I', nnoco, jnoco)
!
! --- BOUCLE SUR LES ZONES
!
    do 100 izone = 1, nzoco
!
! --- LECTURE DES MAILLES/NOEUDS
!
        call lireco(motfac, noma, nomo, izone, listme,&
                    listmm, listne, listnm, nbmaes, nbnoes,&
                    nbmama, nbnoma)
!
! --- ON STOCKE LES MAILLES MAITRES
!
        call cfnbsf(defico, isuco, 'MAIL', nbmama, jdecma)
        call jeveuo(listmm, 'L', jlist)
        do 11 ima = 1, nbmama
            zi(jmaco+jdecma-1+ima) = zi(jlist+ima-1)
11      continue
!
! --- ON STOCKE LES NOEUDS MAITRES
!
        call cfnbsf(defico, isuco, 'NOEU', nbnoma, jdecno)
        call jeveuo(listnm, 'L', jlist)
        do 21 ino = 1, nbnoma
            zi(jnoco+jdecno-1+ino) = zi(jlist+ino-1)
21      continue
!
        isuco = isuco + 1
!
! --- ON STOCKE LES MAILLES ESCLAVES
!
        call cfnbsf(defico, isuco, 'MAIL', nbmaes, jdecma)
        call jeveuo(listme, 'L', jlist)
        do 10 ima = 1, nbmaes
            zi(jmaco+jdecma-1+ima) = zi(jlist+ima-1)
10      continue
!
! --- ON STOCKE LES NOEUDS ESCLAVES
!
        call cfnbsf(defico, isuco, 'NOEU', nbnoes, jdecno)
        call jeveuo(listne, 'L', jlist)
        do 20 ino = 1, nbnoes
            zi(jnoco+jdecno-1+ino) = zi(jlist+ino-1)
20      continue
!
        isuco = isuco + 1
!
100  end do
!
    call jedetr(listme)
    call jedetr(listmm)
    call jedetr(listne)
    call jedetr(listnm)
!
    call jedema()
end subroutine
