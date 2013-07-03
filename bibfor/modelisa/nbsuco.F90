subroutine nbsuco(char, motfac, noma, nomo, nzoco,&
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
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/lireco.h"
    character(len=8) :: char
    character(len=16) :: motfac
    character(len=8) :: noma, nomo
    integer :: nzoco, nmaco, nnoco
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! DETERMINATION DU NOMBRE DE MAILLES ET DE NOEUDS DE CONTACT
! REMPLISSAGE DES POINTEURS ASSOCIES JSUMA,JSUNO
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
! OUT NMACO  : NOMBRE TOTAL DE MAILLES DES SURFACES DE CONTACT
! OUT NNOCO  : NOMBRE TOTAL DE NOEUDS DES SURFACES DE CONTACT
!
!
!
!
    character(len=24) :: defico
    character(len=24) :: psurma, psurno
    integer :: jsuma, jsuno
    integer :: izone, isuco
    integer :: nbmaes, nbnoes
    integer :: nbmama, nbnoma
    character(len=24) :: listme, listmm
    character(len=24) :: listne, listnm
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    nnoco = 0
    nmaco = 0
    isuco = 1
    defico = char(1:8)//'.CONTACT'
!
! --- ACCES AUX STRUCTURES DE DONNEES DE CONTACT
!
    psurma = defico(1:16)//'.PSUMACO'
    psurno = defico(1:16)//'.PSUNOCO'
    call jeveuo(psurma, 'E', jsuma)
    call jeveuo(psurno, 'E', jsuno)
!
! --- NOM DES SD TEMPORAIRES
!
    listmm = '&&NBSUCO.MAIL.MAIT'
    listme = '&&NBSUCO.MAIL.ESCL'
    listnm = '&&NBSUCO.NOEU.MAIT'
    listne = '&&NBSUCO.NOEU.ESCL'
!
! --- ON COMPTE LES MAILLES/NOEUDS DES ZONES DE CONTACT
!
    do 10 izone = 1, nzoco
        call lireco(motfac, noma, nomo, izone, listme,&
                    listmm, listne, listnm, nbmaes, nbnoes,&
                    nbmama, nbnoma)
        nnoco = nnoco+nbnoma+nbnoes
        nmaco = nmaco+nbmama+nbmaes
!
! --- NOMBRE DE MAILLES ET DE NOEUDS MAITRES
!
        zi(jsuma+isuco) = zi(jsuma+isuco-1) + nbmama
        zi(jsuno+isuco) = zi(jsuno+isuco-1) + nbnoma
        isuco = isuco + 1
!
! --- NOMBRE DE MAILLES ET DE NOEUDS ESCLAVES
!
        zi(jsuma+isuco) = zi(jsuma+isuco-1) + nbmaes
        zi(jsuno+isuco) = zi(jsuno+isuco-1) + nbnoes
        isuco = isuco + 1
!
10  end do
!
    call jedetr(listme)
    call jedetr(listmm)
    call jedetr(listne)
    call jedetr(listnm)
!
    call jedema()
end subroutine
