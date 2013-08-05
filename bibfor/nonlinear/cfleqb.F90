subroutine cfleqb(noma, defico, nzoco, nnoqua)
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
!
#include "asterfort/assert.h"
#include "asterfort/cfnbsf.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jeveuo.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfl.h"
#include "asterfort/wkvect.h"
    character(len=8) :: noma
    integer :: nzoco, nnoqua
    character(len=24) :: defico
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES - QUAD8)
!
! ECRITURE LISTE DES NOEUDS QUADRATIQUES
! OBJETS PNOQUA ET CONOQU
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : NOM SD CONTACT DEFINITION
! IN  NZOCO  : NOMBRE TOTAL DE ZONES DE CONTACT
! IN  NNOQUA : NOMBRE TOTAL DE NOEUDS QUADRATIQUES DES SURFACES
!
!
!
!
    character(len=24) :: pzone
    integer :: jzone
    character(len=24) :: pnoqua, conoqu
    integer :: jnoqua, jnoqu
    character(len=24) :: contma
    integer :: jmaco
    integer :: jdecma, nummai, posmai
    integer :: isurf, izone, ima, nutyp, inoqua, isuco
    integer :: inoqto, jdecqu
    integer :: jdes
    integer :: nomili(3, 4)
    integer :: nbsurf, nbma, nbnomi
    integer :: iatyma, itypma
    character(len=8) :: nomtm
    logical :: lveri
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CREATION DES DEUX VECTEURS
!
    pnoqua = defico(1:16)//'.PNOEUQU'
    conoqu = defico(1:16)//'.NOEUQU'
    call wkvect(pnoqua, 'V V I', nzoco+1, jnoqua)
    call wkvect(conoqu, 'V V I', 3*nnoqua, jnoqu)
!
! --- ACCES AUX STRUCTURES DE DONNEES DE CONTACT
!
    contma = defico(1:16)//'.MAILCO'
    pzone = defico(1:16)//'.PZONECO'
    call jeveuo(contma, 'L', jmaco)
    call jeveuo(pzone, 'L', jzone)
!
! --- INITIALISATIONS
!
    jdecqu = 0
    call jeveuo(noma(1:8)//'.TYPMAIL', 'L', iatyma)
!
! --- LISTE DES NOEUDS MILIEUX POUR CHAQUE ZONE
!
    do 10 izone = 1, nzoco
!
        inoqto = 0
!
        lveri = mminfl(defico,'VERIF',izone )
        if (lveri) then
            goto 21
        endif
!
! ----- NOMBRE DE SURFACES DE CONTACT
!
        nbsurf = zi(jzone+izone) - zi(jzone+izone-1)
        ASSERT(nbsurf.eq.2)
!
        do 20 isuco = 1, nbsurf
!
            isurf = nbsurf*(izone-1)+isuco
!
            call cfnbsf(defico, isurf, 'MAIL', nbma, jdecma)
!
            do 30 ima = 1, nbma
!
! --------- NUMERO MAILLE COURANTE
!
                posmai = jdecma+ima
                nummai = zi(jmaco+posmai-1)
!
! --------- TYPE MAILLE COURANTE
!
                itypma = iatyma - 1 + nummai
                nutyp = zi(itypma)
                call jenuno(jexnum('&CATA.TM.NOMTM', nutyp), nomtm)
!
! --------- CONNECTIVITE DE LA MAILLE
!
                call jeveuo(jexnum(noma(1:8)//'.CONNEX', nummai), 'L', jdes)
!
! --------- NOEUDS: SOMMET1-SOMMET2-MILIEU
!
                if (nomtm(1:5) .eq. 'QUAD8') then
                    nbnomi = 4
                    nomili(1,1) = zi(jdes+5-1)
                    nomili(1,2) = zi(jdes+6-1)
                    nomili(1,3) = zi(jdes+7-1)
                    nomili(1,4) = zi(jdes+8-1)
                    nomili(2,1) = zi(jdes+1-1)
                    nomili(2,2) = zi(jdes+2-1)
                    nomili(2,3) = zi(jdes+3-1)
                    nomili(2,4) = zi(jdes+4-1)
                    nomili(3,1) = zi(jdes+2-1)
                    nomili(3,2) = zi(jdes+3-1)
                    nomili(3,3) = zi(jdes+4-1)
                    nomili(3,4) = zi(jdes+1-1)
                else
                    nbnomi = 0
                endif
!
! --------- TABLEAU DES NOEUDS : SOMMET1-SOMMET2-MILIEU
!
                if (nomtm(1:5) .eq. 'QUAD8') then
                    do 92 inoqua = 1, nbnomi
                        zi(jnoqu+jdecqu+3*(inoqua-1)+1-1) = nomili(1, inoqua)
                        zi(jnoqu+jdecqu+3*(inoqua-1)+2-1) = nomili(2, inoqua)
                        zi(jnoqu+jdecqu+3*(inoqua-1)+3-1) = nomili(3, inoqua)
92                  continue
                    jdecqu = jdecqu + 3*nbnomi
                    inoqto = inoqto + 3*nbnomi
                endif
!
30          continue
20      continue
21      continue
!
! ----- MISE A JOUR POINTEUR
!
        zi(jnoqua+izone) = zi(jnoqua+izone-1)+inoqto
!
10  end do
!
    call jedema()
end subroutine
