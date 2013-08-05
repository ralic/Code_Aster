subroutine cfleqa(noma, defico, nzoco, nnoqua)
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
    character(len=8) :: noma
    integer :: nzoco
    character(len=24) :: defico
    integer :: nnoqua
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES - QUAD8)
!
! NOMBRE TOTAL DE NOEUDS QUADRATIQUES
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : NOM SD CONTACT DEFINITION
! IN  NZOCO  : NOMBRE TOTAL DE ZONES DE CONTACT
! OUT NNOQUA : NOMBRE TOTAL DE NOEUDS QUADRATIQUES DES SURFACES
!
!
!
!
    character(len=24) :: pzone
    integer :: jzone
    character(len=24) :: contma
    integer :: jmaco
    integer :: jdecma, nummai, posmai
    integer :: isurf, izone, ima, nutyp, isuco
    integer :: nbsurf, nbma
    integer :: iatyma, itypma
    character(len=8) :: nomtm
    integer :: nbnomi
    logical :: lveri
!
! ----------------------------------------------------------------------
!
    call jemarq()
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
    nnoqua = 0
    nbnomi = 0
    call jeveuo(noma(1:8)//'.TYPMAIL', 'L', iatyma)
!
! --- DECOMPTE DES NOEUDS MILIEUX POUR CHAQUE SURFACE
!
    do 10 izone = 1, nzoco
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
                call jenuno(jexnum('&CATA.TM.NOMTM', nutyp ), nomtm)
!
! --------- NOMBRE DE NOEUDS MILIEUX A STOCKER A PART SUR LA MAILLE
!
                if (nomtm(1:5) .eq. 'QUAD9') then
                    nbnomi = 0
                else if (nomtm(1:5).eq.'TRIA7') then
                    nbnomi = 0
                else if (nomtm(1:5).eq.'QUAD8') then
                    nbnomi = 4
                else if (nomtm(1:5).eq.'TRIA6') then
                    nbnomi = 0
                else
                    nbnomi = 0
                endif
!
! --------- NOMBRE _TOTAL_ DE NOEUDS MILIEUX A STOCKER A PART
!
                nnoqua = nnoqua + nbnomi
!
30          continue
20      continue
21      continue
10  end do
!
    call jedema()
end subroutine
