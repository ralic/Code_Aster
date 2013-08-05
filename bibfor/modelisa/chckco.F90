subroutine chckco(char, noma, ndimg)
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
#include "asterfort/cfcald.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfnumm.h"
#include "asterfort/cfnumn.h"
#include "asterfort/cfposn.h"
#include "asterfort/cftypm.h"
#include "asterfort/cftypn.h"
#include "asterfort/cfzonn.h"
#include "asterfort/jedema.h"
#include "asterfort/jemarq.h"
#include "asterfort/jenuno.h"
#include "asterfort/jexnum.h"
#include "asterfort/mminfi.h"
#include "asterfort/mmtypm.h"
#include "asterfort/u2mesk.h"
    character(len=8) :: char, noma
    integer :: ndimg
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! VERIFICATION DES TANGENTES/NORMALES
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMA   : NOM DU MAILLAGE
! IN  NDIMG  : NOMBRE DE DIMENSIONS DU PROBLEME
!
!
!
!
    character(len=24) :: defico
    integer :: ino, posno, izone
    integer :: ima, posma, numma
    integer :: ndim, nnoma
    integer :: posnno(9), numnno(9)
    character(len=4) :: typno, typma
    character(len=8) :: alias, nomma
    logical :: lpoutr, lpoint
    integer :: itype
    integer :: nmaco
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    nmaco = cfdisi(defico,'NMACO')
!
! --- BOUCLE SUR LES MAILLES
!
    do 20 ima = 1, nmaco
        posma = ima
!
! ----- NUMERO ABSOLU DE LA MAILLE
!
        call cfnumm(defico, 1, posma, numma)
        call jenuno(jexnum(noma//'.NOMMAI', numma ), nomma)
!
! ----- TYPE DE LA MAILLE
!
        call cftypm(defico, posma, typma)
!
! ----- INDICES DANS CONTNO DES NOEUDS DE LA MAILLE
!
        call cfposn(defico, posma, posnno, nnoma)
!
! ----- INDICES ABSOLUS DANS LE MAILLAGE DES NOEUDS DE LA MAILLE
!
        call cfnumn(defico, nnoma, posnno, numnno)
!
! ----- TYPE DE LA MAILLE
!
        call mmtypm(noma, numma, nnoma, alias, ndim)
!
! ----- TYPE DE MAILLE
!
        lpoutr = (alias(1:2).eq.'SE').and.(ndimg.eq.3)
        lpoint = alias.eq.'PO1'
!
! ----- BOUCLE SUR LES NOEUDS DE LA MAILLE
!
        do 15 ino = 1, nnoma
            posno = posnno(ino)
!
! ------- ZONE DU NOEUD
!
            call cfzonn(defico, posno, izone)
!
! ------- TYPE DU NOEUD
!
            call cftypn(defico, posno, typno)
            if (.not.cfcald(defico,izone ,typno )) then
                goto 16
            endif
!
! ------- CHOIX DE LA NORMALE SUIVANT UTILISATEUR
!
            if (typno .eq. 'ESCL') then
                itype = mminfi(defico,'VECT_ESCL',izone)
            else if (typno.eq.'MAIT') then
                itype = mminfi(defico,'VECT_MAIT',izone)
            else
                ASSERT(.false.)
            endif
!
! ------- CALCUL DES TANGENTES EN CE NOEUD SI ELEMENT POINT
!
            if (lpoint) then
!
! --------- MAILLE POI1 SEULEMENT ESCLAVE
!
                if (typma .eq. 'MAIT') then
                    call u2mesk('F', 'CONTACT3_75', 1, nomma)
                endif
!
! -------- CHOIX DE LA NORMALE SUIVANT UTILISATEUR
!
                if ((itype.eq.0) .or. (itype.eq.2)) then
                    call u2mesk('F', 'CONTACT3_60', 1, nomma)
                endif
                goto 15
            endif
!
! ------- CALCUL DES TANGENTES EN CE NOEUD SI ELEMENT POUTRE
!
            if (lpoutr) then
                if (itype .eq. 0) then
                    call u2mesk('F', 'CONTACT3_61', 1, nomma)
                endif
                goto 15
            endif
!
16          continue
!
15      continue
20  end do
!
    call jedema()
end subroutine
