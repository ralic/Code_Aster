subroutine sansno(char, motfac, noma, sans, psans,&
                  nbmocl, tymocl, limocl)
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
! REPONSABLE
!
    implicit      none
#include "jeveux.h"
#include "asterfort/assert.h"
#include "asterfort/cfdisi.h"
#include "asterfort/cfnbsf.h"
#include "asterfort/cfzone.h"
#include "asterfort/jedema.h"
#include "asterfort/jedetr.h"
#include "asterfort/jemarq.h"
#include "asterfort/jeveuo.h"
#include "asterfort/reliem.h"
#include "asterfort/wkvect.h"
    character(len=8) :: char
    character(len=16) :: motfac
    character(len=24) :: sans, psans
    character(len=8) :: noma
    integer :: nbmocl
    character(len=16) :: tymocl(nbmocl), limocl(nbmocl)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODES MAILLEES - LECTURE DONNEES)
!
! LECTURE DES NOEUDS DANS LE MOT-CLEF <MOTGR/MOT> ET STOCKE DANS LA
! SD NOMSD
!
! ----------------------------------------------------------------------
!
!
! NB: ON ELIMINE LES NOEUDS N'APPARTENANT PAS AUX SURFACES DE CONTACT
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  NOMA   : NOM DU MAILLAGE
! IN  NBMOCL : NOMBRE DE MOT-CLEFS A SCRUTER
! IN  LIMOCL : LISTE DES MOTS CLE A SCRUTER
! IN  TYMOCL : LISTE DES TYPES DE MOTS CLE A SCRUTER :
!               / 'GROUP_MA'
!               / 'GROUP_NO'
!               / 'MAILLE'
!               / 'NOEUD'
! OUT SANS   : LISTE DES NOEUDS LUS DANS LES MOT-CLEFS
! OUT PSANS  : POINTEUR PAR ZONE DES NOEUDS LUS DANS LES MOT-CLEFS
!
!
!
!
    character(len=24) :: contno, pzone
    integer :: jnoco, jzone
    integer :: jpsans, jsans
    integer :: stocno
    integer :: i, nbno
    integer :: izone, ino, isurf
    integer :: jdecno, posno, numno
    integer :: nbelim, nuelim, ielim, nbveli
    integer :: jtrav, ltrav
    character(len=24) :: listno
    integer :: jelim
    character(len=24) :: defico
    integer :: nzoco, nnoco
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    defico = char(1:8)//'.CONTACT'
    nzoco = cfdisi(defico,'NZOCO')
    nnoco = cfdisi(defico,'NNOCO')
    listno = '&&SANSNO.SANSNO'
!
! --- ACCES AUX STRUCTURES DE DONNEES DE CONTACT
!
    pzone = defico(1:16)//'.PZONECO'
    contno = defico(1:16)//'.NOEUCO'
!
    call jeveuo(pzone, 'L', jzone)
    call jeveuo(contno, 'L', jnoco)
!
! --- CREATION DES VECTEURS
!
    ltrav = nzoco*nnoco
    call wkvect('&&SANSNO.TRAV', 'V V I', ltrav, jtrav)
    call wkvect(psans, 'G V I', nzoco+1, jpsans)
!
! --- INITIALISATIONS
!
    zi(jpsans) = 0
    stocno = 0
!
    do 70 izone = 1, nzoco
!
! --- LECTURE DES NOEUDS DONNES SOUS SANS_*
!
        call reliem(' ', noma, 'NU_NOEUD', motfac, izone,&
                    nbmocl, limocl, tymocl, listno, nbelim)
!
! --- NOMBRE DE NOEUDS A ELIMINER POUR LA ZONE
!
        if (nbelim .ne. 0) then
            call jeveuo(listno, 'L', jelim)
        endif
!
! --- VERIF SI NOEUD APPARTIENT A SURFACE DE CONTACT
!
        nbveli = 0
        do 50 ielim = 1, nbelim
            nuelim = zi(jelim+ielim-1)
            call cfzone(defico, izone, 'ESCL', isurf)
            call cfnbsf(defico, isurf, 'NOEU', nbno, jdecno)
            do 40 ino = 1, nbno
                posno = jdecno+ino
                numno = zi(jnoco+posno-1)
                if (numno .eq. nuelim) then
                    nbveli = nbveli + 1
                    zi(jtrav+stocno-1+nbveli) = nuelim
                    goto 50
                endif
40          continue
50      continue
!
! --- MISE A JOUR POINTEUR
!
        stocno = stocno + nbveli
        if (stocno .gt. ltrav) ASSERT(.false.)
        zi(jpsans+izone) = zi(jpsans+izone-1) + nbveli
!
70  end do
!
! --- CREATION DU VECTEUR
!
    if (stocno .eq. 0) then
        call wkvect(sans, 'G V I', 1, jsans)
    else
        call wkvect(sans, 'G V I', stocno, jsans)
!
! --- TRANSFERT DU VECTEUR
!
        do 170 i = 1, stocno
            if (zi(jtrav+i-1) .ne. 0) then
                zi(jsans-1+i) = zi(jtrav-1+i)
            endif
170      continue
    endif
!
! --- DESTRUCTION DES VECTEURS DE TRAVAIL TEMPORAIRES
!
    call jedetr(listno)
    call jedetr('&&SANSNO.TRAV')
!
    call jedema()
end subroutine
