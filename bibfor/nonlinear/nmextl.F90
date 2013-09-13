subroutine nmextl(noma, nomo, motfac, iocc, nomcha,&
                  typcha, listno, listma, nbno, nbma,&
                  extrch)
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
#include "asterfort/assert.h"
#include "asterfort/getvtx.h"
#include "asterfort/reliem.h"
#include "asterfort/utmess.h"
    character(len=8) :: noma, nomo
    character(len=16) :: motfac
    integer :: iocc
    character(len=24) :: nomcha
    character(len=4) :: typcha
    integer :: nbno, nbma
    character(len=24) :: listno, listma
    character(len=8) :: extrch
!
! ----------------------------------------------------------------------
!
! ROUTINE *_NON_LINE (EXTRACTION - LECTURE)
!
! LECTURE TOPOLOGIE (NOEUD OU MAILLE)
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  NOMO   : NOM DU MODELE
! IN  MOTFAC : MOT-FACTEUR POUR LIRE
! IN  IOCC   : OCCURRENCE DU MOT-CLEF FACTEUR MOTFAC
! IN  TYPCHA : TYPE DU CHAMP 'NOEU'/'ELGA'
! IN  NOMCHA : NOM DU CHAMP
! IN  LISTNO : LISTE CONTENANT LES NOEUDS
! IN  LISTMA : LISTE CONTENANT LES MAILLES
! OUT NBNO   : LONGUEUR DE LA LISTE DES NOEUDS
! OUT NBMA   : LONGUEUR DE LA LISTE DES MAILLES
! OUT EXTRCH : TYPE D'EXTRACTION SUR LE CHAMP
!                'MIN'      VALEUR MINI SUR TOUTES LES MAILLES/NOEUDS
!                'MAX'      VALEUR MAXI SUR TOUTES LES MAILLES/NOEUDS
!                'MOY'      VALEUR MOYENNE TOUTES LES MAILLES/NOEUDS
!                'MINI_ABS' VALEUR MINI EN ABSOLU SUR TOUTES LES
!                          MAILLES/NOEUDS
!                'MAXI_ABS' VALEUR MAXI EN ABSOLU SUR TOUTES LES
!                          MAILLES/NOEUDS
!                'VALE'     VALEUR TOUTES LES MAILLES/NOEUDS
!
! ----------------------------------------------------------------------
!
    character(len=8) :: oui
    integer :: n1, n2, n3, n4, n5, n6
    character(len=16) :: valk(1)
    integer :: nbmocl
    character(len=16) :: limocl(5), tymocl(5)
!
! ----------------------------------------------------------------------
!
!
! --- INITIALISATIONS
!
    n1 = 0
    n2 = 0
    n3 = 0
    n4 = 0
    n5 = 0
    nbno = 0
    nbma = 0
    extrch = 'VALE'
!
! --- LECTURE DE L'ENDROIT POUR EXTRACTION
!
    valk(1) = nomcha
    if (typcha .eq. 'NOEU') then
        call getvtx(motfac, 'NOEUD', iocc=iocc, nbval=0, nbret=n1)
        call getvtx(motfac, 'GROUP_NO', iocc=iocc, nbval=0, nbret=n2)
        call getvtx(motfac, 'MAILLE', iocc=iocc, nbval=0, nbret=n3)
        call getvtx(motfac, 'GROUP_MA', iocc=iocc, nbval=0, nbret=n4)
        call getvtx(motfac, 'TOUT', iocc=iocc, scal=oui, nbret=n5)
        if ((n1.eq.0) .and. (n2.eq.0) .and. (n3.eq.0) .and. (n4.eq.0) .and. (n5.eq.0)) then
            call utmess('F', 'EXTRACTION_1', sk=valk(1))
        endif
    else if (typcha.eq.'ELGA') then
        call getvtx(motfac, 'MAILLE', iocc=iocc, nbval=0, nbret=n3)
        call getvtx(motfac, 'GROUP_MA', iocc=iocc, nbval=0, nbret=n4)
        call getvtx(motfac, 'TOUT', iocc=iocc, scal=oui, nbret=n5)
        if ((n3.eq.0) .and. (n4.eq.0) .and. (n5.eq.0)) then
            call utmess('F', 'EXTRACTION_2', sk=valk(1))
        endif
    else
        ASSERT(.false.)
    endif
!
! --- TYPE D'EXTRACTION
!
    call getvtx(motfac, 'EVAL_CHAM', iocc=iocc, scal=extrch, nbret=n6)
    if (n6 .eq. 0) then
        extrch = 'VALE'
        call utmess('A', 'EXTRACTION_5', sk=valk(1))
    endif
!
! --- EXTRACTION DES NOEUDS - ILS DOIVENT APPARTENIR AU MODELE -
!
    if (typcha .eq. 'NOEU') then
        nbmocl = 5
        tymocl(1) = 'GROUP_NO'
        tymocl(2) = 'NOEUD'
        tymocl(3) = 'GROUP_MA'
        tymocl(4) = 'MAILLE'
        tymocl(5) = 'TOUT'
        limocl(1) = 'GROUP_NO'
        limocl(2) = 'NOEUD'
        limocl(3) = 'GROUP_MA'
        limocl(4) = 'MAILLE'
        limocl(5) = 'TOUT'
        call reliem(nomo, noma, 'NU_NOEUD', motfac, iocc,&
                    nbmocl, limocl, tymocl, listno, nbno)
        if (nbno .eq. 0) then
            call utmess('F', 'EXTRACTION_3', sk=valk(1))
        endif
    endif
!
! --- EXTRACTION DES MAILLES - ILS DOIVENT APPARTENIR AU MODELE -
!
    if (typcha .eq. 'ELGA') then
        nbmocl = 3
        tymocl(1) = 'GROUP_MA'
        tymocl(2) = 'MAILLE'
        tymocl(3) = 'TOUT'
        limocl(1) = 'GROUP_MA'
        limocl(2) = 'MAILLE'
        limocl(3) = 'TOUT'
        call reliem(nomo, noma, 'NU_MAILLE', motfac, iocc,&
                    nbmocl, limocl, tymocl, listma, nbma)
        if (nbma .eq. 0) then
            call utmess('F', 'EXTRACTION_4', sk=valk(1))
        endif
    endif
!
end subroutine
