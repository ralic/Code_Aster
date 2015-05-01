subroutine op0099()
    implicit none
!=====================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!***********************************************************************
!  P. RICHARD - DATE 09/07/91
!-----------------------------------------------------------------------
!  BUT : OPERATEUR DE DEFINITION DE BASE MODALE POUR SUPERPOSITION OU
!        SYNTHESE MODALE : DEFI_BASE_MODALE
!
!  DEUX TYPES DE BASE MODALE : CLASSIQUE
!                              RITZ
!
! CLASSIQUE : BASE MODALE DE TYPE MIXTE CRAIG-BAMPTON - MAC-NEAL (1)
! --------
! DETERMINEE A PARTIR D'UNE 'INTERF_DYNA' DE TYPE CONNU ET D'UN OU
! DE 'MODE_MECA' BASES SUR LE MEME 'NUME_DDL'
! L'OPERATEUR POINTE SUR LES MODES CALCULES DU OU DES 'MODE_MECA'
! ET CALCULE LES DEFORMEES STATIQUES IMPOSEES PAR LA DEFINITION
! DES INTERFACES
!
! RITZ: BASE DE RITZ (A RE-DEVELOPPER)
! -----
! L'OPERATEUR COLLECTE TOUS LES RESULTATS ET LES MET SOUS UNE MEME
! NUMEROTATION DITE DE REFERENCE
!
!
!
!
#include "jeveux.h"
#include "asterc/getfac.h"
#include "asterc/getres.h"
#include "asterfort/clas99.h"
#include "asterfort/cresol.h"
#include "asterfort/diag99.h"
#include "asterfort/imbamo.h"
#include "asterfort/infmaj.h"
#include "asterfort/infniv.h"
#include "asterfort/orth99.h"
#include "asterfort/refe99.h"
#include "asterfort/ritz99.h"
#include "asterfort/titre.h"
    character(len=8) :: nomres
    character(len=16) :: nomope, nomcon
    character(len=19) :: solveu
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! --- PHASE DE VERIFICATION
!
!-----------------------------------------------------------------------
    integer :: ifm, ioc1, ioc3, ioc4, ioc5, niv
!-----------------------------------------------------------------------
    call infmaj()
    call infniv(ifm, niv)
!
! --- ECRITURE TITRE ET RECUPERATION NOM ARGUMENT
!
    call titre()
    call getres(nomres, nomcon, nomope)
!
! --- CONTROLE DE LA COHERENCE ET CREATION DU .REFE
!
    call refe99(nomres)
!
! --- TYPE DE BASE MODALE CREE
!
    call getfac('CLASSIQUE', ioc1)
    call getfac('RITZ', ioc3)
    call getfac('DIAG_MASS', ioc4)
    call getfac('ORTHO_BASE', ioc5)
!
!
!     -- CREATION DU SOLVEUR :
    solveu='&&OP0099.SOLVEUR'
    call cresol(solveu)
!
!
! --- CAS D'UNE BASE MODALE CLASSIQUE
!
    if (ioc1 .gt. 0) then
        call clas99(nomres)
!
! --- CAS D'UNE BASE MODALE DE RITZ
!
    else if (ioc3.gt.0) then
        call ritz99(nomres)
        call orth99(nomres, 1)
!
! --- CAS D'UNE DIAGONALISATION DE LA MASSE
!
    else if (ioc4.gt.0) then
!
        call diag99(nomres)
!
    else if (ioc5.gt.0) then
!
        call orth99(nomres, 0)
    endif
!
!
! --- IMPRESSION SUR FICHIER
    if (niv .gt. 1) call imbamo(nomres)
!
!
end subroutine
