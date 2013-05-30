subroutine pjspco(moa1, moa2, corres, base, noca)
    implicit   none
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
! ----------------------------------------------------------------------
!     COMMANDE:  PROJ_CHAMP /  METHODE='SOUS_POINT'
! BUT : CALCULER LA STRUCTURE DE DONNEE CORRESP_2_MAILLA
!
! ----------------------------------------------------------------------
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/pjefco.h'
    include 'asterfort/pjmasp.h'
    character(len=8) :: moa1, moa2, noca
    character(len=16) :: corres
    character(len=1) :: base
    character(len=8) :: masp
!     ----------------------------------------------
    call jemarq()
    call assert(base.eq.'V')
!
!
!     CREATION DU MAILLAGE "SOUS-POINT" (MASP) ET
!     REMPLISSAGE DU .PJEF_SP DANS LA SD CORRES
!     QUI EST UN TABLEAU REFERENCANT, POUR CHAQUE NOEUD DE MASP,
!     LES NUMEROS DE MAILLE DE POINTS DE GAUSS ET DE SOUS-POINTS
!     AUXQUELS IL CORRESPOND DANS MOA2
!     ----------------------------------------------
    masp='&&PJSPCO'
    call pjmasp(moa2, masp, corres, noca)
!       CALL CARGEO(MASP)
!
!     -- APPEL A LA ROUTINE "USUELLE" PJEFCO
!
!     ----------------------------------------------
    call pjefco(moa1, masp, corres, 'V')
    call jedema()
end subroutine
