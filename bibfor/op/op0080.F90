subroutine op0080()
    implicit none
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!***********************************************************************
!  P. RICHARD     DATE 12/03/91
!-----------------------------------------------------------------------
!  BUT : OPERATEUR DE CALCUL DE MODES PAR SOUS-STRUCTURATION CYCLIQUE
!-----------------------------------------------------------------------
!
!
!
!
    include 'jeveux.h'
    include 'asterc/getres.h'
    include 'asterfort/argu80.h'
    include 'asterfort/calcyc.h'
    include 'asterfort/desccy.h'
    include 'asterfort/immocy.h'
    include 'asterfort/infmaj.h'
    include 'asterfort/infniv.h'
    include 'asterfort/projcy.h'
    include 'asterfort/refe80.h'
    character(len=8) :: nomres
    character(len=16) :: nomope, nomcon
!
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!
! --- PHASE DE VERIFICATION
!
!-----------------------------------------------------------------------
    integer :: ifm, niv
!-----------------------------------------------------------------------
    call infmaj()
!
    call getres(nomres, nomcon, nomope)
!
! --- RECUPERATION BASE MODALE OU RESULTAT CYCLIQUE ET CREATION .REFE
!
    call refe80(nomres)
!
! --- RECUPERATION DES PRINCIPAUX ARGUMENTS DE LA COMMANDE
!
    call argu80(nomres)
!
! --- CREATION DE LA NUMEROTATION DES DDL CYCLIQUES
!
    call desccy(nomres)
!
! --- CALCUL DES SOUS-MATRICES PROJETEES
!
    call projcy(nomres)
!
! --- CALCUL DES MODES PROPRES DEMANDES
!
    call calcyc(nomres)
!
! --- IMPRESSION DU CONCEPT RESULTAT
!
    call infniv(ifm, niv)
    if (niv .gt. 1) call immocy(nomres, ifm)
!
end subroutine
