function grdeur(nompar)
    implicit none
    integer :: grdeur
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterc/indik8.h'
    include 'asterfort/u2mesk.h'
    character(len=8) :: nompar
! ----------------------------------------------------------------------
!     ENTREES:
!      NOMPAR : NOM D'1 PARAMETRE DE L'OPTION QUE L'ON CALCULE.
!      L'OPTION EST REPRESENTEE PAR LES OBJETS DU COMMON CAII02.
!     SORTIES:
!      GRDEUR : GRANDEUR ASSOCIEE AU PARAMETRE
!
! ----------------------------------------------------------------------
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
!
!     FONCTIONS EXTERNES:
!     -------------------
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: jpar, nbpar
    integer :: iadsgd, iamloc, iaopds, iaopmo, iaopno, iaoppa, iaoptt, ilmloc
    integer :: ilopmo, ilopno, lgco, nparin, npario
!
! DEB-------------------------------------------------------------------
!
    nbpar = zi(iaopds-1+2) + zi(iaopds-1+3) + zi(iaopds-1+4)
    jpar = indik8(zk8(iaoppa),nompar,1,nbpar)
    if (jpar .eq. 0) then
        call u2mesk('F', 'CALCULEL2_61', 1, nompar)
    endif
    grdeur = zi(iaopds-1+4+jpar)
end function
