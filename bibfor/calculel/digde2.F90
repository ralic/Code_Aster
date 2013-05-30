function digde2(modelo)
    implicit none
    integer :: digde2
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    integer :: modelo
! ----------------------------------------------------------------------
! ROUTINE IDENTIQUE A DIGDEL MAIS AVEC UN COMMON POUR ETRE PLUS RAPIDE
!     ENTREES:
!        MODELO : MODE_LOCAL (SON INDICE DANS &CATA.TE.MODELOC )
!
!     SORTIES:
!        DIGDE2 : NOMBRE DE SCALAIRES REPRESENTANT LA GRANDEUR POUR LE
!                 MODE_LOCAL
!
! ----------------------------------------------------------------------
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: modloc
    integer :: iadsgd, iamloc, iaopds, iaopmo, iaopno, iaoppa, iaoptt, ilmloc
    integer :: ilopmo, ilopno, lgco, nparin, npario
!
! DEB-------------------------------------------------------------------
!
    modloc = iamloc - 1 + zi(ilmloc-1+modelo)
    digde2 = zi(modloc-1+3)
end function
