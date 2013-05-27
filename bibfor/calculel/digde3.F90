function digde3(modelo, louc)
    implicit none
    integer :: digde3
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: jacques.pellet at edf.fr
!     ARGUMENTS:
!     ----------
    include 'jeveux.h'
    include 'asterfort/assert.h'
    integer :: modelo
    character(len=1) :: louc
! ----------------------------------------------------------------------
!     ENTREES:
!        MODELO : MODE_LOCAL DE TYPE "MATRICE"
!                 (SON INDICE DANS &CATA.TE.MODELOC )
!        LOUC   : / 'L' : LIGNE
!                 / 'C' : COLONNE
!     SORTIES:
!        DIGDE3 : NOMBRE DE LIGNES (OU COLONNES) DE LA MATRICE
!                 ELEMENTAIRE
!
! ----------------------------------------------------------------------
    integer :: iaoptt, lgco, iaopmo, ilopmo, iaopno, ilopno, iaopds, iaoppa
    integer :: npario, nparin, iamloc, ilmloc, iadsgd
    common /caii02/iaoptt,lgco,iaopmo,ilopmo,iaopno,ilopno,iaopds,&
     &       iaoppa,npario,nparin,iamloc,ilmloc,iadsgd
!
!     VARIABLES LOCALES:
!     ------------------
    integer :: jmodlo, mod1, jmod1
!
! DEB-------------------------------------------------------------------
    call assert(louc.eq.'C' .or. louc.eq.'L')
!
    jmodlo = iamloc - 1 + zi(ilmloc-1+modelo)
    call assert(zi(jmodlo-1+1).eq.5)
!
    if (louc .eq. 'C') then
        mod1=zi(jmodlo-1+4)
    else
        mod1=zi(jmodlo-1+5)
    endif
    jmod1 = iamloc - 1 + zi(ilmloc-1+mod1)
    call assert(zi(jmod1-1+1).eq.2)
    digde3 = zi(jmod1-1+3)
end function
