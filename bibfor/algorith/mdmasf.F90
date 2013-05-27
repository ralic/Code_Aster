subroutine mdmasf(i, dnorm, masgen, nbmode, phicar,&
                  fexgen, accgen, puls2, amogen, coefa)
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
!
!***********************************************************************
! 01/01/91    G.JACQUART AMV/P61 47 65 49 41
!***********************************************************************
!     FONCTION  : CALCULE LA FORCE FLUIDE NORMALE A L'OBSTACLE
!
!-----------------------------------------------------------------------
!                             ARGUMENTS
! .________________.____.______________________________________________.
!        NOCM       MODE                    ROLE
!  ________________ ____ ______________________________________________
!                         VARIABLES DU SYSTEME DYNAMIQUE MODAL
!  ________________ ____ ______________________________________________
!    I              <--   INDICE DU NOEUD DE CHOC
!    DNORM          <--   DISTANCE NORMALE
!    MASGEN         <-->  MATRICE DE MASSE GENERALISEE
!    NBMODE         <--   NOMBRE DE MODES DANS LA BASE
!    PHICAR         <--   PRODUIT PHI T * PHI
!    FEXGEN         <-->  FORCES GENERALISEES
!    ACCGEN          -->  ACCELERATIONS GENERALISEES
!    PULS2           -->  PULSATIONS PROPRES GENERALISEES
!    COEFA           -->  COEFFICIENT DE MASSE AJOUTEE
!-----------------------------------------------------------------------
    implicit none
    integer :: i, nbmode, im
    real(kind=8) :: masgen(*), phicar(*), fexgen(*), accgen(*)
    real(kind=8) :: coefa, dnorm, puls2(*), amogen(*), oldm
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
    do 10 im = 1, nbmode
        oldm = masgen(im)
        masgen(im) = masgen(im) - phicar(im+(i-1)*nbmode)*coefa/dnorm
        fexgen(im) = fexgen(im) - phicar(im+(i-1)*nbmode)* coefa* accgen(im)/dnorm
        puls2(im) = puls2(im)*oldm/masgen(im)
        amogen(im) = amogen(im)*oldm/masgen(im)
10  end do
end subroutine
