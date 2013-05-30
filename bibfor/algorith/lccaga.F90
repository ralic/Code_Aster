subroutine lccaga(loi, cargau)
    implicit none
!
    character(len=4) :: cargau
    character(len=16) :: loi
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2008  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     ----------------------------------------------------------------
!     CHOIX DES CARACTERISTIQUES
!     ----------------------------------------------------------------
!
!     IN
!         LOI : NOM DE LA LOI DE COMPORTEMENT
!     OUT
!         CARGAUS : CHAINE DE CARACTERES PRECISANT CERTAINES
!                     CARACTERISTIQUES DE LA RESOLUTION DES SYSTEMES
!                     LINEAIRES (1ER ARGUMENT DE MGAUSS)
!     ----------------------------------------------------------------
!
    if (loi .eq. 'VISCOCHAB') then
!
!       METHODE 'S' : SURE
        cargau = 'NCSP'
!
    else
!
!       METHODE 'W' : RATEAU
        cargau = 'NCWP'
!
    endif
!
end subroutine
