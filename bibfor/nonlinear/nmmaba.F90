subroutine nmmaba(icodma, compor, e, dsde, sigy,&
                  ncstpm, cstpm)
    implicit none
#include "asterfort/r8inir.h"
#include "asterfort/rcvalb.h"
#include "asterfort/utmess.h"
    character(len=16) :: compor
    integer :: icodma
    integer :: ncstpm
    real(kind=8) :: cstpm(ncstpm)
    real(kind=8) :: e, dsde, sigy
! ----------------------------------------------------------------------
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
!     RECUPERATION DES CARACTERISTIQUES DU MATERIAU POUR LES ELEMENTS
!     MECA_BARRE DANS LE CAS DES COMPORTEMENTS NON LINEAIRES.
! ----------------------------------------------------------------------
!
! IN  : ICODMA : NUMERO DU MATERIAU CODE
!       COMPOR : NOM DE LA RELATION DE COMPORTEMENT
!
! OUT : E      : MODULE D'YOUNG
!       DSDE   : PENTE D'ECROUISSAGE
!       SIGY   : LIMITE ELASTIQUE POUR L,ECROUISSAGE LINEAIRE
!       NCSTPM : NOMBRE DE CONSTANTES DE MATERIAU POUR INTO
!       CSTPM  : CONSTANTES DE MATERIAU :
!           E      : MODULE D'YOUNG
!           SY     : LIMITE ELASTIQUE
!           EPSU   : DEFORMATION ULTIME
!           SU     : CONTRAINTE ULTIME
!           EPSH   : DEFORMATION A LA FIN DU PALIER PLASTIQUE PARFAIT
!           R0     : COEFFICIENT EXPERIMENTAL
!           B      : COEFFICIENT
!           A1     : COEFFICIENT EXPERIMENTAL
!           A2     : COEFFICIENT EXPERIMENTAL
!           ELAN   : RAPPORT LONGUEUR/DIAMETRE DE LA BARRE
!           A6     : COEFFICIENT EXPERIMENTAL FLAMMBAGE
!           C      : COEFFICIENT EXPERIMENTAL FLAMMBAGE
!           COA    : COEFFICIENT EXPERIMENTAL FLAMMBAGE
!
!
! *************** DECLARATION DES VARIABLES LOCALES ********************
!
!-----------------------------------------------------------------------omecl
    integer :: nbpar, nbres, nbval
!-----------------------------------------------------------------------
    parameter    (nbval = 12)
    real(kind=8) :: valpar, valres(nbval)
    integer :: codres(nbval)
    character(len=8) :: nompar, nomela(1) 
    character(len=16) :: nomecl(2), nompim(12)
    character(len=4) :: fami
!
! *********** FIN DES DECLARATIONS DES VARIABLES LOCALES ***************
!
! ****************************** DATA  *********************************
!
    data nomela / 'E' /
    data nomecl / 'D_SIGM_EPSI', 'SY' /
    data nompim / 'SY','EPSI_ULTM','SIGM_ULTM','ELAN','EPSP_HARD',&
     &            'R_PM','EP_SUR_E', 'A1_PM','A2_PM','A6_PM',&
     &            'C_PM','A_PM' /
!
! ********************* DEBUT DE LA SUBROUTINE *************************
!
! --- MESSAGE D'ERREUR SI COMPORTEMENT NON REPERTORIE POUR LES BARRES
!
    if ((compor.ne.'ELAS') .and. (compor.ne.'VMIS_ISOT_LINE') .and.&
        (compor.ne.'VMIS_CINE_LINE') .and. (compor.ne.'VMIS_ASYM_LINE') .and.&
        (compor.ne.'PINTO_MENEGOTTO') .and. (compor.ne.'GRILLE_CINE_LINE') .and.&
        (compor.ne.'GRILLE_ISOT_LINE') .and. (compor.ne.'GRILLE_PINTO_MEN')) then
        call utmess('F', 'ELEMENTS_32', sk=compor)
    endif
!
! --- INITIALISATIONS
!
    fami = 'RIGI'
!
    call r8inir(nbval, 0.d0, valres, 1)
    nbpar = 0
    nompar = '  '
    valpar = 0.d0
!
! --- CARACTERISTIQUES ELASTIQUES
!
    nbres = 2
    call rcvalb(fami, 1, 1, '+', icodma,&
                ' ', 'ELAS', nbpar, nompar, [valpar],&
                1, nomela, valres, codres, 1)
    e = valres(1)
!
! --- CARACTERISTIQUES ECROUISSAGE LINEAIRE
!
    if ((compor.eq.'VMIS_ISOT_LINE') .or. (compor.eq.'VMIS_CINE_LINE') .or.&
        (compor.eq.'GRILLE_CINE_LINE') .or. (compor.eq.'GRILLE_ISOT_LINE')) then
        nbres= 2
!
!
        call r8inir(nbval, 0.d0, valres, 1)
        nbpar = 0
        nompar = '  '
        valpar = 0.d0
        call rcvalb(fami, 1, 1, '+', icodma,&
                    ' ', 'ECRO_LINE', nbpar, nompar, [valpar],&
                    1, nomecl, valres, codres, 1)
        call rcvalb(fami, 1, 1, '+', icodma,&
                    ' ', 'ECRO_LINE', nbpar, nompar, [valpar],&
                    1, nomecl(2), valres(2), codres(2), 0)
        if (codres(2) .ne. 0) valres(2) = 0.d0
        dsde = valres(1)
        sigy = valres(2)
    endif
!
! --- CARACTERISTIQUES MODELE PINTO MENEGOTTO
!
    if ((compor.eq.'PINTO_MENEGOTTO') .or. (compor.eq.'GRILLE_PINTO_MEN')) then
!
        nbres= 12
!
!
        call r8inir(nbval, 0.d0, valres, 1)
        nbpar = 0
        nompar = '  '
        valpar = 0.d0
!
        call rcvalb(fami, 1, 1, '+', icodma,&
                    ' ', 'PINTO_MENEGOTTO', nbpar, nompar, [valpar],&
                    nbres, nompim, valres, codres, 0)
        if (codres(7) .ne. 0) valres(7) = -1.d0
        cstpm(1) =e
        cstpm(2) =valres(1)
        cstpm(3) =valres(2)
        cstpm(4) =valres(3)
        cstpm(10) =valres(4)
        cstpm(5) =valres(5)
        cstpm(6) =valres(6)
        cstpm(7) =valres(7)
        cstpm(8) =valres(8)
        cstpm(9) =valres(9)
        cstpm(11) =valres(10)
        cstpm(12) =valres(11)
        cstpm(13) =valres(12)
!
    endif
!
end subroutine
