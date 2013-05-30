subroutine caraco(char, nomo, motfac, nzoco, iform)
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
    include 'jeveux.h'
    include 'asterfort/caralv.h'
    include 'asterfort/caramx.h'
    include 'asterfort/cazoco.h'
    include 'asterfort/cazocp.h'
    include 'asterfort/cazofm.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    character(len=8) :: char, nomo
    character(len=16) :: motfac
    integer :: nzoco, iform
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (TOUTES METHODES - LECTURE DONNEES)
!
! LECTURE DES PRINCIPALES CARACTERISTIQUES DU CONTACT (SURFACE IREAD)
! REMPLISSAGE DE LA SD 'DEFICO' (SURFACE IWRITE)
!
! ----------------------------------------------------------------------
!
!
! IN  CHAR   : NOM UTILISATEUR DU CONCEPT DE CHARGE
! IN  NOMO   : NOM DU MODELE
! IN  MOTFAC : MOT-CLE FACTEUR (VALANT 'CONTACT')
! IN  NZOCO  : NOMBRE DE ZONES DE CONTACT
! IN  IFORM  : TYPE DE FORMULATION (DISCRETE/CONTINUE/XFEM)
!
!
!
!
    integer :: izone
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- CREATION DES SD DEFINITION DU CONTACT
!
    call caramx(char, iform, nzoco)
!
! --- AFFECTATION FORMULATION/METHODE DE CONTACT
!
    call cazofm(char, motfac, iform, nzoco)
!
! --- LECTURE PARAMETRES GENERAUX
!
    call cazocp(char)
!
! --- LECTURE DES DONNEES PAR ZONE
!
    do 8 izone = 1, nzoco
        call cazoco(char, nomo, motfac, iform, izone,&
                    nzoco)
 8  end do
!
! --- QUELQUES PARAMETRES GLOBAUX
!
    call caralv(char, nzoco, iform)
!
    call jedema()
!
end subroutine
