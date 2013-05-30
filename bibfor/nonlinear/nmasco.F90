subroutine nmasco(typvec, fonact, defico, veasse, cncont)
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
    include 'asterfort/cfdisl.h'
    include 'asterfort/isfonc.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    include 'asterfort/nmchex.h'
    include 'asterfort/vtaxpy.h'
    include 'asterfort/vtzero.h'
    character(len=6) :: typvec
    integer :: fonact(*)
    character(len=24) :: defico
    character(len=19) :: veasse(*)
    character(len=19) :: cncont
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CALCUL)
!
! CONSTRUCTION DU VECTEUR DES FORCES VARIABLES LIEES AU CONTACT
!
! ----------------------------------------------------------------------
!
!
! IN  TYPVEC : TYPE DE VECTEUR APPELANT
!                'CNFINT' - FORCES INTERNES
!                'CNDIRI' - REACTIONS D'APPUI
! IN  FONACT : FONCTIONNALITES ACTIVEES (VOIR NMFONC)
! IN  DEFICO : SD DEFINITION CONTACT
! IN  VEASSE : VARIABLE CHAPEAU POUR NOM DES VECT_ASSE
! OUT CNCONT : VECT_ASSE DES CONTRIBUTIONS DE CONTACT/FROTTEMENT (C/F)
!               C/F METHODE CONTINUE
!               C/F METHODE XFEM
!               C/F METHODE XFEM GRDS GLIS.
!               F   METHODE DISCRETE
!
!
!
!
    logical :: leltc, leltf, lctfd, lpenac, lallv
    integer :: ifdo, n
    character(len=19) :: vect(20)
    real(kind=8) :: coef(20)
    character(len=19) :: cnctdf, cneltc, cneltf
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
! --- INITIALISATIONS
!
    ifdo = 0
    call vtzero(cncont)
!
! --- FONCTIONNALITES ACTIVEES
!
    leltc = isfonc(fonact,'ELT_CONTACT')
    leltf = isfonc(fonact,'ELT_FROTTEMENT')
    lctfd = isfonc(fonact,'FROT_DISCRET')
    lpenac = cfdisl(defico,'CONT_PENA')
    lallv = isfonc(fonact,'CONT_ALL_VERIF' )
!
! --- FORCES DE FROTTEMENT DISCRET
!
    if (typvec .eq. 'CNDIRI') then
        if (((lctfd).or.(lpenac)) .and. (.not.lallv)) then
            call nmchex(veasse, 'VEASSE', 'CNCTDF', cnctdf)
            ifdo = ifdo + 1
            coef(ifdo) = 1.d0
            vect(ifdo) = cnctdf
        endif
    endif
!
! --- FORCES DES ELEMENTS DE CONTACT (XFEM+CONTINUE)
!
    if (typvec .eq. 'CNFINT') then
        if (leltc .and. (.not.lallv)) then
            call nmchex(veasse, 'VEASSE', 'CNELTC', cneltc)
            ifdo = ifdo + 1
            coef(ifdo) = 1.d0
            vect(ifdo) = cneltc
        endif
        if (leltf .and. (.not.lallv)) then
            call nmchex(veasse, 'VEASSE', 'CNELTF', cneltf)
            ifdo = ifdo + 1
            coef(ifdo) = 1.d0
            vect(ifdo) = cneltf
        endif
    endif
!
! --- VECTEUR RESULTANT
!
    do 10 n = 1, ifdo
        call vtaxpy(coef(n), vect(n), cncont)
10  end do
!
    call jedema()
end subroutine
