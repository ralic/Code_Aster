subroutine lisnnn(motfac, iexci, charge)
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
!
    implicit     none
    include 'jeveux.h'
!
    include 'asterc/getexm.h'
    include 'asterc/getvid.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    character(len=16) :: motfac
    integer :: iexci
    character(len=8) :: charge
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! LECTURE DU NOM DE LA CHARGE (PROVENANT DE AFFE_CHAR_*)
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-CLEF FACTEUR DES EXCITATIONS
! IN  IEXCI  : OCCURRENCE DE L'EXCITATION
! OUT CHARGE : NOM DE LA CHARGE (OU DU VECT_ASSE[_GENE])
!
    integer :: nval, iarg
    integer :: eximve, eximvg
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    charge = ' '
!
! --- CHARGE SPECIFIQUE VECT_ASSE OU VECT_ASSE_GENE
!
    eximve = getexm(motfac,'VECT_ASSE')
    if (eximve .eq. 1) then
        call getvid(motfac, 'VECT_ASSE', iexci, iarg, 1,&
                    charge, nval)
        call assert(nval.ge.0)
    endif
!
    eximvg = getexm(motfac,'VECT_ASSE_GENE')
    if (eximvg .eq. 1) then
        call getvid(motfac, 'VECT_ASSE_GENE', iexci, iarg, 1,&
                    charge, nval)
        call assert(nval.ge.0)
    endif
!
! --- CHARGE STANDARD
!
    call getvid(motfac, 'CHARGE', iexci, iarg, 1,&
                charge, nval)
    call assert(nval.ge.0)
!
    call jedema()
end subroutine
