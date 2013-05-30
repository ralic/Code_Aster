subroutine listap(motfac, iexci, typapp)
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
    include 'asterc/getexm.h'
    include 'asterc/getvtx.h'
    include 'asterfort/assert.h'
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    character(len=16) :: motfac
    integer :: iexci
    character(len=16) :: typapp
!
! ----------------------------------------------------------------------
!
! ROUTINE UTILITAIRE (LISTE_CHARGES)
!
! TYPE D'APPLICATION DE LA CHARGE
!
! ----------------------------------------------------------------------
!
!
! IN  MOTFAC : MOT-CLEF FACTEUR
! IN  IEXCI  : OCCURRENCE DU MOT-CLEF FACTEUR
! IN  TYPAPP : TYPE D'APPLICATION DE LA CHARGE
!              FIXE_CSTE
!              FIXE_PILO
!              SUIV
!              DIDI
!
!
!
!
    integer :: eximc
    integer :: n
!
! ----------------------------------------------------------------------
!
    call jemarq()
!
    eximc = getexm(motfac,'TYPE_CHARGE')
    if (eximc .eq. 1) then
        call getvtx(motfac, 'TYPE_CHARGE', iexci, 1, 1,&
                    typapp, n)
        call assert(n.eq.1)
    else
        typapp = 'FIXE_CSTE'
    endif
!
    call jedema()
end subroutine
