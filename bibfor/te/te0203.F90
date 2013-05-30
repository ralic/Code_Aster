subroutine te0203(option, nomte)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2013  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
! ======================================================================
!
    implicit none
    include 'jeveux.h'
!
    include 'asterfort/jevech.h'
    include 'asterfort/lteatt.h'
    include 'asterfort/pipefi.h'
    include 'asterfort/tecach.h'
    character(len=16) :: nomte, option
!
!-----------------------------------------------------------------------
!
!     BUT: PILOTAGE POUR LES ELEMENTS DE JOINT
!
!     OPTION : PILO_PRED_ELAS
!
!-----------------------------------------------------------------------
!
!
!
    integer :: igeom, imater, ideplm, ivarim, npg, jtab(7), iret, lgpg
    integer :: iddepl, idepl0, idepl1, ictau, icopil
    character(len=8) :: typmod(2)
!
!    PARAMETRES DE L'ELEMENT FINI
    npg=2
!
! - PARAMETRES EN ENTREE
    call jevech('PGEOMER', 'L', igeom)
    call jevech('PMATERC', 'L', imater)
    call jevech('PDEPLMR', 'L', ideplm)
    call jevech('PVARIMR', 'L', ivarim)
    call jevech('PDDEPLR', 'L', iddepl)
    call jevech('PDEPL0R', 'L', idepl0)
    call jevech('PDEPL1R', 'L', idepl1)
    call jevech('PCDTAU', 'L', ictau)
!
    if (lteatt(' ','AXIS','OUI')) then
        typmod(1) = 'AXIS'
    else
        typmod(1) = 'PLAN'
    endif
    typmod(2) = 'ELEMJOIN'
!
! RECUPERATION DU NOMBRE DE VARIABLES INTERNES PAR POINTS DE GAUSS :
    call tecach('OON', 'PVARIMR', 'L', 7, jtab,&
                iret)
    lgpg = max(jtab(6),1)*jtab(7)
!
! PARAMETRE EN SORTIE
!
    call jevech('PCOPILO', 'E', icopil)
!
    call pipefi(npg, lgpg, zi(imater), zr(igeom), zr(ivarim),&
                zr(iddepl), zr(ideplm), zr(idepl0), zr(idepl1), zr(ictau),&
                zr(icopil), typmod)
!
end subroutine
