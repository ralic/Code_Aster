subroutine nmcoru(vresi, vresid, convok)
!
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
! person_in_charge: mickael.abbas at edf.fr
!
    implicit     none
    real(kind=8) :: vresi, vresid
    logical :: convok
!
! ----------------------------------------------------------------------
!
! ROUTINE MECA_NON_LINE (ALGORITHME - CONVERGENCE)
!
! VERIFICATION DES CRITERES D'ARRET SUR RESIDU - OPTION PIC
!
! ----------------------------------------------------------------------
!
!
! IN  VRESI  : NORME MAXI DU RESIDU A EVALUER
! IN  VRESID : DONNEE UTILISATEUR POUR CONVERGENCE
! OUT CONVOK . .TRUE. SI CRITERE RESPECTE
!
! ----------------------------------------------------------------------
!
    convok = .true.
    if ((vresi.gt.vresid) .or. (vresi.lt.0.d0)) then
        convok = .false.
    endif
end subroutine
