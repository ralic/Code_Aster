subroutine mmexfr(noma, defico, izone, posmam, tau1,&
                  tau2)
! ======================================================================
! COPYRIGHT (C) 1991 - 2011  EDF R&D                  WWW.CODE-ASTER.ORG
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
! person_in_charge: thomas.desoza at edf.fr
    implicit none
    include 'asterc/r8prem.h'
    include 'asterfort/cfdisi.h'
    include 'asterfort/cfnomm.h'
    include 'asterfort/mminfi.h'
    include 'asterfort/mminfr.h'
    include 'asterfort/mmnorm.h'
    include 'asterfort/normev.h'
    include 'asterfort/provec.h'
    include 'asterfort/u2mesg.h'
    include 'blas/daxpy.h'
    include 'blas/dcopy.h'
    include 'blas/ddot.h'
    include 'blas/dscal.h'
    character(len=8) :: noma
    character(len=24) :: defico
    integer :: izone, posmam
    real(kind=8) :: tau1(3), tau2(3)
!
! ----------------------------------------------------------------------
!
! ROUTINE CONTACT (METHODE CONTINUE - APPARIEMENT - UTILITAIRE)
!
! REDEFINIT LA BASE TANGENTE LOCALE POUR SANS_GROUP_NO_FR
!
! ----------------------------------------------------------------------
!
!
! IN  NOMA   : NOM DU MAILLAGE
! IN  DEFICO : SD POUR LA DEFINITION DU CONTACT
! IN  IZONE  : NUMERO DE LA ZONE DE CONTACT
! IN  POSMAM : POSITION DE LA MAILLE MAITRE DANS LES SD CONTACT
! OUT TAU1   : PREMIER VECTEUR TANGENT
! OUT TAU2   : SECOND VECTEUR TANGENT
!
    integer :: ndimg, ndirex
    real(kind=8) :: vdirex(3), norm(3), norme
    real(kind=8) :: tau1fr(3), tau2fr(3)
    real(kind=8) :: extau1, extau2
    character(len=8) :: nommam
!
! ----------------------------------------------------------------------
!
! --- INITIALISATIONS
!
    ndimg = cfdisi(defico,'NDIM')
!
! --- NOMBRE DE DIRECTIONS A EXCLURE POUR LA ZONE
!
    ndirex = mminfi(defico,'EXCL_DIR',izone)
!
! --- REDEFINITION DU REPERE SI NECESSAIRE (UNE DIRECTION EXCLUE EN 3D)
!
    if ((ndimg.eq.3) .and. (ndirex.eq.1)) then
! ----- DIRECTION D'EXCLUSION
        vdirex(1) = mminfr(defico,'EXCL_FROT_DIRX',izone )
        vdirex(2) = mminfr(defico,'EXCL_FROT_DIRY',izone )
        vdirex(3) = mminfr(defico,'EXCL_FROT_DIRZ',izone )
! ----- ON LA PROJETTE SUR LE PLAN TANGENT
        call dcopy(3, tau1, 1, tau1fr, 1)
        call dcopy(3, tau2, 1, tau2fr, 1)
        extau1 = ddot(3,vdirex,1,tau1,1)
        extau2 = ddot(3,vdirex,1,tau2,1)
        call dscal(3, extau1, tau1fr, 1)
        call daxpy(3, extau2, tau2fr, 1, tau1fr,&
                   1)
        call normev(tau1fr, norme)
        if (norme .le. r8prem()) then
            call cfnomm(noma, defico, 'MAIL', posmam, nommam)
            call u2mesg('F', 'CONTACT3_18', 1, nommam, 1,&
                        izone, 3, vdirex)
        endif
! ----- ON CALCULE TAU2FR PAR PROD. VECT.
        call mmnorm(ndimg, tau1, tau2, norm, norme)
        call provec(tau1fr, norm, tau2fr)
! ----- RECOPIE
        call dcopy(3, tau1fr, 1, tau1, 1)
        call dcopy(3, tau2fr, 1, tau2, 1)
    endif
!
end subroutine
