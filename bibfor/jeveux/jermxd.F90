subroutine jermxd(rval, iret)
    implicit none
    include 'asterfort/u2mesg.h'
    real(kind=8) :: rval
    integer :: iret
!            CONFIGURATION MANAGEMENT OF EDF VERSION
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
! person_in_charge: j-pierre.lefebvre at edf.fr
! ----------------------------------------------------------------------
! REDUIT LA VALEUR LIMITE MAXIMUM DE LA MEMOIRE ALLOUEE DYNAMIQUEMENT
! PAR JEVEUX
!
! IN   RVAL : NOUVELLE LIMITE EN OCTETS
!
! OUT  IRET : CODE RETOUR
!             = 0   LA NOUVELLE VALEUR EST PRISE EN COMPTE
!             =/= 0 IL N'A PAS ETE POSSIBLE D'AFFECTER LA
!                   NOUVELLE VALEUR ET ON RENVOIE LA VALEUR DE LA
!                   MEMOIRE OCCUPEE PAR JEVEUX
!
! DEB ------------------------------------------------------------------
    integer :: lbis, lois, lols, lor8, loc8
    common /ienvje/  lbis , lois , lols , lor8 , loc8
    real(kind=8) :: mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio
    common /r8dyje/ mxdyn, mcdyn, mldyn, vmxdyn, vmet, lgio(2)
! ----------------------------------------------------------------------
    real(kind=8) :: rv(2)
!
    if (rval .le. 0) then
        rv(1)=mcdyn*lois/(1024*1024)
        rv(2)=rval/(1024*1024)
        call u2mesg('F', 'JEVEUX1_72', 0, ' ', 0,&
                    0, 2, rv)
    endif
! ON EVALUE LA VALEUR PASSEE EN ARGUMENT PAR RAPPORT A L'OCCUPATION
! TOTALE COURANTE JEVEUX (OBJETS UTILISÉS)
!
    if (mcdyn*lois .lt. rval) then
        vmxdyn = rval/lois
        iret = 0
    else
        iret = max(1, int(mcdyn))
    endif
! FIN ------------------------------------------------------------------
end subroutine
