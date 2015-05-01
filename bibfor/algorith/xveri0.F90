subroutine xveri0(ndime, elrefp, ksi, iret) 
    implicit none
    character(len=8) :: elrefp
    integer :: iret, ndime
    real(kind=8) :: ksi(*)
!
! ======================================================================
! COPYRIGHT (C) 1991 - 2015  EDF R&D                  WWW.CODE-ASTER.ORG
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
!     BUT: VERIFIER SI LES COORDONNEES DE REFERENCE CALCULEES 
!             SONT DANS LE DOMAINE DE L ELEMENT PARENT DE REFERENCE
!
!     ENTREE
!        NDIME : DIMENSION TOPOLOGIQUE DE L ELEMENT DE REFERENCE
!        ELREFP: TYPE ELEMENT PARENT
!        KSI   : COORDONNEES A VERIFIER
!     SORTIE
!        IRET  : CODE RETOUR : SI IRET = 0 ALORS OK
!                                      > 0 ALORS NOOK
!----------------------------------------------------------------------
    integer :: j
    real(kind=8) :: plan, tole, zero, one
    parameter   (tole=1.d-12)
!INTRODUCTION D UNE TOLERENCE GEOMETRIQUE POUR CAPTER LES POINTS SUR LE BORD DE L ELEMENT
!PAS BESOIN DE MISE A L ECHELLE DANS L ESPACE PARAMETRIQUE DE REFERENCE
!----------------------------------------------------------------------
     iret=0
     zero=-tole
     one=1.d0+tole
     if    ((elrefp .eq. 'TE4') .or. (elrefp .eq. 'T10') .or.&
            (elrefp .eq. 'TR3') .or. (elrefp .eq. 'TR6') .or.&
            (elrefp .eq. 'TR7')) then
           plan=1.d0
           do  j=1,ndime
              if(ksi(j) .lt. zero) iret=iret+1
              plan=plan-ksi(j)
           enddo        
           if(plan .lt. zero) iret=iret+1  
     elseif((elrefp .eq. 'PE6') .or. (elrefp .eq. 'P15') .or.&
            (elrefp .eq. 'P18')) then
           if (abs(ksi(1)) .gt. one) iret=iret+1
           if ((ksi(2) .gt. one) .or. (ksi(3) .gt. one)) iret=iret+1
           plan=1.d0-ksi(2)-ksi(3)
           if(plan .lt. zero) iret=iret+1 
     elseif((elrefp .eq. 'PY5') .or. (elrefp .eq. 'P13')) then
           if (ksi(3) .lt. zero) iret=iret+1
           plan=1.d0-ksi(1)-ksi(2)-ksi(3)
           if(plan .lt. zero) iret=iret+1 
           plan=1.d0-ksi(1)+ksi(2)-ksi(3)
           if(plan .lt. zero) iret=iret+1 
           plan=1.d0+ksi(1)-ksi(2)-ksi(3)
           if(plan .lt. zero) iret=iret+1 
           plan=1.d0+ksi(1)+ksi(2)-ksi(3)
           if(plan .lt. zero) iret=iret+1 
     else
        do  j=1,ndime
           if(abs(ksi(j)) .gt. one)  iret=iret+1
        enddo
     endif
end subroutine
