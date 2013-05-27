subroutine vericp(cmpglo, cmp, nbcmp, iret)
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    implicit none
!
!     CETTE ROUTINE VERIFIE SI LA COMPOSANTE CMP FAIT PARTIE DES COMPO-
!     SANTES CMPGLO DE LA GRANDEUR
!
! IN :  CMPGLO:  LISTE DES COMPOSANTES DE LA GRANDEUR
!       CMP   :  NOM DE LA COMPOSANTE
!       NBCMP :  NOMBRE DE COMPOSANTES DE LA GRANDEUR
!
! OUT : IRET = 0 LA COMPOSANTE CMP APPARTIENT A LA GRANDEUR
!       IRET = 1 LA COMPOSANTE CMP N'APPARTIENT PAS A LA GRANDEUR
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
    character(len=8) :: cmp, cmpglo(1)
    integer :: i, iret, nbcmp
!-----------------------------------------------------------------------
!
    do 1 i = 1, nbcmp
        if (cmp .ne. cmpglo(i)) then
            goto 1
        else
            iret=0
            goto 9999
        endif
 1  continue
!
    iret=1
9999  continue
end subroutine
