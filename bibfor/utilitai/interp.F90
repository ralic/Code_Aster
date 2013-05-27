subroutine interp(tabx, taby, necr, x, y,&
                  iseg)
!     ----------------------------------------------------------------
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
!     ----------------------------------------------------------------
!     AUTEUR : C. DUVAL DEPT AMV
!     ----------------------------------------------------------------
!
!     BUT: FAIT L'INTERPOLATION LINEAIRE ENTRE 2 POINTS D'UNE FONCTION
!
!     ----------------------------------------------------------------
!
!     TABX,TABY /IN/:PARAMETRES ET VALEURS DE LA FONCTION
!     NECR         /IN/:NOMBRE DE POINTS DE LA FONCTION
!     X            /IN/:VALEUR A INTERPOLER
!
!     Y            /OUT/:VALEUR DE LA FONCTION EN X
!     ISEG         /OUT/:RANG DU SEGMENT DE LA FONCTION CONTENANT X
!
!     ----------------------------------------------------------------
!
    implicit none
    real(kind=8) :: tabx(*), taby(*)
    integer :: ipt, iseg, necr
    real(kind=8) :: x, x1, x2, y, y1, y2
!-----------------------------------------------------------------------
    do 1 ,ipt=2,necr
    x1=tabx(ipt-1)
    x2=tabx(ipt)
    if (((x-x1)*(x-x2)) .le. 0.d0) then
        iseg=ipt
        y1=taby(ipt-1)
        y2=taby(ipt)
        if (x1 .eq. x2) then
            y=y1
        else
            y=y1+(x-x1)*(y1-y2)/(x1-x2)
        endif
        goto 9999
    endif
    1 end do
9999  continue
end subroutine
