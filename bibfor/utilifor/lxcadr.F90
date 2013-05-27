subroutine lxcadr(chaine)
    implicit none
    character(len=*) :: chaine
!
!     ------------------------------------------------------------------
!      CADRAGE A GAUCHE D'UN TEXTE
!     ------------------------------------------------------------------
! VAR CHAINE CHAINE A CADRER
!     ------------------------------------------------------------------
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
!     ------------------------------------------------------------------
!     ROUTINE(S) UTILISEE(S) :
!         -
!     ROUTINE(S) FORTRAN     :
!         LEN
!     ------------------------------------------------------------------
! FIN LXCADR
!     ------------------------------------------------------------------
!
!
!-----------------------------------------------------------------------
    integer :: ilong, ldec, long
!-----------------------------------------------------------------------
    long = len(chaine)
    do 10 ilong = 1, long
        if (chaine(ilong:ilong) .ne. ' ') then
            ldec = ilong-1
            goto 11
        endif
10  end do
!     --- CAS DE LA CHAINE BLANCHE ---
    ldec = 0
!     --- CAS STANDARD ---
11  continue
    if (ilong .gt. 0) then
        do 20 ilong = 1, long-ldec
            chaine(ilong:ilong) = chaine(ilong+ldec:ilong+ldec)
20      continue
        chaine(long-ldec+1:) = ' '
    endif
end subroutine
