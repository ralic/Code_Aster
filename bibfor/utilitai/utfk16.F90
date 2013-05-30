subroutine utfk16(lk16, nbk16, k16, ipos)
    implicit none
    character(len=16) :: lk16(*), k16
    integer :: nbk16, ipos
! ======================================================================
! COPYRIGHT (C) 1991 - 2001  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!     ==================================================================
!     ! UTILITAIRE - RECHERCHE DE LA POSITION D'UNE CHAINE K16 DANS UNE!
!     ! LISTE DE CHAINE K16                                          RM!
!     ==================================================================
!     !                                                                !
!     !    RECHERCHE DE LA PREMIERE OCCURENCE DE LA CHAINE K16 DANS LA !
!     !    LISTE DE K16 DONT ON CONNAIT LA LONGUEUR.                   !
!     !    LA ROUTINE RENVOIT LA POSITION DE LA CHAINE DANS LA LISTE   !
!     !    SI LA CHAINE EST ELEMENT DE LA LISTYE ET 0 SINON            !
!     !                                                                !
!     ==================================================================
! IN  ! LK16   ! K16 ! LISTE DE K16                                    !
! IN  ! NBK16  ! IS  ! TAILLE DE LA LISTE LK16                         !
! IN  ! K16    ! K16 ! CHAINE A POSITIONNER DANS LK16                  !
! OUT ! IPOS   ! IS  ! POSITION DANS LK16 DE L'OCCURENCE 1 DE K16      !
!     ==================================================================
!
! --- VARIABLES LOCALES ---
    logical :: trouve, fini
    integer :: i
!
! ====================== DEBUT DU PROGRAMME ============================
!
!-DBG WRITE(6,*)'======= UTFK16 : IN  ======'
    trouve = .false.
    fini = .false.
    i = 1
100  continue
    if (.not. fini) then
        if (lk16(i) .eq. k16) then
            trouve = .true.
            fini = .true.
        else
            i = i + 1
            if (i .gt. nbk16) then
                fini = .true.
            endif
        endif
        goto 100
    endif
    if (trouve) then
        ipos = i
    else
        ipos = 0
    endif
!-DBG WRITE(6,*)'======= UTFK16 : OUT ======'
!
end subroutine
