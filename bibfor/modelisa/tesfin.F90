subroutine tesfin(icl, iv, cv, irteti)
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
    implicit none
!       ----------------------------------------------------------------
!       TESTE SI  L ITEM LUT EN DEBUT DE LIGNE EST
!       LE MOT CLE FIN OU LE MOT CLE FINSF ( EVENTUALITE )
!       ----------------------------------------------------------------
!       IN      ICL     =       CLASSE ITEM
!               IV      =       TAILLE ITEM CARACTERE
!               CV      =       ITEM A TESTER
!       OUT     ( RETURN )      MOT CLE FIN ET FINSF NON RECONNUS
!               ( RETURN 1 )    MOT CLE FIN RECONNU
!               ( RETURN 2 )    MOT CLE FINSF RECONNU
!       ----------------------------------------------------------------
    integer :: icl, iv
    character(len=*) :: cv
    character(len=8) :: mcl
!-----------------------------------------------------------------------
    integer :: irteti
!-----------------------------------------------------------------------
    irteti = 0
!
    if (icl .eq. 3 .and. iv .le. 8) then
        mcl='        '
        mcl(1:iv)=cv(1:iv)
        if (mcl .eq. 'FIN     ') then
            irteti = 1
            goto 9999
        endif
        if (mcl .eq. 'FINSF   ') then
            irteti = 2
            goto 9999
        endif
    endif
!
    irteti = 0
9999  continue
end subroutine
