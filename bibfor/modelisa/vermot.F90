subroutine vermot(icl, iv, cv, cnl, ier,&
                  irteti)
    implicit none
!       ----------------------------------------------------------------
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
! ======================================================================
!       ----------------------------------------------------------------
!       VERIFIE QUE L ITEM LUT EST UN MOT ( MOT ATTENDU )
!       DIFFERENT DE FIN OU FINSF (RESERVES)
!       ----------------------------------------------------------------
!       MOT             =       IDENTIFICATEUR (LXSCAN) <= 8 CARACTERES
!       IN      ICL     =       CLASSE ITEM
!               IV      =       TAILLE ITEM CARACTERE
!               CNL     =       NUMERO LIGNE
!       OUT     IER     =       0       > VRAI  ( RETURN )
!                       =       1       > FAUX  ( RETURN 1 )
!       ----------------------------------------------------------------
    include 'asterfort/u2mesk.h'
    integer :: icl, iv, ier
    character(len=14) :: cnl
    character(len=16) :: cmd, nom
    character(len=8) :: mcl
    character(len=*) :: cv
    character(len=24) :: valk(2)
    common          /opmail/        cmd
!
!-----------------------------------------------------------------------
    integer :: irteti, jv
!-----------------------------------------------------------------------
    irteti = 0
    if (icl .ne. 3) then
        if (iv .gt. 16) jv=16
        if (iv .le. 16) jv=iv
        nom = cv(1:jv)
        valk(1) = cnl
        valk(2) = nom(1:jv)
        call u2mesk('E', 'MODELISA7_81', 2, valk)
        ier = 1
        irteti = 1
        goto 9999
    endif
!
    if (iv .gt. 24) then
        call u2mesk('F', 'MODELISA7_82', 1, cnl)
        ier = 1
        irteti = 1
        goto 9999
    endif
!
    mcl = '        '
    mcl(1:iv) = cv(1:iv)
    if (mcl .eq. 'FIN     ') then
        call u2mesk('E', 'MODELISA7_83', 1, cnl)
        ier = 1
        irteti = 1
        goto 9999
    endif
    if (mcl .eq. 'FINSF   ') then
        call u2mesk('E', 'MODELISA7_84', 1, cnl)
        ier = 1
        irteti = 1
        goto 9999
    endif
!
    irteti = 0
9999  continue
end subroutine
