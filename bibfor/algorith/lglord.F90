subroutine lglord(sig1, sig2, sig3)
!
    implicit    none
    include 'asterfort/jedema.h'
    include 'asterfort/jemarq.h'
    real(kind=8) :: sig1, sig2, sig3
! =================================================================
!            CONFIGURATION MANAGEMENT OF EDF VERSION
! ======================================================================
! COPYRIGHT (C) 1991 - 2002  EDF R&D                  WWW.CODE-ASTER.ORG
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
!
!
! ======================================================================
! =================================================================
! --- BUT : ORDONNER LES CONTRAINTES PRINCIPALES ------------------
! ------- : TEL QUE |SIG1| > |SIG2| > |SIG3| ----------------------
! =================================================================
! IN/OUT : SIG1 :  CONTRAINTE MAXIMALE ----------------------------
! ------ : SIG2 :  CONTRAINTE INTERMEDIAIRE -----------------------
! ------ : SIG3 :  CONTRAINTE MINIMALE ----------------------------
! =================================================================
    real(kind=8) :: tmp
! =================================================================
    call jemarq()
! =================================================================
    if (abs(sig3) .gt. abs(sig1)) then
        tmp = sig1
        sig1 = sig3
        sig3 = tmp
    endif
    if (abs(sig2) .gt. abs(sig1)) then
        tmp = sig1
        sig1 = sig2
        sig2 = tmp
    endif
    if (abs(sig3) .gt. abs(sig2)) then
        tmp = sig2
        sig2 = sig3
        sig3 = tmp
    endif
! =================================================================
    call jedema()
! =================================================================
end subroutine
