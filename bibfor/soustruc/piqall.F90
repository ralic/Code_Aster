subroutine piqall(x, ret, rit, rep, xmax,&
                  lmax, epsi)
    implicit   none
    real(kind=8) :: x, ret, rit, rep, xmax, lmax, epsi
!-----------------------------------------------------------------------
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
!    1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!     OPERATEUR: "DEFI_GROUP" , MOTCLE FACTEUR "EQUE_PIQUA"
!     AUTEUR Y. WADIER
!
!     REALISE L'ALLONGEMENT DE LA TUBULURE
!
!-----------------------------------------------------------------------
!
    real(kind=8) :: ept, lmin
!     ------------------------------------------------------------------
!
    ept = ret - rit
    lmin = rep + ept
!
    if ((lmin-epsi) .le. x .and. x .le. (lmax+epsi)) then
        x = lmin + (x-lmin)*(xmax-lmin)/(lmax-lmin)
    endif
!
end subroutine
