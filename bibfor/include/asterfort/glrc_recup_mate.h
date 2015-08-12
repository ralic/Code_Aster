!
! COPYRIGHT (C) 1991 - 2015  EDF R&D                WWW.CODE-ASTER.ORG
!
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
! 1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
!
          interface
            subroutine glrc_recup_mate(imate, compor, lrgm, ep, lambda, deuxmu, lamf, deumuf, &
                                       gt, gc, gf, seuil, alpha, alfmc)
              integer, intent(in) :: imate
              character(len=16), intent(in) :: compor
              real(kind=8), optional, intent(out) :: lambda
              real(kind=8), optional, intent(out) :: deuxmu
              real(kind=8), optional, intent(out) :: lamf
              real(kind=8), optional, intent(out) :: deumuf
              real(kind=8), optional, intent(out) :: gt
              real(kind=8), optional, intent(out) :: gc
              real(kind=8), optional, intent(out) :: gf
              real(kind=8), optional, intent(out) :: seuil
              real(kind=8), optional, intent(out) :: alpha
              real(kind=8), optional, intent(out) :: alfmc
              real(kind=8), intent(in) :: ep
              aster_logical, intent(in) :: lrgm
            end subroutine glrc_recup_mate
          end interface
