subroutine b3d_sigapp(sigef6, d66, siga6, base_prin)
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
! ======================================================================
! person_in_charge: etienne.grimal at edf.fr
!=====================================================================
!      calcul des contraintes apparentes en fonction des contraintes eff
!      variables externes
!=====================================================================
    implicit none
    real(kind=8) :: sigef6(6)
    real(kind=8) :: d66(6, 6)
    real(kind=8) :: siga6(6)
    logical :: base_prin
!      variables locales
    integer :: i, j
    real(kind=8) :: un
    parameter(un=1.d0)
!
!      calcul des contraintes apparentes
    if (base_prin) then
        do i = 1, 6
            siga6(i)=0.d0
            do j = 1, 6
                if (i .eq. j) then
                    siga6(i)=siga6(i)+(un-d66(i,j))*sigef6(j)
                else
                    if ((j.le.3) .and. (i.le.3)) then
                        siga6(i)=siga6(i)-d66(i,j)*sigef6(j)
                    end if
                end if
            end do
        end do
    else
        do i = 1, 6
            siga6(i)=0.d0
            do j = 1, 6
                if (i .eq. j) then
                    siga6(i)=siga6(i)+(un-d66(i,j))*sigef6(j)
                else
                    siga6(i)=siga6(i)-d66(i,j)*sigef6(j)
                end if
            end do
        end do
    end if
end subroutine
