subroutine elg_nllspc(nbnz, ctemp, mat)
    implicit none
! person_in_charge: mathieu.corus at edf.fr
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
!
!-- CALCUL DU NOYAU D'UN VECTEUR :
!--
!-- CALCUL D'UNE BASE DU NOYAU DE LA CONTRAINTE CONTENUE DANS CTEMP
!-- APPROCHE ITERATIVE, QUI NE REMPLIT QU'UNE TRIANGULAIRE SUPERIEURE
!--
!--  IN : NBNZ   : LONGUEUR DU VECTEUR CTEMP
!--  IN : CTEMP  : VECTEUR CONTENANT LES TERMES DE LA COMBINAISON LIN.
!--  INOUT : MAT : MATRICE RECEVANT LA BASE DU NOYAU
!--
#include "jeveux.h"
#include "asterc/r8prem.h"
#include "asterfort/assert.h"
#include "blas/ddot.h"
!
!
    integer :: nbnz
    real(kind=8) :: ctemp(nbnz), mat(nbnz, nbnz)
!
    integer :: i1, j1, indz, unit
    real(kind=8) :: norm, val, cmax, prec
!
    prec = r8prem()
    unit=6
!
   mat(:,:) = 0.d0
!
    i1=1
100 continue
    if (abs(ctemp(i1)) .gt. prec) then
        indz=i1
        cmax=abs(ctemp(i1))
    else
        i1=i1+1
        goto 100
    endif
!
    if (indz .eq. 1) then
        mat(1,1)=0.d0
    else
        mat(1,1)=1.d0
    endif
!
    do i1 = 2, nbnz
!
        if (abs(ctemp(i1)) .lt. prec) then
            mat(i1,i1)=1.d0
!
        else if (i1 .eq. indz) then
            mat(i1,i1)=0.d0
!
        else
            if (i1 .lt. indz) then
                mat(1,i1)=1.d0
            else
                mat(indz,i1)=1.d0
            endif
!
            do j1 = 1, i1-1
                if (abs(mat(j1,j1)) .gt. prec) then
                    if (abs(mat(j1,i1)) .lt. prec) then
                        val=ddot(j1,mat(1,j1),1,mat(1,i1),1)
                        val=-val / mat(j1,j1)
                        mat(j1,i1)=val
                    endif
                else
                    if (abs(mat(j1,i1)) .lt. prec) then
                        write(unit,*) 'PROBLEME DANS elg_nllspc.F - CAS NON PREVU'
                        ASSERT(.false.)
                    endif
                endif
!
!
            end do
!
            val=ddot(i1-1,mat(1,i1),1,ctemp(1),1)
            mat(i1,i1)=-val/ctemp(i1)
            norm=ddot(i1,mat(1,i1),1,mat(1,i1),1)
!
            norm=sqrt(norm)
            if (norm .gt. prec) then
                do j1 = 1, i1
                    mat(j1,i1)=mat(j1,i1)/norm
                end do
            endif
!
        endif
!
    end do
!
!      WRITE(6,*),'T :'
!      DO 50 I1=1,NBNZ
!        DO 60 J1=1,NBNZ
!C          WRITE(6,*),I1,' - ',J1,':',ZR(MAT+NBNZ*(J1-1)+I1-1)
!          WRITE(6,*),I1,' - ',J1,':',MAT(I1,J1)
!  60    CONTINUE
!  50  CONTINUE
!
end subroutine
