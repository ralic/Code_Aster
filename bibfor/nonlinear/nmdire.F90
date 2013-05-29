subroutine nmdire(noeu1, noeu2, ndim, cnsln, grln,&
                  grlt, compo, vect)
!
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
!   1 AVENUE DU GENERAL DE GAULLE, 92141 CLAMART CEDEX, FRANCE.
! ======================================================================
    implicit none
    include 'jeveux.h'
    include 'asterc/r8prem.h'
    include 'asterfort/jeveuo.h'
    include 'asterfort/provec.h'
    integer :: noeu1, noeu2, ndim
    character(len=19) :: cnsln, grln, grlt
    character(len=8) :: compo
    real(kind=8) :: vect(3)
!
! ----------------------------------------------------------------------
! CALCUL DIRECTION DE PILOTAGE DNOR, DTAN OU DTAN2
! FORMULATION XFEM
! ----------------------------------------------------------------------
!
!
! IN  NOEU1  : EXTREMITE 1 ARETE PILOTEE
! IN  NOEU1  : EXTREMITE 2 ARETE PILOTEE
! IN  NDIM   : DIMENSION PROBLEME
! IN  CNSLSN : NOM CHAM_NO_S LEVEL SET NORMALE
! IN  GRLN   : NOM CHAM_NO_S GRADIENT LEVEL SET NORMALE
! IN  GRLT   : NOM CHAM_NO_S GRADIENT LEVEL SET TANGENTIELLE
! IN  COMPO  : DIRECTION A PILOTER
! OUT  VECT  : VECTEUR NORME DE CETTE DIRECTION DANS LA BASE FIXE
!
!
!
!
    integer :: jgrnno, jlsn, jgrtno, i
    real(kind=8) :: norm(3), tang(3), normn
    real(kind=8) :: tang2(3), normt
    real(kind=8) :: lsn1, lsn2, eps
!
!
    call jeveuo(cnsln//'.CNSV', 'E', jlsn)
    call jeveuo(grln//'.CNSV', 'E', jgrnno)
    call jeveuo(grlt//'.CNSV', 'E', jgrtno)
    lsn1 = zr(jlsn-1+noeu1)
    lsn2 = zr(jlsn-1+noeu2)
    eps=r8prem()
    norm(1)=0.d0
    norm(2)=0.d0
    norm(3)=0.d0
    tang(1)=0.d0
    tang(2)=0.d0
    tang(3)=0.d0
!
    if ((abs(lsn1).le.eps) .and. (abs(lsn2).le.eps)) then
        do 95 i = 1, ndim
            norm(i) = norm(i)+ zr(jgrnno-1+ndim*(noeu1-1)+i)
95      continue
    else
        do 91 i = 1, ndim
            norm(i) = norm(i)+ abs(lsn1)*zr(jgrnno-1+ndim*(noeu2-1)+i) + abs(lsn2)*zr(jgrnno-1+nd&
                      &im*(noeu1-1)+i)
91      continue
    endif
    normn=sqrt(norm(1)**2+norm(2)**2+norm(3)**2)
    norm(1)=norm(1)/normn
    norm(2)=norm(2)/normn
    norm(3)=norm(3)/normn
!
    if (compo(1:4) .eq. 'DTAN') then
        if (ndim .eq. 2) then
            tang(1)=norm(2)
            tang(2)=-norm(1)
        else if (ndim.eq.3) then
            tang2(1)=1.d0
            tang2(2)=0.d0
            tang2(3)=0.d0
            call provec(norm, tang2, tang)
            normt=sqrt(tang(1)**2+tang(2)**2+tang(3)**2)
            if (normt .le. eps) then
                tang(1)=0.d0
                tang(2)=1.d0
                tang(3)=0.d0
            else
                tang(1)=tang(1)/normt
                tang(2)=tang(2)/normt
                tang(3)=tang(3)/normt
            endif
            call provec(norm, tang, tang2)
        endif
    endif
!
    do 92 i = 1, ndim
        if (compo .eq. 'DNOR') then
            vect(i)=norm(i)
        else if (compo.eq.'DTAN2') then
            vect(i)=tang2(i)
        else if (compo.eq.'DTAN') then
            vect(i)=tang(i)
        endif
92  continue
end subroutine
