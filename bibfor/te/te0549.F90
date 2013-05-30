subroutine te0549(option, nomte)
    implicit     none
    include 'jeveux.h'
!
    include 'asterc/r8vide.h'
    include 'asterfort/elref4.h'
    include 'asterfort/jevech.h'
    include 'asterfort/posvar.h'
    character(len=16) :: option, nomte
! ======================================================================
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
! ======================================================================
!    - FONCTION REALISEE:  EXTRACTION DES VARIABLES INTERNES EN THM
!    - ARGUMENTS:
!        DONNEES:      OPTION       -->  OPTION DE CALCUL
!                      NOMTE        -->  NOM DU TYPE ELEMENT
! ======================================================================
! ======================================================================
    integer :: ndim, nno, nnos, npg, ipoids, ivf, idfde, jgano
    integer :: ichg, icompo, ichgs, nume, i, ncmp, inova
! ======================================================================
! --- SELECTION DU TYPE D'INTEGRATION ----------------------------------
! ======================================================================
    call elref4(' ', 'RIGI', ndim, nno, nnos,&
                npg, ipoids, ivf, idfde, jgano)
!
    call jevech('PNOVARI', 'L', inova)
    call jevech('PCOMPOR', 'L', icompo)
!
    if (option .eq. 'VAEX_ELGA') then
!
        call jevech('PVARIGR', 'L', ichg)
        call jevech('PVARIGS', 'E', ichgs)
!
        call posvar(zk16(icompo), ndim, zk24(inova), nume)
!
        read (zk16(icompo+1),'(I16)') ncmp
!
        if (nume .gt. 0) then
            do 30 i = 1, npg
                zr(ichgs-1+i)=zr(ichg-1+(i-1)*ncmp+nume)
30          continue
        else
            do 40 i = 1, npg
                zr(ichgs-1+i)=r8vide()
40          continue
        endif
!
    else if (option.eq.'VAEX_ELNO') then
!
        call jevech('PVARINR', 'L', ichg)
        call jevech('PVARINS', 'E', ichgs)
!
        call posvar(zk16(icompo), ndim, zk24(inova), nume)
!
        read (zk16(icompo+1),'(I16)') ncmp
!
        if (nume .gt. 0) then
            do 50 i = 1, nno
                zr(ichgs-1+i)=zr(ichg-1+(i-1)*ncmp+nume)
50          continue
        else
            do 60 i = 1, nno
                zr(ichgs-1+i)=r8vide()
60          continue
        endif
!
    endif
!
end subroutine
