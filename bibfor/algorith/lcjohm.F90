subroutine lcjohm(imate, resi, rigi, kpi, npg,&
                  nomail, addeme, advico, ndim, dimdef,&
                  dimcon, nbvari, defgem, defgep, varim,&
                  varip, sigm, sigp, drde, ouvh,&
                  retcom)
!
! aslint: disable=W1504
    implicit none
!
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
! ======================================================================
! - VARIABLES D'ENTREE
!
#include "asterfort/rcvalb.h"
#include "asterfort/u2mesg.h"
    integer :: imate, kpi, npg, addeme, advico, ndim, dimdef, dimcon, nbvari
    real(kind=8) :: defgem(dimdef), varim(nbvari), sigm(dimcon)
    character(len=8) :: nomail
    logical :: resi, rigi
!
! - VARIABLES DE SORTIE
!
    integer :: retcom
    real(kind=8) :: defgep(dimdef), varip(nbvari), sigp(dimcon)
    real(kind=8) :: drde(dimdef, dimdef), ouvh
!
! - VARIABLES LOCALES
!
    integer :: ibid, i, kpg, spt
    real(kind=8) :: kni, umc, gamma, kt, clo, para(4), valr(2), tmecn, tmecs
    character(len=8) :: ncra1(4), fami, poum
    integer :: icodre(18)
!
    data ncra1 / 'K','DMAX','GAMMA','KT' /
!
! - RECUPERATION DES PARAMETRES MATERIAU
    fami='FPG1'
    kpg=1
    spt=1
    poum='+'
!
    call rcvalb(fami, kpg, spt, poum, imate,&
                ' ', 'JOINT_BANDIS', 0, ' ', 0.d0,&
                4, ncra1(1), para(1), icodre, 1)
    kni = para(1)
    umc = para(2)
    gamma = para(3)
    kt = para(4)
!
! - MISE A JOUR FERMETURE
    clo = 0.d0
    ouvh = varim(advico)
    clo = umc - ouvh
    clo = clo - defgep(addeme) + defgem(addeme)
!
! - CALCUL CONTRAINTES
!
    if (resi) then
        if ((clo.gt.umc) .or. (clo.lt.-1.d-3)) then
            valr(1) = clo
            valr(2) = umc
            call u2mesg('A', 'ALGORITH17_11', 1, nomail, 0,&
                        ibid, 2, valr)
            retcom = 1
            goto 9000
        endif
!
        ouvh = umc-clo
        varip(advico) = ouvh
!
        do 10 i = 1, dimcon
            sigp(i)=0.d0
10      continue
        sigp(1)=sigm(1) -kni/(1-clo/umc)**gamma*(varim(advico)-varip(&
        advico))
        do 20 i = 2, ndim
            sigp(i)=sigm(i)+kt*(defgep(addeme+1)-defgem(addeme+1))
20      continue
    endif
!
! - CALCUP OPERATEUR TANGENT
!
    if (rigi .and. (kpi .le. npg)) then
        tmecn = kni/(1-clo/umc)**gamma
        tmecs = kt
!
        drde(addeme,addeme)= tmecn
        do 30 i = 2, ndim
            drde(addeme+i-1,addeme+i-1)= tmecs
30      continue
    endif
!
9000  continue
!
end subroutine
